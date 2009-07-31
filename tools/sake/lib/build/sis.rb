#
# sis.rb
#
# Copyright 2004-2006 Helsinki Institute for Information Technology
# (HIIT) and the authors. All rights reserved.
#
# Authors: Tero Hasu <tero.hasu@hut.fi>
#
# Contains code for generating SIS and/or PKG files, given a list of
# files to go into the SIS. Does not support all the PKG file
# constructs, and can only deal with some simple (and typical) cases.
# The primary benefits offered by this module are
#
# * the somewhat buggy Linux-version of makesis, available from
#   http://gnupoc.sourceforge.net/ is supported, in addition to
#   support for the makesis versions supplied on SDKs
#
# * if python2.2 is on the path, supports the compilation of .py files
#   into .pyc files, which results in minor load-time speedups (while
#   on a fast machine the build time slowdown is hardly of
#   significance)
#

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

require 'pp'
require 'open4-0.9.1'

class IO
  def puteol
    write("\r\n")
  end
  def putline(line)
    write(line)
    puteol
  end
  def putlines(lines)
    lines.each do |line|
      putline(line)
    end
  end
end

# Notes about "makesis".
#
# The Linux version of "makesis" appears to corrupt its
# memory if it has to deal with source files in subdirectories,
# so we have to work around that.
#
# makesis 2.0.0 creates its temporary directories
# with rw-rw-r-- permissions (same as temporary files),
# which means that "rm -r" has difficulty deleting those
# directories. This is easy to fix in the source code --
# just use mode 0775 instead of 0664 for directories.

module Symbian
  class SisEntry
    attr_accessor :entry_name, :file_type
    attr_reader :pkg_target

    def initialize op
      @op = op
    end

    def set_pkg_target(path)
      @pkg_target = path.gsub(/\//, "\\")
    end

    def set_pkg_target_by_type(path)
      case @file_type
      when :pyd
        if @op[:v9]
          target = '!:/sys/bin/' + path
        else
          target = '!:/system/libs/' + path
        end
      when :py_wrap
        if @op[:v9]
          target = '!:/resource/' + path
        else
          target = '!:/system/libs/' + path
        end
      when :py_lib
        target = '!:/system/libs/' + path
      when :py_prog
        target = '!:/system/apps/python/' + path
      when :py13_lib
        target = '!:/python/lib/' + path
      when :py13_prog
        target = '!:/python/' + path
      when :app
        if @op[:v9]
          raise
        else
          target = '!:/system/apps/%s/%s' % [@op[:appname], path]
        end
      when :other
        target = '!:/' + path
      else
        raise
      end
      set_pkg_target(target)
    end

    def get_data
      File.read(@entry_name)
    end

    def save_as(file)
      data = get_data
      unless nowrite()
        File.open(file, 'w') do |io|
          io.write(data)
        end
      end
    end
  end # SisEntry

  class SisFile
    def initialize(pkg_header, op = {})
      @pkg_header = pkg_header.strip.split(/\r?\n/)
      @entries = []
      @do_compile = have_python_2_2?
      @op = op
    end

    def add(root, path, type)
      source = root + "/" + path
      entry = SisEntry.new @op
      entry.entry_name = source
      entry.file_type = type
      entry.set_pkg_target_by_type(path)
      add_entry(entry)
    end

    def add_entry(entry)
      @entries.push(entry)
    end

    def write_pkg(target_file)
      elist = @entries.collect do |item|
        [item.entry_name.gsub(/\//, "\\"), item.pkg_target]
      end
      elist.sort! do |a, b|
        a[1] <=> b[1]
      end
      unless nowrite()
        internal_write_pkg(target_file, elist)
      end
      puts "wrote " + target_file if verbose()
    end

    def write_sis(target_file, op = {})
      target_file = File.expand_path(target_file)
      list = @entries.dup

      with_tmp_dir do |tmp_dir|
        ## Unpack into a flat structure.
        list.inject(0) do |count, item|
          file = File.join(tmp_dir, "file" + count.to_s)
          item.save_as(file)
          class <<item
            attr_accessor :file
          end
          item.file = file
          count + 1
        end

        ## Compile .py files.
        if @do_compile
          list.each do |item|
            next unless item.file_type == :py_lib
            compile(File.basename(item.entry_name), item.file)
            item.set_pkg_target(item.pkg_target + "c")
            item.file_type = :pyc_lib
          end
        end

        pkg_file = 'file.pkg'
        sis_file = 'file.sis'
        cd(tmp_dir) do
          ## Write .pkg file.
          elist = list.collect do |item|
            [File.basename(item.file), item.pkg_target]
          end
          internal_write_pkg(pkg_file, elist)

          ## Create .sis file.
          sh("cat " + pkg_file) if $print_pkg
          opt = ($verbose_makesis ? " -v" : "")
          shproc = op[:sh] || proc {|*args| sh(*args)}
          cmd = op[:command] || "makesis"
          shproc.call(format("%s%s %s %s", cmd, opt, pkg_file, sis_file))
          sh("ls -l *.sis") if $devel
        end

        cp(File.join(tmp_dir, sis_file), target_file)
      end
    end

    def with_tmp_dir # block
      # We require a temporary directory to place the source files
      # specified in the PKG file somewhere.
      tmp_dir = File.expand_path('tmp-sis-build')

      rm_rf tmp_dir
      mkdir tmp_dir

      begin
        yield tmp_dir
      ensure
        rm_r tmp_dir unless $devel
      end
    end

    private

    def internal_write_pkg(pkg_file, list)
      File.open(pkg_file, 'w') do |output|
        #output.putline('; generated -- do not edit')
        #output.puteol
        output.putlines(@pkg_header)
        output.puteol
        list.each do |item|
          output.putline('"%s" - "%s"' % item)
        end
      end
    end

    def gives_python_2_2? command
      begin
        # Need to use Open4 to catch STDERR.
        Open4::popen4(command) do |cid, i, o, e|
          for line in e
            if line =~ /^Python 2\.2\./
              return true
            end
          end
        end
      rescue
      end
      return false
    end

    def have_python_2_2?
      gives_python_2_2?("python2.2 -V") or
        gives_python_2_2?("python -V")
    end

    # Could use this if wanted to change the magic number.
    def set_magic(file, bytes)
      raise unless bytes.size == 4
      unless nowrite()
        File.open(file, File::RDWR) do |io|
          io.write(bytes)
        end
      end
    end

    # Compiles the specified file to .pyc.
    # A Python source file called ``py_file`` must exist.
    # The created .pyc file will be given the name ``pyc_file``,
    # or if not specified, the name ``py_file``.
    def compile(source_name, py_file, pyc_file = nil)
      pyc_file ||= py_file

      source_name =~ /\.py$/ or raise
      base = $`
      pyofile = base + '.pyo'

      cp(py_file, source_name)
      command = format('python2.2 -OO -c \'%s\'',
        format('import py_compile; py_compile.compile("%s")', source_name))
      sh(command)
      rm(source_name)

      # Not sure if .pyo files are recognized,
      # so let's rename as .pyc.
      mv(pyofile, pyc_file)
    end
  end
end

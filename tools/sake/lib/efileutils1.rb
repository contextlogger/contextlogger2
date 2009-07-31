#
# efileutils1.rb
#
# Extends +fileutils.rb+.
#
# Copyright 2005 Helsinki Institute for Information Technology (HIIT)
# and the authors. All rights reserved.
#
# Authors: Tero Hasu <tero.hasu@hut.fi>
#
# == License
#
# This file contains significant portions of code from +fileutils.rb+,
# and other code that constitutes modifications to +fileutils.rb+. The
# modifications are covered by this license:
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
#
# == +fileutils.rb+
#
# This file contains code copied from +fileutils.rb+ (in the Ruby
# distribution), used under this license:
#
#   Copyright (c) 2000-2005 Minero Aoki <aamine@loveruby.net>
#
#   This program is free software. You can distribute/modify this
#   program under the same terms of ruby.
#
# We meet the requirements of the Ruby license by complying with this
# condition:
#
#   place your modifications in the Public Domain or otherwise
#   make them Freely Available, such as by posting said
#   modifications to Usenet or an equivalent medium, or by allowing
#   the author to include your modifications in the software.
#
# == Related Modules
#
# Related, built-in Ruby modules include: fileutils.rb, ftools.rb,
# pathname.rb, shell.rb, shellwords.rb, tmpdir.rb.
#

require 'fileutils'
require 'rbconfig'
require 'shellwords'
require 'tmpdir'

module EFileUtils1
  module InnerModule
    include FileUtils

    # == +fileutils.rb+ modifications

    # Overridden not to do any command-specific checking; perhaps one
    # wants to pass the same Hash to multiple commands, some of which
    # might not accept all the options that some other commands do.
    # Also, OPT_TABLE is a somewhat generic name; suppose one includes
    # a module that defines a constant with the same name. So we have
    # chosen not to do any command-specific checking. We do call
    # +fui_check_options+, however.
    #
    # options:: The options to check.
    # optdecl:: The allowed options.
    def fu_check_options(options, *optdecl)
      fui_check_options(options)
    end

    # Overridden to do nothing; we want to use +fui_output_message+
    # instead, and more importantly, we want to be passing options to
    # it, so that it knows how to print. It is a shame that we must do
    # this; otherwise there would be few +fileutils.rb+ methods we
    # would need to override; now we must basically override all
    # methods that call this method.
    def fu_output_message(msg)
      # Do nothing.
    end

    # Overriding to add a :dont option, which just executes the block,
    # passing the current working directory as the block parameter
    # (instead of +dir+). Also adding a :noopcall option, which causes
    # the block to be called also in a :noop case, but without
    # changing directories, pretending that the directory was changed,
    # as reflected in the value passed to the block.
    def cd(dir, op = {}, &block) # :yield: dir
      fui_check_options(op)
      if op[:dont]
        block.call(Dir.pwd)
      else
        old_wd = Dir.pwd
        fui_output_message "cd #{to_shell_string(dir)}", op
        if op[:noop]
          if op[:noopcall]
            block.call(File.expand_path(dir))
          end
        else
          Dir.chdir(dir, &block)
        end
        fui_output_message "cd #{to_shell_string(old_wd)}", op if block
      end
    end

    alias chdir cd

    def mkdir(list, op = {})
      fui_puts_cmd("mkdir",
                   if op[:mode] then "-m %03o" % op[:mode] end,
                   list, op)
      super(list, op)
    end

    def mkdir_p(list, op = {})
      fui_puts_cmd("mkdir -p",
                   if op[:mode] then "-m %03o" % op[:mode] end,
                   list, op)
      super(list, op)
    end

    alias mkpath mkdir_p
    alias makedirs mkdir_p

    def rmdir(list, options = {})
      fui_puts_cmd("rmdir", nil, list, options)
      super(list, options)
    end

    def ln(src, dest, op = {})
      fui_puts_cmd("ln",
                   if op[:force] then "-f" end,
                   [src, dest], op)
      super(src, dest, op)
    end

    def ln_s(src, dest, op = {})
      fui_puts_cmd("ln -s",
                   if op[:force] then "-f" end,
                   [src, dest], op)
      super(src, dest, op)
    end

    alias symlink ln_s

    def cp(src, dest, op = {})
      fui_puts_cmd("cp",
                   if op[:preserve] then "-p" end,
                   [src, dest], op)
      super(src, dest, op)
    end

    alias copy cp

    # Overridden to support +notinto+ and +overwrite+; see +mv+.
    def cp_r(src, dest, op = {})
      fui_puts_cmd("cp -r",
                   if op[:preserve] then "-p" end,
                   [src, dest], op)

      return if op[:noop]

      fui_each_src_dest(src, dest, op) do |s, d|
        src_stat = File::lstat(s)
        dest_stat = fu_stat(d)

        if dest_stat
          reply = op[:overwrite]
          if reply == :no
            next
          elsif reply == :error
            raise Errno::EEXIST, dest
          end
        end

        if dest_stat and dest_stat.directory?
          # Directories definitely cannot be overwritten, and shall
          # we say that we will not delete them either.
          raise Errno::EISDIR, dest
        end

        begin
          fui_copy_any_file s, d, op[:preserve]
        rescue SystemCallError
          raise unless op[:force] || op[:failok]
        end
      end
    end

    # Moves file(s) +src+ to +dest+. If +src+ and +dest+ are on
    # different devices, copying and deleting is done transparently,
    # but is obviously less efficient than a move within a device;
    # note that this behavior is not the same as in fileutils.rb. Note
    # that this version of +mv+ supports directories, also unlike
    # fileutils.rb.
    #
    # src:: The file to move; may be a list of files.
    # dest:: The destination; if a directory, the source files are
    #        moved into the directory, unless +notinto+ is in effect,
    #        in which case the move will fail.
    # op:: Options: +verbose+ and +noop+ have their usual semantics;
    #      +failok+ indicates that errors should be silently ignored;
    #      +overwrite+ is one of :yes, :no, and :error, indicating
    #      whether to overwrite an existing destination file (defaults
    #      to :yes). +force+ implies both +failok+ and
    #      +overwrite=yes+.
    def mv(src, dest, op = {})
      fui_check_options(op)
      command = "mv"
      args = []
      args.push("-f") if op[:force]
      args.push("--reply=%s" % op[:overwrite].id2name) if op[:overwrite]
      args.concat([src, dest].flatten)
      string = exec_command_to_string(command, *args)
      fui_output_message(string, op)

      return if op[:noop]

      fui_each_src_dest(src, dest, op) do |s, d|
        src_stat = File::lstat(s)
        dest_stat = fu_stat(d)
        begin
          if dest_stat
            reply = op[:overwrite]
            if reply == :no
              next
            elsif reply == :error
              raise Errno::EEXIST, dest
            end
          end

          if rename_cannot_overwrite_file? and
              dest_stat and not dest_stat.directory?
            # Files cannot be overwritten, so delete first.
            File.unlink d
          end
          if dest_stat and dest_stat.directory?
            # Directories definitely cannot be overwritten, and shall
            # we say that we will not delete them either.
            raise Errno::EISDIR, dest
          end
          begin
            File.rename s, d
          rescue Errno::EXDEV
            # Originally +copy_entry+, which does not support copying
            # of directory contents; directories are copied, but not
            # their contents, which is not acceptable if we are going
            # to be deleting the source tree.
            fui_copy_any_file s, d, true
            # This was missing from +fileutils.rb+, which really
            # should be considered a bug, but appears to be a feature;
            # a strange "move" it is where the source file remains in
            # place.
            fui_delete_any_file(s, op[:force] || op[:failok])
          end
        rescue SystemCallError
          raise unless op[:force] || op[:failok]
        end
      end
    end

    alias move mv

    def rm(list, op = {})
      fui_puts_cmd("rm",
                   if op[:force] then "-f" end,
                   list, op)
      super(list, op)
    end

    alias remove rm

    def rm_r(list, op = {})
      fui_puts_cmd("rm -r",
                   if op[:force] then "-f" end,
                   list, op)
      super(list, op)
    end

    def install(src, dest, op = {})
      fui_puts_cmd("install",
                   [op[:preserve] && "-p",
                     op[:mode] && ("-m 0%o" % op[:mode])],
                   [src, dest], op)
      super(src, dest, op)
    end

    def chmod(mode, list, op = {})
      fui_puts_cmd("chmod",
                   "%o" % mode,
                   list, op)
      super(mode, list, op)
    end

    # Overridden to add some options. :noop and :verbose are retained.
    # :onlyatime indicates that only access time should be updated.
    # :onlymtime indicates that only modification time should be
    # updated. :time may be used to specify a +Time+ to use instead of
    # current time. :nocreate indicates that no file should be created
    # if it does not exist.
    def touch(list, op = {})
      fui_check_options(op)
      list = fu_list(list)
      command = 'touch'
      args = []
      if op[:nocreate]
        args.push('-c')
      end
      if op[:onlyatime]
        args.push('-a')
      end
      if op[:onlymtime]
        args.push('-m')
      end
      if op[:time]
        args += ['-t', op[:time].strftime('%Y%m%d%H%M.%S')]
      end
      args.concat(list)
      string = exec_command_to_string(command, *args)
      fui_output_message(string, op)

      return if op[:noop]

      for fname in list
        atime = nil
        mtime = nil
        if op[:onlyatime] and op[:onlymtime]
          # A bit weird, but let's interpret this so that we update
          # neither time. The Linux "touch" command does not complain
          # either, although seems to behave somewhat differently.
          st = File.stat(fname)
          atime = st.atime
          mtime = st.mtime
        else
          deftime = op[:time] || Time.now
          if op[:onlyatime]
            st = File.stat(fname)
            atime = deftime
            mtime = st.mtime
          elsif op[:onlymtime]
            st = File.stat(fname)
            atime = st.atime
            mtime = deftime
          else
            atime = mtime = deftime
          end
        end

        begin
          File.utime(atime, mtime, fname)
        rescue Errno::ENOENT
          if op[:nocreate]
            raise
          else
            File.open(fname, 'a') {}
            File.utime(atime, mtime, fname)
          end
        end
      end
    end

    # == Compatibility

    # Defined in older versions of fileutils.
    def fu_stat(path)
      File.stat(path)
    rescue SystemCallError
      nil
    end
    private :fu_stat

    # Defined in older versions of fileutils.
    def fu_lstat(path)
      File.lstat(path)
    rescue SystemCallError
      nil
    end
    private :fu_lstat

    # Defined in older versions of fileutils.
    def fu_traverse(prefix, dereference_root = true)   #:nodoc:
      stack = ['.']
      deref = dereference_root
      while rel = stack.pop
        st = File.lstat("#{prefix}/#{rel}")
        if st.directory? and (deref or not st.symlink?)
          stack.concat Dir.entries("#{prefix}/#{rel}")\
          .reject {|ent| ent == '.' or ent == '..' }\
          .map {|ent| "#{rel}/#{ent.untaint}" }.reverse
        end
        yield rel, deref, st
        deref = false
      end
    end
    private :fu_traverse

    # Defined in older versions of fileutils.
    class CopyContext_    #:nodoc: internal use only
      include ::FileUtils

      def initialize(preserve = false, dereference = false, stat = nil)
        @preserve = preserve
        @dereference = dereference
        @stat = stat
      end

      def copy_entry(src, dest)
        preserve(src, dest) {
          _copy_entry src, dest
        }
      end

      def copy_content(src, dest)
        preserve(src, dest) {
          _copy_content src, dest
        }
      end

      private

      def _copy_entry(src, dest)
        st = stat(src)
        case
        when st.file?
          _copy_content src, dest
        when st.directory?
          begin
            Dir.mkdir File.expand_path(dest)
          rescue => err
            raise unless File.directory?(dest)
          end
        when st.symlink?
          File.symlink File.readlink(src), dest
        when st.chardev?
          raise "cannot handle device file" unless File.respond_to?(:mknod)
          mknod dest, ?c, 0666, st.rdev
        when st.blockdev?
          raise "cannot handle device file" unless File.respond_to?(:mknod)
          mknod dest, ?b, 0666, st.rdev
        when st.socket?
          raise "cannot handle socket" unless File.respond_to?(:mknod)
          mknod dest, nil, st.mode, 0
        when st.pipe?
          raise "cannot handle FIFO" unless File.respond_to?(:mkfifo)
          mkfifo dest, 0666
        when (st.mode & 0xF000) == (_S_IF_DOOR = 0xD000)   # door
          raise "cannot handle door: #{src}"
        else
          raise "unknown file type: #{src}"
        end
      end

      def _copy_content(src, dest)
        st = stat(src)
        File.open(src,  'rb') {|r|
          File.open(dest, 'wb', st.mode) {|w|
            fu_copy_stream0 r, w, (fu_blksize(st) || fu_default_blksize())
          }
        }
      end

      def preserve(src, dest)
        return yield unless @preserve
        st = stat(src)
        yield
        File.utime st.atime, st.mtime, dest
        begin
          chown st.uid, st.gid, dest
        rescue Errno::EPERM
          # clear setuid/setgid
          chmod st.mode & 01777, dest
        else
          chmod st.mode, dest
        end
      end

      def stat(path)
        if @dereference
          @stat ||= ::File.stat(path)
        else
          @stat ||= ::File.lstat(path)
        end
      end

      def chmod(mode, path)
        if @dereference
          ::File.chmod mode, path
        else
          begin
            ::File.lchmod mode, path
          rescue NotImplementedError
            # just ignore this because chmod(symlink) changes attributes of
            # symlink target, which is not our intent.
          end
        end
      end

      def chown(uid, gid, path)
        if @dereference
          ::File.chown uid, gid, path
        else
          begin
            ::File.lchown uid, gid, path
          rescue NotImplementedError
            # just ignore this because chown(symlink) changes attributes of
            # symlink target, which is not our intent.
          end
        end
      end
    end

    # == Extensions

    def sh(command, *args)
      args, op = fui_split_op(args)
      fui_check_options(op)
      execargs, string = to_exec_command_and_string(command, *args)
      fui_output_message(string, op)
      unless(op[:noop])
        unless system(*execargs)
          errormsg = "command failed: code #{$?}: " + string
          raise errormsg unless op[:failok]
          fui_output_message(errormsg, op) if op[:warn]
        end
      end
    end

    # Somewhat like IO.popen. Use the :iomode option to specify mode,
    # which defaults to "r".
    def sh_pipe(command, *args) # yield :io:
      args, op = fui_split_op(args)
      fui_check_options(op)
      iomode = op[:iomode] || "r"
      execargs, string = to_exec_command_and_string(command, *args)
      fui_output_message(string, op)
      unless(op[:noop])
        IO::popen(string, iomode) do |io|
          yield io
        end
      end
    end

    def sh_open4(command, *args)
      require 'open4-0.9.1'
      args, op = fui_split_op(args)
      fui_check_options(op)
      execargs, string = to_exec_command_and_string(command, *args)
      fui_output_message(string, op)
      unless(op[:noop])
        open4(*execargs) do |*io|
          yield *io
        end
      end
    end

    RUBY = Config::CONFIG['RUBY_INSTALL_NAME']

    # Like +sh+, but uses the Ruby executable configured for this
    # runtime as the command.
    def ruby(*args)
      sh(RUBY, *args)
    end

    # Accepts the same parameters as Kernel#exec, and returns their
    # string representation. That is, we will turn the command and its
    # arguments into something that could be typed on a command line.
    def exec_command_to_string command, *args
      string = nil
      if command.is_a? Array
        string = command[0]
      else
        string = command
      end
      string = string.dup
      unless args.empty?
        string << " "
        string << to_shell_string(args)
      end
      return string
    end

    # A bit like exec_command_to_string, but I think this is more user
    # friendly in most situations. Produces [exec_args, command_string],
    # such that one may call Kernel#exec(*exec_args), and print the
    # same command by printing command_string.
    def to_exec_command_and_string command, *args
      if command.respond_to?(:to_str)
        r = command.to_str
        if args.empty?
          # When given a single string, it is subject to shell
          # expansion; this is compatible with "exec", and indeed,
          # perhaps there is no way of passing a single string to "exec"
          # without it being subject to shell expansion.
          [[r], r]
        else
          r = [r] + args
          [r, to_shell_string(r)]
        end
      elsif command.respond_to?(:to_ary)
        r = command.to_ary + args
        [r, to_shell_string(r)]
      else
        raise ArgumentError
      end
    end

    def on_console?
      !ENV['DISPLAY']
    end

    def user_home
      ENV['HOME'] || ENV['LOGDIR'] || raise
    end

    def tmpdir
      Dir::tmpdir
    end

    # Turns a shell string into tokens.
    def to_shell_words(string)
      Shellwords::shellwords(string)
    end

    SHELL_SPECIAL_CHARS = '()|&<> \'"\\$*'
    SHELL_ESC_RE = Regexp.new('([' + Regexp.escape(SHELL_SPECIAL_CHARS) + '])')

    # Turns shell tokens into a shell string.
    def to_shell_string(array)
      Array(array).map do |elem|
        elem.to_str.gsub(SHELL_ESC_RE, '\\\\' + '\1')
      end.join(" ")
    end

    # Outputs the specified message according to the :msgout and
    # :msglabel and :verbose options.
    def fu_puts msg, op = {}
      fui_check_options(op)
      fui_output_message msg, op
    end

    # You may pass a message and/or options. The message only gets
    # printed in +noop+ mode, and otherwise the passed block is
    # executed.
    def when_writing(*args) # :yield:
      args, op = fui_split_op args
      fui_check_options(op)
      if op[:noop]
        unless args.empty?
          msg = args.first
          fui_output_message msg, op
        end
      else
        yield
      end
    end

    def create_file(file, op = {}, &block) # :yields: +IO+
      fui_check_options(op)
      fui_create_file(file, op, &block)
    end

    def create_script(file, op = {}, &block) # :yields: +IO+
      fui_check_options(op)
      op = op.dup
      op[:mode] = 0755
      fui_create_file(file, op, &block)
    end

    #
    # Options: verbose
    #
    # Sets the specified environment variable(s) to the specified
    # value. The caller should pass and even number of keys and
    # values.
    #
    # If this method is called with a block, restores the old value
    # after the block execution has finished.
    #
    def set_env(*args, &block)
      args, op = fui_split_op args
      fui_check_options(op)
      unless (args.size % 2) == 0
        raise ArgumentError, "an even number of arguments required"
      end

      pairs = []
      i = 0
      while i < args.size
        oldval = ENV[args[i]]
        pairs.push [args[i], args[i+1], oldval]
        i += 2
      end

      for key, newval, oldval in pairs
        fui_set_env(key, newval, op)
      end
      if block
        begin
          block.call
        ensure
          for key, newval, oldval in pairs
            fui_set_env(key, oldval, op)
          end
        end
      end
    end

    # == Internal

    private

    def fui_each_src_dest(src, dest, op = {})
      fui_each_src_dest0(src, dest, op) do |s, d|
        raise ArgumentError, "same file: #{s} and #{d}" if fu_same?(s, d)
        yield s, d
      end
    end

    def fui_each_src_dest0(src, dest, op = {})
      intodir = File.directory?(dest) && !op[:notinto]
      Array(src).each do |s|
        if intodir
          yield s.to_str, File.join(dest, File.basename(s))
        else
          yield s.to_str, dest.to_str
        end
      end
    end

    def fui_copy_any_file s, d, preserve = false
      if File.directory?(s)
        fu_traverse(s) do |rel, deref, st|
          ctx = CopyContext_.new(preserve, deref, st)
          ctx.copy_entry "#{s}/#{rel}", "#{d}/#{rel}"
        end
      else
        copy_file s, d, preserve
      end
    end

    def fui_delete_any_file fname, failok = false
      begin
        st = File.lstat(fname)
      rescue
        next if failok
        raise
      end
      if    st.symlink?   then remove_file fname, failok
      elsif st.directory? then remove_dir fname, failok
      else                     remove_file fname, failok
      end
    end

    def fui_set_env(name, value, op)
      if value
        fui_output_message "set #{name}=#{value}", op
      else
        fui_output_message "unset #{name}", op
      end
      ENV[name] = value
    end

    def fui_create_file(file, op) # :yields: +IO+
      mode = File::CREAT|File::WRONLY
      if op[:force]
        mode |= File::TRUNC
      else
        mode |= File::EXCL
      end

      fui_output_message "write #{to_shell_string(file)}", op
      unless op[:noop]
        File.open(file, mode) do |output|
          yield output
        end
      end
      fui_chmod(file, op)
    end

    def fui_chmod(list, op)
      mode = op[:mode]
      return unless mode
      list = fu_list(list)
      command = "chmod %o" % mode
      string = exec_command_to_string(command, *list)
      fui_output_message(string, op)
      return if op[:noop]
      File.chmod(mode, *list)
    end

    # Overridden to use :msgout and :msglabel options instead of the
    # module instance variables @fileutils_output and @fileutils_label;
    # the default output is now $stdout instead of $stderr. It is now
    # also possible to pass options directly to this method.
    def fui_output_message(msg, op)
      if op[:verbose]
        output = op[:msgout] || $stdout
        label = op[:msglabel] || ""
        output.puts(label + msg)
      end
    end

    def fui_puts_cmd(command, swlist, files, op)
      fui_check_options(op)
      comps = [command]
      if swlist
        for sw in [swlist].flatten
          next unless sw
          comps.push(sw)
        end
      end
      if files
        comps.push(to_shell_string(fu_list(files)))
      end
      string = comps.join(" ")
      fui_output_message(string, op)
    end

    # == Options

    public

    # A list of the methods that may have default options.
    FU_METHODS = [
      :cd,
      :chdir,
      :chmod,
      :copy,
      :cp,
      :cp_r,
      :create_file,
      :create_script,
      :fu_puts,
      :install,
      :ln,
      :ln_s,
      :ln_sf,
      :makedirs,
      :mkdir,
      :mkdir_p,
      :mkpath,
      :move,
      :mv,
      :remove,
      :rm,
      :rm_f,
      :rm_r,
      :rm_rf,
      :rmtree,
      :rmdir,
      :ruby,
      :safe_unlink,
      :set_env,
      :sh,
      :sh_pipe,
      :sh_open4,
      :symlink,
      :touch,
      :when_writing]

    # A list of all options known to this module. These are used to
    # check for typos in option lists. We have introduced a
    # considerable number of new options; we are no longer using
    # module instance variables for individual "options" as in
    # fileutils.rb. The meaning of :force is often not obvious, and
    # often it means more than one thing, and it's not possible to as
    # for one and not get the other; thus we have introduced :failok
    # and :overwrite, which allow the separate meanings of :force to
    # captured individually.
    FU_VALID_OPTIONS = [:noop, :verbose, :force, :mode, :preserve,
      :msgout, :msglabel, :dont, :failok, :nowarn, :time,
      :onlyatime, :onlymtime, :nocreate, :overwrite, :notinto,
      :iomode, :noopcall]

    # Global default options affecting all methods listed in
    # FU_METHODS.
    FU_MODULE_OPTIONS = {
      :create_file => {:force => false},
      :create_script => {:force => false},
    }

    # Global default options affecting all methods listed in
    # FU_METHODS. These may be modified by the user, and will take
    # precedence over anything in FU_MODULE_OPTIONS. An even stronger
    # precedence is that method specific options come before
    # non-method-specific ones.
    FU_USER_OPTIONS = {}

    # Gets the non-command-specific default options currently in
    # effect. This could be useful if one wants to perform an action
    # conditionally depending on say whether the :noop option is in
    # effect.
    def fu_get_options
      fui_add_default_options Hash.new
    end

    # Sets the defaults options. If a block is passed, the effect is
    # limited to within the block. Do note that calling this method
    # may be unhealthy in multi-threaded programs.
    def fu_set_options map, &block
      if block
        oldmap = dup_opts FU_USER_OPTIONS
        FU_USER_OPTIONS.clear
        FU_USER_OPTIONS.update(map)
        begin
          block.call
        ensure
          FU_USER_OPTIONS.clear
          FU_USER_OPTIONS.update(oldmap)
        end
      else
        FU_USER_OPTIONS.clear
        FU_USER_OPTIONS.update(map)
      end
    end

    # Modifies the default options. If a block is passed, the effect
    # is limited to within the block. Do note that calling this method
    # may be unhealthy in multi-threaded programs.
    def fu_modify_options map, &block
      if block
        oldmap = dup_opts FU_USER_OPTIONS
        opts_update(FU_USER_OPTIONS, map)
        begin
          block.call
        ensure
          FU_USER_OPTIONS.clear
          FU_USER_OPTIONS.update(oldmap)
        end
      else
        opts_update(FU_USER_OPTIONS, map)
      end
    end

    private

    def fui_split_op args
      op = {}
      if args.last.is_a? Hash
        op = args[-1]
        args = args[0...-1]
      end
      [args, op]
    end

    def opts_update map1, map2
      for methsym, methmap in map2
        oldmap = {}
        if map1.has_key? methsym
          oldmap = map1[methsym]
        end
        map1[methsym] = oldmap.update(methmap)
      end
    end

    # Returns a deep copy of the given options object.
    def dup_opts map
      newmap = map.dup
      for key, elem in newmap
        newmap[key] = elem.dup
      end
      return newmap
    end

    # Adds any defaults options to the passed ones, returning the
    # result.
    # methsym:: The method whose options are being expanded.
    # args:: The arguments to expand.
    def fui_args(methsym, args)
      args = args.dup
      inargs = args.last.is_a?(Hash)
      op = (inargs ? args.last : {})
      op = fui_add_default_options(op, methsym)
      if inargs
        args[-1] = op
      else
        args.push(op)
      end
      return args
    end

    # Returns the "sum" of the options.
    def fui_add_default_options op, methsym = nil
      dop = FU_MODULE_OPTIONS[nil]
      dop = dop ? dop.dup : {}
      dop.update(FU_USER_OPTIONS[nil] || {})
      dop.update(FU_MODULE_OPTIONS[methsym] || {}) if methsym
      dop.update(FU_USER_OPTIONS[methsym] || {}) if methsym
      dop.update(op)
    end

    # We do not command-specific option checking, but we do check for
    # typos, however, i.e. for options not accepted by any method.
    # op:: The options to check.
    def fui_check_options(op)
      h = op.dup
      FU_VALID_OPTIONS.each do |name|
        h.delete name
      end
      unless h.empty?
        raise ArgumentError, "no such option: #{h.keys.join(' ')}"
      end
    end
  end

  include InnerModule

  # It looks like an included Module behaves like a superclass in some
  # sense, as we can override its methods, and even invoke "super" in
  # them. Here we add options checking and default options so that
  # there is no need to do that individually within each method. The
  # benefit of doing this is that we do not need to manually for every
  # single method; in particular, we need do nothing for aliases.
  InnerModule::FU_METHODS.each do |name|
    module_eval(<<-EOS, __FILE__, __LINE__ + 1)
      def #{name.id2name}(*args)
        super(*fui_args(:#{name.id2name}, args))
      end
    EOS
  end

  extend self
end

# :stopdoc:
if $0 == __FILE__
  require 'stringio'
  require 'test/unit'

  include EFileUtils1

  class FuTest < Test::Unit::TestCase # :nodoc:
    def cmpd expected
      fu_modify_options(nil => {:msgout => $stdout})
      yield
    end

    def cmp expected
      sio = StringIO.new ""
      fu_modify_options(nil => {:msgout => sio})
      yield
      sio.rewind
      actual = sio.read
      if expected.kind_of? Regexp
        assert_match(expected, actual)
      else
        assert_equal(expected, actual)
      end
    end

    def setup
      fu_set_options({})
    end

    def test_mv
      fu_modify_options(nil => {:verbose => true})
      fu_modify_options(:move => {:verbose => false})
      cd(tmpdir) do
        cmp("touch file1\n") do
          touch "file1"
        end
        cmp("touch -c file1\n") do
          touch "file1", :nocreate => true
        end
        cmp("touch -a file1\n") do
          touch "file1", :onlyatime => true
        end
        cmp("touch -m file1\n") do
          touch "file1", :onlymtime => true
        end
        cmp("") do
          # Yes, move and mv have different default options now.
          move "file1", "file2", :noop => true
        end
        cmp("mv file1 file2\n") do
          mv "file1", "file2"
        end
        cmp("rm file2\n") do
          rm "file2"
        end
      end
    end

    def test_mkdir
      file = File.join(tmpdir, "testdir")
      fu_modify_options(nil => {:verbose => true})
      cmp("mkdir #{file}\n") { mkdir file }
      cmp("rmdir #{file}\n") { rmdir file }
    end

    def test_cd
      cmp("hello\n") do
        cd("/tmp", :verbose => true, :dont => true) do
          fu_puts "hello", :verbose => true
        end
      end
      cmp("") do
        cd("/tmp", :noop => false) do |dir|
          assert_equal(dir, pwd)
        end
      end
      cmp(%r{cd /tmp}) do
        cd("/tmp", :noop => false, :verbose => true) do
        end
      end
      fu_modify_options(nil => {:verbose => true}) do
        cmp(%r{cd /tmp}) do
          chdir("/tmp", :noop => false) do
          end
        end
      end
    end
  end
end

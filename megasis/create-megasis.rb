PROG_NAME = File.basename(__FILE__)

PURPOSE = %{

Utility for SIS file creation.

}.gsub(/\r?\n/, " ").strip

# -----------------------------------------------------
# Utils...

def get_uid_bytes file
  File.open(file) do |input|
    input.seek(0x08)
    lst = []
    4.times do
      lst << input.getc
    end
    lst
  end
end

def lookup_uid file
  b = get_uid_bytes(file)
  s = ("%c%c%c%c" % b.reverse)
  (s.unpack("N"))
end

def embed_sis file, op = {}
  uid = lookup_uid file
  if op[:check]
    "if not package(0x%08x)\n@%s, (0x%08x)\nendif" % [uid, file.inspect, uid]
  else
    "@%s, (0x%08x)" % [file.inspect, uid]
  end
end

def kjoin lst
  lst.join ", "
end

def opt_join lst
  kjoin(lst.map {|x| '{"%s"}' % x})
end

# Writes +data+ to a new file called +fname+,
# optionally converting LFs to CRLFs.
# Or, if +block+ is given, lets the caller
# do all the writing, without any conversions.
def write_file(fname, data = nil, tocrlf = false, &block) # :yields: io
  File.open(fname, "w") do |output|
    if block
      block.call(output)
    else
      raise "no data given" unless data
      if tocrlf
        require 'strscan'
        sc = StringScanner.new data
        until sc.eos?
          if (ms = sc.scan(/[^\r\n]+/))
            output.write(ms)
          elsif (ms = sc.scan(/\r?\n/))
            output.write("\r\n")
          else
            raise ArgumentError, sc.rest
          end
        end
      else
        output.write(data)
      end
    end
  end
  puts fname
end

$tmpfile_count = 0

def get_tempfile ext = ""
  $tmpfile_count += 1
  "tmpfile%d%s" % [$tmpfile_count, ext]
end

def display_note text
  tmpfile = get_tempfile ".txt"
  write_file(tmpfile, text + "\n", true)
  '"%s" - "", FILETEXT' % tmpfile
end

def sisinfo_ls file
  targets = []
  IO.popen("sisinfo -i -f #{file}") do |input|
    input.each_line do |line|
      items = line.strip.split(/\s+/)
      targets.push(items.first)
    end
  end
  targets
end

def pkg_entries_for_sis srcdir, file
  targets = sisinfo_ls file
  pkg_lines = targets.map do |target|
    srcfile = target.dup
    srcfile.sub!(/^.:/, "")
    srcfile.gsub!(/\\/, "/")
    srcfile = File.join(srcdir, srcfile)
    '"%s" - "%s"' % [srcfile, target]
  end
  pkg_lines
end

def copy_with_sis_ext old_file
  return old_file if old_file =~ /[.]sis$/
  old_basename = File.basename(old_file)
  old_basename =~ /[.]sisx$/ or raise
  new_basename = $` + ".sis"
  new_home = Dir::tmpdir()
  sisfile = File.join(new_home, new_basename)
  File::copy(old_file, sisfile)
  return sisfile
end

require 'fileutils'

def may_fail # block
  begin
    yield
  rescue
  end
end

def unpack_sis sisfile, destdir
  may_fail { FileUtils::rm_r(destdir) }
  system("sisinfo -e %s -f %s" % [destdir, sisfile])
end

# -----------------------------------------------------
# Argument parsing...

require 'ostruct'
$op = OpenStruct.new

require 'optparse'

$op.verbose = true
$non_redistributable = false
$self_signed = false
$op.outfile = nil
$unpack_embedded = false

$opts = OptionParser.new do |opts|
  opts.banner = %{

#{PROG_NAME}

#{PURPOSE}

USAGE: #{$0} option... file...

Usage examples:

  #{$0} XXX

}.strip
  opts.separator ""
  opts.separator "The \"options\" are:"
  opts.separator ""
  opts.on("--self", "make self-signed SIS") do
    $self_signed = true
  end
  opts.on("-h", "--help", "print usage") do
    puts opts.help
    exit 0
  end
  opts.on("-o OUTFILE", "specifies output file path") do |s|
    $op.outfile = s
  end
  opts.on("-q", "--quiet", "be quiet") do
    $op.verbose = false
  end
  opts.on("--nr", "include non-redistributable material") do
    $non_redistributable = true
  end
  opts.on("--unpack", "unpack embedded SIS files") do
    $unpack_embedded = true
  end
end

def usage_error message
  puts $opts.help
  puts "ERROR: " + message
  exit 1
end

begin
  $opts.parse!(ARGV)
rescue OptionParser::InvalidOption => e
  usage_error e.message
end

# -----------------------------------------------------
# Main...

require 'erb'
require 'ftools'
require 'stringio'
require 'tmpdir'
require '../daemon/src/current_config.rb'

def make_pkg srcfile
  srcfile =~ /\.in$/ or raise
  destfile = $op.outfile || $`
  text = File.read(srcfile)
  text = ERB.new(text).result(binding())
  write_file(destfile, text, true)
end

for file in ARGV
  make_pkg file
end

# -----------------------------------------------------

#
# create-megasis.rb
#
# Copyright 2009 Helsinki Institute for Information Technology (HIIT)
# and the authors. All rights reserved.
#
# Authors: Tero Hasu <tero.hasu@hut.fi>
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

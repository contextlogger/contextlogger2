#
# write_file.rb
#
# Copyright 2006 Helsinki Institute for Information Technology (HIIT)
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

# Writes +data+ to a new file called +fname+,
# optionally converting LFs to CRLFs.
# Or, if +block+ is given, lets the caller
# do all the writing, without any conversions.
def write_file(fname, data = nil, tocrlf = false, &block) # :yields: io
  unless nowrite()
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
  end
  puts "wrote " + fname
end

# Like +write_file+, but sets executable permissions.
def write_script(fname, data = nil, tocrlf = false, &block)
  write_file(fname, data, tocrlf, &block)
  chmod(0755, fname)
end

=begin rdoc

Copyright 2006 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

= License

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

=end

require 'open4-0.9.1'

# This implementation is based on regtool, and thus works on Cygwin only.
module Registry
  def has_key? keyname
    status = Open4::popen4("regtool", "check", keyname) do |*ignore|
    end

    case status.exitstatus
    when 0
      true
    when 1
      false
    else
      raise "unexpected exit code"
    end
  end

  # Returns nil if there is no such value.
  def get_value keyname, valuename
    begin
      status = Open4::popen4("regtool", "get", "#{keyname}/#{valuename}") do |c,i,o,e|
        return o.readline.chomp
      end
    rescue EOFError
      return nil
    end
  end

  extend self
end

if $0 == __FILE__
  p(Registry::has_key?('/HKLM/SOFTWARE/ActiveState/ActivePerl'))
  p(Registry::has_key?('/HKLM/SOFTWARE/ActiveState/ActiveFoo'))
  p(Registry::get_value('/HKLM/SOFTWARE/ActiveState/ActivePerl', "CurrentVersion"))
  p(Registry::get_value('/HKLM/SOFTWARE/ActiveState/ActivePerl/635', nil))
  p(Registry::get_value('/HKLM/SOFTWARE/ActiveState/ActivePerl/635', "baz"))
end

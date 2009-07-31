#
# instance_data_util.rb
#
# Copyright 2005 Helsinki Institute for Information Technology (HIIT)
# and the authors.  All rights reserved.
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

class Symbol
  def id2name_no_at
    name = id2name
    if name[0] == ?@
      name[0,1] = ''
    end
    name
  end
end

module InstanceDataUtil

  private

  # If an instance variable has not been set, then it is normally
  # treated as having been set to a <tt>nil</tt> value. This
  # variant of <tt>instance_variable_get</tt> behaves in the same way.
  def instance_variable_get_nil symbol
    begin
      instance_variable_get(symbol)
    rescue NameError
      nil
    end
  end

  def instance_list_push symbol, elem
    begin
      list = instance_variable_get(symbol)
      list.push(elem)
    rescue NameError
      instance_variable_set(symbol, [elem])
    end
  end

end

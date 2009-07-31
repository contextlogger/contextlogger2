=begin rdoc

delegation.rb

Copyright 2007 Helsinki Institute for Information Technology (HIIT)
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

module Sake; end

# A ProjBuild is essentially a Project with some additional
# information that is specific to a particular build. We could
# subclass so that ProjBuild is-a Project, but then we would have to
# initialize all of the Project data for each ProjBuild. It is more
# natural to make a Project (reference) a property of ProjBuild, but
# then calling methods becomes less convenient, and less future proof,
# since the receiver to use varies from method to method. It is more
# natural to indeed make Project a property of ProjBuild, but then
# delegate calls to the Project instance where appropriate. The method
# defined here is designed to do that.
module Sake::Delegation
  def make_delegating_methods objlist, op = {}
    objlist = Array(objlist)
    exclude = op[:exclude] || Object.instance_methods
    objlist = objlist.compact.reverse
    singleton_class.class_eval do
      objlist.each do |obj|
        obj.methods.each do |meth|
          unless exclude.include?(meth)
            meth_sym = meth.to_sym
            define_method(meth_sym) do |*args|
              obj.method(meth_sym).call(*args)
            end
          end
        end
      end
    end
  end
end

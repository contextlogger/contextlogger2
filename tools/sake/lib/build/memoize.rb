require 'build/lang_ext'

# Just about the same as Doug Landauer's "memoize" implementation at
# http://radio.weblogs.com/0100945/2005/11/09.html.
module Memoize
  def memoize(name)
    meth = method(name)
    cache = {}
    singleton_class.class_eval do
      define_method(name) do |*args|
        cache.has_key?(args) ? cache[args] :
          (cache[args] ||= meth.call(*args))
      end
    end
  end
end

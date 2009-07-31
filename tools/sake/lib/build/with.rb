# This one is from Ola Bini's blog at http://ola-bini.blogspot.com/.

module Kernel
  def with(obj = nil, &block)
    (obj || self).instance_eval &block
  end
end

=begin rdoc

traits.rb

Copyright 2005-2008 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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

= Purpose

A simple "traits" implementation inspired by the traits module.

=end

if $0 == __FILE__
  $:.push(File.dirname(File.dirname(File.expand_path(__FILE__))))
end

require 'build/lang_ext'
require 'sake4/instance_data_util'

module Sake; end

module Sake::Traits

  private

  # Supports this sort of usage:
  #
  #   trait :v9, 'cast' => proc {|x| OneUid.new(x)}
  #   trait :basename, 'munge' => :to_str
  #   trait :uid, 'validate' => proc {|x| x.kind_of?(Sake::Uid)}
  def trait(methsym, op = {})
    varsym = ('@' + methsym.id2name).to_sym
    settersym = (methsym.id2name + "=").to_sym

    tf =
      if op["cast"]
        cast = op["cast"]
        proc do |v|
          begin
            cast.call(v)
          rescue Exception => e
            raise "value #{v.inspect} not castable to a value fitting trait #{methsym}: #{e.message}"
          end
        end
      elsif op["munge"]
        cast = op["munge"]
        proc do |v|
          begin
            v.method(cast).call
          rescue Exception => e
            raise "value #{v.inspect} not mungeable to a value fitting trait #{methsym}: #{e.message}"
          end
        end
      elsif op["validate"]
        cast = op["validate"]
        proc do |v|
          if cast.call(v)
            v
          else
            raise "value #{v.inspect} does not validate to a value fitting trait #{methsym}"
          end
        end
      else
        proc {|v| v}
      end

    module_eval do
      include InstanceDataUtil

      define_method(methsym) do
        instance_variable_get_nil(varsym)
      end

      define_method(settersym) do |value|
        instance_variable_set(varsym, tf.call(value))
      end
    end
  end
end

module Sake::Traits::TraitInit
  def trait_init op = {}
    unless op.is_a? Hash
      raise "#{self.class} initialize expecting Hash or nothing, but got #{op.class}"
    end

    for key, value in op
      settersym = (key.id2name + "=").to_sym
      if respond_to? settersym
        send settersym, value
      else
        raise ArgumentError, "invalid trait #{key} for #{self.class}"
      end
    end
  end
end

module Sake::Traits::DefaultTraitInit
  include Sake::Traits::TraitInit

  def initialize op = {}
    trait_init op
  end
end

if $0 == __FILE__
  class Foo
    extend Sake::Traits
    include Sake::Traits::DefaultTraitInit
    trait :foo
    trait :bar, 'cast' => proc {|x| x + 1}
    trait :baz, 'munge' => :to_s
    trait :bamf, 'validate' => proc {|x| x.kind_of?(Numeric)}
  end

  foo = Foo.new

  p(foo.foo)
  foo.foo = 1
  p(foo.foo)

  p(foo.bar)
  foo.bar = 1
  p(foo.bar)

  p(foo.baz)
  foo.baz = 1
  p(foo.baz)

  p(foo.bamf)
  foo.bamf = 1
  p(foo.bamf)

  p(foo.bamf)
  begin
    foo.bamf = "4"
  rescue
  else
    raise "no validation error"
  end
  p(foo.bamf)

  foo = Foo.new :foo => 1, :bar => 2, :baz => 3, :bamf => 4
  p foo
end

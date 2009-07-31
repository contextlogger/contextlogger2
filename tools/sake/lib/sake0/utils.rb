=begin rdoc

utils.rb

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

require 'build/lang_ext'
require 'build/memoize'
require 'pathname'
require 'sake0/sake'
require 'traits-0.10.0'

def true? value
  value ? true : false
end

class Pathname
  def ext(newext)
    self.class.new(self.to_s.sub(/\.[^.]+$/, newext))
  end
end

module DefaultTraitInit
  include TraitInit

  def initialize op = {}
    default_trait_init op
  end

  def default_trait_init op = {}
    unless op.is_a? Hash
      raise "#{self.class} initialize expecting Hash or nothing, but got #{op.class}"
    end
    trait_init op
  end
end

module Delegation
  def delegate_to *objs
    objlist = objs.compact
    singleton_class.class_eval do
      define_method(:method_missing) do |symbol, *args|
        callcc do |ret|
          for obj in objlist
            if obj.respond_to? symbol
              ret.call(obj.method(symbol).call(*args))
            end
          end
          raise "#{obj} has no method called #{symbol}"
        end
      end
    end
  end
end

module Sake
  def self.macrify(string, value = :define)
    string = string.to_s.upcase
    string.gsub!(/ /, "_")
    string = "__" + string + "__"

    case value
    when :define
    when true
      string += ("=1")
    when false
      string += ("=0")
    when String
      string += ("=" + value.inspect)
    when Integer
      string += ("=" + value.to_s)
    else
      raise "unsupported type #{value.class}"
    end

    return string
  end

  def self.macrify_map map, op = {}
    list = []
    for key, value in map
      list.push(Sake::macrify(key, value))
    end
    return list.sort
  end

  def self.define_value_map list
    map = {}
    for elem in list
      map[elem] = :define
    end
    return map
  end

  def self.ensure_pathname string
    case string
    when Pathname
      string
    when String
      Pathname.new string
    else
      raise
    end
  end

  def self.ensure_pathname_list string
    Array(string).map do |elem|
      ensure_pathname elem
    end
  end

  # I believe Rails has something called Integer#ordinalize that does
  # the same thing properly, but this will do here, for now.
  def self.to_rank num
    case num
    when 0, 4..19
      "#{num}th"
    when 1
      "1st"
    when 2
      "2nd"
    when 3
      "3rd"
    else
      raise
    end
  end
end

if $0 == __FILE__
  p [Sake::to_rank(15), Sake::to_rank(0), Sake::to_rank(2)]
end

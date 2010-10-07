=begin rdoc

sake_utils.rb

Copyright 2006 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

= License

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

= Purpose

Utilities that are assumed to only be useful when used with Sake, but
are not assumed to be needed in nearly all makefiles.

=end

require 'sake4/sake'

class Rake::Task
  # Returns task _objects_ (rather than names) for each of the
  # prerequisites of this task. Only returns task objects that
  # actually exist, rather than trying to synthesize anything.
  def deps_o
    @prerequisites.map do |name|
      application.lookup(name, @scope)
    end.compact
  end

  # Returns a list containing the receiver and all of its
  # prerequisites that have an existing, associated task object.
  # Cycles are not a problem, although they may not be allowed anyway.
  # The returned list may be in any order.
  def tasks_in_subtree
    seen = {}
    f = proc do |root|
      unless seen[root]
        seen[root] = root
        for elem in root.deps_o
          f.call(elem)
        end
      end
    end
    f.call(self)
    seen.values
  end
end

# Overriding to provide a way to force a file to be considered as
# uncurrent. Does not cater for situations where we might want other
# tasks considered uncurrent, unfortunately.
class Rake::FileTask
  def force_uncurrent
    @needed = true
  end

  def force_uncurrent_tree
    for elem in tasks_in_subtree
      if elem.respond_to? :force_uncurrent
        elem.force_uncurrent
      end
    end
  end

  alias orig_needed? needed?

  def needed?
    # p "#{name} #{@needed}"
    return true if @needed
    orig_needed?
  end
end

module Sake::Utils
  SAKE_OP_FILE = ".sake_op"

  def sake_op_changed? differ = nil
    data = nil
    begin
      data = File.read(SAKE_OP_FILE)
    rescue Errno::ENOENT
      return true
    end

    differ ||= proc {|x,y| x != y}
    differ.call(eval(data), $sake_op)
  end

  def write_current_sake_op
    File.open(SAKE_OP_FILE, "w") do |output|
      output.puts($sake_op.inspect)
    end
  end

  def mark_all_tasks_uncurrent
    fu_puts "considering all tasks uncurrent"
    for elem in Rake::application.tasks
      obj = Rake::Task[elem]
      if obj and obj.respond_to? :force_uncurrent
        obj.force_uncurrent
      end
    end
  end

  # Marks _all_ defined tasks as uncurrent, if there has been a change
  # in the options passed to Sake. (Alternatively one might consider
  # making only select tasks uncurrent, perhaps recursively.)
  def on_op_change_force_uncurrent
    if sake_op_changed?
      write_current_sake_op
      mark_all_tasks_uncurrent
    end
  end

  extend self
end

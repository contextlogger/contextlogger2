#
# sake.rb
#
# Copyright 2005-2006 Helsinki Institute for Information Technology
# (HIIT) and the authors. All rights reserved.
#
# Authors: Tero Hasu <tero.hasu@hut.fi>
#
# This is Sake (or Symbian Make), but actually it's got nothing to do
# with Symbian; Sake is a generic purpose make tool. It is based on
# the Rake codebase, and just fixes the Rake feature where dry runs do
# not execute actions at all (this makes testing new makefiles
# somewhat risky, since we cannot really try them out safely), and
# provides more utilities commonly needed in makefiles. It requires
# Rake; to avoid platform dependencies, a local copy of Rake is used;
# the local copy is also slightly customized, to better suit the needs
# of Sake. Sake is mostly compatible with Rake, but not quite; to
# avoid having "rake" process incompatible makefiles, the name
# "sakefile" for them.
#
# == License
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

module Sake
end

require 'sake4/traits'

class Module
  include Sake::Traits
end

require 'rbconfig'

# We are using a custom version of Rake, but we still try to override
# things here as much as feasible, to make it easier to upgrade to
# more recent version of Rake.
#
# The manual changes we require:
#
# * RAKEVERSION should be changed to indicate that the file has been
#   modified from the original.
#
# * RakeFileUtils must not be included at the top level; it need not
#   even be defined.
require 'sake0/rake071hiit'

require 'sakefileutils1'
include SakeFileUtils1

def verbose(value = nil, &block)
  unless value.nil?
    fu_modify_options({nil => {:verbose => value}}, &block)
  end
  fu_get_options[:verbose]
end

def nowrite(value = nil, &block)
  unless value.nil?
    fu_modify_options({nil => {:noop => value}}, &block)
  end
  fu_get_options[:noop]
end

# The default in Rake, too (sensibly).
verbose(true)

class Rake::Application
  def usage
    puts "sake [-f sakefile] {options} targets..."
  end

  alias orig_handle_options handle_options

  def handle_options
    orig_handle_options

    # We cannot allow this to be set, as it means that any actions
    # will not even get executed, and we cannot really see what is
    # happening. It is enough if the fileutils are in :noop mode.
    options.dryrun = false
  end

  # Overridden to add a $sake_op global variable with all the x=y type
  # option assignments there, too, in addition to being set into ENV.
  def collect_tasks
    tasks = []
    $sake_op = {}
    ARGV.each do |arg|
      if arg =~ /^(\w+)=(.*)$/
        ENV[$1] = $2
        $sake_op[$1.to_sym] = $2
      else
        tasks << arg
      end
    end
    tasks.push("default") if tasks.size == 0
    tasks
  end
end

class Rake::Task
  # Like GNU Make's $<.
  def dep
    prerequisites.first
  end

  def deps
    prerequisites
  end

  def deps_s
    prerequisites.join(' ')
  end
end

require 'build/globi'

# Pass a block of operations that are allowed to fail
# without causing a make rule to fail. Often useful
# for deleting files that may or may not exist, for
# instance.
def may_fail # block
  begin
    yield
  rescue
  end
end

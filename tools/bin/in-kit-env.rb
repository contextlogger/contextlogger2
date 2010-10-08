#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

PROG_NAME = File.basename(__FILE__)

PURPOSE = %{

Invokes specified command in a suitable environment.

}.gsub(/\r?\n/, " ").strip

# -----------------------------------------------------
# Argument parsing...

require 'ostruct'
$op = OpenStruct.new

require 'optparse'

$op.verbose = true
$op.noop = false
$op.force = false

$opts = OptionParser.new do |opts|
  opts.banner = %{
#{PROG_NAME}

#{PURPOSE}

USAGE: #{PROG_NAME} option... shell_command

Usage examples:

  #{PROG_NAME} -k s60_30 abld freeze gcce

}
  opts.separator ""
  opts.separator "The \"options\" are:"
  opts.separator ""
  opts.on("-h", "--help", "print usage") do
    puts opts.help
    exit 0
  end
  opts.on("-k", "--kit NAME", "specifies SDK name") do |name|
    $op.kit_name = name
  end
end

def usage_error message
  puts $opts.help
  puts "ERROR: " + message
  exit 1
end

begin
  $opts.parse!(ARGV)
rescue OptionParser::InvalidOption => e
  usage_error e.message
end

# -----------------------------------------------------
# Main...

def in_env kit_name, &block
  if kit_name
    epoc_in_plat_env kit_name, &block
  else
    yield block
  end
end

def sh cmd
  puts cmd
  system(cmd) or raise "command #{cmd.inspect} failed"
end

unless ENV['EPOCLOCALRB']
  raise 'EPOCLOCALRB not set'
end
require ENV['EPOCLOCALRB']

kit_name = $op.kit_name || ENV['KITNAME']

unless kit_name
  raise "no kit specified"
end

unless ARGV.empty?
  in_env kit_name do
    sh(ARGV.join(" "))
  end
end

#
# Copyright 2010 Helsinki Institute for Information Technology (HIIT)
# and the authors. All rights reserved.
#
# Authors: Tero Hasu <tero.hasu@hut.fi>
#          Taneli Vähäkangas <taneli.vahakangas@hiit.fi>
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
#

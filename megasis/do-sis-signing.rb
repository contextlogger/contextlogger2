#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

PROG_NAME = File.basename(__FILE__)

PURPOSE = %{

A front-end script for SIS packaging and signing. The devkit and cert
to use is selected based on names given as options.

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

USAGE: #{PROG_NAME} option...

Usage examples:

  KITNAME=s60_30 #{PROG_NAME} --makesis -i file.pkg -o file.sis
  #{PROG_NAME} -k s60_30 --unsign -i file.sisx -o file.sis
  #{PROG_NAME} -k s60_31 --makesis -i file.pkg -o file.sis
  #{PROG_NAME} -k s60_32 --signsis -c dev -i file.sis -o file.sisx
  #{PROG_NAME} -k s60_50 --makesis --signsis -c dev -i file.pkg -o file.sisx
  #{PROG_NAME} -k s60_32 --resign -c new -i file.sisx -o file.sisx

}
  opts.separator ""
  opts.separator "The \"options\" are:"
  opts.separator ""
  opts.on("-c", "--cert NAME", "specifies certificate name") do |name|
    $op.cert_name = name
  end
  opts.on("-h", "--help", "print usage") do
    puts opts.help
    exit 0
  end
  opts.on("-i", "--in FILE", "specifies input file") do |name|
    $op.infile = name
  end
  opts.on("-k", "--kit NAME", "specifies SDK name") do |name|
    $op.kit_name = name
  end
  opts.on("-m", "--makesis", "create SIS") do
    $op.makesis = true
  end
  opts.on("-o", "--out FILE", "specifies output file") do |name|
    $op.outfile = name
  end
  opts.on("--resign", "resign SIS") do
    $op.resign = true
  end
  opts.on("--signsis", "sign SIS") do
    $op.signsis = true
  end
  opts.on("--unsign", "unsign SIS") do
    $op.unsign = true
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

def make_sis pkg, sis
  epocroot = ENV['EPOCROOT']
  raise "EPOCROOT not set" unless epocroot
  sh("makesis -d#{epocroot} #{pkg} #{sis}")
  puts sis
end

def sign_sis cert, plat, sis, sisx
  ci = epoc_cert_info cert, plat
  keyfile = ci.key_file
  certfile = ci.cert_file
  passwd = ci.passphrase
  cmd = "signsis -v -s #{sis} #{sisx} #{certfile} #{keyfile}"
  cmd << " #{passwd.inspect}" if passwd
  sh(cmd)
  puts sisx
end

def unsign_sis plat, sisx, sis
  cmd = "signsis -v -u #{sisx} #{sis}"
  sh(cmd)
  puts sis
end

def make_sign_sis cert, plat, pkg, sisx
  pkg =~ /[.]pkg$/i or raise
  base = $`
  sis = base + ".sis"
  make_sis pkg, sis
  sign_sis cert, plat, sis, sisx
end

unless ENV['EPOCLOCALRB']
  raise 'EPOCLOCALRB not set'
end
require ENV['EPOCLOCALRB']

kit_name = $op.kit_name || ENV['KITNAME']

cert_name = $op.cert_name
if $op.signsis and !cert_name
  raise "cert not specified"
end

if !$op.infile
  raise "no input file specified"
end

if !$op.outfile
  raise "no output file specified"
end

if $op.makesis and $op.signsis
  in_env kit_name do
    make_sign_sis cert_name, kit_name, $op.infile, $op.outfile
  end
elsif $op.makesis
  in_env kit_name do
    make_sis $op.infile, $op.outfile
  end
elsif $op.signsis
  in_env kit_name do
    sign_sis cert_name, kit_name, $op.infile, $op.outfile
  end
elsif $op.unsign
  in_env kit_name do
    unsign_sis kit_name, $op.infile, $op.outfile
  end
elsif $op.resign
  in_env kit_name do
    require 'tempfile'
    sisfile = Tempfile.new("sisfile").path
    unsign_sis kit_name, $op.infile, sisfile
    sign_sis cert_name, kit_name, sisfile, $op.outfile
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

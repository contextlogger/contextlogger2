=begin

Customize this Ruby library to reflect your local Symbian setup, and define an environment variable named EPOCLOCALRB to specify the "require" pathname of the library. (It should be the full path unless it is under your RUBYLIB path.) In other words, tools will import this library with "require ENV['EPOCLOCALRB']".

This simple example uses the included self-signed cert, and assumes that one is using a particular SDK for all targets, and that the environment settings for it have been predefined.

=end

# Internally used utilities, isolated in their own namespace.
module EpocLocalRb
  class CertInfo
    attr_reader :key_file, :cert_file, :passphrase, :max_caps, :sign

    def initialize kf, cf, pp, mc
      @key_file = kf
      @cert_file = cf
      @passphrase = pp
      @max_caps = mc
      @sign = (@cert_file != nil)
    end
  end

  def sh cmd
    puts cmd
    system(cmd) or raise "command #{cmd.inspect} failed"
  end

  extend self
end

SELF30_CAPS = %w{LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData}
SELF32_CAPS = (SELF30_CAPS + %w{Location})

# This function must be defined.
#
# Takes the nickname of a cert and the nickname of a target platform, and returns an object containing [key_file, cert_file, passphrase or nil, max_caps] attributes. Note that the set of maximum capabilities that are allowed self-signed depends on the target platform.
def epoc_cert_info cert, plat
  this_dir = File.dirname(File.expand_path(__FILE__))
  caps = case plat
         when /_3[01]_/
           SELF30_CAPS
         else
           SELF32_CAPS
         end
  args = [File.join(this_dir, "selfsigned.key"),
          File.join(this_dir, "selfsigned.cer"),
          nil,
          caps]
  EpocLocalRb::CertInfo.new(*args)
end

# This function must be defined.
def epoc_set_plat_env plat
end

# This function must be defined.
#
# If you need to set EPOCROOT or whatever to build for a particular target platform, you may do so here. Otherwise just yield.
def epoc_in_plat_env plat # block
  yield
end

#
# Copyright 2010 Helsinki Institute for Information Technology (HIIT)
# and the authors. All rights reserved.
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
#

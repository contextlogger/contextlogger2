=begin rdoc

devkit.rb

Copyright 2006-2010 Helsinki Institute for Information Technology
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

=end

require 'build/lang_ext'
require 'build/path_utils'
require 'build/registry'
require 'pathname'
require 'rbconfig'
require 'sake4/sake'
require 'sake4/target_platform'
require 'sake4/utils'

module Sake
  # A development kit. This is a fairly abstract class, but its
  # instance could be used to refer to uninstalled SDKs, for instance.
  class DevKit
    def self.to_canonical_handle hnd
      lst = hnd.split(/_/)
      raise unless lst.size >= 2
      lst[0] + "_" + lst[1]
    end

    def self.platform_from_handle ahnd
      chnd = to_canonical_handle ahnd
      hnd =
        case chnd
        when 's60_10', 's60_12'
          "s60_09"
        else
          chnd
        end
      Sake::TargetPlatform.new(hnd)
    end

    # While one would generally just specify the target platform to
    # build for, and automatically choose the optimal SDK for doing
    # that, one might sometimes want to specify both the target
    # platform and the SDK to do the build with. Thus we also require
    # a handle for SDKs. This handle is for an SDK instance; the same
    # SDK may have been multiple times, perhaps with same differences
    # between each instance.
    attr_reader :handle

    attr_reader :platform, :target

    def initialize
      @handle or raise

      # Kit platform.
      @platform ||= Sake::TargetPlatform.new(@handle)

      # Target platform.
      @target ||= self.class.platform_from_handle(@handle)
    end

    def vendor_in_pkg?
      platform.s60_3rd_up?
    end

    def supports_python_v? v
      case v
      when 1
        supports_python_v1?
      when 2
        supports_python_v2?
      else
        false
      end
    end

    def supports_python_v1?
      # Here we assume PyS60 SDK v1.3.x or higher. We do not presently
      # specify or try to detect the version of Python SDK installed
      # onto an SDK, although as I recall, at least more recent Python
      # SDKs contain that information in a convenient form.
      File.directory?(File.join(include_home, "python"))
    end

    def supports_python_v2?
      File.directory?(File.join(include_home, "python25"))
    end

    def supports_devices?
      false # we no longer support this
    end

    def prj_platforms
      if platform.f_version.first < 9
        %w{armi thumb wins}
      else
        %w{gcce winscw}
      end
    end
  end

  class GnuPocDevKit < DevKit
    def self.sdk_list
      root = ENV['GNUPOCDEVENV'] || raise
      home = File.join(root, "kits")
      return [] unless File.directory?(home)
      list = []
      Dir[home + "/*"].each do |dir|
        next unless File.directory?(dir)
        dentlist = dir_entries dir, false
        if dentlist.include?("epoc32")
          list.push(new(:handle => File.basename(dir),
                        :sdk_home => dir))
        end
      end
      return list
    end

    attr_reader :sdk_home

    def initialize op = {}
      @handle = (op[:handle] || raise)
      @sdk_home = (op[:sdk_home] || raise)
      @target = op[:target] if op[:target]
      super()
    end

    def devenv_home
      ENV['GNUPOCDEVENV'] || raise
    end

    def bin_home
      # Here we assume the wrappers have been installed, and we also
      # assume this particular directory. Although it does not matter
      # much here, it is handy to have the wrappers around for manual
      # builds.
      File.join(devenv_home, "bin")
    end

    def path
      list = [bin_home, ENV['PATH']]
      list.join(':')
    end

    def epoc32_home
      File.join(sdk_home, "epoc32")
    end

    def include_home
      File.join(epoc32_home, "include")
    end

    def build_home
      File.join(epoc32_home, "build")
    end

    def epocroot
      sdk_home + "/"
    end

    def with_env m, block
      old_m = {}
      m.each_key do |key|
        old_m[key] = ENV[key]
      end
      begin
        m.each do |key, value|
          set_env(key, value)
        end
        block.call
      ensure
        old_m.each do |key, value|
          set_env(key, value)
        end
      end
    end

    def in_env(op = {}, &block)
      m = op.dup
      m['EPOCROOT'] = epocroot
      m['PATH'] = path
      m['KITNAME'] = @handle
      with_env(m, block)
    end

    def has_emulator?
      # Some GnuPoc installations actually support the emulator,
      # apparently, or at least they used to...
      false
    end
  end

  # Provides functionality for determining the set of SDKs available
  # on the development machine.
  module DevKits
    def get_all
      GnuPocDevKit::sdk_list
    end

    def get_all_as_map
      sdkset = get_all
      sdkmap = Hash.new
      for sdk in sdkset
        sdkmap[sdk.handle] = sdk
      end
      return sdkmap
    end

    # The result set will contain exactly the specified set of devices.
    # For unknown devices the returned information will be incomplete.
    # handle_set may be empty.
    def get_exact_set(handle_set)
      sdkmap = get_all_as_map
      handle_set.map do |handle|
        sdk = sdkmap[handle]
        if sdk
          sdk
        else
          DevKit.new(:handle => handle)
        end
      end
    end

    # The result set will contain at most the specified set of devices.
    def get_at_most(handle_set)
      sdkmap = get_all_as_map
      sdklist = []
      for handle in handle_set
        sdk = sdkmap[handle]
        sdklist.push sdk if sdk
      end
      return sdklist
    end

    extend self
  end
end

if $0 == __FILE__
  p Sake::DevKits::get_all
end

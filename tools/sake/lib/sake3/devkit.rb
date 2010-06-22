=begin rdoc

devkit.rb

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
require 'build/path_utils'
require 'build/registry'
require 'pathname'
require 'rbconfig'
require 'sake3/sake'
require 'sake3/target_platform'
require 'sake3/traits'
require 'sake3/utils'

$use_devices = (Config::CONFIG['host_os'] == "cygwin")

$build_env = if ENV['GNUPOCDEVENV']
               :gnupoc
             else
               case Config::CONFIG['host_os']
               when 'cygwin'
                 :cygwin
               when 'linux-gnu'
                 :sdk2unix
               end
             end

module Sake
  # A development kit. This is a fairly abstract class, but its
  # instance could be used to refer to uninstalled SDKs, for instance.
  class DevKit
    # While one would generally just specify the target platform to
    # build for, and automatically choose the optimal SDK for doing
    # that, one might sometimes want to specify both the target
    # platform and the SDK to do the build with. Thus we also require
    # a handle for SDKs. This handle is for an SDK instance; the same
    # SDK may have been multiple times, perhaps with same differences
    # between each instance.
    trait :handle, 'munge' => :to_str

    # This is a canonical identifier for the SDK product whose
    # instance this object represents.
    trait :chandle, 'munge' => :to_str

    include Sake::Traits::TraitInit

    attr_reader :native_target

    # Tries to derive a "chandle" from a handle. If this routine
    # does not recognize a given handle, the "chandle" can still be
    # passed explicitly to the initializer.
    def self.to_canonical_handle hnd
      hnd = hnd.downcase

      # Platform name.
      hnd.sub!(/series60/, "s60")

      # Platform version.
      hnd.sub!(/(v\d)_?(\d)/, "\\1\\2")
      hnd.sub!(/^(n?s60_)(\d+)/, "\\1v\\2")

      # Installation number.
      # If multiple copies of the same SDK are installed,
      # they get a suffix like this, which we shall remove.
      hnd.sub!(/_\d+$/, "")

      # Vendor.
      if hnd !~ /^n/
        hnd = "n" + hnd
      end

      #p hnd

      hnd =
        case hnd
        when "ns60_v26"
          "ns60_2nd_fp2"
        when "ns60_v28"
          "ns60_2nd_fp3"
        when "ns60_v30"
          "ns60_3rd_mr"
        when "ns60_v31"
          "ns60_3rd_fp1"
        when "ns60_v32"
          "ns60_3rd_fp2"
        when "ns60_v50"
          "ns60_5th"
        else
          hnd
        end

      hnd
    end

    def self.platform_from_handle chnd
      case chnd
      when 'ns60_v09', 'ns60_v12'
        # 1st Edition (formerly 0.9)
        # 1st Edition, FP 1 (formerly 1.2)
        Sake::S60Platform.new(:version => [0,9])
      when 'ns60_v20'
        # 2nd Edition (formerly 2.0)
        Sake::S60Platform.new(:version => [2,0])
      when 'ns60_v21'
        # 2nd Edition, FP 1 (formerly 2.1)
        Sake::S60Platform.new(:version => [2,1])
      when 'ns60_2nd_fp2'
        # 2nd Edition, FP 2
        Sake::S60Platform.new(:version => [2,2]) # commonly known as v2.6!
      when 'ns60_2nd_fp3'
        # 2nd Edition, FP 3
        Sake::S60Platform.new(:version => [2,3]) # commonly known as v2.8!
      when 'ns60_3rd', 'ns60_3rd_mr'
        # 3rd Edition
        # 3rd Edition, Maintenance Release
        Sake::S60Platform.new(:version => [3])
      when 'ns60_3rd_fp1'
        # 3rd Edition, FP1
        Sake::S60Platform.new(:version => [3,1])
      when 'ns60_3rd_fp2'
        # 3rd Edition, FP2
        Sake::S60Platform.new(:version => [3,2])
      when "ns60_5th"
        # 5th Edition
        Sake::S60Platform.new(:version => [5,0])
      else
        raise "cannot determine platform from #{chnd}"
      end
    end

    def initialize op = {}
      trait_init op

      @handle or raise

      # We accept all known devices.xml device id entries, even if
      # the id attribute values do not follow our conventions.
      @chandle ||= self.class.to_canonical_handle(@handle)

      @native_target ||= self.class.platform_from_handle(@chandle)
    end

    def vendor
      case @chandle
      when /^n/
        "Nokia"
      else
        raise
      end
    end

    def vendor_in_pkg?
      @native_target.edition >= 3
    end

    # Deprecated.
    def supports_python?
      supports_python_v1?
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
      $use_devices and !%w{ns60_v09 ns60_v10 ns60_v12}.include?(@chandle)
    end

    def prj_platforms
      case @chandle
      when 'ns60_3rd_mr', 'ns60_3rd_fp1', 'ns60_3rd_fp2', 'ns60_5th'
        %w{gcce winscw}
      else
        %w{armi thumb wins}
      end
    end
  end

  # An sdk2unix-based devkit.
  class Sdk2UnixDevKit < DevKit
    def self.sdks_home
      # We have been using this path by convention.
      File.join(ENV['HOME'] || raise, "symbian")
    end

    def self.sdk_list
      home = self.sdks_home
      return [] unless File.directory?(home)
      list = []
      Dir[home + "/*"].each do |dir|
        next unless File.directory?(dir)
        dentlist = dir_entries dir, false
        if dentlist.include?("bin") and
            dentlist.include?("include") and
            dentlist.include?("tools")
          list.push(new(:basename => File.basename(dir)))
        end
      end
      return list
    end

    def initialize op = {}
      @basename = (op[:basename] || raise)
      op = op.dup
      op.delete(:basename)
      op += {:handle => @basename} unless op[:handle]
      super(op)
    end

    def sdk_home
      File.join(self.class.sdks_home, @basename)
    end

    # Returns nil if the default GCC is to be used.
    def gcc_home
      return nil unless $epoc_gcc
      File.join(self.class.sdks_home, $epoc_gcc)
    end

    def bin_home
      File.join(gcc_home || sdk_home, "bin")
    end

    def include_home
      File.join(sdk_home, "include")
    end

    def path
      list = [bin_home] + %w{/usr/local/bin /usr/bin /bin}
      list.join(':')
    end

    def in_env # block
      setgcc = gcc_home
      old_epoc = ENV['EPOC']
      old_gccpath = ENV['GCCPATH'] if setgcc
      old_path = ENV['PATH']
      begin
        set_env('EPOC', sdk_home)
        set_env('GCCPATH', gcc_home) if setgcc
        set_env('PATH', path)
        yield
      ensure
        set_env('EPOC', old_epoc)
        set_env('GCCPATH', old_gccpath) if setgcc
        set_env('PATH', old_path)
      end
    end

    def has_emulator?
      # Some GnuPoc installations support the emulator, but sdk2unix
      # ones do not.
      false
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
        basename = File.basename(dir)
        next if basename =~ /^sf_/ # not supported by sake3
        dentlist = dir_entries dir, false
        if dentlist.include?("epoc32")
          list.push(new(:basename => basename, :sdk_home => dir))
        end
      end
      return list
    end

    trait :sdk_home, 'munge' => :to_str

    def initialize op = {}
      @basename = (op[:basename] || raise)
      op = op.dup
      op.delete(:basename)
      op += {:handle => @basename} unless op[:handle]
      #p op
      super(op)
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

    def in_env # block
      old_epocroot = ENV['EPOCROOT']
      old_path = ENV['PATH']
      begin
        set_env('EPOCROOT', epocroot)
        set_env('PATH', path)
        yield
      ensure
        set_env('EPOCROOT', old_epocroot)
        set_env('PATH', old_path)
      end
    end

    def has_emulator?
      # Some GnuPoc installations actually support the emulator,
      # apparently, or at least they used to...
      false
    end
  end

  # This is a concrete DevKit class that allows builds in a Cygwin
  # environment using a "normal" Windows-based Symbian SDK, e.g. this
  # class has a method for setting the environment of a process to
  # what is required for finding all the standard Symbian build tools.
  #
  # The "devices" script contained in modern Symbian SDKs does not
  # work under Cygwin, it appears. Thus we this class has to try
  # and deal with the paths without help from "devices", which is
  # unfortunate duplication of work. We must ensure that we have
  # ActivePerl on the path, though, and under Cygwin, we can call
  # the .pl script directly.
  class CygwinDevKit < DevKit
    def self.sdk_list
      list = []
      epocroot = ENV['EPOCROOT']
      if epocroot
        list = [CygwinDevKit.new(epocroot)]
      elsif defined? @@sdk_cache
        list = @@sdk_cache.dup
      else
        begin
          commonprogramfiles = ENV['COMMONPROGRAMFILES'] ||
            'c:\\program files\\common files'
          conffile = commonprogramfiles + '\\symbian\\devices.xml'
          require 'rexml/document'
          doc = REXML::Document.new(File.read(conffile))
          doc.elements.each("devices/device") do |elem|
            handle = elem.attributes["id"]
            plat = elem.attributes["name"]
            epocroot = nil
            elem.elements.each("epocroot") do |eroot|
              epocroot = eroot.text
            end
            raise unless epocroot
            list << CygwinDevKit.new(handle, plat, epocroot)
          end
        rescue
          raise
        end

        @@sdk_cache = list.dup
      end
      return list
    end

    attr_accessor :path_additions
    attr_reader :dos_epocroot, :devices_handle

    def initialize(handle, plat, kitroot)
      #p [handle, plat, kitroot]
      super(:handle => handle)

      # The devices.pl script uses handles such as this one.
      @devices_handle = handle + ":" + plat

      kitroot = kitroot.downcase
      if kitroot !~ /\\$/
        kitroot << '\\'
      end

      if kitroot =~ /^([a-z]):(.*)/
        drive = $1
        kitroot = $2
        @dos_epocroot = kitroot
      else
        drive = 'c'
        @dos_epocroot = kitroot
      end

      @cyg_epocroot = '/cygdrive/' + drive + kitroot.gsub(/\\/, '/')

      has_shared = has_shared?

      @path_additions = []

      # Note that Cygwin provides a tool called "regtool" that one can
      # use to query the registry.
      # See http://www.cygwin.com/cygwin-ug-net/using-utils.html
      #
      # For now, the user should make sure that if any of these
      # paths are not correct on her system, they are added
      # to the path prior to doing builds. And they must be before
      # /usr/bin and the like, as otherwise wrong versions might get used.

      activeperl = nil
      key = '/HKLM/SOFTWARE/ActiveState/ActivePerl'
      if Registry::has_key?(key)
        value = Registry::get_value(key, "CurrentVersion")
        if value
          value = Registry::get_value("#{key}/#{value}", nil)
          if value
            activeperl = File.join(path_cygwinize(value), "bin")
          end
        end
      end
      if activeperl
        @path_additions << activeperl
      else
        warn("could not determine ActiveState Perl path")
      end

      # vcvars32.bat should be able to give these to us, if we could quickly and reliably determine the location of that file somehow, but for now, we shall assume that these are on the PATH already.
      @path_additions << "/cygdrive/c/program files/microsoft visual studio/common/tools/winnt:/cygdrive/c/program files/microsoft visual studio/common/msdev98/bin:/cygdrive/c/program files/microsoft visual studio/common/tools:/cygdrive/c/program files/microsoft visual studio/vc98/bin"

      want_gcc = true # ((prj_platforms & %w{armi thumb gcce}) != [])
      want_gcce = prj_platforms.include? "gcce"

      if want_gcce
        # XXX How to look this path up?
        armtoolchain = "/cygdrive/c/apps/armtoolchain/"
        @path_additions << armtoolchain + "bin"
        # @path_additions << armtoolchain + "arm-none-symbianelf/bin"
      end

      if supports_devices?
        key = '/HKLM/SOFTWARE/Symbian/EPOC SDKs'
        if Registry::has_key?(key)
          value = Registry::get_value(key, "CommonPath")
          if value
            @path_additions << File.join(path_cygwinize(value), "tools")
          end
        end
      else
        if has_shared
          # assuming a particular relative location for the shared tools
          shared_home = File.join(File.dirname(@cyg_epocroot), "shared")
          @path_additions << File.join(shared_home, 'epoc32', 'tools')

          if want_gcc
            @path_additions << File.join(shared_home, 'epoc32', 'gcc', 'bin')
          end
        else
          @path_additions << File.join(epoc32_home, 'tools')

          if want_gcc
            @path_additions << File.join(epoc32_home, 'gcc', 'bin')
          end
        end
      end
    end

    def has_shared?
      %w{ns60_v09 ns60_v10 ns60_v12}.include?(@chandle)
    end

    def epoc32_home
      @cyg_epocroot + 'epoc32'
    end

    def path
      # It is important to add these to the front of the path,
      # as we want to make sure that the correct version of say
      # GCC or Perl gets used.
      (@path_additions + [ENV['PATH']]).join(':')
    end

    def in_env # block
      use_devices = supports_devices?
      old_epocroot = ENV['EPOCROOT'] unless use_devices
      old_path = ENV['PATH']
      begin
        set_env('EPOCROOT', @dos_epocroot) unless use_devices
        set_env('PATH', path)
        yield
      ensure
        set_env('EPOCROOT', old_epocroot) unless use_devices
        set_env('PATH', old_path)
      end
    end

    def include_home
      File.join(epoc32_home, "include")
    end

    def build_home
      File.join(epoc32_home, "build")
    end

    def supports_msvc6?
      # MSVC6 building is broken in series60_v21. It is not supported
      # at all in s60_2nd_fp3, although MSVC 2003 support apparently
      # is available with Carbide.vs.
      case @chandle
      when 'ns60_v09', 'ns60_v12', 'ns60_v20', 'ns60_2nd_fp2'
        true
      else
        false
      end
    end

    def has_emulator?
      true
    end

    # This returns all known emulator ABI variants,
    # regardless of whether this devkit supports them all.
    def all_emulator_abi_variants
      %w{wins winscw}
    end

    def build_variants
      %w{urel udeb}
    end

    def emulator_logs_home variant
      File.join(epoc32_home, variant, 'c/logs')
    end

    def each_emulator_app_log_file basename
      for variant in all_emulator_abi_variants
        loghome = emulator_logs_home variant
        applogs = File.join(loghome, basename)
        next unless File.directory? applogs
        for file in Dir.globi(File.join(applogs, '*.txt'))
          yield file
        end
      end
    end

    def each_emulator_app_binary basename
      for variant in all_emulator_abi_variants
        for btype in build_variants
          for file in Dir.globi(File.join(epoc32_home, "release", variant, btype, basename + ".*"))
            yield file
          end
          for file in Dir.globi(File.join(epoc32_home, "release", variant, btype, "z", "system", "libs", basename + ".*"))
            yield file
          end
        end
      end
    end
  end

  # Provides functionality for determining the set of SDKs available
  # on the development machine.
  module DevKits
    def get_all
      case $build_env
      when :gnupoc
        GnuPocDevKit::sdk_list
      when :cygwin
        CygwinDevKit::sdk_list
      when :sdk2unix
        Sdk2UnixDevKit::sdk_list
      else
        # Do not know where to look for device info on this system.
        []
      end
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
    #
    # XXX using multiple copies of the exact same SDK is presently unsupported
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

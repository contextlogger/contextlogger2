=begin rdoc

target_platform.rb

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
require 'pathname'
require 'sake0/sake'
require 'sake0/utils'

module Sake
  # Provides a number of methods for classes that implement the
  # "trait_map" method.
  module PlatformTraits
    def c_defines
      Sake::macrify_map(trait_map)
    end

    def platform_id_list name, version
      list = [name]
      version = version.dup
      idlist = [name.to_s]
      until version.empty?
        idlist.push(version.shift.to_s)
        list.push(idlist.join("_"))
      end
      list.map! {|x| x.to_sym}
      return list
    end
  end

  class TargetPlatform
    include PlatformTraits
  end

  # A Symbian OS platform.
  class SymbianPlatform < TargetPlatform
    trait :version, 'cast' => proc {|x| Array(x)}

    include DefaultTraitInit
    include Memoize

    def initialize op = {}
      default_trait_init op
      memoize(:handle)
    end

    def major_version
      version.first
    end

    def minor_version
      ver = version.second
      (ver && ver.is_a?(Integer)) ? ver : 0
    end

    def version_suffix
      ver = version.last
      (ver && ver.is_a?(String)) ? ver : nil
    end

    # This is intended to be something that the user could type on the
    # command line to specify a platform to target.
    def handle
      minor = minor_version
      suffix = version_suffix
      "symbian_v#{major_version}" +
        ((minor != 0) ? ".#{minor}" : "") +
        (suffix ? suffix : "")
    end

    # Returns 1 for EKA1, and 2 for EKA2.
    def eka_version
      if @version.first < 8
        1
      elsif @version.first > 8
        2
      elsif @version.last == "a"
        1
      elsif @version.last == "b"
        2
      else
        raise
      end
    end

    def trait_map
      list = ["eka#{eka_version}".to_sym] +
        platform_id_list("symbian", @version)

      # RFileLogger available on 7.0s-up
      list.push(:has_flogger) if (@version <=> [7, 0, "s"]) >= 0

      # CCamera available on 7.0s-up
      list.push(:has_ccamera) if (@version <=> [7, 0, "s"]) >= 0

      # Bluetooth stack and hardware settings managed with
      # the Publish and Subscribe API
      list.push(:has_bt_subscribe) if (@version <=> [8]) >= 0

      list.push(:btdevicename_in_btmanclient) if [6, 7].include?(major_version)

      # KRFCOMMPassiveAutoBind, etc.
      list.push(:has_bt_auto_bind) if (@version <=> [8]) >= 0

      # TRfcommSockAddr::SetSecurity
      list.push(:has_bt_set_security) if (@version <=> [8]) >= 0

      # TInt64 macros
      list.push(:has_i64_macros) if (@version <=> [7, 0, "s"]) >= 0

      # TThreadStackInfo
      list.push(:has_thread_stack_info) if (@version <=> [9]) >= 0

      Sake::define_value_map(list)
    end
  end

  # An S60 platform.
  class S60Platform < TargetPlatform
    trait :version, 'cast' => proc {|x| Array(x)}

    include DefaultTraitInit
    include Memoize
    include Delegation

    def initialize op = {}
      default_trait_init op
      memoize(:symbian_platform)
      memoize(:handle)
      delegate_to(symbian_platform)
    end

    def symbian_platform
      case edition
      when 0, 1
        SymbianPlatform.new(:version => 6)
      when 2
        case fp
        when 0, 1
          SymbianPlatform.new(:version => [7, 0, "s"])
        when 2
          SymbianPlatform.new(:version => [8, 0, "a"])
        when 3
          SymbianPlatform.new(:version => [8, 1, "a"])
        else
          raise
        end
      when 3
        case fp
        when 0
          SymbianPlatform.new(:version => [9, 1])
        when 1
          SymbianPlatform.new(:version => [9, 2])
        else
          raise
        end
      else
        raise
      end
    end

    # This is intended to be something that the user could type on the
    # command line to specify a platform to target.
    def handle
      case edition
      when 0
        # s60_0th_fp9 looks weird to say the least, so...
        if fp == 9
          "s60_v0.9"
        else
          raise
        end
      else
        "s60_" + Sake::to_rank(edition) + ((fp == 0) ? "" : "_fp#{fp}")
      end
    end

    # S60 edition.
    def edition
      version.first
    end

    # Feature pack.
    def fp
      version.second || 0
    end

    # Each (Edition, FP) combo has its own ID.
    def platform_id
      case [edition, fp]
      when [0, 9]
        0x101f6f88
      when [1, 0]
        0x101f795f
      when [1, 2]
        0x101f8202
      when [2, 0]
        0x101f7960
      when [2, 1]
        0x101f9115
      when [2, 2]
        0x101f9115
      when [2, 3]
        0x10200bab
      when [3, 0]
        0x101f7961
      when [3, 1]
        0x102032be
      else
        raise
      end
    end

    def pkg_dependency_string
      pkg_dependency_syntax % [platform_id, 0, 0, 0, pkg_dependency_text]
    end

    def pkg_dependency_syntax
      if symbian_platform.major_version < 9
        '(0x%08x), %d, %d, %d, {"%s"}'
      else
        # Platform dependency considered a hardware dependency.
        '[0x%08x], %d, %d, %d, {"%s"}'
      end
    end

    def pkg_dependency_text
      # Apparently it is customary to use this particular string.
      # Also, "Pre-Symbian OS v9 the installer discriminates Product
      # ID dependencies from component dependencies based on the
      # string name ending in text "ProductID"".
      "Series60ProductID"
    end

    def trait_map
      list = platform_id_list("s60", @version)

      # The vibra control API is supposedly available in the Series 60
      # 2.0 platform, but models such as Nokia 6600, 6260, and 7610,
      # for instance, have no vibractrl.dll, or at least not all of
      # them do, so here we are assuming that it is only available
      # from Series 60 v2.6 onwards.
      list.push :has_vibractrl if ((@version <=> [2, 2]) >= 0)

      list.push :has_hwrmvibra if edition >= 3

      symbian_platform.trait_map + Sake::define_value_map(list)
    end
  end

  # An S60 product. In rare cases one may want to target only a
  # specific product; for instance, one might support only Nokia E61,
  # due to an application being tailored for the bigger-than-usual
  # screen size.
  class S60Product < TargetPlatform
    trait :name, 'munge' => :to_str

    include DefaultTraitInit
    include Memoize
    include Delegation

    def initialize op = {}
      default_trait_init op
      memoize(:handle)
      memoize(:s60_platform)
      delegate_to(s60_platform, s60_platform.symbian_platform)
    end

    # This is intended to be something that the user could type on the
    # command line to specify a platform to target.
    def handle
      @name.to_str.strip.downcase.split(/[_ ]+/).join("_")
    end

    def s60_platform
      case handle
      when "nokia_6260", "nokia_7610"
        Sake::S60Platform.new(:version => [2,1])
      when "nokia_6600"
        Sake::S60Platform.new(:version => [2,0])
      when "nokia_6630"
        Sake::S60Platform.new(:version => [2,2])
      when "nokia_e61"
        Sake::S60Platform.new(:version => [3,0])
      when "nokia_n70", "nokia_n72"
        Sake::S60Platform.new(:version => [2,3])
      else
        raise
      end
    end

    def pkg_dependency_text
      @name + " ID"
    end

    # From S60 3rd Ed onwards it is possible to list multiple product
    # IDs in a PKG file, and the SIS will install on any of those
    # products without complaints. On 1st and 2nd editions, only one
    # product ID is allowed. Note that if any product IDs are listed,
    # then no S60 ID should be included.
    def platform_id
      case handle
      when "nokia_6260"
        0x101fb3f4
      when "nokia_6600"
        0x101f7963
      when "nokia_6630"
        0x101f7964
      when "nokia_7610"
        0x101fd5db
      when "nokia_e61"
        0x20001858
      when "nokia_n70", "nokia_n72"
        0x10200f9a
      else
        raise
      end
    end

    def trait_map
      s60_platform.trait_map + {handle.to_sym => nil}
    end
  end

  # Some components target Python for S60. This class provides
  # information about Python installations. There are differences
  # between Python installations depending on Python version and S60
  # version, and we try to capture those differences here.
  class PyS60Platform
    trait :version, 'cast' => proc {|x| Array(x)}
    trait :s60_platform, 'validate' => proc {|x| x.is_a? S60Platform}

    include DefaultTraitInit

    def pyd_install_requires_sis?
      s60_platform.edition >= 3
    end

    def pyd_needs_wrapper?
      s60_platform.edition >= 3
    end

    def application_id
      # XXX
    end
  end
end

if $0 == __FILE__
  plat = Sake::SymbianPlatform.new(:version => [8, 0, "a"])
  p [plat.handle, plat.trait_map, plat.c_defines]

  plat = Sake::S60Platform.new(:version => [2, 1])
  p [plat.handle, plat.c_defines,
    plat.symbian_platform, plat.eka_version]

  plat = Sake::S60Product.new(:name => "Nokia E61")
  p [plat.handle, plat.c_defines,
    plat.symbian_platform, plat.eka_version]

  plat = Sake::S60Product.new(:name => "Nokia 6260")
  p [plat.handle, plat.c_defines,
    plat.symbian_platform, plat.eka_version]
end

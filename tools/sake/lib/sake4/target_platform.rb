=begin rdoc

target_platform.rb

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
require 'pathname'
require 'sake4/sake'
require 'sake4/traits'
require 'sake4/utils'

module Sake
  class TargetPlatform
    include Memoize

    attr_reader :handle, :handle_version
    attr_reader :epoc_version, :s60_version, :sf_version
    attr_reader :f_version # "forced" version

    def initialize handle
      @handle = handle

      @handle_version = handle.split(/_/)

      if handle_version.first == "s60"
        case handle_version.second
        when "09"
          @s60_version = [0, 9]
        when "10"
          @s60_version = [1, 0]
        when "12"
          @s60_version = [1, 2]
        when "20"
          @s60_version = [2, 0]
        when "21"
          @s60_version = [2, 1]
        when "26"
          @s60_version = [2, 6]
        when "28"
          @s60_version = [2, 8]
        when "30"
          @s60_version = [3, 0]
        when "31"
          @s60_version = [3, 1]
        when "32"
          @s60_version = [3, 2]
        when "50"
          @s60_version = [5, 0]
        else
          raise
        end
        determine_epoc_version
      elsif handle_version.first == "sf"
        @sf_version = handle_version.second.to_i
      else
        raise
      end

      if sf_version
        if sf_version == 1
          @f_version = [9, 4]
        elsif sf_version >= 2
          @f_version = [9 + sf_version] # at least 10
        else
          raise
        end
      elsif epoc_version
        @f_version = epoc_version
      else
        raise
      end
    end

    def determine_epoc_version
      case edition
      when 0, 1
        @epoc_version = [6]
      when 2
        case fp
        when 0, 1
          @epoc_version = [7, 0, "s"]
        when 6
          @epoc_version = [8, 0, "a"]
        when 8
          @epoc_version = [8, 1, "a"]
        else
          raise
        end
      when 3
        case fp
        when 0
          @epoc_version = [9, 1]
        when 1
          @epoc_version = [9, 2]
        when 2
          @epoc_version = [9, 3]
        else
          raise
        end
      when 5
        case fp
        when 0
          @epoc_version = [9, 4]
        else
          raise
        end
      else
        raise
      end
    end

    def epoc?
      epoc_version != nil
    end

    def s60?
      s60_version != nil
    end

    def sf?
      sf_version != nil
    end

    def s60_3rd_up?
      (s60? and edition >= 3) or sf?
    end

    def major_epoc_version
      return nil unless epoc_version
      epoc_version.first
    end

    def minor_epoc_version
      return nil unless epoc_version
      ver = epoc_version.second
      (ver && ver.is_a?(Integer)) ? ver : 0
    end

    def epoc_version_suffix
      return nil unless epoc_version
      ver = epoc_version.last
      (ver && ver.is_a?(String)) ? ver : nil
    end

    # Returns 1 for EKA1, and 2 for EKA2.
    def eka_version
      if sf?
        2
      elsif major_epoc_version < 8
        1
      elsif major_epoc_version > 8
        2
      elsif epoc_version_suffix == "a"
        1
      elsif epoc_version_suffix == "b"
        2
      else
        raise
      end
    end

    # S60 edition.
    def edition
      return nil unless s60_version
      s60_version.first
    end

    # Feature pack.
    def fp
      return nil unless s60_version
      s60_version.second
    end

    def pkg_dependency_string
      pkg_dependency_syntax % [platform_id, 0, 0, 0, pkg_dependency_text]
    end

    def pkg_dependency_syntax
      if epoc? and major_epoc_version < 9
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
      # string name ending in text 'ProductID'".
      "Series60ProductID"
    end

    # Each (Edition, FP) combo has its own ID.
    def platform_id
      if s60?
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
        when [2, 6]
          0x101f9115
        when [2, 8]
          0x10200bab
        when [3, 0]
          0x101f7961
        when [3, 1]
          0x102032be
        when [3, 2]
          0x102752ae
        when [5, 0]
          0x1028315f
        else
          raise "unsupported S60 version"
        end
      else
        raise "unsupported platform version"
      end
    end

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

    def trait_map
      vlist = {:eka_version => (eka_version * 10)}
      dlist = ["eka#{eka_version}".to_sym]

      if epoc?
        vlist[:symbian_version] = (major_epoc_version * 10 +
                                   minor_epoc_version)
        dlist += platform_id_list("symbian", epoc_version)
      end

      if s60?
        vlist[:s60_version] = (edition * 10 + fp)
        dlist += platform_id_list("s60", s60_version)
      end

      # RFileLogger available on 7.0s-up
      dlist.push(:has_flogger) if (f_version <=> [7, 0, "s"]) >= 0

      # CCamera available on 7.0s-up
      dlist.push(:has_ccamera) if (f_version <=> [7, 0, "s"]) >= 0

      # Bluetooth stack and hardware settings managed with
      # the Publish and Subscribe API
      dlist.push(:has_bt_subscribe) if (f_version <=> [8]) >= 0

      if [6, 7].include?(major_epoc_version)
        dlist.push(:btdevicename_in_btmanclient)
      end

      # KRFCOMMPassiveAutoBind, etc.
      dlist.push(:has_bt_auto_bind) if (f_version <=> [8]) >= 0

      # TRfcommSockAddr::SetSecurity
      dlist.push(:has_bt_set_security) if (f_version <=> [8]) >= 0

      # TInt64 macros
      dlist.push(:has_i64_macros) if (f_version <=> [7, 0, "s"]) >= 0

      # TThreadStackInfo
      dlist.push(:has_thread_stack_info) if (f_version <=> [9]) >= 0

      if s60?
        # SysStartup
        # Do not know if S60 v1 has this, but v2 appears to, and v3.2 no
        # longer does.
        dlist.push(:has_sys_startup) if ((s60_version <=> [3, 1]) <= 0)

        # xxx Are the following still available in Symbian^2 and up?

        # The vibra control API is supposedly available in the Series 60
        # 2.0 platform, but models such as Nokia 6600, 6260, and 7610,
        # for instance, have no vibractrl.dll, or at least not all of
        # them do, so here we are assuming that it is only available
        # from Series 60 v2.6 onwards.
        dlist.push :has_vibractrl if ((s60_version <=> [2, 2]) >= 0)

        dlist.push :has_hwrmvibra if edition >= 3
      end

      vlist + Sake::define_value_map(dlist)
    end
  end
end

if $0 == __FILE__
  for hand in %w{s60_09 s60_10 s60_32 sf_3}
    plat = Sake::TargetPlatform.new(hand)
    p [plat, plat.eka_version, plat.edition, plat.fp]
  end
end

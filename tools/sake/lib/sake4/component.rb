=begin rdoc

component.rb

Copyright 2006-2007 Helsinki Institute for Information Technology
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

require 'build/path_utils'
require 'pathname'

require 'sake4/sake'

require 'sake4/delegation'
require 'sake4/utils'

require 'sake4/devkit'
require 'sake4/target_platform'

module Sake
  SELF_SIGNED_CAPS = %w{LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData}

  DEV_CERT_CAPS = (SELF_SIGNED_CAPS + %w{Location PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData}).sort

  ALL_CAPS = (DEV_CERT_CAPS + %w{CommDD MultimediaDD DRM DiskAdmin NetworkControl AllFiles TCB}).sort

  DLL_CAPS = ALL_CAPS - %w{TCB}    # typical for manufacturer libraries
end

class Sake::Uid
  class OneUid
    def initialize number
      @number = number.to_int
    end

    attr_reader :number

    def hex_string
      '%08x' % (@number || raise)
    end

    def chex_string
      '0x' + hex_string
    end
  end

  trait :v8, 'cast' => proc {|x| OneUid.new(x)}
  trait :v9, 'cast' => proc {|x| OneUid.new(x)}

  def initialize uid_v8, uid_v9
    if uid_v8 and uid_v9
      self.v8 = uid_v8
      self.v9 = uid_v9
    elsif uid_v8
      self.v8 = uid_v8
      if (uid_v8 & 0xf0000000) == 0x10000000
        # Legacy UID compatibility range. Allocated from Symbian for
        # v8. Of the form 0x1XXXXXXX in v8. Replace first 1 with an f
        # to get a v9 legacy UID. The legacy range falls within
        # the unprotected range, and hence only works for self-signed
        # and DevCert-signed stuff.
        self.v9 = 0xf0000000 | uid_v8
      elsif ((uid_v8 & 0xf0000000) == 0) and
          ((uid_v8 & 0x0f000000) != 0)
        # The E range, i.e. 0xexxxxxxx is the test UID range on v9.
        # In v8, it is (0x01000000 to 0x0FFFFFFF).
        self.v9 = 0xe0000000 | uid_v8
      elsif uid_v8 == 0
        self.v9 = 0
      end
    elsif uid_v9
      self.v9 = uid_v9
    end
  end

  def self.v8 uid_v8
    new uid_v8, nil
  end

  def self.v9 uid_v9
    new nil, uid_v9
  end
end

# Build independent information about a project. A project is a
# collection of related components and files. It is assumed that there
# is a single bld.inf file and a single SIS package per project, while
# there may be multiple DLLs to go into the SIS file.
class Sake::Project
  # Used in naming PKG and SIS files, for instance.
  trait :basename, 'munge' => :to_str

  # The name of the project. Used within PKG file.
  trait :name, 'munge' => :to_str

  # Used as the SIS UID.
  trait :uid, 'validate' => proc {|x| x.kind_of?(Sake::Uid)}

  # Used as the SIS version, for instance.
  trait :version, 'cast' => proc {|x| Array(x)}

  # The project directory.
  trait :dir, 'cast' => Sake::method(:ensure_pathname).to_proc

  # Vendor name. Used in the PKG file, possibly.
  trait :vendor, 'munge' => :to_str

  include Sake::Traits::TraitInit

  def initialize op = {}
    trait_init op
    @dir ||= ($sakefile ?
              Sake::ensure_pathname(File.dirname($sakefile)) :
                Sake::ensure_pathname(Dir.pwd))
    @version ||= [0, 0]
  end

  def major_version
    version[0]
  end

  def minor_version
    version[1]
  end

  def version_string
    '%d.%02d' % [major_version, minor_version]
  end

  def within_tar_path
    '%s-%s' % [@basename, version_string]
  end

  def src_tar_file
    @dir + ('%s-%s-src.tar.gz' % [@basename, version_string])
  end

  def doxyfile_file
    @dir + "doxyfile"
  end

  def doxyfile_in_file
    src_dir + "doxyfile.in"
  end

  def src_dir
    @dir + "src"
  end

  def lib_dir
    @dir + "lib"
  end

  def ruby_src_dir
    lib_dir
  end

  def src_file name
    src_dir + name
  end

  def public_cxx_api_dir
    @dir + "public-cxx-api"
  end

  def private_cxx_api_dir
    @dir + "private-cxx-api"
  end

  def python_api_dir
    @dir + "python-api"
  end

  def ruby_api_dir
    @dir + "ruby-api"
  end

  def download_dir
    @dir + "download"
  end

  def group_dir
    src_dir
  end

  def std_sis_dir
    group_dir
  end

  def in_file_for file
    group_dir + (file.basename.to_s + ".in")
  end

  def bld_inf_in_file
    group_dir + "bld.inf.in"
  end

  def pkg_in_file
    std_sis_dir + (@basename + ".pkg.in")
  end

  def sis_basename
    basename
  end

  # Makes the given pathname "pn" project dir relative, returning the
  # result. The master makefile should use such paths.
  def to_proj_rel pn
    pn.relative_path_from(@dir)
  end
end

# Build-independent information about a component.
#
# A component is some binary and associated files that are built.
# There may be multiple source files, but it is assumed that there is
# a single corresponding MMP file.
#
# You can specify any component-related information, such as :name
# (for display to the user), :basename (for use in filenames), UID(s),
# :version; non-relevant information naturally need not be provided. A
# component may consist of multiple files, e.g. an .app file, and
# related resource files.
class Sake::Component
  trait :project, 'validate' => proc {|x| x.is_a? Sake::Project}

  trait :target_type, 'munge' => :to_sym

  # Long name in an application information file.
  trait :name, 'munge' => :to_str

  # Short name in an application information file.
  trait :short_name, 'munge' => :to_str

  # Used as the component UID in the MMP file. This is not always
  # required, in which case it may be left as a nil value.
  trait :uid3, 'validate' => proc {|x| x.kind_of?(Sake::Uid)}

  # The .mmp file name is derived from this.
  trait :basename, 'munge' => :to_str

  # The binary file name is derived from this.
  trait :bin_basename, 'munge' => :to_str

  # The desired capabilities. It may not be possible to assign all the
  # specified capabilities to the binary, however; depends on the
  # certificate used.
  trait :caps, 'cast' => proc {|x| Array(x)}

  include Sake::Traits::TraitInit
  include Sake::Delegation

  def initialize op = {}
    trait_init op
    raise unless @project
    make_delegating_methods(@project, :exclude => self.class.instance_methods)

    @caps ||= []
    raise unless @name or @basename
    if @name and not @basename
      @basename = @name.gsub(/ /, "_").downcase
    elsif @basename and not @name
      @name = @basename
    end
    @bin_basename ||= @basename
    @short_name ||= @name
  end

  def uid2
    uidnum =
      case target_type
      when :dll, :pyd
        0x1000008d # for both v8 and v9
      when :application
        0x100039ce # for both v8 and v9
      when :exe, :exedll
        0
      when :staticlibrary
        0x01000000 # for v9 at least (maybe not supported for v8)
      else
        raise "UID2 for #{target_type}?"
      end
    Sake::Uid.new uidnum, uidnum
  end

  def mmp_in_file
    group_dir + (@basename + ".mmp.in")
  end
end

# Like Sake::Project, but contains build variant specific information.
# Changes in the relevant project will also be reflected in the
# variant.
class Sake::ProjBuild
  trait :project, 'validate' => proc {|x| x.is_a? Sake::Project}

  trait :target, 'validate' => proc {|x| x.kind_of? Sake::TargetPlatform}
  trait :devkit, 'validate' => proc {|x| x.kind_of? Sake::DevKit}

  # The "abld" build platform. E.g., "gcce", "winscw". A specific
  # build is to be specified, since there is a single SIS file per
  # project build, and a specific set of binaries is to go into that
  # SIS file. However, if there are additional builds, such as an
  # emulator build, that does not go into a SIS, then that could
  # perhaps be supported, but for simplicity, we do not.
  trait :abld_platform, 'munge' => :to_str

  # The "abld" build "build". E.g., "urel", "udeb".
  trait :abld_build, 'munge' => :to_str

  # The handle defaults to the target platform handle, but the user
  # might want to specify a handle, say if doing multiple build
  # variants for a particular target.
  trait :handle, 'munge' => :to_str

  # Properties such as compile-time defines.
  trait :trait_map, 'munge' => :to_hash

  trait :sign, 'validate' => proc {|x| x == true or x == false}

  # Maximum caps allowed by certificate.
  trait :max_caps, 'cast' => proc {|x| Array(x)}

  trait :cert_file, 'cast' => Sake::method(:ensure_pathname).to_proc
  trait :key_file, 'cast' => Sake::method(:ensure_pathname).to_proc
  trait :passphrase, 'munge' => :to_str

  # There is no hurry in initializing this.
  trait :comp_builds, 'cast' => proc {|x| Array(x)}

  # Leave as nil to use the default compiler.
  trait :gcc_version, 'munge' => :to_int

  include Sake::Traits::TraitInit
  include Sake::Delegation

  def initialize op = {}
    trait_init op
    raise unless @devkit
    raise unless @project

    @target ||= @devkit.target
    @trait_map ||= @target.trait_map.dup
    @handle ||= @devkit.handle

    make_delegating_methods(@project, :exclude => self.class.instance_methods)
  end

  # This allows for setting the signing information based on the
  # EPOCLOCALRB settings. You just need to provide a certificate name,
  # possibly passed in via $sake_op[:cert], for instance.
  def set_epoclocalrb_cert_info name
    if target.major_epoc_version < 9
      sign = false
    else
      epoclocalrb = ENV['EPOCLOCALRB'] or raise "EPOCLOCALRB not set"
      require ENV['EPOCLOCALRB']
      ci = epoc_cert_info name, @devkit.handle
      self.max_caps = ci.max_caps
      self.sign = ci.sign
      if ci.sign
        self.cert_file = ci.cert_file
        self.key_file = ci.key_file
        self.passphrase = ci.passphrase
      end
    end
  end

  def basename
    @handle
  end

  def uid
    if @project.uid
      v9_up? ? @project.uid.v9 : @project.uid.v8
    end
  end

  def build_root_path
    "build"
  end

  def build_path
    File.join(build_root_path, basename)
  end

  def build_root_dir
    project.dir + build_root_path
  end

  def build_dir
    project.dir + build_path
  end

  def generated_file name
    build_dir + name
  end

  def sconfig_file
    generated_file("sconfig.hrh")
  end

  def init_sh_file
    generated_file "init.sh"
  end

  def bld_inf_file
    generated_file "bld.inf"
  end

  def pkg_file
    generated_file(project.basename + ".pkg")
  end

  def short_sis_file
    generated_file(project.sis_basename + ".sis")
  end

  def long_sis_file
    longname = "%s-%s-%s.sis" % [project.sis_basename,
      project.version_string,
      basename]
    generated_file longname
  end

  def short_sisx_file
    Pathname.new(short_sis_file.to_s + "x")
  end

  def long_sisx_file
    Pathname.new(long_sis_file.to_s + "x")
  end

  def sign_sis?
    @sign == true
  end

  def v9?
    puts "WARNING: method v9? is deprecated in sake4"
    v9_up?
  end

  def v9_up?
    (target.major_epoc_version >= 9)
  end

  def v8_down?
    not v9_up?
  end

  def macro_list
    Sake::macrify_map(trait_map)
  end

  def defs
    h = {}
    trait_map.map do |key, value|
      case value
      when nil
      when :define
        h[key] = true
      else
        h[key] = value
      end
    end
    return h
  end

  # Makes the given pathname "pn" target build dir relative, returning
  # the result. Any generated makefiles should use such paths.
  def to_build_rel pn
    pn.relative_path_from(build_dir)
  end
end

# Build dependent information about a component.
#
# A particular build of a component, for a single target platform,
# using a single SDK.
class Sake::CompBuild
  trait :proj_build, 'validate' => proc {|x| x.kind_of? Sake::ProjBuild}

  trait :component, 'validate' => proc {|x| x.kind_of? Sake::Component}

  # The capabilities actually used in the build.
  trait :caps, 'cast' => proc {|x| Array(x)}

  include Sake::Traits::TraitInit
  include Sake::Delegation

  def initialize op = {}
    trait_init op
    raise unless @component
    raise unless @proj_build
    make_delegating_methods(@component,
                            :exclude => self.class.instance_methods)
    make_delegating_methods(@proj_build,
                            :exclude => self.class.instance_methods)

    if sign and (not @caps)
      # Derive actual caps from desired caps and maximum allowed caps.
      @caps = (@component.caps & max_caps)
    end
  end

  def uid2
    uu = @component.uid2
    if uu
      v9_up? ? uu.v9 : uu.v8
    end
  end

  def uid3
    return Sake::Uid::OneUid.new(0) if target_type == :staticlibrary
    uu = @component.uid3
    if uu
      v9_up? ? uu.v9 : uu.v8
    end
  end

  def caps_string
    caplist = caps
    if caplist.empty?
      "NONE"
    else
      caplist.join(" ")
    end
  end

  def mmp_file
    generated_file(@component.basename + ".mmp")
  end

  def mmp_target_type
    case target_type
    when :pyd, :dll
      "dll"
    when :application
      v9_up? ? "exe" : "app"
    when :exedll
      v9_up? ? "exe" : "exedll"
    when :exe, :executable
      "exe"
    when :staticlibrary
      "lib"
    else
      raise "unsupported #{target_type}"
    end
  end

  def target_ext
    case target_type
    when :pyd
      "pyd"
    when :dll
      "dll"
    when :application
      v9_up? ? "exe" : "app"
    when :exe, :executable, :exedll
      "exe"
    when :staticlibrary
      "lib"
    else
      raise "unsupported #{target_type}"
    end
  end

  def binary_file
    generated_file("%s.%s" % [@component.bin_basename, target_ext])
  end

  # Specifies the location of the so-called freeze file. Different
  # compilers mangle C++ function signatures differently (into a text
  # representation), and hence we might require multiple copies of
  # these. It is useful to know to location of the freeze file if say
  # we want to verify that the init* function was exported at ordinal
  # 1, and we do not require any freeze file afterwards.
  def freeze_file
    generated_file(@component.basename + ".frz")
  end

  protected

  # An intersection of the specified capabilities, and what a self-signed
  # certificate allows
  def self_signed_caps caps
    Sake::SELF_SIGNED_CAPS & caps
  end

  # An intersection of the specified capabilities, and what a "normal"
  # developer certificate allows.
  def dev_cert_caps caps
    Sake::DEV_CERT_CAPS & caps
  end
end

=begin rdoc

component.rb

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

require 'build/path_utils'
require 'pathname'
require 'sake0/devkit'
require 'sake0/sake'
require 'sake0/target_platform'
require 'sake0/utils'
require 'traits-0.10.0'

# A component is a named and versioned collection of files. You can
# specify any component-related information, such as :name (for
# display to the user), :basename (for use in filenames), :uid_v8,
# :uid_v9 (for Symbian OS v9-up), :version; non-relevant information
# naturally need not be provided. A component may consist of multiple
# files, e.g. a .pyd file, a .sis file, and pure Python source files.
class Sake::Component
  trait :dir, 'cast' => Sake::method(:ensure_pathname).to_proc
  trait :name, 'munge' => :to_str
  trait :vendor, 'munge' => :to_str
  trait :version, 'cast' => proc {|x| Array(x)}
  trait :basename, 'munge' => :to_str
  trait :uid_v8, 'munge' => :to_int
  trait :uid_v9, 'munge' => :to_int

  # The capabilities required for full use of the component.
  trait :caps, 'cast' => proc {|x| Array(x)}

  # Directory for web releases.
  trait :web_dir, 'cast' => Sake::method(:ensure_pathname).to_proc

  include DefaultTraitInit
  include Memoize

  def initialize op = {}
    default_trait_init op
    @caps ||= []
    @name ||= @basename.capitalize
    @dir ||= ($sakefile ?
              Sake::ensure_pathname(File.dirname($sakefile)) :
                Sake::ensure_pathname(Dir.pwd))
    if @uid_v8 and not @uid_v9
      @uid_v9 =
        if (@uid_v8 & 0xf0000000) == 0x10000000
          # Allocated from Symbian, it would seem. Let us use the
          # legacy range on v9. Something to note is that signed and
          # unsigned applications have a different UID range; the
          # legacy range would seem be in the unprotected range, so
          # one must apply for a new UID for the component to get
          # Symbian signing working. But we will probably only be
          # doing Self and DevCert signings, and this UID can be used
          # in both cases.
          0xf0000000 | @uid_v8
        end
    end
  end

  def major_version
    version[0] || 0
  end

  def minor_version
    version[1] || 0
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
    @dir + "doxyfile.in"
  end

  def src_dir
    @dir + "src"
  end

  def lib_dir
    @dir + "lib"
  end

  def python_lib_dir
    @dir + "py-lib"
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

  # Makes the given pathname "pn" project (component) dir relative,
  # returning the result. The master makefile should use such paths.
  def to_proj_rel pn
    pn.relative_path_from(@dir)
  end
end

module Sake
  SELF_SIGNED_CAPS = %w{LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData}

  DEV_CERT_CAPS = (SELF_SIGNED_CAPS + %w{Location PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData}).sort
end

# A particular build of a component, for a single target platform,
# using a single SDK.
class Sake::CompBuild
  trait :component, 'validate' => proc {|x| x.kind_of? Sake::Component}
  trait :target, 'validate' => proc {|x| x.kind_of? Sake::TargetPlatform}
  trait :devkit, 'validate' => proc {|x| x.kind_of? Sake::DevKit}

  # The capabilities actually used in the build.
  trait :caps, 'cast' => proc {|x| Array(x)}

  # .lib files to link against.
  trait :libs, 'cast' => proc {|x| Array(x)}

  # C++ source files to compile.
  trait :cxx_files, 'cast' => Sake::method(:ensure_pathname_list).to_proc

  # The handle defaults to the target platform handle, but the user
  # might want to specify a handle, say if doing multiple build
  # variants for a particular target.
  trait :handle, 'munge' => :to_str

  trait :sign, 'validate' => proc {|x| x == true or x == false}
  trait :sign_type, 'validate' => proc {|x| [:self, :dev_cert, :symbian].include? x}

  trait :cert_file, 'cast' => Sake::method(:ensure_pathname).to_proc
  trait :key_file, 'cast' => Sake::method(:ensure_pathname).to_proc
  trait :passphrase, 'munge' => :to_str

  # Properties such as compile-time defines.
  trait :trait_map, 'munge' => :to_hash

  trait :mmp_add, 'munge' => :to_str

  include DefaultTraitInit
  include Delegation

  def initialize op = {}
    default_trait_init op
    @component && @devkit or raise
    @target ||= @devkit.native_target
    @handle ||= @devkit.handle
    @trait_map ||= @target.trait_map.dup
    @caps ||= component.caps
    @libs ||= []
    @cxx_files ||= []
    delegate_to @component
  end

  # Creates a self-signed variant build.
  def to_self_signed
    # dup and clone do not seem to work when using traits, so
    # let's do this the unmaintainable way for now...
    self.class.new(:handle => @handle + "_self",
                   :caps => self_signed_caps(@component.caps),
                   :component => @component,
                   :target => @target,
                   :devkit => @devkit,
                   :sign => true,
                   :sign_type => :self)
  end

  # Creates a dev cert signed variant build.
  def to_dev_signed
    self.class.new(:handle => @handle + "_dev",
                   :caps => dev_cert_caps(@component.caps),
                   :component => @component,
                   :target => @target,
                   :devkit => @devkit,
                   :sign => true,
                   :sign_type => :dev_cert)
  end

  def basename
    @handle
  end

  def uid
    ((@target.symbian_platform.major_version < 9) ?
     @component.uid_v8 :
       @component.uid_v9)
  end

  def uid_hex_string
    '%08x' % (uid || (raise "no UID for #{self.inspect}"))
  end

  def uid_chex_string
    '0x' + uid_hex_string
  end

  def build_root_path
    "build"
  end

  def build_path
    File.join(build_root_path, basename)
  end

  def build_root_dir
    @component.dir + build_root_path
  end

  def build_dir
    @component.dir + build_path
  end

  def generated_file name
    build_dir + name
  end

  def init_sh_file
    generated_file "init.sh"
  end

  def bld_inf_file
    generated_file "bld.inf"
  end

  def mmp_file
    generated_file(@component.basename + ".mmp")
  end

  def gnumakefile
    generated_file "GNUmakefile"
  end

  def pkg_file
    generated_file(@component.basename + ".pkg")
  end

  def pyd_file
    generated_file("%s.pyd" % [@component.basename])

#     if (@target.symbian_platform.major_version < 9)
#       generated_file("%s.pyd" % [@component.basename])
#     else
#       # We could consider adding the UID to prevent name clashes.
#       # But given that the UIDs vary depending on the platform,
#       # this causes problems when it comes to the init<module>
#       # function; we would need a macro that defines the module
#       # name string and the init function symbol.
#       #generated_file("_%s_%s.pyd" % [@component.basename, uid_chex_string])

#       generated_file("%s.pyd" % [@component.basename])
#     end
  end

  def needs_pyd_wrapper?
    # As of version 1.3.15, no wrapper is required.
    false
    #(@target.symbian_platform.major_version >= 9)
  end

  def pyd_wrapper_file
    generated_file(@component.basename + ".py")
  end

  # Specifies the location of the so-called freeze file.
  # Different compilers mangle C++ function signatures
  # differently (into a text representation), and hence
  # we might require multiple copies of these.
  # But we just want to verify that the init* function
  # was exported at ordinal 1, and we do not require
  # any freeze file afterwards.
  def freeze_file
    generated_file(@component.basename + ".frz")
  end

  def short_sis_file
    generated_file(@component.basename + ".sis")
  end

  def long_sis_file
    longname = "%s-%s-%s.sis" % [@component.basename,
      @component.version_string,
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

  def macro_list
    Sake::macrify_map(trait_map)
  end

  # Makes the given pathname "pn" target build dir relative, returning
  # the result. Any generated makefiles should use such paths.
  def to_build_rel pn
    pn.relative_path_from(build_dir)
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

if $0 == __FILE__
  comp = Sake::Component.new :basename => 'miso', :version => 2, :uid_v8 => 0xfffffff, :dir => (File.join(ENV["HOME"], "trunk/s4all/wrapgen/new-miso"))
  p comp
  p comp.dir
  p comp.version_string

  devkit = Sake::DevKits::get_all_info.first || raise

  bld = Sake::CompBuild.new :component => comp, :devkit => devkit
  puts bld.long_sis_file.to_s
  puts bld.to_build_rel(bld.long_sis_file).to_s
  puts bld.to_proj_rel(bld.long_sis_file).to_s
  puts path_windowsify(bld.to_proj_rel(bld.long_sis_file).to_s)
  p bld.dir
  p bld.version_string
  p bld.uid_chex_string
end

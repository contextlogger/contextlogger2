=begin rdoc

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

require 'erb'
require 'build/path_utils'
require 'sake0/component'
require 'sake0/sake'
require 'build/write_file'

# TODO:
#
# * we could consider creating directories using the "directory" rule
#
# * many of the rules do not check whether their target is up to date;
#   also, many rules do all build variants at once, even though some
#   of them might be up to date
#
# * Sake::CompBuild does not work with def_rdoc_tasks
#
# * Documenting public C++ APIs with Doxygen is currently not
#   supported; indeed a typical .pyd project would have no such API.

# Provides code for defining commonly needed tasks or sets of tasks
# for Sake makefiles. Rake has similar utilities for defining tasks,
# but we are keeping things somewhat simpler here.
module Sake::Tasks
  def def_rdoc_tasks op = {}
    desc "Builds Ruby API HTML documentation under ruby-api/."
    task :rubydoc do
      require 'rdoc/rdoc'
      rdoc = RDoc::RDoc.new
      cd('lib', :noop => false) do
        when_writing("building API docs") do
          rdoc.document %w{--op ../ruby-api}
        end
      end
    end

    desc "Builds Ruby API RI documentation under ~/.rdoc/."
    task :ri do
      require 'rdoc/rdoc'
      rdoc = RDoc::RDoc.new
      cd('lib', :noop => false) do
        when_writing("building RI docs") do
          rdoc.document %w{--ri}
        end
      end
    end
  end

  def def_list_devices_tasks op = {}
    alldevs = Sake::DevKits::get_all
    devices = alldevs
    if op[:builds]
      devices = op[:builds].map do |bld|
        bld.devkit
      end.uniq
    end

    desc "Prints out a list of installed SDKs."
    task :devkits do
      for device in alldevs
        puts device.handle
      end
    end

    desc "Prints out a list of installed Python SDKs."
    task :pydevkits do
      for device in alldevs
        puts device.handle if device.supports_python?
      end
    end

    desc "Prints out a list of project-supported targets."
    task :targets do
      for device in devices
        puts device.handle
      end
    end
  end

  def listcall(obj, device)
    case obj
    when nil
      []
    when Array
      obj
    when Proc
      obj.call(device)
    else
      raise
    end
  end
  private :listcall

  # op:: The +builds+ option must be passed, listing the builds for
  #      which a bld.inf file is required.
  def def_bld_inf_tasks op = {}
    builds = op[:builds] || raise

    desc "Produces a bld.inf file for each targeted device."
    task :bld_inf do
      require 'stringio'
      for build in builds
        mkdir_p build.to_proj_rel(build.build_dir).to_s
        file = build.to_proj_rel(build.bld_inf_file).to_s
        text = ""
        sio = StringIO.new text
        sio.puts "PRJ_PLATFORMS"
        sio.puts build.devkit.prj_platforms.join(" ")
        sio.puts ""
        sio.puts "PRJ_MMPFILES"
        sio.puts "%s.mmp" % build.component.basename
        write_file(file, text, true)
      end
    end
  end

  def mmp_file_text build, op
    srclist = build.cxx_files.map do |file|
      [path_windowsify(build.to_build_rel(file.dirname).to_s),
        file.basename.to_s]
    end

    userinclist = [path_windowsify(build.to_build_rel(build.src_dir).to_s)]

    liblist = build.libs

    # If we accumulate a large number of macros, we may have to generate
    # a C++ header with these defines instead of passing these in as
    # compiler options.
    maclist = build.macro_list

    is_v9 = (build.target.symbian_platform.major_version >= 9)
    targettype = "dll"
    targetpath = is_v9 ? "\\sys\\bin\\" : "\\system\\libs\\"

    caplist = build.caps
    if caplist.empty?
      caplist = "NONE"
    else
      caplist = caplist.join(" ")
    end

    text = <<'EOF'
targettype      <%= targettype %>
target          <%= build.pyd_file.basename %>

uid             0x1000008d <%= build.uid_chex_string %>

<% unless is_v9 %>
targetpath      <%= targetpath %>
<% end %>

exportunfrozen

systeminclude   \epoc32\include
<% if op[:open_c] %>
systeminclude   \epoc32\include\stdapis
<% else %>
systeminclude   \epoc32\include\libc
<% end %>
systeminclude   \epoc32\include\python

<% for userinc in userinclist %>
userinclude <%= userinc %>
<% end %>

<% for lib in liblist %>
library <%= lib %>
<% end %>

<% for srcpath, srcfile in srclist %>
sourcepath <%= srcpath %>
source <%= srcfile %>
<% end %>

<% for macro in maclist %>
macro <%= macro %>
<% end %>

<% if is_v9 %>
CAPABILITY <%= caplist %>
<% end %>

<% madd = build.mmp_add
   if madd %>
<%= madd %>
<% end %>
EOF
    return ERB.new(text).result(binding())
  end

  def def_mmp_tasks op = {}
    builds = op[:builds] || raise

    desc "Produces an MMP file for each targeted device."
    task :mmp do
      for build in builds
        mmpfile = build.to_proj_rel(build.mmp_file).to_s
        text = mmp_file_text build, op
        mkdir_p(build.to_proj_rel(build.build_dir).to_s)
        write_file(mmpfile, text, true)
      end
    end
  end

  def pn_join pnlist
    slist = pnlist.map do |x|
      x.to_s
    end
    slist.join(" ")
  end

  def gnumakefile_text build, op
    uid = build.uid_hex_string

    userinclist = [build.to_build_rel(build.src_dir).to_s]

    pydfile = build.to_build_rel(build.pyd_file).to_s

    liblist = build.libs

    cxxlist = build.cxx_files.map do |file|
      build.to_build_rel(file)
    end
    objlist = cxxlist.map do |file|
      file.basename.ext(".o")
    end

    maclist = build.macro_list
    macros_s = maclist.collect do |macro|
      "-D" + macro
    end.join(" ")

    text = <<'EOF'
# This makefile is for builds with sdk2unix-style SDKs.
#
# To use this makefile, you must define the environment
# variable "EPOC" so that it points to the root of the SDK
# you wish to use. Additionally, you must set your PATH
# so that it points the the "bin" directory of the SDK.
#
# To use a non-SDK-default GCC, you may also specify
# a path to the GCC tree that you wish to use by setting
# the "GCCPATH" environment variable. In that case, your
# path should point to the "bin" directory of the GCC
# tree instead of that of the SDK tree.

default : <%= pydfile %>

ifdef GCCPATH
  include $(GCCPATH)/lib/makerules/dll
else
  ifndef EPOC
    $(error EPOC not set)
  else
    include $(EPOC)/lib/makerules/dll
  endif
endif

LIBS := $(patsubst %,$(EPOCTRGREL)/%,<%= liblist.join(" ") %>)

OBJECTS := <%= pn_join(objlist) %>

U1 := 10000079 # dynamic library
U2 := 1000008d # Python extension
U3 := <%= uid %> # application specific

CFLAGS += -O
<% for incpath in userinclist %>
CFLAGS += -I<%= incpath %>
<% end %>
CFLAGS += -I$(EPOC)/include/python
CFLAGS += <%= macros_s %> -DUID3=0x$(U3)

# It is looking like we must explicitly list the objects that this
# rule depends on. And we must include the commands to execute,
# or Make will assume the rule fulfilled without doing anything.
# But having to replicate these commands here is annoying.
%.def1 : %.o
%.def1 : $(OBJECTS)
<%= "\t" %>@echo "[DLL1  ] $*"
<%= "\t" %>@$(DT) -S$(AS) -m arm_interwork --output-def $*.def2 $(OBJECTS)
<%= "\t" %>@perl -p -e 's,\n,\r\n,;s, ; .*, NONAME,;s,R3UNUSED ,,' $*.def2 > $*.def3

# Overriding this to get a .map file created;
# required by bc_dump.pl (available from Symbian website).
%.ex2: %.exp
<%= "\t" %>@echo "[LD    ] $*"
<%= "\t" %>@$(LD) $(LDFLAGS) -Map $*.pyd.map -o $*.ex2 $*.exp $(EX) $(OBJECTS) $(LIBS)

# Cancelling these (defined in epoc.general).
# We are providing an explicit rule for all our C++ sources.
%.o : %.cpp
%.o : %.cc

<% for sfile in cxxlist
     ofile = sfile.basename.ext(".o")
 %>
<%= ofile %> : <%= sfile %>
<%= "\t" %>@echo "[C++   ] $*"
<%= "\t" %>@$(CCC) $(CFLAGS) -c <%= sfile %>
<% end %>

# Unfortunately petran has no option to suppress the banner.
%.pyd : %.ex2
<%= "\t" %>@echo "[PYD   ] $*"
<%= "\t" %>@$(PT) $*.ex2 $*.pyd -nocall -uid1 0x$(U1) -uid2 0x$(U2) -uid3 0x$(U3)
EOF
    return ERB.new(text).result(binding())
  end

  def def_gnumakefile_tasks op = {}
    builds = op[:builds] || raise

    desc "Produces a GNUmakefile for each targeted device."
    task :gnumakefile do
      for build in builds
        text = gnumakefile_text(build, op)
        mkdir_p(build.to_proj_rel(build.build_dir).to_s)
        file = build.to_proj_rel(build.gnumakefile).to_s
        write_file(file, text)
      end
    end
  end

  def def_makefile_tasks op = {}
    def_bld_inf_tasks op
    def_mmp_tasks op
    def_gnumakefile_tasks op

    desc "Produces makefiles for all supported build environments."
    task :makefiles => [:bld_inf, :mmp, :gnumakefile]
  end

  def check_build_env
    case $build_env
    when :cygwin
      if `which perl` !~ /activestate/i
        raise "your build environment does not seem Symbian customized: for one thing, building with Cygwin default Perl will not work"
      end
    end
  end

  def cw_to_win_path path, op = {}
    path = to_shell_string(path)
    path = `cygpath -w #{path}`.chomp
    if op[:driveless]
      path = path_split_drive(path)[1]
    end
    return path
  end

  def cw_to_win_path_no_drive path
    cw_to_win_path path, :driveless => true
  end

  def sh_dev build, *args
    if build.devkit.supports_devices?
      args, op = fui_split_op(args)
      devh = "@" + build.devkit.devices_handle
      if args.size == 1
        args = [args.first + " " + devh, op]
      else
        args.concat [devh, op]
      end
    end
    sh(*args)
  end

  def check_init_ordinal build, plat
    command = %w{perl -S efreeze.pl}
    frzfile = build.freeze_file.basename.to_s
    may_fail do; rm frzfile; end
    buildpath = cw_to_win_path_no_drive(Dir.pwd) + "\\" + build.mmp_file.basename(".mmp") + "\\" + plat
    deffile = cw_to_win_path_no_drive(build.devkit.build_home) + buildpath + "\\" + build.pyd_file.basename(".pyd") + "{000a0000}.def"
    command.push frzfile
    command.push deffile
    sh_dev(build, *command)

    re = Regexp.new(Regexp.escape("init" + build.pyd_file.basename(".pyd")) + ".*@\\s+(\\d+)")
    File.open(frzfile) do |input|
      for line in input
        if line =~ re
          raise "wrong ordinal" unless $1.to_i == 1
          return
        end
      end
    end
    raise "module init not found in freeze file"
  end

  def def_pyd_tasks op = {}
    builds = op[:builds] || raise

    desc "Builds shell scripts for manual build environment setup."
    task :init_sh do
      for build in builds
        build.devkit.in_env do
          inifile = build.to_proj_rel(build.init_sh_file)
          mkdir_p(inifile.dirname.to_s)
          create_file(inifile.to_s, :force => true) do |output|
            output.puts("export EPOCROOT=" + to_shell_string(ENV["EPOCROOT"]))
            output.puts("export PATH=" + to_shell_string(ENV["PATH"]))
          end
        end
      end
    end

    desc "Builds a .pyd file for each targeted device."
    case $build_env
    when :cygwin
      task :pyd => [:bld_inf, :mmp] do
        #check_build_env

        for build in builds
          blddir = build.to_proj_rel(build.build_dir).to_s
          mkdir_p(blddir)

          cd(blddir, :noopcall => true) do
            build.devkit.in_env do
              sh_dev(build, "perl -S bldmake.pl bldfiles")

              plats = build.devkit.prj_platforms.dup
              if plats.include? "gcce" and
                  plats.include? "armi"
                raise "choose either gcce or armi"
              end
              do_wins = plats.include? "wins"
              plats.delete "wins"

              abld_arg = to_shell_string(cw_to_win_path_no_drive(Dir.pwd) + "\\\\")

              if do_wins and build.devkit.supports_msvc6?
                sh_dev(build, "perl -S makmake.pl %s vc6" % build.to_build_rel(build.mmp_file).to_s)
                sh_dev(build, "perl -S abld.pl #{abld_arg} -v build wins udeb")
              end

              do_abld = proc do |plat|
                sh_dev(build, "perl -S abld.pl #{abld_arg} -v build #{plat} urel")
                cp(File.join(build.devkit.epoc32_home, 'release/%s/urel/%s' % [plat, build.pyd_file.basename.to_s]), build.build_dir.to_s)

                if build.devkit.handle == 'ns60_3rd_mr'
                  check_init_ordinal(build, plat)
                end
              end

              do_abld.call("armi") if plats.include? "armi"
              do_abld.call("gcce") if plats.include? "gcce"
            end
          end
        end
      end
    when :gnupoc
      task :pyd => [:bld_inf, :mmp] do
        for build in builds
          blddir = build.to_proj_rel(build.build_dir).to_s
          mkdir_p(blddir)

          cd(blddir, :noopcall => true) do
            build.devkit.in_env do
              plats = build.devkit.prj_platforms.dup

              do_abld = proc do |plat|
                built_pyd = File.join(build.devkit.epoc32_home, 'release/%s/urel/%s' % [plat, build.pyd_file.basename.to_s])

                may_fail do; rm built_pyd; end
                sh("abld", "build", "-v", plat, "urel")

                # One should note that the that the path is not
                # specific enough: if we do two or more variant builds
                # of the same component with the same SDK, then both
                # of the built binaries will have the same path, so
                # it's essential to immediately copy the resulting
                # file after it has been built. The problem here is
                # that if "abld" considers the binary built already,
                # then it will not get built, and only the last built
                # version of the binary will be copied anywhere, and
                # it may be the wrong variant. (This has caused us
                # problems with capabilities, for instance. The
                # "sisinfo" tool, for instance, can be used to check
                # capability correctness.) Hence doing the "rm" above
                # is quite essential.
                cp(built_pyd, build.build_dir.to_s)

                # This file does not have the capabilities marked...
                #cp(File.join(build.devkit.epoc32_home, "build" + build.build_dir.to_s, build.component.basename, plat, "urel", build.pyd_file.basename.to_s), build.build_dir.to_s)
              end

              # We could try build for THUMB also, if listed in the
              # bld.inf file, but say the PyS60 SDK does not have
              # THUMB binaries. So we would have to know whether the
              # project in question uses any libraries that have no
              # THUMB variant available, but we presently do not.
              sh("bldmake bldfiles")
              do_abld.call("armi") if plats.include? "armi"
              do_abld.call("gcce") if plats.include? "gcce"
            end
          end
        end
      end
    when :sdk2unix
      task :pyd => [:gnumakefile] do
        #check_build_env

        for build in builds
          blddir = build.to_proj_rel(build.build_dir).to_s
          mkdir_p(blddir)
          cd(blddir, :noopcall => true) do
            build.devkit.in_env do
              sh("make")
            end
          end
        end
      end
    else
      task :pyd do
        raise "do not know how to build on this system"
      end
    end

    # XXX after building a .pyd, we migth want to automatically check
    # that any Python init function is exported at ordinal 1

    desc "Builds a .pyd wrapper for each device requiring it."
    task :pyd_wrapper do
      for build in builds
        next unless build.needs_pyd_wrapper?
        wfile = build.to_proj_rel(build.pyd_wrapper_file).to_s
        mkdir_p(File.dirname(wfile))
        pydbasename = build.pyd_file.basename.to_s
        basename = "_" + build.component.basename
        # Do we have to install on the C: drive, or could we do a
        # search in this script to look for it from other drives' sys
        # directories? XXX
        text = <<'EOF'
import imp
<%= basename %> = imp.load_dynamic('<%= basename %>', 'c:\\sys\\bin\\<%= pydbasename %>')
del imp
from <%= basename %> import *
del <%= basename %>
EOF
        text = ERB.new(text).result(binding())
        write_file(wfile, text, false)
      end
    end

    task :pyd => :pyd_wrapper
  end

  def sis_spec(build, op = {})
    require 'build/sis'

    # We could insist on Python being installed for Python components,
    # but we do not necessarily want to dictate any particular
    # installation order. For S60 1st and 2nd ed. phones we could use
    # this entry; the UID is probably different for 3rd ed.
    #
    #   ;;; Python for Series 60
    #   (0x10201510), 0, 0, 0, {"Python"}

    # In S60 3rd ed SDKs, vendor name(s) are required.
    # Apparently both unique and localized versions.

    vendorname = build.component.vendor
    vendor ||= "unspecified vendor"

    header = '
&EN

#{<%= build.component.name.inspect %>}, (<%= build.uid_chex_string %>), <%= build.major_version %>, <%= build.minor_version %>, 0

<% if build.devkit.vendor_in_pkg? %>
;; Non-localized vendor name.
:<%= vendorname.inspect %>

;; Localized vendor names.
%{<%= vendorname.inspect %>}
<% end %>

;; Platform dependency.
<%= build.target.pkg_dependency_string %>
'
    header = ERB.new(header).result(binding())

    is_v9 = (build.target.symbian_platform.major_version >= 9)
    sisfile = Symbian::SisFile.new(header, :v9 => is_v9)

    pydfile = build.to_proj_rel(build.pyd_file)

    list = [[pydfile.dirname.to_s, pydfile.basename.to_s, :pyd]]
    if build.needs_pyd_wrapper?
      wrapfile = build.to_proj_rel(build.pyd_wrapper_file)
      list.push [wrapfile.dirname.to_s, wrapfile.basename.to_s, :py_wrap]
    end

    if build.python_lib_dir.exist?
      for path in Pathname.glob("#{build.python_lib_dir}/**/*.py")
        list.push [build.to_proj_rel(build.python_lib_dir).to_s,
                   path.relative_path_from(build.python_lib_dir).to_s,
                   :py13_lib]
      end
    end

    for elem in list
      sisfile.add(*elem)
    end

    return sisfile
  end

  def def_sis_tasks op = {}
    $devel = $sake_op.has_key?(:devel)

    builds = op[:builds] || raise

    desc "Builds PKG files for all targets."
    task :pkg do
      for build in builds
        pkgfile = build.to_proj_rel(build.pkg_file)
        mkdir_p pkgfile.dirname.to_s
        sis_spec(build, op).write_pkg(pkgfile.to_s)
      end
    end

    task :makefiles => :pkg

    # note that for third edition we want all kinds of SIS files:
    # self-signed, devcert-signed, unsigned XXX; naturally, no one
    # should be distributing any devcert-signed ones

    desc "Builds SIS files for all targets."
    task :sis do
      $verbose_makesis = $sake_op.has_key? :verbose_makesis
      $verbose_signsis = $sake_op.has_key? :verbose_signsis

      for build in builds
        makesis_cmd = build.devkit.supports_devices? ? "perl -S makesis.pl" : "makesis"

        build.devkit.in_env do
          shproc = proc {|*args| sh_dev(build, *args)}
          sis_spec(build, op).write_sis(build.to_proj_rel(build.short_sis_file).to_s, :sh => shproc, :command => makesis_cmd)
        end
        ln(build.to_proj_rel(build.short_sis_file).to_s,
           build.to_proj_rel(build.long_sis_file).to_s,
           :force => true)

        if build.sign_sis?
          build.devkit.in_env do
            command = (build.devkit.supports_devices?) ? %w{perl -S signsis.pl} : %w{signsis}
            command.push "-v" if $verbose_signsis
            command.push "-s" # sign
            command.push build.to_proj_rel(build.short_sis_file).to_s
            command.push build.to_proj_rel(build.short_sisx_file).to_s
            build.cert_file or raise
            build.key_file or raise
            command.push build.to_proj_rel(build.cert_file).to_s
            command.push build.to_proj_rel(build.key_file).to_s
            if build.passphrase
              command.push build.passphrase
            end
            sh_dev(build, *command)
          end
          ln(build.to_proj_rel(build.short_sisx_file).to_s,
             build.to_proj_rel(build.long_sisx_file).to_s,
             :force => true)
        end
      end
    end
  end

  def def_clean_tasks op = {}
    host_os = Config::CONFIG['host_os']
    builds = op[:builds] || raise
    cleantasks = []

    unless op[:no_cleanlog]
      desc "Cleans up any emulator log files."
      task :cleanlog do
        for build in builds
          if build.devkit.has_emulator?
            logpath = op[:log_path] || build.component.basename || raise
            build.devkit.each_emulator_app_log_file logpath do |file|
              rm file
            end
          end
        end
      end

      cleantasks.push(:cleanlog)
    end

    unless op[:no_cleanemu]
      desc "Cleans up any emulator binaries."
      task :cleanemu do
        for build in builds
          if build.devkit.has_emulator?
            basename = build.component.basename
            build.devkit.each_emulator_app_binary basename do |file|
              rm file
            end
          end
        end
      end
      cleantasks.push(:cleanemu)
    end

    unless op[:no_cleantemp]
      desc "Cleans up any temporary build files."
      task :cleantemp do
        case $build_env
        when :cygwin
          for build in builds
            if build.devkit.has_emulator?
              may_fail do; rm_r build.devkit.build_home; end
            end
          end
          bd = build.to_proj_rel(build.build_dir).to_s + "/"
          rm(Dir.globi(bd + "ABLD.BAT") +
               Dir.globi(bd + '*.dsp') +
               Dir.globi(bd+ '*.dsw') +
               Dir.globi(bd + '*.sup.make') +
               Dir.globi('*.uid.cpp'))
        when :sdk2unix
          for build in builds
            bd = build.to_proj_rel(build.build_dir).to_s
            for ext in %w{.o .Lib .def2 .def3 .ex1 .bas .exp .map .ex2}
              rm Dir.glob(bd + '/*' + ext)
            end
          end
        end

        cond_with(builds.first) do |build|
          src = build.to_proj_rel(build.src_dir).to_s
          pyc_files = Dir[src + "/*.pyc"]
          rm(pyc_files)
        end
      end
      cleantasks.push(:cleantemp)
    end

    unless op[:no_cleanbak]
      desc "Cleans up any backup files."
      task :cleanbak do
        rm Dir.globi('**/*~')
      end
      cleantasks.push(:cleanbak)
    end

    task :clean => cleantasks

    unless op[:no_cleanbuild]
      desc "Cleans up everything that has been built."
      task :cleanbuild do
        for build in builds
          may_fail do; rm_r build.build_root_path; end
        end
      end
    end
  end

  def cond_with value
    if value
      yield value
    end
  end

  def def_doxygen_tasks op = {}
    build = op[:build] || raise

    # For setting up prerequisites correctly, we must know what C++
    # files need generating before running doxygen.
    gensrc = op[:gen_src] || []

    # The directories from which to look for C++ files to document.
    cxx_src_dirs = Array(op[:cxx_src_dirs] || build.to_proj_rel(build.src_dir).to_s)

    # The directory in which to generate documentation.
    cxx_api_doc_home = build.to_proj_rel(build.private_cxx_api_dir).to_s

    desc "Generates a Doxygen config file."
    file "doxyfile" => [$sakefile, "doxyfile.in"] do
      # You better copy an existing doxyfile.in from another project to
      # use as a template. Or you can use "doxygen -g doxyfile.in" to get
      # a template with all the Doxygen default settings.
      text = File.read("doxyfile.in")
      text = ERB.new(text).result(binding())
      write_file("doxyfile", text, false)
    end

    desc "Builds C++ API documentation."
    task :cxxdoc => (["doxyfile"] + gensrc)  do
      sh("doxygen doxyfile")
    end

    task :cleandoc do
      may_fail { rm_r(cxx_api_doc_home) }
    end

    unless op[:no_cleanbuild]
      task :cleanbuild => :cleandoc do
        # We consider the doxyfile as something one would want to
        # include in a distribution, and therefore :cleantemp will not
        # touch it; :cleanbuild is required.
        may_fail { rm("doxyfile") }
      end
    end
  end

  def def_pydoc_tasks op = {}
    build = op[:build] or raise "no build given"
    py_file = op[:py_file] or raise "no .py file specified"

    api_doc_dir = build.python_api_dir
    api_doc_home = build.to_proj_rel(api_doc_dir).to_s
    py_lib_home = py_file.dirname.to_s

    desc "Builds Python API documentation."
    # The dependency here is useful if the .py file should be
    # something that is generated by the build system.
    task :pydoc => build.to_proj_rel(py_file).to_s do
      ppath = ENV['PYTHONPATH']
      ppath = (ppath ? "#{py_lib_home}:#{ppath}" : py_lib_home)
      mkdir_p(api_doc_home)
      cd(api_doc_home, :noopcall => true) do
        set_env('PYTHONPATH', ppath) do
          sh("pydoc", "-w", py_file.relative_path_from(api_doc_dir).to_s)
        end
      end

      # Kill any absolute paths. This will probably break as soon as
      # the pydoc output format changes, but...
      when_writing do
        docfile = api_doc_dir + (py_file.basename(".py").to_s + ".html")
        text = File.read(docfile.to_s)
        #re = Regexp.new(Regexp.escape(py_lib_home + "/"))
        re = /<br><a href=\"file:[^\"]+\">[^<]+<\/a>/
        text.gsub!(re, "")
        write_file(build.to_proj_rel(docfile).to_s, text, true)
      end
    end

    task :cleandoc do
      may_fail { rm_r(api_doc_home) }
    end
  end

  def def_dist_tasks op = {}
    builds = op[:builds] or raise "no builds given"
    doc_build = builds.first or raise "at least one build required"
    comp = doc_build.component

    desc "Builds a source distribution."
    task :dist => [:makefiles, :pydoc, :cxxdoc] do
      require 'build/archive'

      files = []
      paths = []

      src = comp.to_proj_rel(comp.src_dir).to_s
      bldr = doc_build.build_root_path

      files.concat %w{COPYING README}
      files.concat Dir["#{src}/*.h"]
      files.concat Dir["#{src}/*.cpp"]
      files.concat Dir["#{src}/*.py"]
      files.concat Dir["#{bldr}/**/*.h"]
      files.concat Dir["#{bldr}/**/*.cpp"]
      files.concat Dir["#{bldr}/**/*.py"]
      files.concat Dir["test-programs/*.py"]

      more_files = op[:more_files]
      more_files &&= Array(more_files)
      if more_files
        files.concat(more_files)
      end

      paths.push comp.private_cxx_api_dir
      paths.push comp.python_api_dir
      paths.push comp.doxyfile_file

      for build in builds
        paths.push build.bld_inf_file
        paths.push build.mmp_file
        paths.push build.gnumakefile
        paths.push build.pkg_file
      end

      files += paths.map do |path|
        comp.to_proj_rel(path).to_s
      end

      files.delete_if do |file|
        not File.exist? file
      end

      tar_with_dir(comp.to_proj_rel(comp.src_tar_file).to_s, comp.within_tar_path, files)
    end

    task :cleanbuild do
      may_fail { rm comp.to_proj_rel(comp.src_tar_file).to_s }
    end
  end

  def def_web_tasks op = {}
    builds = op[:builds] or raise "no builds given"
    doc_build = builds.first or raise "at least one build required"
    comp = doc_build.component

    desc "Prepares a web release."
    task :web do
      web_dir = comp.web_dir

      pages = op[:html_pages] || [web_dir + "index.html"]
      if pages.empty?
        mkdir_p(web_dir.to_s)
      else
        for htmlfile in pages
          raise unless htmlfile.exist?
          sh("tidy", "-utf8", "-e", htmlfile.to_s)
        end
      end

      install 'COPYING', web_dir.to_s, :mode => 0644

      api_sfile = comp.python_api_dir + (comp.basename + ".html")
      api_dfile = web_dir + ("%s-%s-api.html" % [comp.basename, comp.version_string])
      install api_sfile.to_s, api_dfile.to_s, :mode => 0644

      api_sfile = op[:py_file] || (comp.src_dir + (comp.basename + ".py"))
      api_dfile = web_dir + ("%s-%s-api.py" % [comp.basename, comp.version_string])
      install api_sfile.to_s, api_dfile.to_s, :mode => 0644

      install comp.to_proj_rel(comp.src_tar_file).to_s, web_dir.to_s, :mode => 0644

      unless $sake_op[:no_sis]
        for build in builds
          # For all targets we distribute and unsigned SIS, but in the case
          # of 3rd edition, we only want an unsigned SIS for DevCert
          # capability builds.
          if build.target.edition < 3 or
              build.sign_type == :dev_cert
            install build.to_proj_rel(build.long_sis_file).to_s, web_dir.to_s, :mode => 0644
          end

          # For 3rd edition builds, we distribute a self-signed SIS.
          if build.target.edition >= 3 and
              build.sign_type == :self
            install build.to_proj_rel(build.long_sisx_file).to_s, web_dir.to_s, :mode => 0644
          end
        end
      end
    end
  end

  def force_uncurrent_on_op_change op = {}
    require 'sake0/sake_utils'

    # Note that this does not apply to any tasks implicitly or explicitly
    # defined afterwards. Note also that while this affects Sake tasks,
    # say GNU make tasks are not affected.
    Sake::Utils::on_op_change_force_uncurrent

    task :cleanbuild do
      may_fail { rm Sake::Utils::SAKE_OP_FILE }
    end
  end

  extend self
end

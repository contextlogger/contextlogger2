=begin rdoc

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

require 'erb'
require 'stringio'
require 'build/path_utils'
require 'sake3/component'
require 'sake3/sake'
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
#   supported.

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
    alldevs = alldevs.sort {|x, y| x.handle <=> y.handle}
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

    desc "Prints out a list of installed Python v1 SDKs."
    task :pydevkits_v1 do
      for device in alldevs
        puts device.handle if device.supports_python_v1?
      end
    end

    desc "Prints out a list of installed Python v2 SDKs."
    task :pydevkits_v2 do
      for device in alldevs
        puts device.handle if device.supports_python_v2?
      end
    end

    desc "Prints out a list of project-supported targets."
    task :targets do
      for device in devices
        puts device.handle
      end
    end
  end

  # op:: The +builds+ option must be passed, listing the builds for
  #      which a bld.inf file is required.
  def def_bld_inf_tasks op = {}
    builds = op[:builds] || raise

    desc "Produces a bld.inf file for each targeted device."
    task :bld_inf do
      for build in builds
        mkdir_p build.to_proj_rel(build.build_dir).to_s
        file = build.to_proj_rel(build.bld_inf_file).to_s
        src_file = build.to_proj_rel(build.bld_inf_in_file).to_s
        text = File.read(src_file)
        text = ERB.new(text).result(binding())
        write_file(file, text, true)
      end
    end
  end

  def def_mmp_tasks op = {}
    builds = op[:builds] || raise

    desc "Produces an MMP file for each targeted device."
    task :mmp do
      for pbuild in builds
        for build in pbuild.comp_builds
          file = build.to_proj_rel(build.mmp_file).to_s
          src_file = build.to_proj_rel(build.mmp_in_file).to_s
          text = File.read(src_file)
          text = ERB.new(text).result(binding())
          mkdir_p(build.to_proj_rel(build.build_dir).to_s)
          write_file(file, text, true)
        end
      end
    end
  end

  def sconfig_file_text build, op
    deflist = []
    build.trait_map.each do |key, value|
      next if value == :undef

      string = key.to_s.upcase
      string.gsub!(/ /, "_")
      string = "#define __" + string + "__ "

      case value
      when :define, true
        string += "1"
      when false
        string += "0"
      when String
        string += value.inspect
      else
        string += value.to_s
      end

      deflist.push(string)
    end

    harness = "__#{build.sconfig_file.basename.to_s.gsub(/\./, '_').upcase}__"
    text = ""
    sio = StringIO.new text
    sio.puts "#ifndef #{harness}"
    sio.puts "#define #{harness}"
    sio.puts ""
    for item in deflist.sort
      sio.puts item
    end
    sio.puts ""
    sio.puts "#endif"
    text
  end

  def def_sconfig_tasks op = {}
    builds = op[:builds] || raise

    desc "Produces a build config file for each targeted device."
    task :sconfig do
      for build in builds
        sconfigfile = build.to_proj_rel(build.sconfig_file).to_s
        text = sconfig_file_text build, op
        mkdir_p(build.to_proj_rel(build.build_dir).to_s)
        begin
          old_text = File.read(sconfigfile)
        rescue
          old_text = nil
        end
        # The config file is typically included in a lot of places,
        # and hence it is a bad idea to change the modification time
        # here if there is no change to the file.
        if text != old_text
          write_file(sconfigfile, text, false)
        end
      end
    end
  end

  def pn_join pnlist
    slist = pnlist.map do |x|
      x.to_s
    end
    slist.join(" ")
  end

  def def_makefile_tasks op = {}
    def_bld_inf_tasks op
    def_mmp_tasks op
    def_sconfig_tasks op

    desc "Produces makefiles for all supported build environments."
    task :makefiles => [:bld_inf, :mmp, :sconfig]
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
    # If the ABI was frozen, would not need to deal with this weird suffix business.
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

  def chcolor(num)
    "\033[#{num}m"
  end

  def red(text)
    "#{chcolor(31)}#{text}#{chcolor(0)}"
  end

  WARNING_RE = /warning/i
  ERROR_RE = /(?:fatal error|^make.*Error|error: )/i

  def sh_check_build_errors cmd
    raise unless cmd.respond_to? :to_str
    sh_pipe(cmd.to_str + ' 2>&1') do |input|
      input.each_line do |line|
        line.chomp!
        if line =~ WARNING_RE
          puts(red(line))
        else
          puts(line)
        end
        if line =~ ERROR_RE
          puts "build error"
          exit 1
        end
      end
    end
  end

  def def_binary_tasks op = {}
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

    if op[:pyd_wrapper]
      desc "Builds a .pyd wrapper for each device requiring it."
      task :pyd_wrapper do
        for pbuild in builds
          for build in pbuild.comp_builds
            next unless build.needs_pyd_wrapper?
            wfile = build.to_proj_rel(build.pyd_wrapper_file).to_s
            mkdir_p(File.dirname(wfile))
            pydfilename = build.binary_file.basename.to_s
            basename = "_" + build.pyd_wrapper_basename
            text = <<'EOF'
import imp
<%= basename %> = imp.load_dynamic('<%= basename %>', '\\sys\\bin\\<%= pydfilename %>')
from <%= basename %> import *
EOF
            text = ERB.new(text).result(binding())
            write_file(wfile, text, false)
          end
        end
      end

      task :bin => :pyd_wrapper
    end # op[:pyd_wrapper]

    desc "Builds binaries for each targeted device."
    case $build_env
    when :cygwin, :gnupoc
      task :bin => [:bld_inf, :mmp, :sconfig] do
        for build in builds
          blddir = build.to_proj_rel(build.build_dir).to_s
          mkdir_p(blddir)

          cd(blddir, :noopcall => true) do
            build.devkit.in_env do
              do_abld = proc do |aplat, abuild|
                built_files = build.comp_builds.map do |compb|
                  aabi = aplat
                  #require 'pp'
                  #pp ["INFO", build, aabi]
                  if compb.target_type == :staticlibrary and
                      build.devkit.native_target.version < [5,2]
                    aabi = "armv5"
                  end

                  File.join(build.devkit.epoc32_home,
                            'release/%s/%s/%s' %
                            [aabi, abuild,
                             compb.binary_file.basename.to_s])
                end

                for built_file in built_files
                  may_fail do; rm built_file; end
                end

                cmd = "abld build -v #{aplat} #{abuild}"
                sh_check_build_errors cmd

                #p ['VERSION', build.devkit.native_target.version]
                if build.devkit.native_target.version >= [5,2]
                  for built_file in built_files
                    bf = built_file
                    bt = bf.sub("/gcce/", "/armv5/")
                    #p [bf, bt]
                    if bt != bf
                      cp(bf, bt)
                    end
                  end
                end

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
                #
                # Naturally, this copying must be kept in mind when
                # writing a PKG file, and it is also best kept in mind
                # that this does not help with resource files, which
                # we are not presently copying. But typically variants
                # do not have their differences in resource files.
                for built_file in built_files
                  cp(built_file, build.build_dir.to_s)
                end
              end

              sh_check_build_errors "bldmake bldfiles"
              do_abld.call(build.abld_platform, build.abld_build)
            end
          end
        end
      end
    else
      task :bin do
        raise "do not know how to build on this system"
      end
    end
  end

  def def_sis_tasks op = {}
    $devel = $sake_op.has_key?(:devel)

    builds = op[:builds] || raise

    desc "Builds PKG files for all targets."
    task :pkg do
      for build in builds
        file = build.to_proj_rel(build.pkg_file).to_s
        src_file = build.to_proj_rel(build.pkg_in_file).to_s
        text = File.read(src_file)
        text = ERB.new(text).result(binding())
        mkdir_p(build.to_proj_rel(build.build_dir).to_s)
        write_file(file, text, true)
      end
    end

    task :makefiles => :pkg

    desc "Builds SIS files for all targets."
    task :sis => :pkg do
      $verbose_makesis = $sake_op.has_key? :verbose_makesis
      $verbose_signsis = $sake_op.has_key? :verbose_signsis

      for build in builds
        blddir = build.to_proj_rel(build.build_dir).to_s
        mkdir_p(blddir)

        cd(blddir, :noopcall => true) do
          build.devkit.in_env do
            command = ["makesis"]
            command.push "-v" if $verbose_makesis
            epocroot = ENV["EPOCROOT"]
            command.push "-d#{epocroot}" if epocroot
            command.push(build.to_build_rel(build.pkg_file))
            command.push(build.to_build_rel(build.short_sis_file))
            sh(*command)
          end
        end
        ln(build.to_proj_rel(build.short_sis_file).to_s,
           build.to_proj_rel(build.long_sis_file).to_s,
           :force => true)

        if build.sign_sis?
          build.devkit.in_env do
            command = %w{signsis}
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
            sh(*command)
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
        when :cygwin, :gnupoc
          for build in builds
            #p build.devkit.build_home
            #if build.devkit.has_emulator?
            may_fail do; rm_r build.devkit.build_home; end
            #end
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

    # May also be :public.
    kinds = Array(op[:kind] || :private)

    # For setting up prerequisites correctly, we must know what C++
    # files need generating before running doxygen.
    gensrc = op[:gen_src] || []

    # The directories from which to look for C++ files to document.
    cxx_src_dirs = Array(op[:cxx_src_dirs] || build.to_proj_rel(build.src_dir).to_s)

    # The directory in which to generate documentation.
    public_doc_home = build.to_proj_rel(build.public_cxx_api_dir).to_s
    private_doc_home = build.to_proj_rel(build.private_cxx_api_dir).to_s

    doc_tasks = []

    kinds.each do |kind|
      case kind
      when :public
        doxysuffix = "api"
        cxx_api_doc_home = public_doc_home
        api_name = "API"
      when :private
        doxysuffix = "int"
        cxx_api_doc_home = private_doc_home
        api_name = "Internal API"
      else
        raise
      end

      doxyfile_in = build.to_proj_rel(build.doxyfile_in_file).to_s
      doxyfile_out = "doxyfile-" + doxysuffix

      desc "Generates a Doxygen #{kind.to_s} API config file."
      file doxyfile_out => [$sakefile, doxyfile_in] do
        # You better copy an existing doxyfile.in from another project to
        # use as a template. Or you can use "doxygen -g doxyfile.in" to get
        # a template with all the Doxygen default settings.
        text = File.read(doxyfile_in)
        text = ERB.new(text).result(binding())
        write_file(doxyfile_out, text, false)
      end

      desc "Builds #{kind.to_s} C++ API documentation."
      task_name = ('cxxdoc_' + doxysuffix).to_sym
      doc_tasks.push(task_name)
      task task_name => ([doxyfile_out] + gensrc)  do
        sh("doxygen", doxyfile_out)
      end
    end

    desc "Builds C++ API documentation."
    task :cxxdoc => doc_tasks

    task :cleandoc do
      may_fail { rm_r(public_doc_home) }
      may_fail { rm_r(private_doc_home) }
    end

    unless op[:no_cleanbuild]
      task :cleanbuild => :cleandoc do
        # We consider the doxyfile as something one would want to
        # include in a distribution, and therefore :cleantemp will not
        # touch it; :cleanbuild is required.
        may_fail { rm("doxyfile-int") }
        may_fail { rm("doxyfile-api") }
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

  def def_epydoc_tasks op = {}
    build = op[:build] or raise "no build given"
    py_files = op[:py_file] or raise "no .py file(s) specified"
    py_files = Array(py_files)
    py_file = py_files.first or raise "empty list of .py files given"

    api_doc_dir = build.python_api_dir
    api_doc_home = build.to_proj_rel(api_doc_dir).to_s
    py_lib_home = py_file.dirname.to_s

    g = lambda {|x| x.relative_path_from(api_doc_dir).to_s}
    py_files_r = py_files.map {|x| g.call(x)}

    desc "Builds Python API documentation."
    # The dependency here is useful if the .py file should be
    # something that is generated by the build system.
    task :pydoc => build.to_proj_rel(py_file).to_s do
      ppath = ENV['PYTHONPATH']
      ppath = (ppath ? "#{py_lib_home}:#{ppath}" : py_lib_home)
      mkdir_p(api_doc_home)
      cd(api_doc_home, :noopcall => true) do
        set_env('PYTHONPATH', ppath) do
          sh("epydoc", "--simple-term", "--verbose", "--html", *py_files_r)
        end
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
    task :dist do
      require 'build/archive'

      files = []
      paths = []

      src = comp.to_proj_rel(comp.src_dir).to_s
      bldr = doc_build.build_root_path

      if op[:usual_files]
        files.concat %w{COPYING README}
        files.concat Dir["#{src}/*.h"]
        files.concat Dir["#{src}/*.cpp"]
        files.concat Dir["#{src}/*.py"]
        files.concat Dir["#{bldr}/**/*.h"]
        files.concat Dir["#{bldr}/**/*.cpp"]
        files.concat Dir["#{bldr}/**/*.py"]
        files.concat Dir["test-programs/*.py"]
      end

      more_files = op[:files]
      more_files &&= Array(more_files)
      if more_files
        files.concat(more_files)
      end

      if op[:usual_files]
        paths.push comp.private_cxx_api_dir
        paths.push comp.python_api_dir
        paths.push comp.doxyfile_file

        for build in builds
          paths.push build.bld_inf_file
          paths.push build.mmp_file
          paths.push build.pkg_file
        end
      end

      files += paths.map do |path|
        comp.to_proj_rel(path).to_s
      end

      files.delete_if do |file|
        not File.exist? file
      end
      files.sort!

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

    desc "Builds website material."
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

=begin
      install 'COPYING', web_dir.to_s, :mode => 0644

      api_sfile = comp.python_api_dir + (comp.basename + ".html")
      api_dfile = web_dir + ("%s-%s-api.html" % [comp.basename, comp.version_string])
      install api_sfile.to_s, api_dfile.to_s, :mode => 0644

      api_sfile = op[:py_file] || (comp.src_dir + (comp.basename + ".py"))
      api_dfile = web_dir + ("%s-%s-api.py" % [comp.basename, comp.version_string])
      install api_sfile.to_s, api_dfile.to_s, :mode => 0644
=end

      if op[:src_dist]
        install comp.to_proj_rel(comp.src_tar_file).to_s, web_dir.to_s, :mode => 0644
      end

      if op[:bin_dist]
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
    require 'sake3/sake_utils'

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

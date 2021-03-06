# -*- ruby -*-
# sake variant sake4

# This makefile for a custom build tool drives the ABLD-based Symbian
# builds. In other build configurations you may safely ignore this file.

require 'sake4/component'

require 'src/current_config'

$uid_v9 = $UID_V9
$basename = $APP_BASENAME
$version = [$MAJOR_VERSION, $MINOR_VERSION]

$curl_as_source = true
$pamp_curl = true

$proj = Sake::Project.new(:basename => $basename,
                          :name => $APP_NAME,
                          :version => $version,
                          :uid => Sake::Uid.v9($uid_v9),
                          :vendor => "HIIT")

class <<$proj
  def pkg_in_file
    group_dir + ("module.pkg.in")
  end
end

$app = Sake::Component.new(:project => $proj,
                           :target_type => ($IS_APPLICATION ?
                                            :application : :exe),
                           :basename => $basename,
                           :bin_basename => $basename,
                           :uid3 => Sake::Uid.v9($uid_v9),
                           :caps => Sake::ALL_CAPS)

class <<$app
  def mmp_in_file
    group_dir + ("module.mmp.in")
  end
end

#require 'pp'
#pp [$proj, $app]; exit

$comp_list = [$app].compact

$kits = Sake::DevKits::get_exact_set([$KIT_NAME])

if $sake_op[:comps]
  comps = $sake_op[:comps].strip.split(/,/)
  $comp_list.delete_if do |comp|
    !comps.include?(comp.basename)
  end
end

$builds = $kits.map do |kit|
  build = Sake::ProjBuild.new(:project => $proj,
                              :handle => $VARIANT_NAME,
                              :devkit => kit)
  build.abld_platform = (build.v9_up? ? "gcce" : "armi")
  build.abld_build = ($ABLD_VARIANT || raise)
  #build.handle = (build.handle + "_udeb") if $sake_op[:udeb]
  build
end

def try_load file
  begin
    load file
  rescue LoadError; end
end

# For any v9 builds, configure certificate info for signing.
begin
  load 'local/signing.rb'
rescue LoadError
  # Default signing configuration.
  $builds = $builds.map do |build|
    build.set_epoclocalrb_cert_info($CERT_NAME || raise)
    build.sign = $SIGNED
    if build.sign
      build.max_caps = (build.max_caps & $CAPABILITIES).sort
    end
    build
  end
end

$builds.delete_if do |build|
  (build.sign and !build.cert_file)
end

if $sake_op[:builds]
  blist = $sake_op[:builds]
  $builds.delete_if do |build|
    !blist.include?(build.handle)
  end
end

desc "Prints a list of possible builds."
task :builds do
  for build in $builds
    puts build.handle
  end
end

desc "Prints info about possible builds."
task :build_info do
  for build in $builds
    puts "#{build.handle}:"
    puts "  project name  #{build.project.name}"
    puts "    basename    #{build.project.basename}"
    puts "  target        #{build.target.handle}"
    puts "  devkit        #{build.devkit.handle}"
    puts "  abld platform #{build.abld_platform}"
    puts "  abld build    #{build.abld_build}"
    puts "  sign SIS?     #{build.sign}"
    puts "  cert file     #{build.cert_file}"
    puts "  privkey file  #{build.key_file}"
    puts "  cert caps     #{build.max_caps.inspect}"
    puts "  components    #{build.comp_builds.map {|x| x.component.basename}.inspect}"
  end
end

class HexNum
  def initialize num
    @num = num
  end

  def to_s
    "0x%08x" % @num
  end
end

# Defines site-specific $default_ values.
# Which we do not want into any released binaries.
if $sake_op[:site]
  try_load('local/default_options.rb')
end

$exeb = Hash.new
for build in $builds
  map = build.trait_map

  # To define __UID__ for header files.
  if build.uid
    map[:uid] = HexNum.new(build.uid.number)
  end

  # NDEBUG controls whether asserts are to be compiled in (NDEBUG is
  # defined in UDEB builds). Normally an assert results in something
  # being printed to the console. To also have the errors logged, you
  # will want to enable logging by setting "logging=true". Without
  # this setting, there will be no dependency on the (deprecated) file
  # logger API, and errors are still displayed on the console (if you
  # have one and have time to read it). "logging=true" has no effect
  # if your SDK does not have the required API.
#  map[:do_logging] = (($sake_op[:logging] and map[:has_flogger]) ? 1 : 0)

  #map[:upload_time_expr] = ($sake_op[:upload_time_expr] || $default_upload_time_expr || :undef)
  #map[:upload_url] = ($sake_op[:upload_url] || $default_upload_url || :undef)
  #map[:username] = ($sake_op[:username] || $default_username || :undef)

  # Each build variant shall have all of the components.
  build.comp_builds = $comp_list.map do |comp|
    b = Sake::CompBuild.new(:proj_build => build,
                            :component => comp)
    $exeb[build] = b
    b
  end
end

task :default => [:bin, :sis]

require 'sake4/tasks'

Sake::Tasks::def_list_devices_tasks(:builds => $builds)

Sake::Tasks::def_makefile_tasks(:builds => $builds)

Sake::Tasks::def_binary_tasks(:builds => $builds)

Sake::Tasks::def_sis_tasks(:builds => $builds)

Sake::Tasks::def_clean_tasks(:builds => $builds)

task :all => [:makefiles, :bin, :sis]

$doc_build = $builds.last

# Configure any rules related to releasing and uploading and such
# things. Probably at least involves copying or uploading the
# distribution files somewhere.
try_load('local/releasing.rb')

Sake::Tasks::force_uncurrent_on_op_change

def sis_info opt
  for build in $builds
    if build.short_sisx_file.exist?
      sh("sisinfo -f #{build.short_sisx_file} #{opt}")
    end
  end
end

task :sis_ls do
  sis_info "-i"
end

task :sis_cert do
  sis_info "-c"
end

task :sis_struct do
  sis_info "-s"
end

try_load('local/transfer.rb')

#
# sakefile.rb
#
# Copyright 2009 Helsinki Institute for Information Technology (HIIT)
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

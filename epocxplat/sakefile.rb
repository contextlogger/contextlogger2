# -*- ruby -*-
# sake variant sake4

require 'sake4/component'

$projname = $sake_op[:projname]
$basename = "epocxplat"
$uid = $sake_op[:uid].hex
$version = $sake_op[:version].split(".").map {|x| x.to_i}
$caps = $sake_op[:caps].split(/\s+/)

uidobj = Sake::Uid.new($uid, $uid)

$proj = Sake::Project.new(:basename => $basename,
                          :name => $projname,
                          :version => $version,
                          :uid => uidobj)

$app = Sake::Component.new(:project => $proj,
                           :target_type => :dll,
                           :basename => $basename,
                           :bin_basename => $sake_op[:bin_basename],
                           :uid3 => uidobj,
                           :caps => $caps)

class <<$app
  def mmp_in_file
    group_dir + ("module.mmp.in")
  end
end

$comp_list = [$app].compact

if $sake_op[:kits]
  $kits = Sake::DevKits::get_exact_set($sake_op[:kits].strip.split(/,/))
else
  $kits = Sake::DevKits::get_all
end

if $sake_op[:comps]
  comps = $sake_op[:comps].strip.split(/,/)
  $comp_list.delete_if do |comp|
    !comps.include?(comp.basename)
  end
end

$builds = $kits.map do |kit|
  build = Sake::ProjBuild.new(:project => $proj,
                              :devkit => kit)
  build.abld_platform = (build.v9_up? ? "gcce" : "armi")
  build.abld_build = ($sake_op[:rel] || raise)
  build.handle = ($sake_op[:handle] || raise)
  build
end

def try_load file
  begin
    load file
  rescue LoadError; end
end

if $sake_op[:builds]
  blist = $sake_op[:builds]
  $builds.delete_if do |build|
    !blist.include?(build.handle)
  end
end

$builds.sort! do |x,y|
  x.handle <=> y.handle
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
    puts "  cert caps     #{$comp_build.caps.inspect}"
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
  if $sake_op[:logging] and map[:has_flogger]
    map[:do_logging] = :define
  end

  # Each build variant shall have all of the components.
  build.comp_builds = $comp_list.map do |comp|
    b = Sake::CompBuild.new(:proj_build => build,
                            :component => comp)
    b.caps = $caps
    $comp_build = b
    b
  end
end

task :default => [:bin]

require 'sake4/tasks'

Sake::Tasks::def_list_devices_tasks(:builds => $builds)

Sake::Tasks::def_makefile_tasks(:builds => $builds)

Sake::Tasks::def_binary_tasks(:builds => $builds)

Sake::Tasks::def_clean_tasks(:builds => $builds)

Sake::Tasks::force_uncurrent_on_op_change

=begin rdoc

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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

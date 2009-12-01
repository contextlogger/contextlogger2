# -*- ruby -*-
# sake variant sake3

# The build tool driving this makefile is a custom one.

require 'sake3/component'

require 'src/current_config'

$uid_v8 = 0x08460002
$basename = $APP_BASENAME
$version = [$MAJOR_VERSION, $MINOR_VERSION]

$curl_as_source = true
$pamp_curl = true

$proj = Sake::Project.new(:basename => $basename,
                          :name => "CL2 App",
                          :version => $version,
                          :uid => Sake::Uid.v8($uid_v8),
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
                           :uid3 => Sake::Uid.v8($uid_v8),
                           :caps => Sake::ALL_CAPS)

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
                              :handle => $VARIANT_NAME,
                              :devkit => kit)
  build.abld_platform = (build.v9? ? "gcce" : "armi")
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
try_load('local/signing.rb')

$builds.delete_if do |build|
  (build.sign and !build.cert_file)
#  (build.sign and !build.cert_file) or (not build.v9?)
end

# This option will eventually move from here to cl2embed.
case ($sqlite = $sake_op[:sqlite])
when nil, "source"
  # This option causes SQLite to be built from source and linked in
  # statically into the application.
  $sqlite = :source
when "symbian"
  # Symbian provides an SQLite port, but it is not available for all
  # S60 3rd edition devices. This option indicates whether to use
  # their DLL.
  $sqlite = sq.intern
else
  raise "sqlite=#{$sqlite.inspect} not supported"
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
    puts "  components    #{build.comp_builds.map {|x| x.component.basename}.ins
pect}"
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

$sqlite = :source # safer option (if relevant headers should change)
$sqlite = :static # faster option (relevant headers rarely change in a relevant way)

if $sqlite == :static or $sqlite == :source
  $use_sqlite3h = true
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

  # This define will allow us to pick the correct header.
  if $use_sqlite3h
    map[:use_sqlite3h] = :define
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
  #map[:iap_id] = (($sake_op[:iap_id] && $sake_op[:iap_id].to_i) || $default_iap_id.to_i || :undef)
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

require 'sake3/tasks'

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

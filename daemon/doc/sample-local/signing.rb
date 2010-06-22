this_dir = File.dirname(File.expand_path(__FILE__))

$builds = $builds.map do |build|
  if build.target.symbian_platform.major_version < 9
    build
  else
    build.sign = $SIGNED
    if $SIGNED
      build.max_caps = %w{LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData}
      build.cert_file = File.join(this_dir, "selfsigned.cer")
      build.key_file = File.join(this_dir, "selfsigned.key")
    end
    build
  end
end

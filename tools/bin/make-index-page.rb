#!/usr/bin/env ruby
path = (ARGV.shift || raise)
dirname = File.basename(path)
indexfile = File.join(path, "index.html")
filelist = nil
Dir::chdir(path) do
  filelist = Dir.glob('*')
end
filelist.delete("index.html")
p filelist

require 'erb'
indata = DATA.read
outdata = ERB.new(indata).result(binding())
File.open(indexfile, "w") do |output|
  output.write(outdata)
end
puts(indexfile)

__END__
<html>
<head>
<title><%= dirname %>/</title>
</head>
<body>
<h1><%= dirname %>/</h1>
<% filelist.each do |f| %>
<p><a href="<%= f %>"><%= f %></a></p>
<% end %>
</body>
</html>

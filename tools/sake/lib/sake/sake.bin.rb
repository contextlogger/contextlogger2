=begin rdoc

sake.bin.rb

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

$LOAD_PATH.unshift(File.expand_path("..", File.dirname(__FILE__)))

SAKEFILES = %w{sakefile sakefile.rb Sakefile Sakefile.rb}
for file in SAKEFILES
  if File.exist? file
    unless File.readable? file
      raise "makefile #{file} exists, but is not readable"
    end
    $sakefile = File.expand_path(file)
    break
  end
end

unless $sakefile
  raise "no makefile found (looked for #{SAKEFILES.inspect})"
end

File.open($sakefile) do |input|
  emptyre = /^$/
  commentre = /^#/
  variantre = /^#+\s+sake\s+variant\s+([a-zA-Z][a-zA-Z0-9]*)/
  for line in input
    next if line =~ emptyre
    break unless line =~ commentre
    if line =~ variantre
      $sake_variant = $1
      break
    end
  end
end
unless $sake_variant
  raise "makefile does not state which Sake variant to use"
end

begin
  require(File.join($sake_variant, "init"))
rescue LoadError
end

load($sakefile)

require(File.join($sake_variant, "run"))

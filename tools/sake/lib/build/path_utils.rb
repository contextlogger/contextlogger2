=begin rdoc

path_utils.rb

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

def path_windowsify(path)
  path.gsub(/\//, "\\")
end

def path_unixify path
  path.gsub(/\\/, "/")
end

def path_newext(path)
  path.sub(/\.[^\.]*$/, ".o")
end

def path_split_drive path
  path =~ /^([a-z]):(.*)/i or raise
  [$1, $2]
end

def path_cygwinize(path)
  path = path.downcase
  drive, path = path_split_drive path
  path = path_unixify path
  "/cygdrive/#{drive}#{path}"
end

def dir_entries(dir, with_path)
  list = Dir.entries(dir)
  list.delete('.')
  list.delete('..')
  if with_path
    list.map! {|x| File.join(dir, x)}
  end
  return list
end

def dir_empty? dir
  dir_entries(dir, false).empty?
end

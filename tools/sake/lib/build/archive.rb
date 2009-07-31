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

# Creates a tar archive file.
# tarfile:: The pathname of the archive file.
# dir:: The directory under which all the files in the archive should
#       reside.
# list:: A list of files to include in the archive.
# op:: +srcdir+ may be passed.
def tar_with_dir(tarfile, dir, list, op = {})
  tarfile = File.expand_path(tarfile)
  srcdir = op[:srcdir] || Dir.pwd

  require 'tmpdir'
  cd(Dir::tmpdir, :noop => false) do
    rm_rf dir
    for file in list
      srcfile = File.join(srcdir, file)
      destfile = File.join(dir, file)
      mkdir_p File.dirname(destfile)
      cp_r srcfile, destfile
    end
    sh("tar",
       "--create", "--verbose", "--gzip",
       "--file", tarfile,
       dir)
  end
end

module ArcUtil
  def dir_entries(dir, with_path)
    list = Dir.entries(dir)
    list.delete('.')
    list.delete('..')
    if with_path
      list.map! {|x| File.join(dir, x)}
    end
    return list
  end

  def fjoin(*args)
    File.join(*(args.compact))
  end

  def scan_files(root, relpath, op = {})
    with_root = op[:with_root]
    excludes = Array(op[:exclude] || [])
    func = proc do |file|
      path = fjoin(root, file)
      if File.file?(path)
        incl = true
        for exc in excludes
          if exc =~ file
            incl = false
            break
          end
        end
        incl ? [with_root ? path : file] : []
      elsif File.directory?(path)
        dir_entries(path, false).inject([]) do |res, child|
          res.concat(func.call(fjoin(file, child)))
        end
      else
        warn "neither file or directory #{path}"
        []
      end
    end
    func.call(relpath)
  end

  extend self
end

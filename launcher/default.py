#
# default.py
#
# Copyright 2007 Helsinki Institute for Information Technology
# and the authors.
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
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# This file comes from the S4ALL project ISC software source tree.
# 
# We are imitating Nokia's default.py here to get ourselves a console,
# but now we want to launch a specific app always. This allows us to
# install a .py file in the usual location (from where the "Python"
# application looks for scripts to run), and also have that script
# launchable via its own icon. And the "application" only has to be
# installed once.

import os
import sys
import appuifw
import series60_console

script_dirs = [(u'c:','c:\\python'),
               (u'e:','e:\\python')]
for path in ('c:\\python\\lib','e:\\python\\lib'):
    if os.path.exists(path):
        sys.path.append(path)

# Init console first thing so we can see any errors.
app_console = series60_console.Console()
appuifw.app.body = app_console.text
sys.stderr = sys.stdout = app_console

app_path = os.path.split(appuifw.app.full_name())[0]
app_drive = app_path[:2]
app_script = app_drive + "\\python\\cl2launcher.py"

orig_exit_key_handler = appuifw.app.exit_key_handler

def run_app():
    try:
        try:
            #print "launching", app_script
            execfile(app_script, globals())
        finally:
            appuifw.app.exit_key_handler = orig_exit_key_handler
            appuifw.app.title = u'Console'
            appuifw.app.body = app_console.text
            appuifw.app.screen = 'normal'
            appuifw.app.menu = [(u"Restart", run_app)]
            sys.stderr = sys.stdout = app_console
    except:
        import traceback
        traceback.print_exc()

appuifw.app.menu = []
run_app()

#
# Copyright 2007 Helsinki Institute for Information Technology (HIIT)
# and the authors.  All rights reserved.
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

import appuifw
import time
import e32
import sys
import os
import urllib

try:
    import miso
    have_miso = True
except ImportError:
    have_miso = False

try:
    import cl2_client
    have_cl2 = True
except ImportError:
    have_cl2 = False

wd_pattern = u"*[08460006]*"
cl2_pattern = u"*[e8460002]*"
magic_file = u"c:\\data\\cl2\\disable_autostart.txt"
db_file = "e:\\log.db"

def makedirs(path):
    try:
        os.makedirs(path)
    except:
        # Strangely throws an exception if the path already exists.
        pass

def make_file(file, data):
    makedirs(os.path.dirname(file))
    fp = open(file, "w")
    try:
        fp.write(data)
    finally:
        fp.close()

class GUI:
    def __init__(self):
        self.lock = e32.Ao_lock()
        self.main_title = u"CL2 Launcher"
        self.old_title = appuifw.app.title
        appuifw.app.title = self.main_title

        app_path = os.path.split(appuifw.app.full_name())[0]
        self.app_drive = app_path[:2]

        main_menu = [
            (u"CL2 running?", self.show_is_cl2_running),
            (u"Start CL2", self.start_cl2_daemon),
            (u"Stop CL2", self.stop_cl2_daemon),
            (u"Kill CL2", self.kill_cl2_daemon),

            (u"WD running?", self.show_is_wd_running),
            (u"Start WD", self.start_wd_daemon),
            (u"Stop WD", self.stop_wd_daemon),
            (u"Kill WD", self.kill_wd_daemon),

            (u"WD autostart enabled?", self.is_wd_enabled),
            (u"Disable WD autostart", self.disable_wd),
            (u"Enable WD autostart", self.enable_wd),

            (u"Delete database", self.delete_db),
            (u"Reboot device", self.reboot_device),
            (u"View log file", self.show_log),

            (u"Exit", self.abort)
            ]

        appuifw.app.menu = main_menu
        appuifw.app.exit_key_handler = self.abort

    def show_is_wd_running(self):
        if have_miso:
            if miso.have_process(wd_pattern):
                appuifw.note(u"WD running", "info")
            else:
                appuifw.note(u"WD not running", "info")
        else:
            appuifw.note(u"Miso not installed", "error")

    def show_is_cl2_running(self):
        if have_miso:
            if miso.have_process(u"cl2app"):
                appuifw.note(u"Named CL2 running", "info")
            elif miso.have_process(cl2_pattern):
                appuifw.note(u"CL2 running", "info")
            else:
                appuifw.note(u"CL2 not running", "info")
        else:
            appuifw.note(u"Miso not installed", "error")

    def start_wd_daemon(self):
        """
        Launches the daemon.
        """
        daemon = (self.app_drive + u"\\sys\\bin\\cl2watchdog.exe")
        # The args are: executable path, command string, and whether
        # to wait for the started process to exit.
        print("starting " + daemon)
        appuifw.e32.start_exe(daemon, '')

    def kill_wd_daemon(self):
        """
        Terminates the daemon. This requires sufficient caps, or we
        will get KERN-EXEC 46.
        """
        if have_miso:
            # http://wiki.forum.nokia.com/index.php/How_to_start_and_stop_exe
            count = miso.kill_process(wd_pattern, 0)
            if count == 0:
                appuifw.note(u"Nothing to kill", "info")
            else:
                appuifw.note(u"%d process(es) killed" % count, "info")
        else:
            appuifw.note(u"Miso not installed", "error")

    def stop_wd_daemon(self):
        """
        Asks the daemon to stop by sending it a request.
        """
        # The Symbian PubSub mechanism would be one option for
        # implementing this.
        appuifw.note(u"No protocol for stopping WD", "error")

    def start_cl2_daemon(self):
        """
        Launches the daemon.
        """
        daemon = (self.app_drive + u"\\sys\\bin\\cl2app.exe")
        # The args are: executable path, command string, and whether
        # to wait for the started process to exit.
        print("starting " + daemon)
        appuifw.e32.start_exe(daemon, '')

    def kill_cl2_daemon(self):
        """
        Terminates the daemon. This requires sufficient caps, or we
        will get KERN-EXEC 46.
        """
        if have_miso:
            # http://wiki.forum.nokia.com/index.php/How_to_start_and_stop_exe
            count = miso.kill_process(cl2_pattern, 0)
            if count == 0:
                appuifw.note(u"Nothing to kill", "info")
            else:
                appuifw.note(u"%d process(es) killed" % count, "info")
        else:
            appuifw.note(u"Miso not installed", "error")

    def stop_cl2_daemon(self):
        """
        Asks the daemon to stop by sending it a request.
        """
        if not have_cl2:
            appuifw.note(u"CL2 client lib not installed", "error")
            return
        try:
            session = cl2_client.Session()
        except:
            appuifw.note(u"failed to connect to CL2", "error")
            return
        try:
            try:
                res = session.eval("do cl2.shutdown(); return 'ok' end")
                if res == u"ok":
                    appuifw.note(u"CL2 shutdown OK", "info")
                else:
                    appuifw.note(u"unexpected response from CL2", "error")
            except:
                appuifw.note(u"failed to request shutdown", "error")
        finally:
            session.close()

    def is_wd_enabled(self):
        if os.path.exists(magic_file):
            appuifw.note(u"Autostart disabled", "info")
        else:
            appuifw.note(u"Autostart enabled", "info")

    def disable_wd(self):
        make_file(magic_file, "x")
        appuifw.note(u"Autostart disabled", "info")

    def enable_wd(self):
        os.unlink(magic_file)
        appuifw.note(u"Autostart enabled", "info")

    def delete_db(self):
        os.unlink(db_file)
        appuifw.note(u"Database deleted", "info")

    def reboot_device(self):
        """
        Causes the device to reboot. Do not know how safe this is,
        though.
        """
        appuifw.e32.start_exe(u'z:\\sys\\bin\\starter.exe', '')

    def show_log(self):
        logdir = u'c:\\logs\\cl2\\'

        try:
            flist = [ unicode(file)
                      for file
                      in os.listdir(logdir)
                      if file.endswith(".txt") ]
        except:
            # The directory not existing will cause an exception that
            # appears to be quietly consumed, i.e., it is not printed
            # to the console.
            flist = []

        if len(flist) == 0:
            appuifw.note(u"No logs to display", "info")
            return

        index = appuifw.popup_menu(flist, u'Select file to view')
        if index is None:
            return
        fname = logdir + flist[index]

        print "opening " + fname
        doc_lock = e32.Ao_lock()
        ch = appuifw.Content_handler(doc_lock.signal)
        ch.open(fname)
        doc_lock.wait()

        # These cause some problems when used repeatedly.
        #appuifw.Content_handler().open(fname)
        #appuifw.Content_handler().open_standalone(fname)

    def abort(self):
        self.lock.signal()

    def loop(self):
        self.lock.wait()

    def close(self):
        appuifw.app.menu = []
        appuifw.app.body = None
        appuifw.app.exit_key_handler = None
        appuifw.app.title = self.old_title

def main():
    gui = GUI()
    try:
        gui.loop()
    finally:
        gui.close()

if __name__ == '__main__':
    main()

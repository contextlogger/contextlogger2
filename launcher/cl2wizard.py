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

# This is an installation wizard for the ContextLogger2 phone client,
# intended for one-off use in initial configuration.

import appuifw
import time
import e32
import sys
import os
import urllib
import socket

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
data_dir = "c:\\data\\cl2"
log_db_file = "e:\\data\\cl2" + "\\log.db"
alt_log_db_file = "c:\\data\\cl2" + "\\log.db"
config_db_file = data_dir + "\\config.db"
config_file = data_dir + "\\config.txt"
logs_dir = u'c:\\logs\\cl2'

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

def rm_file(file):
    try:
        os.unlink(file)
    except:
        # Assuming non-existent file.
        pass

# Cannot delete directories containing directories.
def shallow_rmtree(dn):
    pn = dn + "\\"
    try:
        for fn in os.listdir(pn):
            os.unlink(pn + fn)
        os.rmdir(dn)
    except:
        # Assuming non-existent directory.
        pass

def make_logs_dir():
    makedirs(logs_dir)

def delete_logs_dir():
    shallow_rmtree(logs_dir)

def print_iap_list():
    ap_list = socket.access_points()
    print repr(ap_list)

# Returns a Unicode string (or None), so use s.encode("utf-8") as
# required if you need bytes.
def ask_iap_name():
    # [{'iapid': 17, 'name': u"FooBar"}, ...]
    ap_list = socket.access_points()
    name_list = [ r['name'] for r in ap_list ]
    index = appuifw.popup_menu(name_list, u'Select IAP')
    if index is None:
        return None
    return name_list[index]

def format_time(tm):
    if e32.pys60_version_info[0] >= 2:
        fmt = "%F %T UTC"
    else:
        fmt = "%Y-%m-%d %H:%M:%S UTC"
    return time.strftime(fmt, tm)

class GUI:
    def __init__(self):
        self.lock = e32.Ao_lock()
        self.main_title = u"CL2 Wizard"
        self.old_title = appuifw.app.title
        appuifw.app.title = self.main_title

        app_path = os.path.split(appuifw.app.full_name())[0]
        self.app_drive = app_path[:2]

        main_menu = [
            (u"Status overview", self.status_overview),
                           
            (u"(1) List IAPs", print_iap_list),
            (u"(2) Create config file", self.create_config_file),
            (u"(3) Install SSL cert", self.install_cert),
            (u"(4) Start WD", self.start_wd_daemon),
            (u"(5) WD running?", self.show_is_wd_running),
            (u"(6) Logger running?", self.show_is_cl2_running),
	    (u"(7) Upload now", self.upload_now),
	    (u"(8) Last upload when?", self.show_upload_time),

            (u"Logger",
             ((u"Start Logger", self.start_cl2_daemon),
              (u"Stop Logger", self.stop_cl2_daemon),
              (u"Kill Logger", self.kill_cl2_daemon))),

            (u"Watchdog",
             ((u"Stop WD", self.stop_wd_daemon),
              (u"Kill WD", self.kill_wd_daemon),

              (u"WD autostart enabled?", self.is_wd_enabled),
              (u"Disable WD autostart", self.disable_wd),
              (u"Enable WD autostart", self.enable_wd))),

            (u"Logging",
             ((u"View log file", self.show_log),
              (u"Create logs directory", make_logs_dir),
              (u"Delete logs directory", delete_logs_dir))),

            (u"Extra",
             ((u"Delete log database", self.delete_log_db),
              (u"Delete config database", self.delete_config_db),
              (u"View config file", self.show_config_file),
              (u"Delete config file", self.delete_config_file))),

            (u"Exit", self.abort)
            ]

        appuifw.app.menu = main_menu
        appuifw.app.exit_key_handler = self.abort

        self.sensor_list = None
	self.current_sensor = None
        self.app_info = None

	print "Welcome."

    def is_wd_running(self):
        return miso.have_process(wd_pattern)

    def is_cl2_running(self):
        # In practice it is the former pattern that matches if any, as
        # we have been unsuccessful in naming the process.
        return (miso.have_process(cl2_pattern) or
                miso.have_process(u"cl2app"))

    def show_is_wd_running(self):
        if have_miso:
            if self.is_wd_running():
                appuifw.note(u"WD running", "info")
            else:
                appuifw.note(u"WD not running", "info")
        else:
            appuifw.note(u"Miso not installed", "error")

    def show_is_cl2_running(self):
        if have_miso:
            if self.is_cl2_running():
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

    def with_daemon_session(self, f):
        if not have_cl2:
            appuifw.note(u"CL2 client lib not installed", "error")
            return
        try:
            session = cl2_client.Session()
        except:
            appuifw.note(u"Failed to connect to CL2", "error")
            return
        try:
            return f(session)
        finally:
            session.close()

    def daemon_exec_ok(self, lua_s, ok_s, fail_s):
        def f(session):
            try:
                res = session.eval(lua_s)
                if res == u"ok":
                    appuifw.note(unicode(ok_s), "info")
                else:
                    appuifw.note(unicode(fail_s), "error")
            except:
                appuifw.note(unicode(fail_s), "error")
        self.with_daemon_session(f)

    def daemon_query(self, lua_s):
        def f(session):
            try:
                res = session.eval(lua_s)
		return res
            except:
		pass
        return self.with_daemon_session(f)

    def status_overview(self):
        b2s = lambda v: v and "yes" or "no"
        running = self.is_cl2_running()
        print ""
        print "logger running: %s" % b2s(running)
        print "watchdog running: %s" % b2s(self.is_wd_running())
        if not running: return
        if self.app_info is None:
            self.app_info = self.daemon_query(""" return string.format("%s v%s (%s) %s %s", cl2.app_name, cl2.app_version, cl2.app_variant, cl2.build_type, cl2.compiler_version and string.format("%s v%s", cl2.compiler_name, cl2.compiler_version) or cl2.compiler_name) """)
        print self.app_info
        print self.daemon_query("""
do
  local function get (n) return (cl2.config_get(n) or "(default)") end
  return string.format("Params: [IAP: %s] [BT interval: %s] [GPS interval: %s] [upload time: %s]", get("iap"), get("sensor.btprox.interval"), get("sensor.gps.interval"), get("uploader.time_expr"))
end """)
        print self.daemon_query(""" do local lst = cl2.all_sensor_names(); local function get (n) if cl2.is_sensor_supported(n) then if cl2.is_sensor_running(n) then return "active" else return "inactive" end else return "unsupported" end end; local function map (f, lst) local nlst = {}; for i,v in ipairs(lst) do nlst[i] = f(v); end; return nlst; end; local function kjoin (lst) local s = ""; local first = true; local add = function (x) s = (s .. x) end; for i,x in ipairs(lst) do if first then first = false else add(" ") end; add(x); end; return s; end; local function g (n) return string.format("[%s: %s]", n, get(n)); end; return ("Sensors: " .. kjoin(map(g, lst))); end """)
        print ""

    def upload_now(self):
        self.daemon_exec_ok(""" cl2.upload_now(); return "ok" """, u"Upload requested", u"Failed to request upload")

    def show_upload_time(self):
        v = self.daemon_query("""do local t = cl2.get_upload_time(); if t then return t else return "nil" end; end""")
        if v is None:
            return
        if v == "nil":
            appuifw.note(u"No uploads", "info")
        else:
            try:
                t = int(v)
            except ValueError:
                appuifw.note(u"Failed to query time", "error")
                return
            tm = time.gmtime(t)
            ts = format_time(tm)
            appuifw.note(u"Last upload at %s" % ts, "info")
                
    def stop_cl2_daemon(self):
        """
        Asks the daemon to stop by sending it a request.
        """
        def f(session):
            try:
                res = session.eval("do cl2.shutdown(); return 'ok' end")
                if res == u"ok":
                    appuifw.note(u"CL2 shutdown OK", "info")
                else:
                    appuifw.note(u"unexpected response from CL2", "error")
            except:
                appuifw.note(u"Failed to request shutdown", "error")
        self.with_daemon_session(f)

    def is_wd_enabled(self):
        if os.path.exists(magic_file):
            appuifw.note(u"Autostart disabled", "info")
        else:
            appuifw.note(u"Autostart enabled", "info")

    def disable_wd(self):
        make_file(magic_file, "x")
        appuifw.note(u"Autostart disabled", "info")

    def enable_wd(self):
        rm_file(magic_file)
        appuifw.note(u"Autostart enabled", "info")

    def delete_log_db(self):
        rm_file(log_db_file)
        rm_file(alt_log_db_file)
        appuifw.note(u"Database deleted", "info")

    def delete_config_db(self):
        rm_file(config_db_file)
        appuifw.note(u"Database deleted", "info")

    def delete_config_file(self):
        rm_file(config_file)
        appuifw.note(u"Config file deleted", "info")

    def create_config_file(self):
        iap_name = ask_iap_name()
        if iap_name is None:
            return
        iap_name = iap_name.encode("utf-8")
        iap_expr = """return cl2.iap_id_by_name('%s');""" % iap_name

        #import sysinfo
        #username = appuifw.query(u"Username:", "text", unicode(sysinfo.imei()))
        username = appuifw.query(u"Username:", "text", u"johndoe")
        if username is None:
            return
        username = username.encode("utf-8")
        
        db_drive = appuifw.query(u"Log drive:", "text", u"e")
        if db_drive is None:
            return
        if len(db_drive) != 1:
            appuifw.note(u"Must be one letter", "error")
            return
        dbdir = (db_drive + u":\\\\data\\\\cl2").encode("utf-8")
        
        ldt = appuifw.query(u"Disk threshold (MB):", "number", 10)
        if ldt is None:
            return
        database_disk_threshold = ldt * 1e6
        
	text = """
	return {
	username = "%s";
	iap = "%s";
        database_dir = "%s";
        database_disk_threshold = %d;
	}
	""" % (username, iap_expr, dbdir, database_disk_threshold)
	make_file(config_file, str(text))
        appuifw.note(u"Config file written", "info")

    def show_config_file(self):
        doc_lock = e32.Ao_lock()
        ch = appuifw.Content_handler(doc_lock.signal)
        ch.open(unicode(config_file))
        doc_lock.wait()

    def install_cert(self):
        fn = u"cl2-ca-cert.der"
        certfile = None
        for dn in ('c:\\data\\cl2\\', 'e:\\data\\cl2\\'):
            pn = dn + fn
            if os.path.exists(pn):
                certfile = pn
        if certfile is None:
            appuifw.note(u"No cert to install", "error")
            return
        doc_lock = e32.Ao_lock()
        ch = appuifw.Content_handler(doc_lock.signal)
        ch.open(certfile)
        doc_lock.wait()

    def show_log(self):
        logdir = logs_dir + "\\"
        
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

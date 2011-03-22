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
db_dir = "e:\\data\\cl2"
data_dir = "c:\\data\\cl2"
log_db_file = db_dir + "\\log.db"
config_db_file = data_dir + "\\config.db"
config_file = data_dir + "\\config.txt"

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

def do_nothing():
    pass

def choose_execute_action(strings, actions, title = u'Select action'):
    index = appuifw.popup_menu(strings, title)
    if index is not None:
        actions[index]()

class GUI:
    def __init__(self):
        self.lock = e32.Ao_lock()
        self.main_title = u"CL2 Launcher"
        self.old_title = appuifw.app.title
        appuifw.app.title = self.main_title

        app_path = os.path.split(appuifw.app.full_name())[0]
        self.app_drive = app_path[:2]

        # 30 seems to be the max number of menu items one can have.
        main_menu = [
            (u"Status overview", self.status_overview),
            
            (u"CL2 running?", self.show_is_cl2_running),
            (u"Start CL2", self.start_cl2_daemon),
            (u"Stop CL2", self.stop_cl2_daemon),
            (u"Kill CL2", self.kill_cl2_daemon),

            (u"Watchdog",
             ((u"WD running?", self.show_is_wd_running),
              (u"Start WD", self.start_wd_daemon),
              (u"Stop WD", self.stop_wd_daemon),
              (u"Kill WD", self.kill_wd_daemon),

              (u"WD autostart?", self.is_wd_enabled),
              (u"Disable WD autostart", self.disable_wd),
              (u"Enable WD autostart", self.enable_wd))),

	    (u"Upload now", self.upload_now),
	    (u"Remokon now", self.remokon_now),

            (u"Sensors",
             ((u"Select sensor", self.select_sensor),
              (u"Sensor operation...", self.operate_sensor))),

	    (u"Parameters",
             ((u"Select IAP", self.select_iap),
              (u"Set IAP name", self.set_iap_name),
              (u"Set upload frequency", self.select_upload_time),
              (u"Set BT scan interval", self.set_btprox_interval),
              (u"Set GPS scan interval", self.set_gps_interval))),

            (u"Misc",
             ((u"View log file", self.show_log),
              (u"Reboot device", self.reboot_device))),

            (u"Exit", self.abort)
            ]

        appuifw.app.menu = main_menu
        appuifw.app.exit_key_handler = self.abort

        self.sensor_list = None
	self.current_sensor = None
        self.app_info = None

	print "Welcome."
        b2s = lambda v: v and "available" or "not installed"
        print "Miso library: %s" % b2s(have_miso)
        print "CL2 library: %s" % b2s(have_cl2)

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
        if self.is_cl2_running():
            appuifw.note(u"CL2 already running", "info")
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
		return
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
        print "State: [uploads allowed: %s]" % self.daemon_query(""" if cl2.are_uploads_allowed() then return "yes" else return "no" end """)
        print self.daemon_query(""" do local lst = cl2.all_sensor_names(); local function get (n) if cl2.is_sensor_supported(n) then if cl2.is_sensor_running(n) then return "active" else return "inactive" end else return "unsupported" end end; local function map (f, lst) local nlst = {}; for i,v in ipairs(lst) do nlst[i] = f(v); end; return nlst; end; local function kjoin (lst) local s = ""; local first = true; local add = function (x) s = (s .. x) end; for i,x in ipairs(lst) do if first then first = false else add(" ") end; add(x); end; return s; end; local function g (n) return string.format("[%s: %s]", n, get(n)); end; return ("Sensors: " .. kjoin(map(g, lst))); end """)
        print ""

    def select_sensor(self):
        if self.sensor_list is None:
            res = self.daemon_query(""" do
local function kjoin (lst)
  local s = ""
  local first = true
  local add = function (x) s = (s .. x) end
  for i,x in ipairs(lst) do
    if first then first = false else add(",") end
    add(x)
  end
  return s
end
return kjoin(cl2.all_sensor_names())
end """)
            if res is None: return
            self.sensor_list = res.split(",")
        choices = [ unicode(x) for x in self.sensor_list ]
        index = appuifw.popup_menu(choices, u'Select sensor')
        if index is None:
            return
	self.current_sensor = str(choices[index])
        appuifw.note(u"Sensor %s selected" % self.current_sensor, "info")

    def upload_now(self):
        self.daemon_exec_ok(""" cl2.upload_now(); return "ok" """, u"Upload requested", u"Failed to request upload")

    def remokon_now(self):
        self.daemon_exec_ok(""" cl2.remokon_timed(5 * 60); return "ok" """, u"Remokon requested", u"Failed to request remokon")

    def operate_sensor(self):
	if self.current_sensor is None:
            self.select_sensor()
	if self.current_sensor is None:
            return
        pairs = [
            (u"Sensor supported?", self.sensor_is_supported),
            (u"Sensor running?", self.sensor_is_running),
            (u"Start sensor", self.sensor_start),
            (u"Stop sensor", self.sensor_stop),
            (u"Allow sensor", lambda: self.sensor_allow(True)),
            (u"Forbid sensor", lambda: self.sensor_allow(False))
            ]
        strings = [ x[0] for x in pairs ]
        actions = [ x[1] for x in pairs ]
        choose_execute_action(strings, actions,
                              u"%s:" % self.current_sensor.capitalize())

    def sensor_is_supported(self):
	if self.current_sensor is None:
	    appuifw.note(u"No sensor selected", "error")
	    return
	res = self.daemon_query(""" if cl2.is_sensor_supported("%s") then return "yes" else return "no" end """ % self.current_sensor)
	if res == "yes" or res == "no":
	    appuifw.note(u"Sensor %s is %ssupported" % (self.current_sensor, res == "no" and "not " or ""), "info")
	else:
	    appuifw.note(u"Query failure", "error")

    def sensor_is_running(self):
	if self.current_sensor is None:
	    appuifw.note(u"No sensor selected", "error")
	    return
	res = self.daemon_query(""" if cl2.is_sensor_running("%s") then return "yes" else return "no" end """ % self.current_sensor)
	#print res
	if res == "yes" or res == "no":
	    appuifw.note(u"Sensor %s is %srunning" % (self.current_sensor, res == "no" and "not " or ""), "info")
	else:
	    appuifw.note(u"Query failure", "error")
	
    def sensor_start(self):
	if self.current_sensor is None:
	    appuifw.note(u"No sensor selected", "error")
	    return
        self.daemon_exec_ok(""" cl2.sensor_start("%s"); return "ok" """ % (self.current_sensor), u"Sensor %s started" % (self.current_sensor), u"Failed to start sensor %s" % (self.current_sensor))
	
    def sensor_stop(self):
	if self.current_sensor is None:
	    appuifw.note(u"No sensor selected", "error")
	    return
        self.daemon_exec_ok(""" cl2.sensor_stop("%s"); return "ok" """ % (self.current_sensor), u"Sensor %s stopped" % (self.current_sensor), u"Failed to stop sensor %s" % (self.current_sensor))
	
    def sensor_allow(self, flag):
	if self.current_sensor is None:
	    appuifw.note(u"No sensor selected", "error")
	    return
        self.daemon_exec_ok(""" cl2.config_set("sensor.%s.autostart", "return %s"); return "ok" """ % (self.current_sensor, flag and "true" or "false"), u"Sensor %s %s" % (self.current_sensor, flag and "allowed" or "disallowed"), u"Failed to change configuration")
	    
    def select_upload_time(self):
        def esc(s):
            return "return \\\"%s\\\"" % s
        choices = [
            (u"Never", "never"),
            (u"Every night", "every day at 02:00"),
            (u"Every hour", "0 minutes past every hour"),
            (u"Every 10 mins", "0 minutes past every hour and 10 minutes past every hour and 20 minutes past every hour and 30 minutes past every hour and 40 minutes past every hour and 50 minutes past every hour"),
            ]
        strings = [ x[0] for x in choices ]
        index = appuifw.popup_menu(strings, u'Select action')
        if index is None:
            return
        value = esc(choices[index][1])
        self.daemon_exec_ok(""" cl2.config_set("uploader.time_expr", "%s"); return "ok" """ % value, "Upload time set OK", "Failed to set upload time")
    
    def select_iap(self):
        apid = socket.select_access_point()
        if apid is None:
            return
        self.daemon_exec_ok(""" cl2.config_set("iap", "return %d"); return "ok" """ % apid,
                            "IAP ID set to %d" % apid, "Failed to set IAP")

    def set_iap_name(self):
        new_value = appuifw.query(u"IAP name:", "text", u"Elisa Internet")
        if new_value is None:
            return
        self.daemon_exec_ok(""" cl2.config_set("iap", "return cl2.iap_id_by_name(\\"%s\\")"); return "ok" """ % new_value, "IAP set", "Failed to set IAP")
        
    def set_btprox_interval(self):
        new_value = appuifw.query(u"Interval (secs):", "number", 5 * 60)
        if new_value is None:
            return
        self.daemon_exec_ok(""" cl2.config_set("sensor.btprox.interval", "return %d"); return "ok" """ % int(new_value), "BT scan interval set", "Failed to set interval")
        
    def set_gps_interval(self):
        new_value = appuifw.query(u"Interval (secs):", "number", 5 * 60)
        if new_value is None:
            return
        self.daemon_exec_ok(""" cl2.config_set("sensor.gps.interval", "return %d"); return "ok" """ % int(new_value), "GPS scan interval set", "Failed to set interval")
        
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
        os.unlink(magic_file)
        appuifw.note(u"Autostart enabled", "info")

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

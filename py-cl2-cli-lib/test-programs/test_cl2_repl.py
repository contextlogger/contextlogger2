import cl2_client
import appuifw
import e32
import os
import sys
import traceback

def print_exception():
    traceback.print_exception(*sys.exc_info())

class GUI:
    def __init__(self):
        self.lock = e32.Ao_lock()
        self.main_title = u"CL2 Repl"
        self.old_title = appuifw.app.title

        app_path = os.path.split(appuifw.app.full_name())[0]
        self.app_drive = app_path[:2]

        main_menu = [
            (u"Eval", self.ask),
            (u"Exit", self.abort)
            ]

        appuifw.app.title = self.main_title
        appuifw.app.menu = main_menu
        appuifw.app.exit_key_handler = self.abort

        self.session = cl2_client.Session()
        self.code = u""

    def ask(self):
        evtext = appuifw.query(u"Code:", "text", self.code)
        if evtext is not None:
            self.code = evtext
            self.test(evtext)

    def test(self, expr):
        print(expr)
        try:
            print(repr(self.session.eval(expr)))
        except:
            print_exception()

    def abort(self):
        self.lock.signal()

    def loop(self):
        self.lock.wait()

    def close(self):
        self.session.close()
        appuifw.app.menu = []
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
    print "all done"


from cl2_client import *
session = Session()
#print(repr(dir(session)))

def test(expr, name):
    print name
    try:
        print(repr(session.eval(expr)))
    except ScriptError, e:
        print(e)

try:
    test(u"555", "syntax error")
    test(u"x = 555", "non-string result")
finally:
    session.close()
print "all done"

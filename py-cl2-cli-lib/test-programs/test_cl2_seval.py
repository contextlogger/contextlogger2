import cl2_client
session = cl2_client.Session()

def test(expr):
    print(expr)
    print(repr(session.eval(expr)))

try:
    test("do x = 555; return x end")
    test("do function f() local x = 555; return x end; return f() end")
    test("return type(os.time())")
    test("return os.time()")
    test("return math['pi']")
    test("return cl2['version']")
    test("return type(cl2.shutdown)")
finally:
    session.close()
print "all done"

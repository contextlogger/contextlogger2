import cl2_client
session = cl2_client.Session()

def test(expr):
    print(expr)
    print(repr(session.eval(expr)))

try:
    test("error(555)")
    #test(u"do function f() local function g() error(555) end; local s, e = pcall(g); return e end; return f() end")
finally:
    session.close()
print "all done"

import cl2_client
session = cl2_client.Session()

def test(expr):
    print(expr)
    print(repr(session.eval(expr)))

try:
    test("do cl2.shutdown(); return 'ok' end")
finally:
    session.close()
print "all done"

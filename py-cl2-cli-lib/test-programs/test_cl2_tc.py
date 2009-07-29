import _cl2_client
session = _cl2_client.Session()
print(repr(dir(session)))
try:
    print "tick count is %d" % session.tick_count()
finally:
    session.close()
print "all done"

import cl2_client
session = cl2_client.Session()
try:
    print "len is %d" % session.des16_len(u"four")
finally:
    session.close()
print "all done"

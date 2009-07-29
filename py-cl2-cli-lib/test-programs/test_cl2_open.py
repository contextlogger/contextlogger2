import cl2_client
# This will fail with KErrNotFound if the server is not running.
session = cl2_client.Session()
session.close()
print "all done"

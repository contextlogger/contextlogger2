BASENAME := py_cl2_client
VERSION := 1.01
CERT := dev

default : symbian

-include local/custom.mk

symbian :
	sake cert=$(CERT) logging=true udeb=true kits=s60_30

clean :
	-rm src/*.pyc doxyfile-int

cleaner : clean
	-rm -r build private-cxx-api python-api

release :
	sake all release kits=s60_30 cert=self
	sake all release kits=s60_30 cert=dev
	sake web

upload_dry :
	sake upload dry=true

upload :
	sake upload

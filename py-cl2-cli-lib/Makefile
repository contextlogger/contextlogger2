BASENAME := py_cl2_client
VERSION := 1.01
CERT := dev
SAKE := ../tools/bin/sake

default : symbian

-include local/custom.mk

symbian :
	$(SAKE) cert=$(CERT) logging=true udeb=true kits=s60_30

clean :
	-rm src/*.pyc doxyfile-int

cleaner : clean
	-rm -r build private-cxx-api python-api

release :
	$(SAKE) all release kits=s60_30 cert=self
	$(SAKE) all release kits=s60_30 cert=dev
	$(SAKE) web

upload_dry :
	$(SAKE) upload dry=true

upload :
	$(SAKE) upload

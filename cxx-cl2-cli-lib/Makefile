BASENAME := cl2cli
VERSION := 1.01
CERT := dev
SAKE := ../tools/bin/sake

default : symbian

-include local/custom.mk

symbian :
	$(SAKE) cert=$(CERT) logging=true udeb=true kits=s60_30

release :
	$(SAKE) all release kits=s60_30 cert=self
	$(SAKE) all release kits=s60_30 cert=dev
	$(SAKE) web

upload_dry :
	$(SAKE) upload dry=true

upload :
	$(SAKE) upload

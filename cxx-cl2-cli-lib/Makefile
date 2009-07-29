BASENAME := cl2cli
VERSION := 1.01
CERT := dev

default : symbian

-include local/custom.mk

symbian :
	sake cert=$(CERT) logging=true udeb=true kits=s60_30

release :
	sake all release kits=s60_30 cert=self
	sake all release kits=s60_30 cert=dev
	sake web

upload_dry :
	sake upload dry=true

upload :
	sake upload

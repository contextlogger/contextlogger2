BASENAME := keyevents
VERSION := 0.01
CERT := dev

symbian :
	sake --trace kits=s60_30 udeb=true cert=$(CERT)

makefiles :
	sake --trace kits=s60_30 udeb=true cert=$(CERT) makefiles

sis_ls :
	sake --trace kits=s60_30 udeb=true cert=$(CERT) sis_ls

sis_cert :
	sake --trace kits=s60_30 udeb=true cert=$(CERT) sis_cert

sis_struct :
	sake --trace kits=s60_30 udeb=true cert=$(CERT) sis_struct

-include local/custom.mk

changelog :
	darcs changes > doc/changelog.txt

release : changelog
	-rm -r build
	sake kits=s60_30 cert=self
	sake kits=s60_30 cert=dev

upload_dry :
	sake upload dry=true

upload :
	sake upload

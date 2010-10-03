BASENAME := keyevents
VERSION := 0.01
CERT := dev
SAKE := ../tools/bin/sake

default : symbian

symbian : symbian30 symbian31 symbian32 symbian50

symbian30 :
	$(SAKE) --trace kits=s60_30 udeb=true cert=$(CERT)

symbian31 :
	$(SAKE) --trace kits=s60_31 udeb=true cert=$(CERT)

symbian32 :
	$(SAKE) --trace kits=s60_32 udeb=true cert=$(CERT)

symbian50 :
	$(SAKE) --trace kits=s60_50 udeb=true cert=$(CERT)

makefiles :
	$(SAKE) --trace kits=s60_30 udeb=true cert=$(CERT) makefiles

sis_ls :
	$(SAKE) --trace kits=s60_30 udeb=true cert=$(CERT) sis_ls

sis_cert :
	$(SAKE) --trace kits=s60_30 udeb=true cert=$(CERT) sis_cert

sis_struct :
	$(SAKE) --trace kits=s60_30 udeb=true cert=$(CERT) sis_struct

-include local/custom.mk

changelog :
	darcs changes > doc/changelog.txt

release : changelog
	-rm -r build
	$(SAKE) kits=s60_30 cert=self
	$(SAKE) kits=s60_30 cert=dev

upload_dry :
	$(SAKE) upload dry=true

upload :
	$(SAKE) upload

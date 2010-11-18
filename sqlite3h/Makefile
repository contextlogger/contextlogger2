BASENAME := sqlite3h
VERSION := 0.01
CERT := dev
SAKE := ../tools/bin/sake

default : symbian

symbian : symbian30 symbian31 symbian32 symbian50 symbian52

# Presently building a DLL is not an option, unless one has the
# expensive ARM compiler, or tweaks lots of Symbian headers.
# Hence the "static" option here.
symbian30 :
	$(SAKE) --trace static=true kits=s60_30 udeb=true cert=$(CERT)

symbian31 :
	$(SAKE) --trace static=true kits=s60_31 udeb=true cert=$(CERT)

symbian32 :
	$(SAKE) --trace static=true kits=s60_32 udeb=true cert=$(CERT)

symbian50 :
	$(SAKE) --trace static=true kits=s60_50 udeb=true cert=$(CERT)

symbian52 :
	$(SAKE) --trace static=true kits=s60_52 udeb=true cert=$(CERT)

symbian_rel :
	$(SAKE) --trace static=true kits=s60_30 cert=$(CERT)

changelog :
	darcs changes > doc/changelog.txt

release : changelog
	-rm -r build
	$(SAKE) kits=s60_30 static=true cert=self
	$(SAKE) kits=s60_30 static=true cert=dev

upload_dry :
	$(SAKE) upload dry=true

upload :
	$(SAKE) upload

test :
	make -f unix.mk test

unix :
	make -f unix.mk main

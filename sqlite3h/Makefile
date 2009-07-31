BASENAME := sqlite3h
VERSION := 0.01
CERT := dev
SAKE := ../tools/bin/sake

# Presently building a DLL is not an option, unless one has the
# expensive ARM compiler, or tweaks lots of Symbian headers.
# Hence the "static" option here.
symbian :
	$(SAKE) --trace static=true kits=s60_30 udeb=true logging=true cert=$(CERT)

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

BASENAME := sqlite3h
VERSION := 0.01
CERT := dev

# Presently building a DLL is not an option, unless one has the
# expensive ARM compiler, or tweaks lots of Symbian headers.
# Hence the "static" option here.
symbian :
	sake --trace static=true kits=s60_30 udeb=true logging=true cert=$(CERT)

changelog :
	darcs changes > doc/changelog.txt

release : changelog
	-rm -r build
	sake kits=s60_30 static=true cert=self
	sake kits=s60_30 static=true cert=dev

upload_dry :
	sake upload dry=true

upload :
	sake upload

test :
	make -f unix.mk test

unix :
	make -f unix.mk main

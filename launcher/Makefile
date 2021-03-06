CERT := dev
DEVICE := default

BASENAME := cl2launcher
SHORTCAPTION := CL2Launch,CL2Launch
LONGCAPTION := CL2 Launcher,CL2 Launcher
UID := e8460007

VERSION := 0.51

# Note: We are using a modified version of ensymble, hacked to include
#       an --unsigned option. An alternative would be to let ensymble
#       sign the package with its built-in cert, and then use signsis -u
#       to unsign.
ENSYMBLE := ../tools/ensymble-0.29/ensymble.py
SIGNSPEC := `../tools/bin/symbian-config-ensymble-cert -eE -c $(CERT)`

ifeq ($(CERT),unsigned)
DIST_EXT := sis
DIST_CERT := dev
BUILT_SIS := $(BASENAME)_$(CERT).sis
SIGNSPEC := $(SIGNSPEC) --unsigned
else
DIST_EXT := sisx
DIST_CERT := $(CERT)
BUILT_SIS := $(BASENAME)_$(CERT).sisx
endif

APPOPT := --uid=0x$(UID) --heapsize=4k,4M --appname=$(BASENAME) --version=$(VERSION) --lang=EN,FI --icon=icon.svgz "--shortcaption=$(SHORTCAPTION)" "--caption=$(LONGCAPTION)" "--vendor=HIIT,HIIT" --encoding=utf8,ascii $(SIGNSPEC) --verbose --extrasdir=extras

ifeq ($(CERT),unsigned)
MOVE :=
else
MOVE := mv $(BASENAME)_$(CERT).sis $(BUILT_SIS)
endif

DIST_NAME := $(BASENAME)-$(VERSION)-s60_30_py1_$(DIST_CERT).$(DIST_EXT)
DIST_HOME := ../download

ifeq ($(CERT),dev)
DIST_COPY :=
else
DIST_COPY := cp $(BUILT_SIS) $(DIST_HOME)/$(BASENAME)-$(VERSION)-s60_30_py1_$(DIST_CERT).$(DIST_EXT)
endif

# For CA cert installation support, create this file.
CA_FILE := cl2-ca-cert.der
CERT_FILES := $(wildcard $(CA_FILE))
ifeq ($(CERT_FILES),)
CERT_COPY := @echo no certificate to package
else
CERT_COPY := cp --dereference $(CA_FILE) myfiles/extras/data/cl2/
endif

default : sis

# Note that ensymble does not automatically include the backup XML file for the app, and so we must add it.
sis :
	-rm -r myfiles
	mkdir -p myfiles/extras/python
	mkdir -p myfiles/extras/data/cl2
	mkdir -p myfiles/extras/private/$(UID)
	cp default.py myfiles/
	cp $(BASENAME).py myfiles/extras/python/
	cp cl2wizard.py myfiles/extras/python/
	cp backup_registration.xml myfiles/extras/private/$(UID)/
	$(CERT_COPY)
	$(ENSYMBLE) py2sis $(APPOPT) myfiles $(BASENAME)_$(CERT).sis
	$(MOVE)

dist : sis
	mkdir -p $(DIST_HOME)
	$(DIST_COPY)

release :
	$(MAKE) dist CERT=self
	$(MAKE) dist CERT=unsigned

# For updating the actual script without reinstalling the SIS file.
updatel :
	putfile -d $(DEVICE) $(BASENAME).py

updatew :
	putfile -d $(DEVICE) cl2wizard.py

update : updatel updatew

push :
	obex -d $(DEVICE) $(BASENAME)_$(CERT).sisx

install :
	putfile --mkdir -d $(DEVICE) --dir e:/auto_install $(BASENAME)_$(CERT).sisx

all : sis update install

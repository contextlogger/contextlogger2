CERT := dev
DEVICE := default

BASENAME := cl2launcher
SHORTCAPTION := CL2Launch,CL2Launch
LONGCAPTION := CL2 Launcher,CL2 Launcher

VERSION := 0.51

# Note: We are using a modified version of ensymble, hacked not to self-sign by default.
#       See tools/ensymble/ for a patch.
#
# Note: We are using a custom tool here to get the certificate paths and such.
#       Such a tool is site-specific in nature, and not included in the distribution.
#       You can either implement a script with the same interface, or pass 
#       a suitable SIGNSPEC to "make" when invoking it.
ENSYMBLE := ensymble
SIGNSPEC := `symbian-config -eE $(CERT)`

DIST_HOME := ../download

ifeq ($(CERT),unsigned)
DIST_EXT := sis
DIST_CERT := dev
BUILT_SIS := $(BASENAME)_$(CERT).sis
else
DIST_EXT := sisx
DIST_CERT := $(CERT)
BUILT_SIS := $(BASENAME)_$(CERT).sisx
endif

DIST_NAME := $(BASENAME)-$(VERSION)-s60_30_py1_$(DIST_CERT).$(DIST_EXT)

APPOPT := --uid=0xe8460007 --heapsize=4k,4M --appname=$(BASENAME) --version=$(VERSION) --lang=EN,FI --icon=icon.svgz "--shortcaption=$(SHORTCAPTION)" "--caption=$(LONGCAPTION)" "--vendor=HIIT,HIIT" --encoding=utf8,ascii $(SIGNSPEC) --verbose --extrasdir=extras

ifeq ($(CERT),unsigned)
MOVE :=
else
MOVE := mv $(BASENAME)_$(CERT).sis $(BUILT_SIS)
endif

ifeq ($(CERT),dev)
DIST_COPY :=
else
DIST_COPY := cp $(BUILT_SIS) $(DIST_HOME)/$(BASENAME)-$(VERSION)-s60_30_py1_$(DIST_CERT).$(DIST_EXT)
endif

default : sis

sis :
	-rm -r myfiles
	mkdir -p myfiles/extras/python
	mkdir -p myfiles/extras/data/cl2
	cp default.py myfiles/
	cp $(BASENAME).py myfiles/extras/python/
	cp cl2wizard.py myfiles/extras/python/
	cp cl2-ca-cert.der myfiles/extras/data/cl2/
	$(ENSYMBLE) py2sis $(APPOPT) myfiles $(BASENAME)_$(CERT).sis
	$(MOVE)

dist : sis
	mkdir -p $(DIST_HOME)
	$(DIST_COPY)

release :
	$(MAKE) dist CERT=self
	$(MAKE) dist CERT=unsigned

# For updating the actual script without reinstalling the SIS file.
update :
	putfile -d $(DEVICE) $(BASENAME).py

push :
	obex -d $(DEVICE) $(BASENAME)_$(CERT).sisx

install :
	putfile -d $(DEVICE) --dir e:/auto_install $(BASENAME)_$(CERT).sisx

all : sis update install

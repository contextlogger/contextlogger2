CERT := dev
DEVICE := default

VERSION := 1.0.0

ifeq ($(CERT),self)
CAPSPEC := --caps=ReadUserData+WriteUserData+NetworkServices+LocalServices+UserEnvironment
else
CAPSPEC := --caps=LocalServices+NetworkServices+ReadUserData+UserEnvironment+WriteUserData+Location+PowerMgmt+ProtServ+ReadDeviceData+SurroundingsDD+SwEvent+TrustedUI+WriteDeviceData
endif

ifeq ($(CERT),unsigned)
SIGNSPEC := $(CAPSPEC)
else
SIGNSPEC := `symbian-config -e $(CERT)` $(CAPSPEC)
endif

BASENAME := cl2launcher
SHORTCAPTION := CL2Launch,CL2Launch
LONGCAPTION := CL2 Launcher,CL2 Launcher
APPOPT := --uid=0xe8460007 --heapsize=4k,4M --appname=$(BASENAME) --version=$(VERSION) --lang=EN,FI --icon=icon.svgz "--shortcaption=$(SHORTCAPTION)" "--caption=$(LONGCAPTION)" "--vendor=HIIT,HIIT" --encoding=utf8,ascii $(SIGNSPEC) --verbose default.py

default : sis

sis :
	ensymble py2sis $(APPOPT) $(BASENAME)_$(CERT).sis
	mv $(BASENAME)_$(CERT).sis $(BASENAME)_$(CERT).sisx

update :
	putfile -d $(DEVICE) $(BASENAME).py

push :
	obex -d $(DEVICE) $(BASENAME)_$(CERT).sisx

install :
	putfile -d $(DEVICE) --dir e:/auto_install $(BASENAME)_$(CERT).sisx

all : sis update install

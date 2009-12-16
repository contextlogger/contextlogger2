# This Makefile is unfortunately very much specific to a particular non-standard
# build environment, but it should be easy enough to build this software without
# this makefile.
#
# The "build" directory contains the usual bld.inf and .mmp and .pkg files.
# Before invoking the standard Symbian build tools on those, you'll just have
# to first compile the .rss file to produce the autostart.rsc file, which
# then goes into the SIS file. And note that you'll have to sign with proper
# permissions for boot-time starting to work.
#
# Building has been tested to work with the S60 3rd Edition MR SDK.

BASENAME := cl2watchdog
KIT := s60_30
SDKHOME := $(GNUPOCDEVENV)/kits/$(KIT)
CERT := dev
VERSION := 0.51

PLT_ENV := PLTCOLLECTS=`pwd`/../tools/schemelib:
SAKE := ../tools/bin/sake

DIST_HOME := ../download

ifeq ($(CERT),dev)
DIST_EXT := sis
else
DIST_EXT := sisx
endif

BUILT_SIS := build/$(KIT)_udeb_$(CERT)/$(BASENAME).$(DIST_EXT)
DIST_COPY := cp $(BUILT_SIS) $(DIST_HOME)/$(BASENAME)-$(VERSION)-$(KIT)_$(CERT).$(DIST_EXT)

default : symbian

symbian : codegen
	sake kits=s60_30 udeb=true logging=true cert=$(CERT)

symbian_all : autostart.rsc symbian

autostart.rsc : autostart.rss
	in-gnupoc-env s60_30 perl -S epocrc.pl -m045,046,047 -I $(SDKHOME)/epoc32/include -I $(SDKHOME)/epoc32/include/variant -DLANGUAGE_SC -u autostart.rss -o

epocrc_options :
	in-gnupoc-env s60_30 perl -S epocrc.pl   # shows options

codegen : src/process_handle_observer.h src/timer_observer.h src/watchdog.h

%.h %.cpp : %.scm
	$(PLT_ENV) $<

.PRECIOUS : %.h
.PRECIOUS : %.cpp

dist : symbian
	mkdir -p $(DIST_HOME)
	$(DIST_COPY)

release :
	$(MAKE) dist CERT=dev

clean :
	-rm .sake_op *~

# Any host-specific rules.
-include local/custom.mk

changelog :
	darcs changes > doc/changelog.txt

upload_dry :
	$(SAKE) upload dry=true

upload :
	$(SAKE) upload

#
# Copyright 2009 Helsinki Institute for Information Technology (HIIT)
# and the authors. All rights reserved.
#
# Authors: Tero Hasu <tero.hasu@hut.fi>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

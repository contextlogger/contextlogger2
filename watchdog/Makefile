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

SDKHOME := $(GNUPOCDEVENV)/kits/s60_30

PLT_ENV := PLTCOLLECTS=`pwd`/../tools/schemelib:

default : codegen symbian

autostart.rsc : autostart.rss
	in-gnupoc-env s60_30 perl -S epocrc.pl -m045,046,047 -I $(SDKHOME)/epoc32/include -I $(SDKHOME)/epoc32/include/variant -DLANGUAGE_SC -u autostart.rss -o

epocrc_options :
	in-gnupoc-env s60_30 perl -S epocrc.pl   # shows options

codegen : src/process_handle_observer.h src/timer_observer.h src/watchdog.h

%.h %.cpp : %.scm
	$(PLT_ENV) $<

.PRECIOUS : %.h
.PRECIOUS : %.cpp

clean :
	-rm .sake_op *~

# Any host-specific rules.
-include local/custom.mk

changelog :
	darcs changes > doc/changelog.txt

upload_dry :
	sake upload dry=true

upload :
	sake upload


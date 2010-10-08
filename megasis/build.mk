SAKE := ../tools/bin/sake

CURRENT_CONFIG := ../daemon/src/current_config.mk

include $(CURRENT_CONFIG)

# Note that the libs-s60 directory is not a part of the distribution.
# It should contain all the embedded third-party SIS files for the
# megaSIS. It must also contain a makefile with a "build" rule, which
# can do anything you like in order to make the files ready for
# packaging in the megaSIS. We use the rule (among other things) for
# re-signing the embedded SIS files with the same cert as we use for
# the megaSIS itself.
external :
	cd ../libs-s60 && $(MAKE) build

# Currently we only do UDEB builds, and so we do not require UREL
# builds of static libs yet either.
static_libs :
	cd ../sqlite3h && $(SAKE) static=true kits=$(KIT_NAME) udeb=true

s60_30_self30 :
	cd ../daemon && $(MAKE)
	cd ../cxx-cl2-cli-lib && $(SAKE) cert=self30 kits=s60_30
	cd ../py-cl2-cli-lib && $(SAKE) cert=self30 kits=s60_30
	cd ../launcher && $(MAKE) CERT=self30 sis
	cd ../epocxplat && $(MAKE) CERT=self30 cl2

s60_30_dev_sub :
	cd ../daemon && $(MAKE)
	cd ../cxx-cl2-cli-lib && $(SAKE) cert=dev kits=s60_30
	cd ../py-cl2-cli-lib && $(SAKE) cert=dev kits=s60_30
	cd ../keyevents && $(SAKE) cert=dev kits=s60_30
	cd ../watchdog && $(MAKE)
	cd ../epocxplat && $(MAKE) CERT=dev cl2

s60_30_dev : s60_30_dev_sub
	cd ../launcher && $(MAKE) CERT=dev sis

s60_30_unsigned : s60_30_dev_sub
	cd ../launcher && $(MAKE) CERT=unsigned sis

s60_30_self32 :
	cd ../daemon && $(MAKE)
	cd ../cxx-cl2-cli-lib && $(SAKE) cert=self32 kits=s60_30
	cd ../py-cl2-cli-lib && $(SAKE) cert=self32 kits=s60_30
	cd ../launcher && $(MAKE) CERT=self32 sis
	cd ../epocxplat && $(MAKE) CERT=self32 cl2

hpe non-use : s60_30_dev

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

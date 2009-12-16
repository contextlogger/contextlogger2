CURRENT_CONFIG := ../daemon/src/current_config.mk

include $(CURRENT_CONFIG)

# Currently we only do UDEB builds, and so we do not require UREL builds
# of static libs yet either.
static_libs :
	cd ../sqlite3h && sake static=true kits=$(KIT_NAME) udeb=true cert=dev

s60_30_self30 :
	cd ../daemon && make
	cd ../cxx-cl2-cli-lib && sake cert=self30 kits=s60_30
	cd ../py-cl2-cli-lib && sake cert=self30 kits=s60_30
	cd ../launcher && make CERT=self30 sis

s60_30_dev_sub :
	cd ../daemon && make
	cd ../cxx-cl2-cli-lib && sake cert=dev kits=s60_30
	cd ../py-cl2-cli-lib && sake cert=dev kits=s60_30
	cd ../keyevents && sake cert=dev kits=s60_30
	cd ../watchdog && make

s60_30_dev : s60_30_dev_sub
	cd ../launcher && make CERT=dev sis

s60_30_unsigned : s60_30_dev_sub
	cd ../launcher && make CERT=unsigned sis

s60_30_self32 :
	cd ../daemon && make
	cd ../cxx-cl2-cli-lib && sake cert=self32 kits=s60_30
	cd ../py-cl2-cli-lib && sake cert=self32 kits=s60_30
	cd ../launcher && make CERT=self32 sis

non-use : s60_30_dev

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

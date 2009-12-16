CURRENT_CONFIG := ../daemon/src/current_config.mk

include $(CURRENT_CONFIG)

DIST_HOME := ../download

release :
	mkdir -p $(DIST_HOME)
	$(MAKE) s60_30_self30 s60_30_self32 s60_30_unsigned
	cp cl2_megasis-$(VERSION_STRING)-s60_30_self30.sisx $(DIST_HOME)/
	cp cl2_megasis-$(VERSION_STRING)-s60_30_self32.sisx $(DIST_HOME)/
	cp cl2_megasis-$(VERSION_STRING)-s60_30_dev.sis $(DIST_HOME)/

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

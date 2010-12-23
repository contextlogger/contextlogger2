CURRENT_CONFIG := ../daemon/src/current_config.mk

include $(CURRENT_CONFIG)

PKG_SCRIPT := create-megasis.rb
BASENAME := cl2_megasis
TEMPLATE := template.pkg.in

# For internal trials, include non-releasable SIS files as well. If
# the SIS is unsigned, unpack the contents, as signing embedded SIS
# files is not really possible anyway. An alternative would be to
# supply the endividual unsigned files and the PKG file, which might
# actually be the more sensible option.
FLAGS := $(and $(IS_TRIAL), --nr)
# unpacking no longer supported $(and $(NOT__SIGNED), --unpack)

# Note that $(DIST_VARIANT_NAME) is defined only for release variants.
PKGFILE := $(BASENAME)-$(DIST_VARIANT_NAME).pkg
SISFILE := $(BASENAME)-$(VERSION_STRING)-$(DIST_VARIANT_NAME).sis
SISXFILE := $(SISFILE)x

default : clean $(PKGFILE) $(if $(SIGNED), $(SISXFILE), $(SISFILE))

$(PKGFILE) : $(TEMPLATE) $(PKG_SCRIPT) $(CURRENT_CONFIG)
	ruby $(PKG_SCRIPT) $(FLAGS) -o $(PKGFILE) $(TEMPLATE)

# Best to do this even if PKG has not changed; signing key may have
# changed.
clean : 
	-rm $(PKGFILE) $(SISFILE) $(SISXFILE)

$(SISXFILE) :
	ruby do-sis-signing.rb --kit $(KIT_NAME) --makesis --signsis --cert $(CERT_NAME) -o $@ -i $(PKGFILE)

$(SISFILE) :
	ruby do-sis-signing.rb --kit $(KIT_NAME) --makesis -o $@ -i $(PKGFILE)

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

BASENAME := epocxplat # nickname
VERSION := 0.01 # decimal number with two-digit fraction
KIT := s60_30 # nickname
CAPA := devcert # nickname, or one of self, self32, devcert, publisher, manufacturer
CAPS := # list of capabilities
DEBUG := true # true or nothing
DEVICE := default # nickname

## For packaging only.
CERT := dev # nickname
SISVER := 2 # 1 or 2
OWNERUID := # 8-digit hex number 
SIGN := true # true or nothing

SELF_CAPS := LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData
SELF32_CAPS := $(SELF_CAPS) Location
DEVCERT_CAPS := $(SELF32_CAPS) PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData
PUBLISHER_CAPS := $(DEVCERT_CAPS) CommDD DiskAdmin NetworkControl MultimediaDD
MANUFACTURER_CAPS := $(PUBLISHER_CAPS) AllFiles DRM TCB

ifeq ($(CAPA),self)
  CAPS := $(SELF_CAPS)
else ifeq ($(CAPA),self32)
  CAPS := $(SELF32_CAPS)
else ifeq ($(CAPA),devcert)
  CAPS := $(DEVCERT_CAPS)
else ifeq ($(CAPA),publisher)
  CAPS := $(PUBLISHER_CAPS)
else ifeq ($(CAPA),manufacturer)
  CAPS := $(MANUFACTURER_CAPS)
endif

REL := $(if $(DEBUG),udeb,urel)

DLL_VARIANT := $(KIT)_$(REL)_$(CAPA)

PKGIN_FILE := src/module.pkg.in
PKG_FILE := sis/$(BASENAME)_$(REL)_$(CAPA).pkg
SIS_FILE := sis/$(BASENAME)_$(REL)_$(CAPA).sis
SISX_FILE := sis/$(BASENAME)_$(REL)_$(CAPA)_$(CERT).sisx

SAKE_COMMAND := sake --trace handle=$(DLL_VARIANT) kits=$(KIT) rel=$(REL) "caps=$(CAPS)" version=$(VERSION)

default : dll

-include local.mk

dll :
	$(SAKE_COMMAND)

define sake_template
$(1) :
	$(SAKE_COMMAND) $(1)
endef
SAKE_RULES := build_info
$(foreach name,$(SAKE_RULES),$(eval $(call sake_template,$(name))))

# Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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

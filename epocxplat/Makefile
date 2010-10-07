# string
PROJECT_NAME := epocxplat Library
# nickname
BASENAME := epocxplat
# decimal number with two-digit fraction
VERSION := 0.01
# nickname
KIT := s60_30
# nickname, or one of self, self32, devcert, publisher, manufacturer
CAPA := devcert
# list of capabilities
CAPS :=
# true or nothing
DEBUG := true
# nickname
DEVICE := default

## For packaging only.
# nickname
CERT := dev
# 1 or 2
SISVER := 2
# 8-digit hex number 
OWNERUID :=
# true or nothing
SIGN := true

SELF_CAPS := LocalServices NetworkServices ReadUserData UserEnvironment WriteUserData
SELF32_CAPS := $(SELF_CAPS) Location
DEVCERT_CAPS := $(SELF32_CAPS) PowerMgmt ProtServ ReadDeviceData SurroundingsDD SwEvent TrustedUI WriteDeviceData
PUBLISHER_CAPS := $(DEVCERT_CAPS) CommDD DiskAdmin NetworkControl MultimediaDD
MANUFACTURER_CAPS := $(PUBLISHER_CAPS) AllFiles DRM TCB

ifeq ($(CAPA),self)
  CAPLIST := $(SELF_CAPS)
else ifeq ($(CAPA),self32)
  CAPLIST := $(SELF32_CAPS)
else ifeq ($(CAPA),devcert)
  CAPLIST := $(DEVCERT_CAPS)
else ifeq ($(CAPA),publisher)
  CAPLIST := $(PUBLISHER_CAPS)
else ifeq ($(CAPA),manufacturer)
  CAPLIST := $(MANUFACTURER_CAPS)
else
  CAPLIST := $(or $(CAPS),none)
endif

REL := $(if $(DEBUG),udeb,urel)

DLL_VARIANT := $(KIT)_$(REL)_$(CAPA)

PKGIN_FILE := src/module.pkg.in
PKG_FILE := sis/$(BASENAME)_$(REL)_$(CAPA).pkg
SIS_FILE := sis/$(BASENAME)_$(REL)_$(CAPA).sis
SISX_FILE := sis/$(BASENAME)_$(REL)_$(CAPA)_$(CERT).sisx

SAKE_COMMAND := sake --trace handle=$(DLL_VARIANT) kits=$(KIT) rel=$(REL) "caps=$(CAPLIST)" version=$(VERSION) "projname=$(PROJECT_NAME)"

default : dll

-include local.mk

caps :
	@echo $(CAPA): $(CAPLIST)

dll :
	$(SAKE_COMMAND)

define sake_template
$(1) :
	$(SAKE_COMMAND) $(1)
endef
SAKE_RULES := build_info makefiles
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

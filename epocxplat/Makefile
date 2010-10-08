# string
PROJECT_NAME := epocxplat Library
# nickname
BASENAME := epocxplat
# decimal number with two-digit fraction
VERSION := 0.01
# 8-digit hex number 
UID_V8 := 0846000d
# 8-digit hex number 
UID_V9 := e846000d

# nickname
KIT := s60_30
# nickname, or one of self, self32, devcert, publisher, manufacturer
CAPA := devcert
# list of capabilities
CAPS :=
# true or nothing
DEBUG := true

## For packaging only.
# er3, er6, or er9
SISFMT := er9
# 8-digit hex number 
OWNER_UID :=
# true or nothing
SIGN := true
# nickname
CERT := dev

## For file transfer only.
# nickname
DEVICE := default

DLL_PASS := CAPA=$(CAPA) CAPS=$(CAPS) DEBUG=$(DEBUG)
SIS_PASS := OWNER_UID=$(OWNER_UID) SIGN=$(SIGN) CERT=$(CERT)

# er3 = ER 3, 4, 5 (SIS UID 0x1000006d) unsupported
# er6 = ER 6, 7, 8 (SIS UID 0x10003a12)   supported
# er9 = ER 9       (SIS UID 0x10201a7a)   supported
ifeq ($(SISFMT),er6)
  UID3 := $(UID_V8)
  SIGN :=
else ifeq ($(SISFMT),er9)
  UID3 := $(UID_V9)
else
  $(error unsupported SIS format $(SISFMT))
endif

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
SIS_DIR := packages
ifeq ($(SISFMT),er6)
PKG_FILE := $(SIS_DIR)/$(BASENAME)_2nd_$(REL).pkg
SIS_FILE := $(SIS_DIR)/$(BASENAME)_2nd_$(REL).sis
else
PKG_FILE := $(SIS_DIR)/$(BASENAME)_3rd_$(REL)_$(CAPA).pkg
SIS_FILE := $(SIS_DIR)/$(BASENAME)_3rd_$(REL)_$(CAPA).sis
SISX_FILE := $(SIS_DIR)/$(BASENAME)_3rd_$(REL)_$(CAPA)_$(CERT).sisx
endif

SAKE_EXE := sake
#SAKE_EXE := ../tools/bin/sake
SAKE_COMMAND := $(SAKE_EXE) --trace handle=$(DLL_VARIANT) kits=$(KIT) rel=$(REL) "caps=$(CAPLIST)" version=$(VERSION) "projname=$(PROJECT_NAME)" uid_v8=0x$(UID_V8) uid_v9=0x$(UID_V9)

default : dll

-include local.mk

caps :
	@echo $(CAPA): $(CAPLIST)

dll :
	$(SAKE_COMMAND)
	mkdir -p $(SIS_DIR)
	cp build/$(DLL_VARIANT)/$(BASENAME).dll $(SIS_DIR)/$(BASENAME)_$(DLL_VARIANT).dll

# xxx rule for freezing exports, and mmp adjustment

SIS_VERSION := $(VERSION)
OWNER_UID_SFX := $(and $(OWNER_UID),_$(OWNER_UID))
RUBY_VARLIST := BASENAME OWNER_UID_SFX PROJECT_NAME REL SISFMT UID3 SIS_VERSION
RUBY_EXPORTS := $(foreach name,$(RUBY_VARLIST),$(name) = "$($(name))";)

pkg :
	mkdir -p $(dir $(PKG_FILE))
	ruby -e '$(RUBY_EXPORTS) require "erb"; puts(ERB.new(File.read("$(PKGIN_FILE)")).result(binding()))' > $(PKG_FILE)

SISTOOL := ../megasis/do-sis-signing.rb -k $(KIT)

all : 3rd-sis

3rd-sis :
	$(MAKE) dll KIT=s60_30 $(DLL_PASS)
	$(MAKE) dll KIT=s60_31 $(DLL_PASS)
	$(MAKE) dll KIT=s60_32 $(DLL_PASS)
	$(MAKE) dll KIT=s60_50 $(DLL_PASS)
	$(MAKE) sis SISFMT=er9 $(SIS_PASS)

sis : $(if $(SIGN),signed-sis,unsigned-sis)

unsupported-txt :
	mkdir -p $(SIS_DIR)
	echo Unsupported platform. No DLL installed. Please abort installation. > $(SIS_DIR)/unsupported.txt

unsigned-sis : pkg unsupported-txt
	mkdir -p $(SIS_DIR)
	(cd $(SIS_DIR) && $(SISTOOL) --makesis -o $(SIS_FILE) -i $(PKG_FILE))

signed-sis : pkg unsupported-txt
	mkdir -p $(SIS_DIR)
	(cd $(SIS_DIR) && $(SISTOOL) --makesis --signsis --cert $(CERT) -o $(SISX_FILE) -i $(PKG_FILE))

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

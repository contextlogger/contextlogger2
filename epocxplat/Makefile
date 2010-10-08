## Fixed values for this project.
# nickname
export BASENAME := epocxplat
# decimal number with two-digit fraction
export VERSION := 0.01

## Defaults for standalone packages, override for others.
# string
export PROJECT_NAME := epocxplat Library
# nickname of owning application
export OWNER :=
# If putting in the same SIS file as owning app, can use the app UID.
# 8-digit hex number
export UID3 :=

## Build specific information.
# nickname
export KIT :=
# nickname, or one of self, self32, devcert, publisher, manufacturer
export CAPA := devcert
# list of capabilities
export CAPS :=
# true or nothing
export DEBUG := true
# EPOC Release, either 8 (or lower) or 9 (or higher)
export ER :=

## For packaging only.
# true or nothing
export SIGN := true
# nickname
export CERT := dev

## For file transfer only.
# nickname
export DEVICE := default

# Set default values if not provided.
# As a rule of thumb, we should not infer conflicting values.
ifeq ($(strip $(or $(ER),$(KIT))),)
  ER := 9
  KIT := s60_30
else 
  ifeq ($(KIT),)
    ifeq ($(ER),8)
      KIT := s60_20
    else ifeq ($(ER),9)
      KIT := s60_30
    else
      $(error unsupported value for ER $(ER))
    endif
  else ifeq ($(ER),)
    ifeq ($(KIT),s60_12)
      ER := 8
    else ifeq ($(KIT),s60_20)
      ER := 8
    else ifeq ($(KIT),s60_21)
      ER := 8
    else ifeq ($(KIT),s60_26)
      ER := 8
    else ifeq ($(KIT),s60_28)
      ER := 8
    else
      ER := 9
    endif
  endif
endif

ifeq ($(ER),9)
  IS_V9 := true
else
  IS_V8 := true
  SIGN := false
endif

ifeq ($(UID3),)
  # 8-digit hex number
  UID_V8 := 0846000d
  # 8-digit hex number
  UID_V9 := e846000d
  UID3 := $(if $(IS_V9),$(UID_V9),$(UID_V8))
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

OWNERSFX := $(and $(OWNER),_$(OWNER))

CAPASFX := $(and $(IS_V9),_$(CAPA))
VARPFX := $(BASENAME)$(OWNERSFX)
VARSFX := $(REL)$(CAPASFX)
DLL_VARIANT := $(VARPFX)_$(KIT)_$(VARSFX)
DLL_FN_FMT := $(VARPFX)_%s_$(VARSFX).dll

BIN_BASENAME := $(BASENAME)_$(UID3)
BIN_FN := $(BIN_BASENAME).dll

PKGIN_FILE := src/module.pkg.in
SIS_DIR := packages
ifeq ($(IS_V9),)
SIS_BN := $(BASENAME)$(OWNERSFX)_2nd_$(REL)
PKG_FILE := $(SIS_BN).pkg
SIS_FILE := $(SIS_BN).sis
else
SIS_BN := $(BASENAME)$(OWNERSFX)_3rd_$(REL)
PKG_FILE := $(SIS_BN)_$(CAPA).pkg
SIS_FILE := $(SIS_BN)_$(CAPA).sis
SISX_FILE := $(SIS_BN)_$(CAPA)_$(CERT).sisx
endif

SAKE_EXE := sake
#SAKE_EXE := ../tools/bin/sake
SAKE_COMMAND := $(SAKE_EXE) --trace handle=$(DLL_VARIANT) kits=$(KIT) rel=$(REL) "caps=$(CAPLIST)" version=$(VERSION) "projname=$(PROJECT_NAME)" uid=0x$(UID3) bin_basename=$(BIN_BASENAME)

default : dll

-include local.mk

caps :
	@echo $(CAPA): $(CAPLIST)

FREEZE_COMMAND := ruby $(CURDIR)/../tools/bin/in-kit-env.rb -k $(KIT) abld freeze gcce

freeze :
	(cd build/$(DLL_VARIANT) && $(FREEZE_COMMAND))

dll :
	$(SAKE_COMMAND)
	mkdir -p $(SIS_DIR)
	cp build/$(DLL_VARIANT)/$(BIN_FN) $(SIS_DIR)/$(DLL_VARIANT).dll

define sake_template
$(1) :
	$(SAKE_COMMAND) $(1)
endef
SAKE_RULES := build_info makefiles
$(foreach name,$(SAKE_RULES),$(eval $(call sake_template,$(name))))

# er3, er6, or er9
# er3 = ER 3, 4, 5 (SIS UID 0x1000006d) unsupported
# er6 = ER 6, 7, 8 (SIS UID 0x10003a12)   supported
# er9 = ER 9       (SIS UID 0x10201a7a)   supported
SISFMT := $(if $(IS_V9),er9,er6)

SIS_VERSION := $(VERSION)
PKG_VARLIST := BASENAME BIN_FN DLL_FN_FMT PROJECT_NAME SISFMT UID3 SIS_VERSION
PKG_EXPORTS := $(foreach name,$(PKG_VARLIST),$(name) = "$($(name))";)

pkg :
	mkdir -p $(SIS_DIR)
	ruby -e '$(PKG_EXPORTS) require "erb"; puts(ERB.new(File.read("$(PKGIN_FILE)")).result(binding()))' > $(SIS_DIR)/$(PKG_FILE)

SISTOOL := ../../megasis/do-sis-signing.rb -k $(KIT)

sis : $(if $(SIGN),signed-sis,unsigned-sis)

unsupported-txt :
	mkdir -p $(SIS_DIR)
	echo Unsupported platform. No DLL installed. Please abort installation. > $(SIS_DIR)/unsupported.txt

unsigned-sis : pkg unsupported-txt
	(cd $(SIS_DIR) && $(SISTOOL) --makesis -o $(SIS_FILE) -i $(PKG_FILE))

signed-sis : pkg unsupported-txt
	(cd $(SIS_DIR) && $(SISTOOL) --makesis --signsis --cert $(CERT) -o $(SISX_FILE) -i $(PKG_FILE))

# The -e switch exports "export" variables to the sub-make, except
# where overridden by an explicit command-line definition.
3rd-sis :
	$(MAKE) -e dll KIT=s60_30
	$(MAKE) -e dll KIT=s60_31
	$(MAKE) -e dll KIT=s60_32
	$(MAKE) -e dll KIT=s60_50
	$(MAKE) -e sis ER=9

all : 3rd-sis

cl2-% :
	$(MAKE) -e $(patsubst cl2-%,%,$@) UID3=e846000e OWNER=cl2 "PROJECT_NAME=epocxplat for CL2" ER=9

cl2 : cl2-3rd-sis

clean :
	-rm -r build $(SIS_DIR)

sub-test :
	$(MAKE) sub-echo
	$(MAKE) DEVICE=foo sub-echo
	$(MAKE) -e sub-echo
	$(MAKE) -e DEVICE=foo sub-echo

sub-echo :
	@echo DEVICE is $(DEVICE).

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

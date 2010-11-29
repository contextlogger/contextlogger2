VARIANT_NAME := s60_30_dev

-include src/current_config.mk

CC := gcc

SRC_PATH := src
APP_C_FILES := cf_query.c client-run.c config_db.c db_creation.c er_errors.c kr_controller.c libluasqlite3.c ld_create.c ld_log_db.c ld_logging.c main_posix.c rk_jabber_session.c rk_remokon.c sa_sensor_list_log_db.c sa_sensor_mark.c sa_sensor_timer.c up_shared.c up_uploader_curl.c ut_timer_libev.c utils_cl2.c
APP_CXX_FILES := ac_app_context.cpp bb_blackboard.cpp cf_rcfile.cpp lua_bindings.cpp lua_cl2.cpp sa_array.cpp

SHARED_PATH := ../shared
SHARED_C_FILES := error_list.c gx_maybe_string.c gxerror.c logging-time.c platform_error.c utilities.c
SHARED_CXX_FILES := assertions_cxx.cpp logging.cpp utilities_cxx.cpp

TIMESPEC_PATH := ../time-spec
TIMESPEC_C_FILES := moment_parser.c time_utils.c

ALL_RAGEL_FILES := src/rk_jabber_session.rl
RAGEL_C_FILES := $(patsubst %.rl, %.c, $(ALL_RAGEL_FILES))
RAGEL_GV_FILES := $(patsubst %.rl, %.gv, $(ALL_RAGEL_FILES))
RAGEL_PNG_FILES := $(patsubst %.rl, %.png, $(ALL_RAGEL_FILES))
RAGEL_GEN_FILES := $(RAGEL_C_FILES) $(RAGEL_GV_FILES) $(RAGEL_PNG_FILES)

IKSEMEL_INC_PATH := ../iksemel/include
IKSEMEL_SRC_PATH := ../iksemel/src
IKSEMEL_C_FILES := base64.c dom.c filter.c iks.c ikss_stream.c ikst_transport_libev.c ikstack.c jabber.c md5.c sax.c sha.c utility.c
FULL_IKSEMEL_C_FILES := $(patsubst %, $(IKSEMEL_SRC_PATH)/%, $(IKSEMEL_C_FILES))
IKSEMEL_DEP_FLAGS := -I$(IKSEMEL_INC_PATH) -I$(IKSEMEL_SRC_PATH)

ifeq ($(UPLOAD_WITH_CURL),true)
CURL_CONFIG := curl-config
CURL_VERSION := $(shell $(CURL_CONFIG) --version)
CURL_CFLAGS := $(shell $(CURL_CONFIG) --cflags)
CURL_LDFLAGS := $(shell $(CURL_CONFIG) --libs)
CURL_LSFLAGS := $(shell $(CURL_CONFIG) --static-libs)
CURL_FEATURES := $(shell $(CURL_CONFIG) --features)
CURL_PREFIX := $(shell $(CURL_CONFIG) --prefix)
endif

C_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_C_FILES)) $(patsubst %, $(SHARED_PATH)/common/%, $(SHARED_C_FILES)) $(patsubst %, $(TIMESPEC_PATH)/%, $(TIMESPEC_C_FILES)) $(FULL_IKSEMEL_C_FILES)
CXX_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_CXX_FILES)) $(patsubst %, $(SHARED_PATH)/common/%, $(SHARED_CXX_FILES))

APP_QT_HEADERS := sa_sensor_light_qt.hpp
APP_MOC_FILES := $(patsubst %.hpp,moc_%.cpp,$(APP_QT_HEADERS))
QT_HEADERS := $(patsubst %, $(SRC_PATH)/%, $(APP_QT_HEADERS))
MOC_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_MOC_FILES))

GEN_FILES := $(RAGEL_GEN_FILES)

BUILD_LUA := true
ifeq ($(BUILD_LUA),true)
LUA_CFLAGS := -I../lua/src -I../lua/etc
LUA_LDFLAGS :=
LUA_C_FILES := $(patsubst %, ../lua/src/%, lapi.c lauxlib.c lbaselib.c lcode.c ldblib.c ldebug.c ldo.c ldump.c lfunc.c lgc.c linit.c liolib.c llex.c lmathlib.c lmem.c loadlib.c lobject.c lopcodes.c loslib.c lparser.c lstate.c lstring.c lstrlib.c ltable.c ltablib.c ltm.c lundump.c lvm.c lzio.c print.c)
else
LUA_CFLAGS := -I/usr/include/lua5.1
LUA_LDFLAGS := -llua5.1
LUA_C_FILES := 
endif

# We need this for fastdep at least.
KEYEVENTS_CFLAGS := -I../keyevents/inc

ALL_C_FILES := $(C_FILES) $(LUA_C_FILES)
ALL_CXX_FILES := $(CXX_FILES)
ALL_OBJ_FILES := $(patsubst %.c, %.o, $(ALL_C_FILES)) $(patsubst %.cpp, %.o, $(ALL_CXX_FILES))

DEP_FLAGS := -I$(SRC_PATH) -I$(SHARED_PATH) -I$(TIMESPEC_PATH) $(CURL_CFLAGS) $(LUA_CFLAGS) $(KEYEVENTS_CFLAGS) $(IKSEMEL_DEP_FLAGS) -I../epocxplat/src
COMMON_FLAGS := $(DEP_FLAGS) -fexceptions
GNOME_FLAGS := `pkg-config --cflags gobject-2.0` -DG_DISABLE_DEPRECATED=1
FEATURES := 
WARNINGS := -Wall -Wmissing-declarations -Wsign-compare -Werror
CFLAGS := $(WARNINGS) $(COMMON_FLAGS) $(GNOME_FLAGS) $(FEATURES)
CXXFLAGS := $(CFLAGS)
LDFLAGS := $(CURL_LSFLAGS) -lev -lpthread `pkg-config --libs gobject-2.0` -lsqlite3 $(LUA_LDFLAGS)
MOC_FLAGS := $(DEP_FLAGS) $(GNOME_FLAGS)

PLT_ENV := PLTCOLLECTS=`pwd`/../tools/schemelib:

SAKE := ../tools/bin/sake

KIT := $(or $(KIT_NAME), s60_30)
CERT := $(or $(CERT_NAME), dev)

DEFAULTS := epoc_codegen binary-and-run

default : $(DEFAULTS)

src/current_config.mk :
	$(PLT_ENV) ./configure.scm $(VARIANT_NAME)

config : 
	$(PLT_ENV) ./configure.scm $(VARIANT_NAME)

# Unfortunately the file produced by this rule contains some absolute paths, which means that the end result is platform specific. But then, some of the binaries might be as well. You may just want to do a "make clean" when moving over to another host.
.depend : Makefile $(GEN_FILES)
	fastdep $(DEP_FLAGS) --remakedeptarget=$@ $(ALL_C_FILES) $(ALL_CXX_FILES) > $@

-include .depend

ifeq ($(WITH_QMAKE),true)
  ifeq ($(IS_SYMBIAN),true)
    BUILD_BINARY := binary-with-qmake
  else
    BUILD_BINARY := main-with-qmake
  endif
else
  ifeq ($(IS_SYMBIAN),true)
    BUILD_BINARY := binary-with-sake
  else
    BUILD_BINARY := main
  endif
endif

binary-and-run : bin $(and $(NOT__IS_SYMBIAN), run)

bin : $(and $(WITH_QT),$(NOT__WITH_QMAKE),moc) $(BUILD_BINARY)

run :
	./main

binary-with-sake :
	$(SAKE) --trace kits=$(KIT) cert=$(CERT)

binary-with-qmake : 
	cd qt-group && qmake && make

main-with-qmake : binary-with-qmake
	cp qt-group/main .

main : $(ALL_OBJ_FILES)
	g++ -o $@ $(ALL_OBJ_FILES) $(LDFLAGS)

GOB_FLAGS := --always-private-header --output-dir $(SRC_PATH)

moc : $(MOC_FILES)

$(MOC_FILES) : $(SRC_PATH)/moc_%.cpp : $(SRC_PATH)/%.hpp
	moc $(MOC_FLAGS) $< -o $@

%.c %.h %-private.h : %.gob
	gob2 $(GOB_FLAGS) $<

src/rk_jabber_session.c : src/rk_jabber_session.rl
	ragel -C -o $@ $<

%.gv : %.rl
	ragel -V -o $@ $<

%.png : %.gv
	dot -Tpng $< > $@

epoc_codegen : src/timer_generic_epoc.h src/ut_telephony_epoc.h

ragel : $(RAGEL_C_FILES) $(RAGEL_GV_FILES) $(RAGEL_PNG_FILES)

codegen : ragel epoc_codegen src/sa_sensor_list_log_db.h $(SHARED_PATH)/common/error_list.h
	cd ../time-spec && make codegen

src/sa_sensor_list_log_db.h src/sa_sensor_list_log_db.c src/sa_sensor_list_integration.cpp : src/sa_sensor_list_spec.ss src/sa_sensor_list_make.ss src/sa_sensor_list_dsl.ss
	$(PLT_ENV) $<

src/timer_generic_epoc.h src/timer_generic_epoc.cpp : src/timer_generic_epoc.scm
	$(PLT_ENV) $<

src/ut_telephony_epoc.h src/ut_telephony_epoc.cpp : src/ut_telephony_epoc.scm
	$(PLT_ENV) $<

$(SHARED_PATH)/common/error_list.h $(SHARED_PATH)/common/error_list.c : $(SHARED_PATH)/common/error_list.scm
	$(PLT_ENV) $<

clean :
	-rm main .depend $(ALL_OBJ_FILES) $(GEN_FILES) .sake_op

# We are presently compiling all of these with the default rules.
.PRECIOUS : %.c
.PRECIOUS : %.h
.PRECIOUS : %.cpp

# Any host-specific rules.
-include local/custom.mk

apidoc :
	doxygen src/doxyfile-int

# Some of the Ubuntu packages required for building.
ubuntu-dev :
	sudo aptitude install libglib2.0-dev libsqlite3-dev liblua5.1-dev

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

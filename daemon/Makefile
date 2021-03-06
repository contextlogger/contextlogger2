VARIANT_NAME := s60_30_dev

-include src/current_config.mk

CC := gcc

SRC_PATH := src
APP_C_FILES := cf_query.c client-run.c config_db.c db_creation.c er_errors.c kr_controller.c libluasqlite3.c ld_create.c ld_log_db.c ld_logging.c main_posix.c sa_sensor_list_log_db.c sa_sensor_mark.c sa_sensor_timer.c up_shared.c ut_timer_libev.c utils_cl2.c
APP_CXX_FILES := ac_app_context.cpp bb_blackboard.cpp cf_rcfile.cpp lua_bindings.cpp lua_cl2.cpp sa_array.cpp
APP_QT_HEADERS := $(and $(USE_QT_SENSORS),sa_qt_sensors.hpp) $(and $(LIGHT_ENABLED),sa_sensor_light_qt.hpp) $(and $(PROXIMITY_ENABLED),sa_sensor_proximity_qt.hpp) $(and $(WITH_TAP_SENSORS),sa_sensor_tap_qt.hpp) iodeviceseq_qt.hpp rk_remokon_qt.hpp up_uploader_qt_private.hpp ut_abs_timer_qt_epoc.hpp ut_abs_timer_qt_pure.hpp window_demo_qt.hpp

SHARED_PATH := ../shared
SHARED_DIR := $(SHARED_PATH)/common
SHARED_C_FILES := error_list.c gx_maybe_string.c gxerror.c logging-time.c platform_error.c utilities.c
SHARED_CXX_FILES := assertions_cxx.cpp logging.cpp utilities_cxx.cpp
SHARED_QT_HEADERS := QsKineticScroller.hpp

TIMESPEC_PATH := ../time-spec
TIMESPEC_C_FILES := moment_parser.c time_utils.c

ALL_RAGEL_FILES :=
RAGEL_C_FILES := $(patsubst %.rl, %.c, $(ALL_RAGEL_FILES))
RAGEL_GV_FILES := $(patsubst %.rl, %.gv, $(ALL_RAGEL_FILES))
RAGEL_PNG_FILES := $(patsubst %.rl, %.png, $(ALL_RAGEL_FILES))
RAGEL_GEN_FILES := $(RAGEL_C_FILES) $(RAGEL_GV_FILES) $(RAGEL_PNG_FILES)

C_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_C_FILES)) $(patsubst %, $(SHARED_DIR)/%, $(SHARED_C_FILES)) $(patsubst %, $(TIMESPEC_PATH)/%, $(TIMESPEC_C_FILES))
CXX_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_CXX_FILES)) $(patsubst %, $(SHARED_DIR)/%, $(SHARED_CXX_FILES))

APP_MOC_BASENAMES := $(patsubst %.hpp,moc_%.cpp,$(APP_QT_HEADERS))
SHARED_MOC_BASENAMES := $(patsubst %.hpp,moc_%.cpp,$(SHARED_QT_HEADERS))
QT_HEADERS := $(patsubst %, $(SRC_PATH)/%, $(APP_QT_HEADERS)) $(patsubst %, $(SHARED_DIR)/%, $(SHARED_QT_HEADERS))
APP_MOC_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_MOC_BASENAMES))
SHARED_MOC_FILES := $(patsubst %, $(SHARED_DIR)/%, $(SHARED_MOC_BASENAMES))
MOC_FILES := $(APP_MOC_FILES) $(SHARED_MOC_FILES)

GEN_FILES := $(RAGEL_GEN_FILES) $(MOC_FILES)

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

DEP_FLAGS := -I$(SRC_PATH) -I$(SHARED_PATH) -I$(TIMESPEC_PATH) $(LUA_CFLAGS) $(KEYEVENTS_CFLAGS) -I../epocxplat/src
COMMON_FLAGS := $(DEP_FLAGS) -fexceptions
# "moc" is confused by "-pthread", which some versions of pkg-config generate; "moc" will make that a path prefix for #include files, which breaks builds
GNOME_FLAGS := `pkg-config --cflags-only-I gobject-2.0` -DG_DISABLE_DEPRECATED=1
FEATURES := 
WARNINGS := -Wall -Wmissing-declarations -Wsign-compare -Werror
CFLAGS := $(WARNINGS) $(COMMON_FLAGS) $(GNOME_FLAGS) $(FEATURES)
CXXFLAGS := $(CFLAGS)
LDFLAGS := -lev -lpthread `pkg-config --libs gobject-2.0` -lsqlite3 $(LUA_LDFLAGS)
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

.PHONY : moc rm-moc remoc

# When running this rule, ensure that you have a compatible version of "moc" on the PATH. The Qt major version matching for the "moc" may well be enough.
moc : $(MOC_FILES)

rm-moc :
	-rm $(MOC_FILES)

$(APP_MOC_FILES) : $(SRC_PATH)/moc_%.cpp : $(SRC_PATH)/%.hpp
	moc $(MOC_FLAGS) $< -o $@

$(SHARED_MOC_FILES) : $(SHARED_DIR)/moc_%.cpp : $(SHARED_DIR)/%.hpp
	moc $(MOC_FLAGS) $< -o $@

%.c %.h %-private.h : %.gob
	gob2 $(GOB_FLAGS) $<

%.gv : %.rl
	ragel -V -o $@ $<

%.png : %.gv
	dot -Tpng $< > $@

epoc_codegen : src/timer_generic_epoc.h src/ut_telephony_epoc.h

ragel : $(RAGEL_C_FILES) $(RAGEL_GV_FILES) $(RAGEL_PNG_FILES)

codegen : ragel epoc_codegen src/sa_sensor_list_log_db.h $(SHARED_DIR)/error_list.h
	cd ../time-spec && make codegen

src/sa_sensor_list_log_db.h src/sa_sensor_list_log_db.c src/sa_sensor_list_integration.cpp : src/sa_sensor_list_spec.ss src/sa_sensor_list_make.ss src/sa_sensor_list_dsl.ss
	$(PLT_ENV) $<

src/timer_generic_epoc.h src/timer_generic_epoc.cpp : src/timer_generic_epoc.scm
	$(PLT_ENV) $<

src/ut_telephony_epoc.h src/ut_telephony_epoc.cpp : src/ut_telephony_epoc.scm
	$(PLT_ENV) $<

$(SHARED_DIR)/error_list.h $(SHARED_DIR)/error_list.c : $(SHARED_DIR)/error_list.scm
	$(PLT_ENV) $<

old-log :
	-mkdir -p uploads
	echo hello world > uploads/log--notreally--2010-09-25-04-01-43.db

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

rm-apidoc :
	-rm -r ../api/*

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

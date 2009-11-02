include src/current_config.mk

CC := gcc

SRC_PATH := src
APP_C_FILES := cf_query.c client-run.c config_db.c db_creation.c er_errors.c kr_controller.c libluasqlite3.c log-db-create.c log-db-logging.c posix-main.c rk_jabber_session.c rk_remokon.c sa_sensor_list_log_db.c sa_sensor_mark.c sa_sensor_timer_libev.c up_shared.c up_uploader_curl.c ut_timer_libev.c utils_cl2.c
APP_CXX_FILES := ac_app_context.cpp cf_rcfile.cpp lua_bindings.cpp lua_cl2.cpp sa_array.cpp

SHARED_PATH := ../shared
SHARED_C_FILES := error_list.c logging-time.c platform_error.c utilities.c
SHARED_CXX_FILES := assertions_cxx.cpp logging.cpp utilities_cxx.cpp

TIMESPEC_PATH := ../time-spec
TIMESPEC_C_FILES := moment_parser.c time_utils.c

ALL_RAGEL_FILES := src/rk_jabber_session.rl
RAGEL_C_FILES := $(patsubst %.rl, %.c, $(ALL_RAGEL_FILES))

IKSEMEL_INC_PATH := ../iksemel/include
IKSEMEL_SRC_PATH := ../iksemel/src
IKSEMEL_C_FILES := base64.c dom.c filter.c iks.c ikss_stream.c ikst_transport_libev.c ikstack.c jabber.c md5.c sax.c sha.c utility.c
FULL_IKSEMEL_C_FILES := $(patsubst %, $(IKSEMEL_SRC_PATH)/%, $(IKSEMEL_C_FILES))
IKSEMEL_DEP_FLAGS := -I$(IKSEMEL_INC_PATH) -I$(IKSEMEL_SRC_PATH)

C_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_C_FILES)) $(patsubst %, $(SHARED_PATH)/common/%, $(SHARED_C_FILES)) $(patsubst %, $(TIMESPEC_PATH)/%, $(TIMESPEC_C_FILES)) $(FULL_IKSEMEL_C_FILES)
CXX_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_CXX_FILES)) $(patsubst %, $(SHARED_PATH)/common/%, $(SHARED_CXX_FILES))

GOB_BASENAMES := log-db
GOB_FILES := $(patsubst %, $(SRC_PATH)/%.gob, $(GOB_BASENAMES))
GOB_H_FILES := $(patsubst %.gob, %.h, $(GOB_FILES))
# These do not affect the external interface of the components, but there might be "friend" components depending on some of these.
GOB_PRIV_H_FILES := $(patsubst %.gob, %-private.h, $(GOB_FILES))
GOB_C_FILES := $(patsubst %.gob, %.c, $(GOB_FILES))
# We are not using --no-touch-headers, as that might lead to repeated invocations of gob2. Anyway for as long as gob2 is not invoked unnecessarily, there should be little extra work caused by this. Sure probably the public header does not change as often as the implementation, but this is okay.
# We would really need --no-extern-c because of GOB's indiscriminate enclosing of almost everything in an extern "C"; this is an issue when #including a GOB-generated header into a C++ source file, as then there may end up being C++ declarations with C linkage, which will not work. But not having an --no-extern-c for all the C functions will lead to problems as well at linking time.
GOB_FLAGS := --always-private-header --output-dir $(SRC_PATH)

GEN_FILES := $(GOB_H_FILES) $(GOB_PRIV_H_FILES) $(GOB_C_FILES) $(RAGEL_C_FILES)

# We assume Curl built separately for each platform to avoid system incompatibility issues.
CURL_CONFIG := $(HOME)/local/bin/curl-config
CURL_VERSION := $(shell $(CURL_CONFIG) --version)
CURL_CFLAGS := $(shell $(CURL_CONFIG) --cflags)
CURL_LDFLAGS := $(shell $(CURL_CONFIG) --libs)
CURL_LSFLAGS := $(shell $(CURL_CONFIG) --static-libs)
CURL_FEATURES := $(shell $(CURL_CONFIG) --features)
CURL_PREFIX := $(shell $(CURL_CONFIG) --prefix)

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

ALL_C_FILES := $(C_FILES) $(GOB_C_FILES) $(LUA_C_FILES)
ALL_CXX_FILES := $(CXX_FILES)
ALL_OBJ_FILES := $(patsubst %.c, %.o, $(ALL_C_FILES)) $(patsubst %.cpp, %.o, $(ALL_CXX_FILES))

DEP_FLAGS := -I$(SRC_PATH) -I$(SHARED_PATH) -I$(TIMESPEC_PATH) $(CURL_CFLAGS) $(LUA_CFLAGS) $(KEYEVENTS_CFLAGS) $(IKSEMEL_DEP_FLAGS)
COMMON_FLAGS := $(DEP_FLAGS)
GNOME_FLAGS := `pkg-config --cflags gobject-2.0` -DG_DISABLE_DEPRECATED=1
FEATURES := -D__DO_LOGGING__=1
WARNINGS := -Wall -Wmissing-declarations -Wsign-compare -Werror
CFLAGS := $(WARNINGS) $(COMMON_FLAGS) $(GNOME_FLAGS) $(FEATURES)
CXXFLAGS := $(CFLAGS)
LDFLAGS := $(CURL_LSFLAGS) -lev -lpthread `pkg-config --libs gobject-2.0` -lsqlite3 $(LUA_LDFLAGS)

PLT_ENV := PLTCOLLECTS=`pwd`/../tools/schemelib:

default : epoc_codegen binary

binary : $(if $(IS_SYMBIAN), symbian, posix_test)

config : 
	$(PLT_ENV) ./configure.scm $(VARIANT_NAME)

# Unfortunately the file produced by this rule contains some absolute paths, which means that the end result is platform specific. But then, some of the binaries might be as well. You may just want to do a "make clean" when moving over to another host.
.depend : Makefile $(GEN_FILES)
	fastdep $(DEP_FLAGS) --remakedeptarget=$@ $(ALL_C_FILES) $(ALL_CXX_FILES) > $@

-include .depend

posix_test : main
	./main

main : $(ALL_OBJ_FILES)
	g++ -o $@ $(ALL_OBJ_FILES) $(LDFLAGS)

%.c %.h %-private.h : %.gob
	gob2 $(GOB_FLAGS) $<

src/rk_jabber_session.c : src/rk_jabber_session.rl
	ragel -C -o $@ $<

epoc_codegen : src/timer_generic_epoc.h

ragel : $(RAGEL_C_FILES)

src/sa_sensor_list_log_db.h src/sa_sensor_list_log_db.c src/sa_sensor_list_integration.cpp : src/sa_sensor_list_spec.ss src/sa_sensor_list_make.ss src/sa_sensor_list_dsl.ss
	$(PLT_ENV) $<

src/timer_generic_epoc.h src/timer_generic_epoc.cpp : src/timer_generic_epoc.scm
	$(PLT_ENV) $<

src/cf_rcfile_list.h src/cf_rcfile_list.c src/cf_rcfile_list_private.h : src/cf_rcfile_list_spec.ss
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
	sudo aptitude install gob2 libglib2.0-dev libsqlite3-dev liblua5.1-dev

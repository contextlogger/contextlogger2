CC := gcc

SRC_PATH := src
APP_C_FILES := client-run.c er_errors.c log-db-create.c log-db-logging.c posix-main.c sa_sensor_list_log_db.c sa_sensor_timer_posix.c up_shared.c up_uploader_curl.c utils_cl2.c
APP_CXX_FILES := cf_rcfile.cpp local_server.cpp lua_cl2.cpp sa_array.cpp

SHARED_PATH := ../shared
SHARED_C_FILES := error_list.c evq_event.c logging-time.c platform_error.c evq_queue.c evq_timer.c utilities.c
SHARED_CXX_FILES := assertions_cxx.cpp logging.cpp utilities_cxx.cpp

TIMESPEC_PATH := ../time-spec
TIMESPEC_C_FILES := moment_parser.c time_utils.c

C_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_C_FILES)) $(patsubst %, $(SHARED_PATH)/common/%, $(SHARED_C_FILES)) $(patsubst %, $(TIMESPEC_PATH)/%, $(TIMESPEC_C_FILES))
CXX_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_CXX_FILES)) $(patsubst %, $(SHARED_PATH)/common/%, $(SHARED_CXX_FILES))

GOB_BASENAMES := client-cl2 log-db
GOB_FILES := $(patsubst %, $(SRC_PATH)/%.gob, $(GOB_BASENAMES))
GOB_H_FILES := $(patsubst %.gob, %.h, $(GOB_FILES))
# These do not affect the external interface of the components, but there might be "friend" components depending on some of these.
GOB_PRIV_H_FILES := $(patsubst %.gob, %-private.h, $(GOB_FILES))
GOB_C_FILES := $(patsubst %.gob, %.c, $(GOB_FILES))
# We are not using --no-touch-headers, as that might lead to repeated invocations of gob2. Anyway for as long as gob2 is not invoked unnecessarily, there should be little extra work caused by this. Sure probably the public header does not change as often as the implementation, but this is okay.
GOB_FLAGS := --always-private-header --output-dir $(SRC_PATH)

GEN_FILES := $(GOB_H_FILES) $(GOB_PRIV_H_FILES) $(GOB_C_FILES)

# We assume Curl built separately for each platform to avoid system incompatibility issues.
CURL_CONFIG := $(HOME)/local/bin/curl-config
CURL_VERSION := $(shell $(CURL_CONFIG) --version)
CURL_CFLAGS := $(shell $(CURL_CONFIG) --cflags)
CURL_LDFLAGS := $(shell $(CURL_CONFIG) --libs)
CURL_LSFLAGS := $(shell $(CURL_CONFIG) --static-libs)
CURL_FEATURES := $(shell $(CURL_CONFIG) --features)
CURL_PREFIX := $(shell $(CURL_CONFIG) --prefix)

ALL_C_FILES := $(C_FILES) $(GOB_C_FILES)
ALL_CXX_FILES := $(CXX_FILES)
ALL_OBJ_FILES := $(patsubst %.c, %.o, $(ALL_C_FILES)) $(patsubst %.cpp, %.o, $(ALL_CXX_FILES))

DEP_FLAGS := -I$(SRC_PATH) -I$(SHARED_PATH) -I$(TIMESPEC_PATH) $(CURL_CFLAGS)
COMMON_FLAGS := $(DEP_FLAGS) -D__DO_LOGGING__=1
GNOME_FLAGS := `pkg-config --cflags gobject-2.0` -DG_DISABLE_DEPRECATED=1
FEATURES := -D__FEATURE_UPLOADER__ -D__UPLOAD_WITH_CURL__
WARNINGS := -Wall -Wmissing-declarations -Wsign-compare -Werror
CFLAGS := $(WARNINGS) $(COMMON_FLAGS) $(GNOME_FLAGS) $(FEATURES)
CXXFLAGS := $(CFLAGS)
LDFLAGS := $(CURL_LSFLAGS) -lpthread `pkg-config --libs gobject-2.0` -lsqlite3 -llua5.1

PLT_ENV := PLTCOLLECTS=`pwd`/../tools/schemelib:

default : epoc_codegen posix_test

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

epoc_codegen : src/timer_generic_epoc.h

src/sa_sensor_list_log_db.h src/sa_sensor_list_log_db.c src/sa_sensor_list_integration.cpp : src/sa_sensor_list_spec.ss src/sa_sensor_list_make.ss src/sa_sensor_list_dsl.ss
	$(PLT_ENV) $<

src/timer_generic_epoc.h src/timer_generic_epoc.cpp : src/timer_generic_epoc.scm
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

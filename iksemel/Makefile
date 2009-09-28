CC := gcc

SRC_PATH := test
APP_C_FILES := tst-stream.c

IKSEMEL_INC_PATH := include
IKSEMEL_SRC_PATH := src
IKSEMEL_C_FILES := base64.c dom.c filter.c iks.c ikss_stream.c ikst_transport_libev.c ikstack.c jabber.c md5.c sax.c sha.c utility.c

C_FILES := $(patsubst %, $(SRC_PATH)/%, $(APP_C_FILES)) $(patsubst %, $(IKSEMEL_SRC_PATH)/%, $(IKSEMEL_C_FILES))

ALL_C_FILES := $(C_FILES)
ALL_CXX_FILES :=
ALL_OBJ_FILES := $(patsubst %.c, %.o, $(ALL_C_FILES)) $(patsubst %.cpp, %.o, $(ALL_CXX_FILES))

DEP_FLAGS := -I$(IKSEMEL_INC_PATH) -I$(IKSEMEL_SRC_PATH)
COMMON_FLAGS := $(DEP_FLAGS)
WARNINGS := -Wall -Wmissing-declarations -Wsign-compare
CFLAGS := $(WARNINGS) $(COMMON_FLAGS)
LDFLAGS := -lev

PLT_ENV := PLTCOLLECTS=`pwd`/../tools/schemelib:

default : posix_test

list :
	@echo $(ALL_OBJ_FILES)

# Unfortunately the file produced by this rule contains some absolute paths, which means that the end result is platform specific. But then, some of the binaries might be as well. You may just want to do a "make clean" when moving over to another host.
.depend : Makefile $(GEN_FILES)
	fastdep $(DEP_FLAGS) --remakedeptarget=$@ $(ALL_C_FILES) $(ALL_CXX_FILES) > $@

-include .depend

posix_test : main
	./main

main : $(ALL_OBJ_FILES)
	g++ -o $@ $(ALL_OBJ_FILES) $(LDFLAGS)

clean :
	-rm main .depend $(ALL_OBJ_FILES)

# We are presently compiling all of these with the default rules.
.PRECIOUS : %.c
.PRECIOUS : %.h
.PRECIOUS : %.cpp

# Any host-specific rules.
-include local/custom.mk

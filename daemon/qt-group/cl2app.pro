include(../src/current_config.pri)
TEMPLATE = app
CONFIG += debug
WITH_QT {
  CONFIG += qt
}
INCLUDEPATH = ../src ../../shared ../../time-spec
DEPENDPATH = ../src ../../shared/common ../../time-spec
SOURCES += cf_query.c client-run.c config_db.c db_creation.c er_errors.c kr_controller.c libluasqlite3.c ld_create.c ld_log_db.c ld_logging.c sa_sensor_list_log_db.c sa_sensor_mark.c sa_sensor_timer.c up_shared.c utils_cl2.c
SOURCES += ac_app_context.cpp bb_blackboard.cpp cf_rcfile.cpp lua_bindings.cpp lua_cl2.cpp sa_array.cpp
SOURCES += error_list.c gx_maybe_string.c gxerror.c logging-time.c platform_error.c utilities.c
SOURCES += assertions_cxx.cpp logging.cpp utilities_cxx.cpp
SOURCES += moment_parser.c time_utils.c
DEFINES += G_DISABLE_DEPRECATED
MY_WARNING_FLAGS = -Wall -Wmissing-declarations -Wsign-compare
MY_CCFLAGS = -fexceptions $$MY_WARNING_FLAGS
QMAKE_CFLAGS += $$MY_CCFLAGS
QMAKE_CXXFLAGS += $$MY_CCFLAGS
WITH_QT {
  SOURCES += ut_timer_qt.cpp
  HEADERS += ut_timer_qt_private.hpp
  SOURCES += main_qt.cpp
}
WITH_LIBEV {
  SOURCES += ut_timer_libev.c
  LIBS += -lev
  SOURCES += main_posix.c
}
!LUA_FROM_SOURCE {
  INCLUDEPATH += /usr/include/lua5.1
  LIBS += -llua5.1
}
LUA_FROM_SOURCE {
  INCLUDEPATH += ../../lua/src ../../lua/etc
  DEPENDPATH += ../../lua/src ../../lua/etc
  SOURCES += lapi.c lauxlib.c lbaselib.c lcode.c ldblib.c ldebug.c ldo.cpp ldump.c lfunc.c lgc.c linit.c liolib.c llex.c lmathlib.c lmem.c loadlib.cpp lobject.c lopcodes.c loslib.c lparser.c lstate.c lstring.c lstrlib.c ltable.c ltablib.c ltm.c lundump.c lvm.c lzio.c print.c
}
unix {
  TARGET = main
  LIBS += -lpthread -lsqlite3
  CONFIG += link_pkgconfig
  PKGCONFIG = glib-2.0
}
symbian {
  TARGET.CAPABILITY = $$CAPABILITIES
}

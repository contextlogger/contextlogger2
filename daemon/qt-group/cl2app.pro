# Make sure no "qt" is assumed by default. (Could also use -= qt)
CONFIG =

include(../src/current_config.pri)
TEMPLATE = app
CONFIG += debug
VERSION = $$VERSION_STRING
WITH_QT {
  message("with Qt")
  # Note: Do not use Qt 4.7.0 on Linux, as there is an event loop memory bug.
  CONFIG += qt
}
!WITH_QT {
  message("without Qt")
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
  TARGET = cl2app
  TARGET.UID2 = 0x00000000
  TARGET.UID3 = 0x$$UID_V9__HEX
  TARGET.EPOCSTACKSIZE = 0x10000
  TARGET.CAPABILITY = $$CAPABILITIES
  LIBS += sqlite3h.lib
  LIBS += euser.lib
  DO_LOGGING {
    LIBS += flogger.lib
  }
  PROFILE_ENABLED {
    HAVE_PROFILEENGINE_LIB {
      LIBS += profileengine.lib
    }
    !HAVE_PROFILEENGINE_LIB {
      LIBS += profileeng.lib # SDK Plugins
    }
  }
  SMSEVENT_ENABLED {
    LIBS += gsmu.lib # CSmsPDU
    LIBS += msgs.lib # CMsvSession
    LIBS += smcm.lib # CSmsClientMtm
  }
  LIBS += apgrfx.lib
  LIBS += apparc.lib
  LIBS += avkon.lib
  LIBS += bafl.lib
  LIBS += bluetooth.lib
  LIBS += btmanclient.lib
  LIBS += charconv.lib
  LIBS += commdb.lib
  LIBS += commonui.lib # CErrorUI
  LIBS += cone.lib
  LIBS += efsrv.lib
  LIBS += eikcoctl.lib
  LIBS += eikcore.lib
  LIBS += esock.lib
  LIBS += etel3rdparty.lib # CTelephony
  LIBS += http.lib
  LIBS += inetprotutil.lib # URI parsers
  LIBS += insock.lib # Internet protocol support for esock
  LIBS += lbs.lib
  LIBS += ws32.lib

  LIBS += aknnotify.lib eiksrv.lib # CAknGlobalNote
  LIBS += cntmodel.lib # CContactDatabase
  #  LIBS += estor.lib # RReadStream
  #  LIBS += etel.lib # RCall, RLine
  #  LIBS += logcli.lib # CLogClient
  #  LIBS += pbkeng.lib # CPbkContactEngine

  WEBURL_ENABLED {
    LIBS += epocxplat_e846000e.lib
  }

  LIBS += euserhl.lib    # High-Level APIs

  LIBS += libc.lib       # PIPS
  LIBS += libm.lib       # required for Lua
  LIBS += libglib.lib    # Open C
  LIBS += libgobject.lib # Open C
  LIBS += libpthread.lib # Open C
  #  LIBS += libstdcpp.lib  # Open C++

  KEYPRESS_ENABLED:HAVE_ANIM {
    LIBS += keyevents_client_2000af44.lib
  }
}

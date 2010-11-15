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
  QT -= gui
}
!WITH_QT {
  message("without Qt")
}
DEPENDPATH = ../src ../../shared/common ../../time-spec
SOURCES += cf_query.c client-run.c config_db.c db_creation.c er_errors.c kr_controller.c libluasqlite3.c ld_create.c ld_log_db.c ld_logging.c sa_sensor_list_log_db.c sa_sensor_mark.c sa_sensor_timer.c up_shared.c utils_cl2.c
SOURCES += ac_app_context.cpp bb_blackboard.cpp cf_rcfile.cpp lua_bindings.cpp lua_cl2.cpp sa_array.cpp
SOURCES += error_list.c gx_maybe_string.c gxerror.c logging-time.c platform_error.c utilities.c
SOURCES += assertions_cxx.cpp logging.cpp utilities_cxx.cpp
SOURCES += moment_parser.c time_utils.c
DEFINES += G_DISABLE_DEPRECATED
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
!symbian {
  INCLUDEPATH = ../src ../../shared ../../time-spec

  MY_WARNING_FLAGS = -Wall -Wmissing-declarations -Wsign-compare
  MY_CCFLAGS = -fexceptions $$MY_WARNING_FLAGS
  QMAKE_CFLAGS += $$MY_CCFLAGS
  QMAKE_CXXFLAGS += $$MY_CCFLAGS

  TARGET = main
  LIBS += -lpthread -lsqlite3
  CONFIG += link_pkgconfig
  PKGCONFIG = glib-2.0
}
symbian {
  MMP_RULES += "SYSTEMINCLUDE \\epoc32\\include\\stdapis\\glib-2.0"
  MMP_RULES += "USERINCLUDE ..\\build\\$$VARIANT_NAME" # xxx sconfig.hrh missing unless we invoke sake first
  MMP_RULES += "USERINCLUDE ..\\inc"
  MMP_RULES += "USERINCLUDE ..\\src"
  MMP_RULES += "USERINCLUDE ..\\..\\shared"
  MMP_RULES += "USERINCLUDE ..\\..\\lua\\src"
  MMP_RULES += "USERINCLUDE ..\\..\\lua\\etc"
  MMP_RULES += "USERINCLUDE ..\\..\\sqlite3h\\src\\sqlite3"
  MMP_RULES += "USERINCLUDE ..\\..\\epocxplat\\src"
  FEATURE_UPLOADER {
    MMP_RULES += "USERINCLUDE ..\\..\\time-spec"
  }
  HAVE_ANIM {
    MMP_RULES += "USERINCLUDE ..\\..\\keyevents\\inc"
  }

  DESTDIR = ../build/$$VARIANT_NAME
  TARGET = cl2app
  TARGET.UID2 = 0x00000000
  TARGET.UID3 = 0x$$UID_V9__HEX
  TARGET.VID = 0
  TARGET.EPOCSTACKSIZE = 0x10000
  TARGET.CAPABILITY = $$CAPABILITIES
  DEFINES += __HIDE_IPC_V1__ __HAVE_APPLICATION_CONFIG__

  LIBS += -lsqlite3h.lib
  # LIBS += -leuser.dll # (included by qmake)
  DO_LOGGING {
    LIBS += -lflogger.dll
  }
  PROFILE_ENABLED {
    HAVE_PROFILEENGINE_LIB {
      LIBS += -lprofileengine.dll
    }
    !HAVE_PROFILEENGINE_LIB {
      LIBS += -lprofileeng.dll # SDK Plugins
    }
  }
  SMSEVENT_ENABLED {
    LIBS += -lgsmu.dll # CSmsPDU
    LIBS += -lmsgs.dll # CMsvSession
    LIBS += -lsmcm.dll # CSmsClientMtm
  }
  LIBS += -lapgrfx.dll
  LIBS += -lapparc.dll
  LIBS += -lavkon.dll
  LIBS += -lbafl.dll
  LIBS += -lbluetooth.dll
  LIBS += -lbtmanclient.dll
  LIBS += -lcharconv.dll
  LIBS += -lcommdb.dll
  LIBS += -lcommonui.dll # CErrorUI
  LIBS += -lcone.dll
  LIBS += -lefsrv.dll
  LIBS += -leikcoctl.dll
  LIBS += -leikcore.dll
  LIBS += -lesock.dll
  LIBS += -letel3rdparty.dll # CTelephony
  LIBS += -lhttp.dll
  LIBS += -linetprotutil.dll # URI parsers
  LIBS += -linsock.dll # Internet protocol support for esock
  LIBS += -llbs.dll
  LIBS += -lws32.dll

  LIBS += -laknnotify.dll -leiksrv.dll # CAknGlobalNote
  LIBS += -lcntmodel.dll # CContactDatabase
  #  LIBS += -lestor.dll # RReadStream
  #  LIBS += -letel.dll # RCall, RLine
  #  LIBS += -llogcli.dll # CLogClient
  #  LIBS += -lpbkeng.dll # CPbkContactEngine

  WEBURL_ENABLED {
    LIBS += -lepocxplat_e846000e.dll
  }

  LIBS += -leuserhl.dll    # High-Level APIs

  # LIBS += -llibc.dll       # PIPS (included by qmake)
  # LIBS += -llibm.dll       # required for Lua (included by qmake)
  LIBS += -llibglib.dll    # Open C
  LIBS += -llibgobject.dll # Open C
  # LIBS += -llibpthread.dll # Open C (included by qmake)
  #  LIBS += -llibstdcpp.dll  # Open C++

  KEYPRESS_ENABLED:HAVE_ANIM {
    LIBS += -lkeyevents_client_2000af44.dll
  }

  # shared
  SOURCES += epoc-time.cpp

  IS_APPLICATION {
    SOURCES += cl2appapp.cpp
    SOURCES += cl2appappui.cpp
    SOURCES += cl2appcontainer.cpp
    SOURCES += cl2appdocument.cpp
  }
  SOURCES += epoc-ao-gerror.cpp
  SOURCES += epoc-appfocus.cpp
  SOURCES += epoc-btprox.cpp
  SOURCES += epoc-callstatus.cpp
  SOURCES += epoc-cellid.cpp
  SOURCES += epoc-cliapi-server.cpp
  SOURCES += epoc-gps.cpp
  SOURCES += epoc-iap.cpp
  SOURCES += epoc-inactivity.cpp
  SOURCES += epoc-indicator.cpp
  KEYPRESS_ENABLED {
    HAVE_ANIM {
      SOURCES += epoc-keypress-anim.cpp
    }
    !HAVE_ANIM {
      SOURCES += epoc-keypress.cpp
    }
  }
  PROFILE_ENABLED {
    HAVE_PROFILEENGINE_LIB {
      SOURCES += epoc-profile-31.cpp
    }
    !HAVE_PROFILEENGINE_LIB {
      SOURCES += epoc-profile.cpp
    }
  }
  SOURCES += epoc-s60-version.cpp
  SOURCES += epoc-smsevent.cpp
  WEBURL_ENABLED {
    SOURCES += epoc-weburl.cpp
  }
  SOURCES += kr_diskspace.cpp
  SOURCES += kr_plat_ao_epoc.cpp
  FEATURE_LOCALSERVER {
    SOURCES += local_server.cpp
  }
  !IS_APPLICATION {
    SOURCES += main_epoc.cpp
  }
  SOURCES += symbian_auto_ptr.cpp
  SOURCES += ut_diskspace_epoc.cpp
  SOURCES += ut_retry_epoc.cpp
  SMSEVENT_ENABLED {
    SOURCES += ut_sms_epoc.cpp
  }
  SOURCES += ut_telephony_epoc.cpp
  CALLSTATUS_ENABLED|SMSEVENT_ENABLED {
    SOURCES += ut_telno_epoc.cpp
  }
  SOURCES += ut_timer_epoc.cpp

}

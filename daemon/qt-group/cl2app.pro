include(../src/current_config.pri)
TEMPLATE = app
CONFIG = debug
!isEmpty(WITH_QT) {
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
QMAKE_CFLAGS += -fexceptions
QMAKE_CFLAGS += -Wall -Wmissing-declarations -Wsign-compare -Werror
!isEmpty(WITH_QT) {
  SOURCES += ut_timer_qt.cpp
  HEADERS += ut_timer_qt_private.hpp
  SOURCES += main_qt.cpp
}
!isEmpty(WITH_LIBEV) {
  SOURCES += ut_timer_libev.c
  LIBS += -lev
  SOURCES += posix-main.c
}
unix {
  TARGET = main
  LIBS += -lpthread -lsqlite3
  INCLUDEPATH += /usr/include/lua5.1
  LIBS += -llua5.1
  CONFIG += link_pkgconfig
  PKGCONFIG = glib-2.0
}
symbian {
}

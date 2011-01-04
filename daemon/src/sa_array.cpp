#include "sa_array_private.h"

#include "cf_query.h"
#include "kr_controller_private.h" // for runtime config queries
#include "ld_logging.h" // for error handling routines

#include "common/assertions.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#ifdef __EPOC32__
#include <e32std.h>
#endif

#include <string.h>

#define SA_ARRAY_INTEGRATION 1

#define sa_set_symbian_error(errCode,msg) \
    if (error) \
      *error = ((errCode == KErrNoMemory) ? gx_error_no_memory : gx_error_new(domain_symbian, errCode, msg ": %s (%d)", plat_error_strerror(errCode), errCode));

#define sa_typical_symbian_sensor_start(object,msg) { \
  success = TRUE; \
  TRAPD(_errCode, success = (object)->StartL(error));		       \
  if (_errCode) { sa_set_symbian_error(_errCode, msg); success = FALSE; } \
}

#define sa_typical_symbian_sensor_create(expr,msg) { \
  TRAPD(_errCode, expr); \
  if (_errCode) { sa_set_symbian_error(_errCode, msg); success = FALSE; } \
  else { success = TRUE; }					      \
}

#define sa_typical_symbian_sensor_reconfigure(sn) {			\
    (self->iSensor_##sn)->Reconfigure(key, value);			\
    success = TRUE;							\
}

#define sa_set_qt_error(ex, msg)					\
{									\
  if (error)								\
    *error = gx_error_new(domain_qt, -1, "Qt error: %s", ex.what());	\
}

#define sa_typical_qt_sensor_create(expr, msg)	\
{						\
  try {						\
    expr ;					\
    success = TRUE;				\
  } catch (const std::exception &_ex) {		\
    sa_set_qt_error(_ex, msg);			\
    success = FALSE;				\
  }						\
}
    
#define sa_reconfigure_ignore_all_keys { success = TRUE; }

/* Sensor implementation includes.
   There may be variant implementations for different builds.
 */
#if __APPFOCUS_ENABLED__
#include "epoc-appfocus.hpp"
#endif
#if __BTPROX_ENABLED__
#include "epoc-btprox.hpp"
#endif
#if __GPS_ENABLED__
#include "epoc-gps.hpp"
#endif
#if __INACTIVITY_ENABLED__
#include "epoc-inactivity.hpp"
#endif
#if __INDICATOR_ENABLED__
#include "epoc-indicator.hpp"
#endif
#if __KEYPRESS_ENABLED__
#if __HAVE_ANIM__
#include "epoc-keypress-anim.hpp"
#else
#include "epoc-keypress.hpp"
#endif
#endif // __KEYPRESS_ENABLED__
#if __PROFILE_ENABLED__
#if __HAVE_PROFILEENGINE_LIB__
#include "epoc-profile-31.hpp"
#else
#include "epoc-profile.hpp"
#endif
#endif // __PROFILE_ENABLED__
#if __TIMER_ENABLED__
#include "sa_sensor_timer.h"
#endif
#if __MARK_ENABLED__
#include "sa_sensor_mark.h"
#endif

#include "epoc-callstatus.hpp"
#include "epoc-cellid.hpp"
#include "epoc-httpurl.hpp"
#include "epoc-smsevent.hpp"
#include "epoc-weburl.hpp"

#include "sa_sensor_light_api.h"
#include "sa_sensor_proximity_api.h"
#include "sa_sensor_tap_api.h"

// This file is generated, and included only once here. Code for
// creating, destroying, starting, and stopping sensors comes from
// here. The code is designed to mesh well with this file.
// "sa_sensor_list_integration.h" may be #included elsewhere as well.
#include "sa_sensor_list_integration.cpp"

extern "C" struct _sa_Array
{
  ac_AppContext* ac; // not owned
  LogDb* logDb; // not owned

#if __APPFOCUS_ENABLED__
  CSensor_appfocus *iSensor_appfocus;
#endif
#if __BTPROX_ENABLED__
  CSensor_btprox *iSensor_btprox;
#endif
#if __GPS_ENABLED__
  CSensor_gps *iSensor_gps;
#endif
#if __INACTIVITY_ENABLED__
  CSensor_inactivity *iSensor_inactivity;
#endif
#if __INDICATOR_ENABLED__
  CSensor_indicator *iSensor_indicator;
#endif
#if __KEYPRESS_ENABLED__
  CSensor_keypress *iSensor_keypress;
#endif
#if __PROFILE_ENABLED__
  CSensor_profile *iSensor_profile;
#endif
#if __TIMER_ENABLED__
  sa_Sensor_timer* iSensor_timer;
#endif
#if __MARK_ENABLED__
  sa_Sensor_mark* iSensor_mark;
#endif
  DECLARE_SENSOR_callstatus;
  DECLARE_SENSOR_cellid;
  DECLARE_SENSOR_httpurl;
  DECLARE_SENSOR_light;
  DECLARE_SENSOR_doubletap;
  DECLARE_SENSOR_proximity;
  DECLARE_SENSOR_singletap;
  DECLARE_SENSOR_smsevent;
  DECLARE_SENSOR_weburl;
};

/* Sensor starting. (Statement.)
   Must set "success" (gboolean) and "error" (GError**) to indicate what happened.
 */
#define SENSOR_APPFOCUS_START sa_typical_symbian_sensor_start(self->iSensor_appfocus, "failed to start appfocus scanning")
#define SENSOR_BTPROX_START sa_typical_symbian_sensor_start(self->iSensor_btprox, "failed to start btprox scanning")
#define SENSOR_GPS_START sa_typical_symbian_sensor_start(self->iSensor_gps, "failed to start gps scanning")
#define SENSOR_INACTIVITY_START sa_typical_symbian_sensor_start(self->iSensor_inactivity, "failed to start inactivity scanning")
#define SENSOR_INDICATOR_START sa_typical_symbian_sensor_start(self->iSensor_indicator, "failed to start indicator scanning")
#define SENSOR_KEYPRESS_START sa_typical_symbian_sensor_start(self->iSensor_keypress, "failed to start keypress scanning")
#define SENSOR_PROFILE_START sa_typical_symbian_sensor_start(self->iSensor_profile, "failed to start profile scanning")
#define SENSOR_TIMER_START { success = sa_Sensor_timer_start(self->iSensor_timer, error); }
#define SENSOR_MARK_START { success = sa_Sensor_mark_start(self->iSensor_mark, error); }

/* Sensor stopping. (Statement.) */
#define SENSOR_APPFOCUS_STOP { self->iSensor_appfocus->Stop(); }
#define SENSOR_BTPROX_STOP { self->iSensor_btprox->Stop(); }
#define SENSOR_GPS_STOP { self->iSensor_gps->Stop(); }
#define SENSOR_INACTIVITY_STOP { self->iSensor_inactivity->Stop(); }
#define SENSOR_INDICATOR_STOP { self->iSensor_indicator->Stop(); }
#define SENSOR_KEYPRESS_STOP { self->iSensor_keypress->Stop(); }
#define SENSOR_PROFILE_STOP { self->iSensor_profile->Stop(); }
#define SENSOR_TIMER_STOP { sa_Sensor_timer_stop(self->iSensor_timer); }
#define SENSOR_MARK_STOP { sa_Sensor_mark_stop(self->iSensor_mark); }

/* Sensor running querying. (Boolean expression.) */
#define SENSOR_APPFOCUS_IS_RUNNING (self->iSensor_appfocus->IsActive())
#define SENSOR_BTPROX_IS_RUNNING (self->iSensor_btprox->IsActive())
#define SENSOR_GPS_IS_RUNNING (self->iSensor_gps->IsActive())
#define SENSOR_INACTIVITY_IS_RUNNING (self->iSensor_inactivity->IsActive())
#define SENSOR_INDICATOR_IS_RUNNING (self->iSensor_indicator->IsActive())
#define SENSOR_KEYPRESS_IS_RUNNING (self->iSensor_keypress->IsActive())
#define SENSOR_PROFILE_IS_RUNNING (self->iSensor_profile->IsActive())
#define SENSOR_TIMER_IS_RUNNING (sa_Sensor_timer_is_active(self->iSensor_timer))
#define SENSOR_MARK_IS_RUNNING (sa_Sensor_mark_is_active(self->iSensor_mark))

/* Sensor destruction. (Statement.) */
#define SENSOR_APPFOCUS_DESTROY { delete self->iSensor_appfocus; self->iSensor_appfocus = NULL; }
#define SENSOR_BTPROX_DESTROY { delete self->iSensor_btprox; self->iSensor_btprox = NULL; }
#define SENSOR_GPS_DESTROY { delete self->iSensor_gps; self->iSensor_gps = NULL; }
#define SENSOR_INACTIVITY_DESTROY { delete self->iSensor_inactivity; self->iSensor_inactivity = NULL; }
#define SENSOR_INDICATOR_DESTROY { delete self->iSensor_indicator; self->iSensor_indicator = NULL; }
#define SENSOR_KEYPRESS_DESTROY { delete self->iSensor_keypress; self->iSensor_keypress = NULL; }
#define SENSOR_PROFILE_DESTROY { delete self->iSensor_profile; self->iSensor_profile = NULL; }
#define SENSOR_TIMER_DESTROY { sa_Sensor_timer_destroy(self->iSensor_timer); }
#define SENSOR_MARK_DESTROY { sa_Sensor_mark_destroy(self->iSensor_mark); }

/* Sensor creation. (Statement.) 
   Must set "success" (gboolean) and "error" (GError**) to indicate what happened.
*/
#define SENSOR_PROFILE_CREATE sa_typical_symbian_sensor_create(self->iSensor_profile = CSensor_profile::NewL(self->logDb), "profile sensor initialization")
#define SENSOR_BTPROX_CREATE sa_typical_symbian_sensor_create(self->iSensor_btprox = CSensor_btprox::NewL(self->logDb), "btprox sensor initialization")
#define SENSOR_GPS_CREATE sa_typical_symbian_sensor_create(self->iSensor_gps = CSensor_gps::NewL(self->logDb), "gps sensor initialization")
#define SENSOR_INACTIVITY_CREATE sa_typical_symbian_sensor_create(self->iSensor_inactivity = CSensor_inactivity::NewL(self->ac), "inactivity sensor initialization")
#define SENSOR_INDICATOR_CREATE sa_typical_symbian_sensor_create(self->iSensor_indicator = CSensor_indicator::NewL(self->ac), "indicator sensor initialization")
#define SENSOR_APPFOCUS_CREATE sa_typical_symbian_sensor_create(self->iSensor_appfocus = CSensor_appfocus::NewL(self->logDb), "appfocus sensor initialization")
#define SENSOR_KEYPRESS_CREATE sa_typical_symbian_sensor_create(self->iSensor_keypress = CSensor_keypress::NewL(self->logDb), "keypress sensor initialization")
#define SENSOR_TIMER_CREATE { self->iSensor_timer = sa_Sensor_timer_new(self->logDb, error); success = (self->iSensor_timer != NULL); }
#define SENSOR_MARK_CREATE { self->iSensor_mark = sa_Sensor_mark_new(self->logDb, error); success = (self->iSensor_mark != NULL); }

#define reconfigure_not_supported_by_component(key) { \
    if (error) \
      *error = gx_error_new(domain_cl2app, code_not_supported, "configuration key '%s' not supported by concerned component", key); \
    success = FALSE; \
  }

/* Sensor reconfiguring. (Statement.) */
#define SENSOR_APPFOCUS_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#define SENSOR_BTPROX_RECONFIGURE(key,value) sa_typical_symbian_sensor_reconfigure(btprox)
#define SENSOR_GPS_RECONFIGURE(key,value) sa_typical_symbian_sensor_reconfigure(gps)
#define SENSOR_INACTIVITY_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#define SENSOR_INDICATOR_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#define SENSOR_KEYPRESS_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#define SENSOR_PROFILE_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#define SENSOR_TIMER_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys
#define SENSOR_MARK_RECONFIGURE(key,value) sa_reconfigure_ignore_all_keys

// Defined for sa_sensor_list_integration.h.
// We only consider OOM errors as fatal.
#define handle_any_sensor_start_failure {		\
    if ((!success) && error) {				\
      if (*error == gx_error_no_memory)			\
	gx_dblog_fatal_error_clear(self->logDb, error);	\
      else						\
	gx_dblog_error_clear(self->logDb, error);	\
    }							\
  }

static gboolean sensor_autostart_is_allowed(const gchar* cfg_key)
{
  gboolean dvalue = TRUE; // default value
#if __GPS_ENABLED__ && !__IS_DEMO__
  // GPS autostart can cause problems due to phones without integrated
  // GPS looking for an external GPS device via Bluetooth, so we do
  // not generally have this on by default.
  if (strcmp(cfg_key, "sensor.gps.autostart") == 0)
    dvalue = FALSE;
#endif
  return force_get_ConfigDb_bool(cfg_key, dvalue);
}

#define SENSOR_AUTOSTART_IS_ALLOWED(_name) \
  sensor_autostart_is_allowed("sensor." #_name ".autostart")

/** Instantiates a sensor array consisting of all supported sensors. */
extern "C" sa_Array *sa_Array_new(ac_AppContext* ac,
				  GError** error)
{
  sa_Array* self = g_try_new0(sa_Array, 1);
  if (G_UNLIKELY(!self)) {
    if (error) *error = gx_error_no_memory; // out of memory
    return NULL;
  }

  self->ac = ac;
  self->logDb = ac_LogDb(ac);

  gboolean success; // for the macro

  // We indeed require that it be possible to instantiate every single
  // sensor that is supported. Starting them up is another matter.
  CREATE_ALL_SENSORS_OR_FAIL;

  return self;

 fail:
  logt("some sensor failed to initialize");
  sa_Array_destroy(self);
  return NULL;
}
  
/** Starts all supported sensors. */
extern "C" void sa_Array_start(sa_Array* self)
{
  gboolean success = TRUE;
  GError* localError = NULL;
  GError** error = &localError;

  // Errors will be logged and cleared.
  TRY_START_ALL_SUPPORTED_SENSORS;
}
  
/** Stops all supported sensors. */
extern "C" void sa_Array_stop(sa_Array* self)
{
  STOP_ALL_SUPPORTED_SENSORS;
}
  
/** Destroys a sensor array. Naturally all the sensors in it are stopped. */
extern "C" void sa_Array_destroy(sa_Array* self)
{
  if (self) {
    // We assume that destroying a sensor also stops it, as we are not
    // requesting stopping separately. In fact here we cannot even
    // assume all of the sensors objects have been created.
    DESTROY_ALL_SENSORS;
    logt("all sensors destroyed");
    g_free(self);
    logt("sensor array object freed");
  }
}
  
/** Returns FALSE for unknown names. */
extern "C" gboolean sa_sensor_is_supported(const gchar* name)
{
  RETURN_WHETHER_NAMED_SENSOR_IS_SUPPORTED(name);
  return FALSE;
}

extern "C" gboolean sa_Array_sensor_is_running(sa_Array* self, const gchar* name)
{
  // This macro requires that you implement the macro
  // SENSOR_<NAME>_IS_RUNNING for each supported sensor.
  // SENSOR_<NAME>_IS_RUNNING need not check whether the sensor is
  // supported; non-supported sensors are not running.
  RETURN_WHETHER_NAMED_SENSOR_IS_RUNNING(name);
  return FALSE;
}

extern "C" void sa_Array_sensor_stop(sa_Array* self, const gchar* name)
{
  // This macro requires that you implement the macro
  // SENSOR_<NAME>_STOP for each supported sensor. SENSOR_<NAME>_STOP
  // need not check whether the sensor is supported or running.
  STOP_NAMED_SENSOR(name);
}

// We try to have decent error reporting here, with interactive starting attempts in mind.
extern "C" gboolean sa_Array_sensor_start(sa_Array* self, const gchar* name, GError** error)
{
  // This macro requires that you implement the macro
  // SENSOR_<NAME>_START for each supported sensor. SENSOR_<NAME>_START
  // need not check whether the sensor is supported or running.
  gboolean success;
  START_NAMED_SENSOR_OR_FAIL(name);

  if (error)
    *error = gx_error_new(domain_cl2app, code_not_supported, "sensor '%s' not supported", name);
  return FALSE;
}

extern "C" gboolean sa_Array_reconfigure(sa_Array* self, const gchar* key, const gchar* value, GError** error)
{
  (void)self;
  (void)value;
  (void)error;

  {
    gboolean success;
    // This macro returns if it finds a match.
    RECONFIGURE_MATCHING_SENSOR(key, value);
  }

  /*
  if (error)
    *error = gx_error_new(domain_cl2app, code_not_supported, "configuration key '%s' does not concern any supported component", key);
  return FALSE;
  */

  // We shall not complain about unsupported keys. It is possible that
  // some sensor is not compiled in, or not yet supported, or
  // whatever; why should the user not be able to add a config entry
  // for it anyway. If the entry need not be propagated at this time,
  // then the propagation surely does not fail.
  return TRUE;
}

/**

sa_array.cpp

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

 **/

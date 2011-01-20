#include "epoc-gps.hpp"

#include "epoc-gps-positioner.hpp"

#include "cf_query.h"
#include "er_errors.h"
#include "kr_controller_private.h"
#include "sa_sensor_list_log_db.h"

#include "common/assertions.h"
#include "common/epoc-time.h"
#include "common/error_list.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/utilities.h"

#include <e32math.h> 

#include <stdio.h>
#include <string.h>

// For docs, see for instance
// http://www.forum.nokia.com/document/CDL_Extension_S60_3rd_Ed_FP2/GUID-759FBC7F-5384-4487-8457-A8D4B76F6AA6/html/classRPositionServer.html.
//
// Also worth looking at src/ext/gps of PyS60.
//
// Especially useful is the Location Acquisition API Specification PDF.

// See also: http://wiki.forum.nokia.com/index.php/KIS000850_-_RPositioner::NotifyPositionUpdate_return_KErrInUse_after_few_minutes

// -------------------------------------------------------------------

#define DEFAULT_POSITION_SCAN_INTERVAL_SECS (5 * 60)

// -------------------------------------------------------------------

CSensor_gps* CSensor_gps::NewL(ac_AppContext* aAppContext)
{
  CSensor_gps* obj = new (ELeave) CSensor_gps(aAppContext);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop();
  return obj;
}

CSensor_gps::CSensor_gps(ac_AppContext* aAppContext) : 
  CActiveRunG(EPriorityStandard),
  iAppContext(aAppContext)
{
  iLogDb = ac_LogDb(aAppContext);
  iPositionUpdateIntervalSecs = DEFAULT_POSITION_SCAN_INTERVAL_SECS;
  CActiveScheduler::Add(this);
}

void CSensor_gps::ConstructL()
{
  RefreshPositionUpdateIntervalSecs();
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iPositionServer, iPositionServer.Connect());
  iPositionModuleStatusEvent.SetRequestedEvents(TPositionModuleStatusEventBase::EEventDeviceStatus|TPositionModuleStatusEventBase::EEventSystemModuleEvent);
}

CSensor_gps::~CSensor_gps()
{
  DELETE_Z(iPositioner);
  Cancel(); // safe when AO inactive as DoCancel not called
  SESSION_CLOSE_IF_OPEN(iPositionServer);
}

gboolean CSensor_gps::StartL(GError** error)
{
  if (!IsActive()) {
    Stop(); // ensure stopped
    
    CreateBestPositionerL();

    // Start observing for module status events, as such events might
    // call for changes in the way we do or should do positioning.
    MakeRequest();

    log_db_log_status(iLogDb, NULL, "gps sensor started");
  }

  return TRUE;
}

void CSensor_gps::Stop()
{
  if (IsActive()) {
    Cancel();
    
    // Stop positioner, if any.
    DELETE_Z(iPositioner);

    log_db_log_status(iLogDb, NULL, "gps sensor stopped");
  }
}

void CSensor_gps::MakeRequest() 
{
  iPositionServer.NotifyModuleStatusEvent(iPositionModuleStatusEvent, iStatus);
  SetActive();
}
  
void CSensor_gps::CreateBestPositionerL()
{
  TPositionModuleId bestId;
  if (ChooseBestPositionerL(bestId))
    CreateSpecifiedPositionerL(bestId);
}

void CSensor_gps::CreateSpecifiedPositionerL(TPositionModuleId bestId)
{
  iNumScanFailures = 0;
  iPositioner = CPositioner_gps::NewL(iPositionServer, *this, bestId, iPositionUpdateIntervalSecs, 0);
  iPositioner->MakeRequest();
}

// Returns true iff there was anything good enough to choose. In that
// case the module ID will be returned as well.
TBool CSensor_gps::ChooseBestPositionerL(TPositionModuleId& aBestId)
{
  TUint numModules;
  User::LeaveIfError(iPositionServer.GetNumModules(numModules));
  TUint i;
  TPositionModuleInfo moduleInfo;
  int bestScore = -1;
  int bestIndex = -1;
  for (i=0; i<numModules; i++) {
    User::LeaveIfError(iPositionServer.GetModuleInfoByIndex(i, moduleInfo));

#if __DO_LOGGING__
    {
      TBuf<KPositionMaxModuleName> moduleName;
      moduleInfo.GetModuleName(moduleName);
      gchar* nameString = ConvToUtf8CStringL(moduleName);
      logg("considering positioning module '%s'", nameString);
      g_free(nameString);
    }
#endif

    TPositionModuleStatus moduleStatus;
    TInt moduleStatusError = iPositionServer.GetModuleStatus(moduleStatus, moduleInfo.ModuleId());
    
    if (moduleInfo.IsAvailable() &&
	!moduleStatusError && 
	(moduleStatus.DeviceStatus() != TPositionModuleStatus::EDeviceDisabled) &&
	(moduleInfo.Capabilities() & TPositionModuleInfo::ECapabilitySatellite) &&
	(moduleInfo.TechnologyType() != TPositionModuleInfo::ETechnologyUnknown) &&
	(moduleInfo.TechnologyType() != TPositionModuleInfo::ETechnologyNetwork) &&
	(moduleInfo.DeviceLocation() != TPositionModuleInfo::EDeviceUnknown)) {
      int score = 0;
      if (moduleInfo.DeviceLocation() == TPositionModuleInfo::EDeviceExternal) {
	/*
	  // Not required: Bluetooth GPS can be disabled globally in
          // the device settings. See the Position application in 
          // S60 3.0 devices, and the settings dialog in later devices.
	  if (iPlatformVersion.iMajor == 3 && iPlatformVersion.iMinor == 0)
	  // Avoid the BT device search dialog.
	  continue;
	*/
	score += 0xf;
      }
      if (moduleInfo.TechnologyType() == TPositionModuleInfo::ETechnologyAssisted)
	score += 1;
      logg("module score is %d", score);
      if (score > bestScore) {
	bestScore = score;
	bestIndex = i;
      }
    }
  }
  if (bestIndex >= 0) {
    User::LeaveIfError(iPositionServer.GetModuleInfoByIndex(bestIndex, moduleInfo));

    {
      TBuf<KPositionMaxModuleName> moduleName;
      moduleInfo.GetModuleName(moduleName);
      gchar* nameString = ConvToUtf8CStringL(moduleName);
      dblogg("chose positioning module '%s'", nameString);
      guilogf("gps: using module '%s'", nameString);
      g_free(nameString);
    }

    aBestId = moduleInfo.ModuleId();
    return ETrue;
  }
  return EFalse;
}

// Cannot use the current positioner any longer, so delete it, and try
// to create a new one.
void CSensor_gps::CurrentModuleUnavailable()
{
  DELETE_Z(iPositioner);
  TRAPD(errCode, CreateBestPositionerL());
  if (errCode) {
    // If we cannot initialize with the best one, then in theory
    // we could try less good ones, but let us not go there
    // without good reason.
    log_db_log_status(iLogDb, NULL, "ERROR: handling present positioning module becoming unavailable failed: %s (%d)", plat_error_strerror(errCode), errCode);
  }
}

// Is the newly ready device something we want to use; we may or may
// not have a current one.
void CSensor_gps::NewModuleAvailable()
{
  TPositionModuleId bestId;
  TRAPD(errCode, {
      if (ChooseBestPositionerL(bestId)) {
	if (!iPositioner || (bestId != iPositioner->ModuleId())) {
	  DELETE_Z(iPositioner);
	  CreateSpecifiedPositionerL(bestId);
	}
      }});
  if (errCode) {
    log_db_log_status(iLogDb, NULL, "ERROR: handling new positioning module availability failed: %s (%d)", plat_error_strerror(errCode), errCode);
  }
}

// Invoked upon a NotifyModuleStatusEvent completion.
gboolean CSensor_gps::RunGL(GError** error) 
{
  assert_error_unset(error);

  TInt errCode = iStatus.Int();
  dblogg("positioning module status event (%d)", errCode);

  if (errCode) {
    Stop();
    if (!log_db_log_status(iLogDb, error, "INACTIVATE: gps: failure observing positioning module status: %s (%d)", plat_error_strerror(errCode), errCode)) {
      return FALSE;
    }
  } else {
    // We are interested in these kinds of events:
    // * current module is removed
    // * currently used module stops working in some way
    // * some other (possibly better) module becomes available
    // * a new (possibly better) module is installed
    // Note also that there might not be a current module at all.

    TPositionModuleId moduleId = iPositionModuleStatusEvent.ModuleId();
    TBool haveCurrent = EFalse;
    TBool aboutCurrent = EFalse;
    if (iPositioner) {
      haveCurrent = ETrue;
      TPositionModuleId currentId = iPositioner->ModuleId();
      if (moduleId == currentId) {
	aboutCurrent = ETrue;
      }
    }

    TPositionModuleStatus moduleStatus;
    iPositionModuleStatusEvent.GetModuleStatus(moduleStatus);
    
    TPositionModuleStatusEventBase::TModuleEvent occurredEvents = iPositionModuleStatusEvent.OccurredEvents();
    logg("occurred position events: %d", (int)occurredEvents);
    if (occurredEvents & TPositionModuleStatusEventBase::EEventDeviceStatus) {
      TPositionModuleStatus::TDeviceStatus deviceStatus = moduleStatus.DeviceStatus();
      dblogg("device status now %d", (int)deviceStatus);
      if (aboutCurrent && 
	  ((deviceStatus == TPositionModuleStatus::EDeviceDisabled) ||
	   (deviceStatus == TPositionModuleStatus::EDeviceError) ||
           // Should not become inactive when it is something we are
           // using. This may indicate it was disabled in the
           // configuration.
	   (deviceStatus == TPositionModuleStatus::EDeviceInactive))) {
	CurrentModuleUnavailable();
      } else if (!aboutCurrent &&
		 ((deviceStatus == TPositionModuleStatus::EDeviceReady) ||
                  // Again, this may signal that another module has
                  // become available but is not yet active as it is
                  // not used yet. It is worth checking if it is
                  // listed as available.
		  (deviceStatus == TPositionModuleStatus::EDeviceInactive))) {
	NewModuleAvailable();
      }
    }

    // Can it ever really happen that we get both a system module
    // event and a non-system module event? We shall deem this
    // unlikely, and will not deal with such situations in a very
    // optimal way necessarily.
    if (occurredEvents & TPositionModuleStatusEventBase::EEventSystemModuleEvent) {
      TPositionModuleStatusEventBase::TSystemModuleEvent systemModuleEvent = iPositionModuleStatusEvent.SystemModuleEvent();
      dblogg("system module event was %d", (int)systemModuleEvent);
      if (aboutCurrent &&
	  ((systemModuleEvent == TPositionModuleStatusEventBase::ESystemError) || (systemModuleEvent == TPositionModuleStatusEventBase::ESystemModuleRemoved))) {
	CurrentModuleUnavailable();
      } else if (!aboutCurrent &&
		 ((systemModuleEvent == TPositionModuleStatusEventBase::ESystemModuleInstalled))) {
	NewModuleAvailable();
      }
    }

    MakeRequest();
  }

  return TRUE;
}

gboolean CSensor_gps::PositionerEventL(GError** error)
{
  assert_error_unset(error);
  assert(iPositioner);

  // See "lbserrors.h" for LBS-specific return codes for
  // NotifyPositionUpdate requests. Note that somewhat unusually there
  // are some positive values as well.
  TInt errCode = iPositioner->StatusCode();
  
  if (errCode == KErrTimedOut) {
    // Can expect to get this after the time period specified with
    // SetUpdateTimeOut.
    logt("gps request timed out, retrying");
    iPositioner->MakeRequest();
  } else if (errCode == KErrCancel) {
    // Not really expected here, but whatever. Do nothing.
  } else if (errCode < 0) {
    // xxx Do not yet quite know what sort of errors might be getting,
    // so shall merely log errors and keep retrying. Do want to make
    // sure, however, that we do not get an awful lot of errors
    // growing our log file to an unreasonable size, and hence will
    // stop the scanner if there are lots of consecutive error
    // completions. We can be cleverer here once we know what sort of
    // errors we might be getting. But it is reasonable to assume that
    // there may be immediate error returns in cases such as a
    // positioning module being or having become unavailable.
    iNumScanFailures++;
    dblogg("%dth consecutive failure in gps: %s (%d)", iNumScanFailures, plat_error_strerror(errCode), errCode);
    // xxx maybe should support KErrServerBusy by trying again only after a small delay
    switch (errCode) {
    case KErrAccessDenied: // Perhaps some capability thing.
      // Locally severe error. Give up.
      log_db_log_status(iLogDb, NULL, "INACTIVATE: gps: scanner cannot continue");
      break;
    case KErrArgument:
    case KErrPositionBufferOverflow:
      assert(0 && "unexpected gps error");
      break;
    default:
      if (iNumScanFailures < 100) {
	iPositioner->MakeRequest();
      } else {
	log_db_log_status(iLogDb, NULL, "INACTIVATE: gps: stopping scanning due to too many errors");
      }
    }
  } else {
    iNumScanFailures = 0;
    if (errCode == KPositionQualityLoss) {
      // The positioning module could not get any information.
      logt("no gps info available");
    } else if (errCode == KErrNone || errCode == KPositionPartialUpdate) {
      // Now this data can get quite complicated and I am not sure it
      // makes sense for us to start cramming it into a variety of
      // fields and tables for fancy querying unless we have a clear
      // requirement to support certain kinds of queries efficiently. So
      // we shall merely record it all into a single field in some
      // textual format, at least for the time being. We shall choose
      // Lua expression syntax for this purpose. Evaluating such strings
      // can be done with assert(loadstring(s))().
#if __DO_LOGGING__
      TBool isPartial = (errCode == KPositionPartialUpdate);
      logg("got %s gps reading", isPartial ? "partial" : "full");
#endif

      // Is it so that if some module does not support satellite info
      // then if we try to request it we get nothing? Well, we are
      // only choosing GPS modules, and GPS is based on satellites, so
      // surely there always is satellite info.

      const TPositionSatelliteInfo& positionInfo = iPositioner->PositionInfo();

#define SAT_STRING_MAX 300
#define COU_STRING_MAX 300
      gchar satString[SAT_STRING_MAX];
      gchar couString[COU_STRING_MAX];
      satString[0] = '\0';
      couString[0] = '\0';

      if (positionInfo.PositionClassType() & EPositionSatelliteInfoClass) {
	TInt satellites = positionInfo.NumSatellitesInView();
	TInt used_satellites = positionInfo.NumSatellitesUsed();
	TReal32 horizontal_dop = positionInfo.HorizontalDoP();
	TReal32 vertical_dop = positionInfo.VerticalDoP();
	TReal32 time_dop = positionInfo.TimeDoP();
	TTime satTime = positionInfo.SatelliteTime();

#if __DO_LOGGING__
#define TIME_STRING_MAX 50
	_LIT(KDateTimeFormat, "%F%Y-%M-%D %H:%T:%S.%C");
        // Note that KMaxTimeFormatSpec is the max size of the format
        // spec, and not the result, as I understand it.
	TBuf<TIME_STRING_MAX> satTime16;
        // The leave will not happen if the format string and data are
        // valid and the destination buffer is large enough.
	satTime.FormatL(satTime16, KDateTimeFormat);
	gchar satTime8[TIME_STRING_MAX + 1];
	ConvToUtf8CString(satTime8, TIME_STRING_MAX, satTime16);

	logg("satellite info: num satellites in view %d, num used satellites %d, hdop %g, vdop %g, tdop %g, satellite time %s", satellites, used_satellites, horizontal_dop, vertical_dop, time_dop, satTime8);
#endif

	// We convert Symbian time to Unix time for consistency.
	time_t satUnixTime = LocalEpocTimeToUnixTime(satTime); // assuming 32-bit int

	// Build 'satString' for database insertion.
	{
	  // "satellites": {"horizontal_dop": 2.20000004768372,
	  // "used_satellites": 4, "vertical_dop": 1.0, "time":
	  // 1225980390.097, "satellites": 10, "time_dop": 1.0}
	  GString* gs = NULL;
	  SET_TRAP_OOM(if (gs) g_string_free(gs, TRUE);
		       User::LeaveNoMemory());
	  gs = g_string_sized_new(128);
	  g_string_printf(gs, "{time: %d", (int)satUnixTime);
	  if (!Math::IsNaN(satellites))
	    g_string_append_printf(gs, ", satellites: %d", satellites);
	  if (!Math::IsNaN(used_satellites))
	    g_string_append_printf(gs, ", used_satellites: %d", used_satellites);
	  if (!Math::IsNaN(horizontal_dop))
	    g_string_append_printf_fix(gs, ", horizontal_dop: %.6f", (double)horizontal_dop);
	  if (!Math::IsNaN(vertical_dop))
            // %f and %g seem to be a bit broken for this function at
            // least, and maybe also for the other GLib printf
            // functions. We can try a more recent Open C, or
            // implement a replacement using C99 or Symbian functions.
            // Or possibly we could backport a more recent version of
            // the function in question from GLib, if there are not
            // too many internal dependencies.
	    g_string_append_printf_fix(gs, ", vertical_dop: %.6f", (double)vertical_dop);
	  if (!Math::IsNaN(time_dop))
	    g_string_append_printf_fix(gs, ", time_dop: %.6f", (double)time_dop);
	  g_string_append_c(gs, '}');
	  UNSET_TRAP_OOM();

	  if (gs->len < SAT_STRING_MAX)
	    // For easier memory management.
	    strcpy(satString, gs->str);
	  else
	    // Did not fit fully, do not log partial data that may not
	    // be parseable.
	    satString[0] = '\0';
	  g_string_free(gs, TRUE);
	  logt(satString);
	}
      }

      if (positionInfo.PositionClassType() & EPositionCourseInfoClass) {
	TCourse course;
	positionInfo.GetCourse(course);
	TReal32 speed = course.Speed();
	TReal32 heading = course.Heading();
	TReal32 course_value = course.Course();
	TReal32 speed_accuracy = course.SpeedAccuracy();
	TReal32 heading_accuracy = course.HeadingAccuracy();
	TReal32 course_accuracy = course.CourseAccuracy();
	logg("course info: speed %g m/s, heading %g deg, course %g, speed_accuracy %g m/s, heading_accuracy %d deg, course_accuracy %g", speed, heading, course_value, speed_accuracy, heading_accuracy, course_accuracy);

	// "course": {"speed": 6.0, "heading": 305.880004882812, "heading_accuracy": 5.80999994277954, "speed_accuracy": 8.35999965667725
	{
#define APPEND_STRING(s) { int slen = strlen(s); if (space <= slen) goto fail; space -= slen; strcpy(output, s);  output += slen; }
#define APPEND_SEP { if (!first) APPEND_STRING(", "); }
#define COU_UPDATE_STATE(r) { if (r < 0 || r >= space) goto fail; space -= r; output += r; first = EFalse; }
	  int space = COU_STRING_MAX;
	  int r;
	  TBool first = ETrue;
	  gchar* output = couString;
	  APPEND_STRING("{");
	  if (!Math::IsNaN(speed)) { r = g_snprintf_fix(output, space, "speed: %.6f", (double)speed); COU_UPDATE_STATE(r); }
	  if (!Math::IsNaN(heading)) { APPEND_SEP; r = g_snprintf_fix(output, space, "heading: %.6f", (double)heading); COU_UPDATE_STATE(r); }
	  if (!Math::IsNaN(course_value)) { APPEND_SEP; r = g_snprintf_fix(output, space, "course: %.6f", (double)course_value); COU_UPDATE_STATE(r); }
	  if (!Math::IsNaN(speed_accuracy)) { APPEND_SEP; r = g_snprintf_fix(output, space, "speed_accuracy: %.6f", (double)speed_accuracy); COU_UPDATE_STATE(r); }
	  if (!Math::IsNaN(heading_accuracy)) { APPEND_SEP; r = g_snprintf_fix(output, space, "heading_accuracy: %.6f", (double)heading_accuracy); COU_UPDATE_STATE(r); }
	  if (!Math::IsNaN(course_accuracy)) { APPEND_SEP; r = g_snprintf_fix(output, space, "course_accuracy: %.6f", (double)course_accuracy); COU_UPDATE_STATE(r); }
	  APPEND_STRING("}");
	  goto done;
	fail:
	  couString[0] = '\0';
	done: ;
	  logt(couString);
	}
      }

      if (positionInfo.PositionClassType() & EPositionInfoClass) {
	TPosition position;
	positionInfo.GetPosition(position);
        // It seems that with network based positioning we only get
        // lat and lon and hacc, and the rest are NaN.
	TReal64 lat = position.Latitude();
	TReal64 lon = position.Longitude();
	// To use Google Maps to look these up, can use something like http://maps.google.com/maps?q=60.187537,+24.804940&hl=en
	TReal32 alt = position.Altitude();
	TReal32 vacc = position.VerticalAccuracy();
	TReal32 hacc = position.HorizontalAccuracy();
	//logg("position info: latitude %lg deg, longitude %lg deg, altitude %g m, vertical_accuracy %g m, horizontal_accuracy %g m", lat, lon, alt, vacc, hacc);
	guilogf("gps: lat %lg, lon %lg, alt %g, vacc %g m, hacc %g m", lat, lon, alt, vacc, hacc);
	if (!Math::IsNaN(lat) &&
	    !Math::IsNaN(lon)) {
	  if (!log_db_log_gps(iLogDb, lat, lon, alt, vacc, hacc, couString, satString, error)) {
	    return FALSE;
	  }
	} else {
	  logt("supposedly a full reading has NaN value(s) in coordinates");
	}
      }
    } else {
      dblogg("warning, unknown gps request status code (%d)", errCode);
    }
    iPositioner->MakeRequest();
  }

  return TRUE;
}

const char* CSensor_gps::Description() 
{
  return "gps";
}
  
void CSensor_gps::DoCancel() 
{
  // Ignoring the undocumented error code.
  iPositionServer.CancelRequest(EPositionServerNotifyModuleStatusEvent);
}

void CSensor_gps::Reconfigure(const gchar* name, const gchar* value)
{
  if (strcmp(name, "sensor.gps.interval") == 0) {
    RefreshPositionUpdateIntervalSecs();
  }
}

void CSensor_gps::RefreshPositionUpdateIntervalSecs()
{
  // Takes effect when we next create a positioner object.
  try_get_ConfigDb_int("sensor.gps.interval",
		       &iPositionUpdateIntervalSecs,
		       NULL, NULL);
}

/**

epoc-gps.cpp

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

#include "epoc-gps.hpp"

#include "epoc-gps-module.hpp"
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
  iAppContext(aAppContext),
  iModuleId(KPositionNullModuleId)
{
  iLogDb = ac_LogDb(aAppContext);
  iPositionUpdateIntervalSecs = DEFAULT_POSITION_SCAN_INTERVAL_SECS;
}

void CSensor_gps::ConstructL()
{
  RefreshPositionUpdateIntervalSecs();
  iRetryAo = CRetryAo::NewL(*this, 30, 120);
  iModuleAo = CPosModuleStatAo::NewL(*this);
}

CSensor_gps::~CSensor_gps()
{
  Stop();
  delete iRetryAo;
  delete iModuleAo;
}

void CSensor_gps::Stop()
{
  iRetryAo->Cancel();
  iModuleAo->Cancel();
  DELETE_Z(iPositioner);
  iState = EInactive;
}

void CSensor_gps::StartL()
{
  if (!IsActive()) {
    iRetryAo->ResetFailures();

    iModuleId = KPositionNullModuleId;

    TPositionModuleId bestId = iModuleAo->ChooseBestPositionerL();

    if (bestId != KPositionNullModuleId) {
      TRAPD(errCode, CreateSpecifiedPositionerL(bestId));
      if (errCode) {
	er_log_symbian(0, errCode, "WARNING: failed to create positioner");
      }
    }

    // Observe changes in module status.
    iModuleAo->MakeRequest();

    iState = EActive;
  }
}

void CSensor_gps::CreateSpecifiedPositionerL(TPositionModuleId bestId)
{
  // This is just the module we last tried to use; we do not require
  // we actually succeed in creating the positioner for it.
  iModuleId = bestId;

  RPositionServer& server = iModuleAo->PositionServer();
  iPositioner = CPositioner_gps::NewL(server, *this, 
				      bestId, iPositionUpdateIntervalSecs, 0);
  iPositioner->MakeRequest();
}

void CSensor_gps::PosModChangeL()
{
  TPositionModuleId bestId = iModuleAo->ChooseBestPositionerL();
  if (bestId != iModuleId) {
    iRetryAo->Cancel();
    iRetryAo->ResetFailures();
    DELETE_Z(iPositioner);

    if (bestId != KPositionNullModuleId) {
      TRAPD(errCode, CreateSpecifiedPositionerL(bestId));
      if (errCode) {
	er_log_symbian(0, errCode, "WARNING: failed to create positioner");
      }
    }
  }
  iModuleAo->MakeRequest();
}

TBool CSensor_gps::PosModIsCurrent(TPositionModuleId id) const
{
  return (iModuleId != KPositionNullModuleId) && (iModuleId == id);
}

void CSensor_gps::PosModErrorL(TInt errCode)
{
  er_log_symbian(0, errCode, "INACTIVATE: gps: positioning module status tracking error");
  Stop();
}

// Called if PosModChangeL or PosModErrorL leaves.
void CSensor_gps::PosModLeave(TInt errCode)
{
  er_log_symbian(er_FATAL, errCode, "gps: leave in positioning module status reporting handler");
}

void CSensor_gps::RetryTimerExpired(CRetryAo* src, TInt errCode)
{
  if (errCode) {
    er_log_symbian(er_FATAL, errCode, "gps: retry timer error");
    return;
  }
  assert(iPositioner);
  iPositioner->MakeRequest();
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
    // SetUpdateTimeOut. We haven't set such a time period, though,
    // and presumably then should not get this.
    logt("gps request timed out (unexpectedly), retrying");
    iPositioner->MakeRequest();
  } else if (errCode == KErrCancel) {
    // Not really expected here, but whatever. Do nothing.
  } else if (errCode < 0) {
    // Do not yet quite know what sort of errors might be getting, so
    // shall merely log errors and keep retrying. Do want to make
    // sure, however, that we do not get an awful lot of errors
    // growing our log file to an unreasonable size, and hence will
    // stop the scanner if there are lots of consecutive error
    // completions. We can be cleverer here once we know what sort of
    // errors we might be getting. But it is reasonable to assume that
    // there may be immediate error returns in cases such as a
    // positioning module being or having become unavailable.
    switch (errCode) {
    case KErrAccessDenied: // Perhaps some capability thing.
      // Locally severe error. Give up.
      er_log_symbian(0, errCode, "gps: positioner error, giving up on positioner");
      // We will not reset state further, lest we be asked to switch
      // back to the same module again.
      DELETE_Z(iPositioner);
      break;
    case KErrArgument:
    case KErrPositionBufferOverflow:
      er_log_symbian(er_FATAL, errCode, "gps: unexpected positioning error");
      break;
    default:
      if (!iRetryAo->Retry()) {
	er_log_symbian(0, errCode, "INACTIVATE: gps: stopping scanning due to too many errors");
	Stop();
      }
    }
  } else {
    iRetryAo->ResetFailures();
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

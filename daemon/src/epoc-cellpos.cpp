/*

References:

http://www.forum.nokia.com/document/Cpp_Developers_Library/GUID-759FBC7F-5384-4487-8457-A8D4B76F6AA6/html/Location_Acquisition_API4.html

*/

#include "epoc-cellpos.hpp"

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

// -------------------------------------------------------------------
// internal parameters...

// We will never ask the positioner for service more frequently than
// this.
#define MIN_SCAN_REQUEST_INTERVAL_SECS (3 * 60)

// We will not drain the battery for longer than this if we do not get
// a position fix, and we call Cancel. If we do get a position, it
// should be enough to not call MakeRequest.
#define SATELLITE_QUERY_TIMEOUT_SECS (60)

// This is given as a parameter to the positioner, and it indicates
// how frequently it should be prepared to provide periodic updates.
// In our case such time periods are irregular, and it probably makes
// sense not to set this to avoid possibly spending battery on
// needless guarantees. It seems this must be shorter than the timeout
// interval, and indeed anything else does not make much sense.
#define POSITIONER_SCAN_INTERVAL_SECS (0)

// -------------------------------------------------------------------

CSensor_cellpos* CSensor_cellpos::NewL(ac_AppContext* aAppContext)
{
  CSensor_cellpos* obj = new (ELeave) CSensor_cellpos(aAppContext);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop();
  return obj;
}

CSensor_cellpos::CSensor_cellpos(ac_AppContext* aAppContext) : 
  iAppContext(aAppContext),
  iModuleId(KPositionNullModuleId)
{
  iLogDb = ac_LogDb(aAppContext);
}

void CSensor_cellpos::ConstructL()
{
  iRetryAo = CRetryAo::NewL(*this, 4, 5); // 4 tries, 5 secs
  iModuleAo = CPosModuleStatAo::NewL(*this);
  iCellChangeHandle.Register(ac_get_Blackboard(iAppContext),
			     bb_dt_cell_id,
			     this);
}

CSensor_cellpos::~CSensor_cellpos()
{
  Stop();
  delete iRetryAo;
  delete iModuleAo;
}

void CSensor_cellpos::Stop()
{
  iRetryAo->Cancel();
  iModuleAo->Cancel();
  DELETE_Z(iPositioner);
  iState = EInactive;
}

void CSensor_cellpos::StartL()
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

void CSensor_cellpos::CreateSpecifiedPositionerL(TPositionModuleId bestId)
{
  // This is just the module we last tried to use; we do not require
  // we actually succeed in creating the positioner for it.
  iModuleId = bestId;

  RPositionServer& server = iModuleAo->PositionServer();
  iPositioner = CPositioner_gps::NewL(server, *this, 
				      bestId, 
				      POSITIONER_SCAN_INTERVAL_SECS,
				      SATELLITE_QUERY_TIMEOUT_SECS);
}

void CSensor_cellpos::PosModChangeL()
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

TBool CSensor_cellpos::PosModIsCurrent(TPositionModuleId id) const
{
  return (iModuleId != KPositionNullModuleId) && (iModuleId == id);
}

void CSensor_cellpos::PosModErrorL(TInt errCode)
{
  er_log_symbian(0, errCode, 
		 "INACTIVATE: cellpos: positioning module status tracking error");
  Stop();
}

// Called if PosModChangeL or PosModErrorL leaves.
void CSensor_cellpos::PosModLeave(TInt errCode)
{
  er_log_symbian(er_FATAL, errCode, "cellpos: leave in positioning module status reporting handler");
}

void CSensor_cellpos::RetryTimerExpired(CRetryAo* src, TInt errCode)
{
  if (errCode) {
    er_log_symbian(er_FATAL, errCode, "cellpos: retry timer error");
    return;
  }
  assert(iPositioner);
  iPositioner->MakeRequest();
}

void CSensor_cellpos::BbChangedL(bb::RHandle* self, enum bb_DataType dt,
				 gpointer data, int len)
{
  if (!IsActive()) 
    return; // stopped
  assert(dt == bb_dt_cell_id);
  guilogf("cellpos: cell changed");
  if (!iPositioner) {
    guilogf("cellpos: no positioner");
    return; // no positioner
  }
  if (iPositioner->IsActive() || iRetryAo->IsActive()) {
    guilogf("cellpos: already positioning");
    return; // doing positioning
  }
  TTime now;
  now.UniversalTime();
  TTime earliestTime(iLastScanTime.Int64());
  earliestTime += TTimeIntervalSeconds(MIN_SCAN_REQUEST_INTERVAL_SECS);
  if (now <= earliestTime) {
    guilogf("cellpos: too early");
    return; // too early
  }
  guilogf("cellpos: positioning...");
  iPositioner->MakeRequest();
}

// xxx this could be shared with other satellite sensors
static void LogSatelliteInfoL(LogDb* aLogDb, const char* aSensorName,
			      const TPositionSatelliteInfo& positionInfo)
{
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
    guilogf("%s: lat %lg, lon %lg, alt %g, vacc %g m, hacc %g m", aSensorName, lat, lon, alt, vacc, hacc);
    if (!Math::IsNaN(lat) &&
	!Math::IsNaN(lon)) {
      log_db_log_position(aLogDb, lat, lon, alt, vacc, hacc, couString, satString, NULL);
    } else {
      logt("supposedly full reading has NaN value(s) in coordinates");
    }
  }
}

gboolean CSensor_cellpos::PositionerEventL(GError** error)
{
  assert_error_unset(error);
  assert(iPositioner);

  iLastScanTime.UniversalTime();

  // See "lbserrors.h" for LBS-specific return codes for
  // NotifyPositionUpdate requests. Note that somewhat unusually there
  // are some positive values as well.
  TInt errCode = iPositioner->StatusCode();

  if (errCode < 0) {
    // Do not yet quite know what sort of errors might be getting, so
    // shall merely log errors and keep retrying. Do want to make
    // sure, however, that we do not get an awful lot of errors
    // growing our log file to an unreasonable size, and hence will
    // stop the scanner if there are lots of consecutive error
    // completions. We can be cleverer here once we know what sort of
    // errors we might be getting. But it is reasonable to assume that
    // there may be immediate error returns in cases such as a
    // positioning module being or having become unavailable.
    switch (errCode) { // see <lbserrors.h>
    case KErrTimedOut:
      // Can expect to get this after the time period specified with
      // SetUpdateTimeOut. If we didn't get the position in that time,
      // then we did not.
      guilogf("cellpos: GPS timeout");
      break;

      guilogf("cellpos: positioning error");

      // Used positioning module became unavailable. (Before we had
      // time to stop using it.)
    case KErrNotFound:
      // Perhaps some capability thing.
    case KErrAccessDenied: 
      // Could apparently happen if the user refuses to connect an
      // external Bluetooth GPS device, for example.
    case KErrCancel:
      er_log_symbian(0, errCode, 
		     "cellpos: positioner error, giving up on positioner");
      // We will not reset state further, lest we be asked to switch
      // back to the same module again.
      DELETE_Z(iPositioner);
      break;

    case KErrArgument:
    case KErrPositionBufferOverflow:
      er_log_symbian(er_FATAL, errCode, "cellpos: unexpected positioning error");
      break;

    default:
      // We may not actually need the retry AO at all. The cell ID
      // changes will determine when to retry.
      er_log_symbian(0, errCode, 
		     "cellpos: positioner error, not retrying");
      /*
      if (!iRetryAo->Retry()) {
	er_log_symbian(0, errCode, "INACTIVATE: cellpos: stopping scanning due to too many errors");
	Stop();
      }
      */
    }
  } else { // errCode >= 0 (yes, there are _positive_ codes as well)
    iRetryAo->ResetFailures();

    switch (errCode) { // see <lbserrors.h>
    case KPositionQualityLoss: // no data
      {
	guilogf("cellpos: got no GPS data");
	break;
      }
      
    case KErrNone: // full data
    case KPositionPartialUpdate: // partial data
      {
#if __DO_LOGGING__
      TBool isPartial = (errCode == KPositionPartialUpdate);
      logg("got %s gps reading", isPartial ? "partial" : "full");
#endif

      // Is it so that if some module does not support satellite info
      // then if we try to request it we get nothing? Well, we are
      // only choosing GPS modules, and GPS is based on satellites, so
      // surely there always is satellite info.
      const TPositionSatelliteInfo& positionInfo = iPositioner->PositionInfo();

      LogSatelliteInfoL(iLogDb, "cellpos", positionInfo);
      
      break;
      }

    default: // unknown completion code
      {
	guilogf("cellpos: unknown completion code %d", errCode);
	log_db_log_status(iLogDb, NULL, 
			  "cellpos: unknown completion code %d", errCode);
      }
    }
  }

  return TRUE;
}

void CSensor_cellpos::Reconfigure(const gchar* name, const gchar* value)
{
}

/**

epoc-cellpos.cpp

Copyright 2009-2011 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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

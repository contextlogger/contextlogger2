#ifndef __epoc_gps_hpp__
#define __epoc_gps_hpp__

#include "application_config.h"

#if __GPS_ENABLED__

#include "epoc-gps-observer.hpp"

#include "ac_app_context.h"
#include "epoc-ao-gerror.hpp"
#include "ld_log_db.h"
#include "ut_retry_epoc.hpp"
#include "utils_cl2.h"

#include <e32std.h>
#include <lbs.h> // link against lbs.lib
#include <lbssatellite.h>

#include <glib.h>

class CPositioner_gps;
class CPosModuleStatAo;

NONSHARABLE_CLASS(CSensor_gps) :
  public CBase,
  public MObserverPosMod,
  public MObserver_gps,
  public MRetryAoObserver
{
 public:
  static CSensor_gps* NewL(ac_AppContext* aAppContext);
  virtual ~CSensor_gps();

 private:
  CSensor_gps(ac_AppContext* aAppContext);
  void ConstructL();

 private: // MObserverPosMod
  virtual void PosModSwitchToModuleL(TPositionModuleId aModuleId);
  virtual void PosModNoModuleL();
  virtual void PosModErrorL(TInt errCode);
  virtual void PosModLeave(TInt errCode);

 private: // MObserver_gps
  virtual gboolean PositionerEventL(GError** error);

 private: // MRetryAoObserver
  virtual void RetryTimerExpired(CRetryAo* src, TInt errCode);

 private:
  ac_AppContext* iAppContext; // not owned
  LogDb* iLogDb; // not owned

  CPosModuleStatAo* iModuleAo; // owned

  // Used for positioning when a suitable positioning module is
  // available. When nothing suitable is available, this is NULL.
  CPositioner_gps* iPositioner; // owned

  CRetryAo* iRetryAo; // owned

  TInt iPositionUpdateIntervalSecs;

  enum TState {
    EInactive = 0, // not started
    EActive, // module status query outstanding
    ERetryWaiting // waiting to retry module status query
  };
  TState iState;

 private:
  void RefreshPositionUpdateIntervalSecs();
  void CreateSpecifiedPositionerL(TPositionModuleId bestId);

 public:
  void StartL();
  void Stop();
  TBool IsActive() const { return iState != EInactive; }
  void Reconfigure(const gchar* name, const gchar* value);
};

#endif // __GPS_ENABLED__

// --------------------------------------------------
// sensor array integration
// --------------------------------------------------

#if defined(SA_ARRAY_INTEGRATION)
#if __GPS_ENABLED__
#define DECLARE_SENSOR_gps CSensor_gps* iSensor_gps
#define SENSOR_GPS_DESTROY DELETE_Z(self->iSensor_gps)
#define SENSOR_GPS_CREATE sa_typical_symbian_sensor_create(self->iSensor_gps = CSensor_gps::NewL(self->ac), "gps sensor initialization")
#define SENSOR_GPS_START sa_trap_symbian_sensor_start(self->iSensor_gps, "failed to start gps scanning")
#define SENSOR_GPS_STOP { self->iSensor_gps->Stop(); }
#define SENSOR_GPS_IS_RUNNING (self->iSensor_gps->IsActive())
#define SENSOR_GPS_RECONFIGURE(key,value) sa_typical_symbian_sensor_reconfigure(gps)
#else
#define DECLARE_SENSOR_gps
#endif
#endif /* SA_ARRAY_INTEGRATION */

#endif /* __epoc_gps_hpp__ */

/**

epoc-gps.hpp

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

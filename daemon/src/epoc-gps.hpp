#ifndef __epoc_gps_hpp__
#define __epoc_gps_hpp__

#include "application_config.h"

#if __GPS_ENABLED__

#include "epoc-gps-observer.hpp"

#include "ac_app_context.h"
#include "epoc-ao-gerror.hpp"
#include "ld_log_db.h"
#include "utils_cl2.h"

#include <e32std.h>
#include <lbs.h> // link against lbs.lib
#include <lbssatellite.h>

#include <glib.h>

class CPositioner_gps;

NONSHARABLE_CLASS(CSensor_gps) :
  public CActiveRunG, 
  public MObserver_gps
{
 public:

  static CSensor_gps* NewL(ac_AppContext* aAppContext);

  virtual ~CSensor_gps();

  gboolean StartL(GError** error);

  void Stop();

  void Reconfigure(const gchar* name, const gchar* value);

 private:
 
  CSensor_gps(ac_AppContext* aAppContext);

  void ConstructL();

 private:

  void MakeRequest();

  virtual gboolean RunGL(GError** error);
  
  virtual const char* Description();
  
  virtual void DoCancel();

  // These are for changing positioners if need be.
  void CurrentModuleUnavailable();
  void NewModuleAvailable();
  void CreateBestPositionerL();
  void CreateSpecifiedPositionerL(TPositionModuleId bestId);
  TBool ChooseBestPositionerL(TPositionModuleId& bestId);

  void RefreshPositionUpdateIntervalSecs();

 private: // MObserver_gps

  virtual gboolean PositionerEventL(GError** error);

 private:

  ac_AppContext* iAppContext; // not owned
  LogDb* iLogDb; // not owned

  DEF_SESSION(RPositionServer, iPositionServer);

  TPositionModuleStatusEvent iPositionModuleStatusEvent;

  TInt iNumScanFailures;

  // Used for positioning when a suitable positioning module is
  // available. When nothing suitable is available, this is NULL.
  CPositioner_gps* iPositioner; // owned

  TInt iPositionUpdateIntervalSecs;
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
#define SENSOR_GPS_START sa_typical_symbian_sensor_start(self->iSensor_gps, "failed to start gps scanning")
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

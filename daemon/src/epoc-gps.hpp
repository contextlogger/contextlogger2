#ifndef __epoc_gps_hpp__
#define __epoc_gps_hpp__

#include "application_config.h"

#if __GPS_ENABLED__

#include "epoc-ao-gerror.hpp"
#include "epoc-s60-version.hpp"
#include "log-db.h"
#include "utils_cl2.h"

#include <e32std.h>
#include <lbs.h> // link against lbs.lib
#include <lbssatellite.h>

#include <glib.h>

class CPositioner_gps;

NONSHARABLE_CLASS(MObserver_gps)
{
 public:
  virtual gboolean PositionerEventL(GError** error) = 0;
};

NONSHARABLE_CLASS(CSensor_gps) :
  public CActiveRunG, 
  public MObserver_gps
{
 public:

  static CSensor_gps* NewL(LogDb* aLogDb);

  virtual ~CSensor_gps();

  gboolean StartL(GError** error);

  void Stop();

  void Reconfigure(const gchar* name, const gchar* value);

 private:
 
  CSensor_gps(LogDb* aLogDb);

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

  LogDb* iLogDb; // not owned

  DEF_SESSION(RPositionServer, iPositionServer);

  TPositionModuleStatusEvent iPositionModuleStatusEvent;

  TInt iNumScanFailures;

  // Used for positioning when a suitable positioning module is
  // available. When nothing suitable is available, this is NULL.
  CPositioner_gps* iPositioner; // owned

  TPlatformVersion iPlatformVersion; // xxx could be more "global"

  TInt iPositionUpdateIntervalSecs;
};

#endif // __GPS_ENABLED__

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

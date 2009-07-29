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
};

#endif // __GPS_ENABLED__

#endif /* __epoc_gps_hpp__ */

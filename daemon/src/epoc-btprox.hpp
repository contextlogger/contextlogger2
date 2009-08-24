#ifndef __epoc_btprox_hpp__
#define __epoc_btprox_hpp__

#include "application_config.h"

#if __BTPROX_ENABLED__

#include "epoc-ao-gerror.hpp"
#include "log-db.h"
#include "utils_cl2.h"

#include <bt_sock.h>
#include <e32base.h>
#include <es_sock.h>

#include <glib.h>

NONSHARABLE_CLASS(CSensor_btprox) : public CActiveRunG
{
 public:

  static CSensor_btprox* NewL(LogDb* aLogDb);

  virtual ~CSensor_btprox();

  gboolean StartL(GError** error);

  void Stop();

  void Reconfigure(const gchar* name, const gchar* value);

 private:
 
  CSensor_btprox(LogDb* aLogDb);

  void ConstructL();

  TBool EnsureBtInit();
  void TryBtInitL();
  void BtClose();
  void TryStartScanning();
  gboolean HandleScanEvent(TInt errCode, GError** error);

  void RefreshBaseScanIntervalSecs();

  // Three different ways internally to make an asynch. request.
  void SetTimer();
  void BtDiscover();
  void BtNext();

 private: // CActiveG

  virtual gboolean RunGL(GError** error);
  virtual void DoCancel();
  virtual const char* Description();

 private:

  enum TState {
    EInactive = 0, // StartL not invoked
    EScanWaiting, // BT init done, scan interval wait
    EDiscovering, // BT scan in progress
    ERetryWaiting // BT init not done, waiting before reinit attempt
  };
  TState iState;

  // owned, for now, can share once required by other sensors
  DEF_SESSION(RSocketServ, iSocketServ);

  DEF_SESSION(RHostResolver, iHostResolver);
  
  TInquirySockAddr iInquirySockAddr;
  TNameEntry iNameEntry;

  TInt iNumScanFailures;

  TInt iBaseScanIntervalSecs;
  
  GPtrArray* iResult;
  GPtrArray* iOldResult;

  DEF_SESSION(RTimer, iTimer);

  LogDb* iLogDb; // not owned
};

#endif // __BTPROX_ENABLED__

#endif /* __epoc_btprox_hpp__ */

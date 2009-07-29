#ifndef __epoc_appfocus_hpp__
#define __epoc_appfocus_hpp__

#include "application_config.h"

#if __APPFOCUS_ENABLED__

#include "epoc-ao-gerror.hpp"
#include "log-db.h"
#include "utils_cl2.h"

#include <e32std.h>
#include <w32std.h> // link against: ws32.lib

#include <glib.h>

class CMyWindowGroup;

NONSHARABLE_CLASS(CSensor_appfocus) :
  public CActiveRunG
{
 public:

  static CSensor_appfocus* NewL(LogDb* aLogDb);

  virtual ~CSensor_appfocus();

  gboolean StartL(GError** error);

  void Stop();

 private:
 
  CSensor_appfocus(LogDb* aLogDb);

  void ConstructL();

 private:

  // Makes the next observing request.
  void MakeRequest();
  
  virtual gboolean RunGL(GError** error);
  
  virtual const char* Description();
  
  virtual void DoCancel();

 private:

  LogDb* iLogDb; // not owned

  TBool iFocusChangeEventsEnabled;

  CMyWindowGroup* iMyWindowGroup;

  DEF_SESSION(RWsSession, iWsSession);

};

#endif // __APPFOCUS_ENABLED__

#endif /* __epoc_appfocus_hpp__ */

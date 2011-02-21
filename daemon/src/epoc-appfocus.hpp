/*
 !concept {:name => "Tracking application focus on Symbian",
   :desc => "Keeping track of which application has the focus."}
*/

#ifndef __epoc_appfocus_hpp__
#define __epoc_appfocus_hpp__

#include "application_config.h"

#if __APPFOCUS_ENABLED__

#include "epoc-ao-gerror.hpp"
#include "ld_log_db.h"
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

/**

epoc-appfocus.hpp

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

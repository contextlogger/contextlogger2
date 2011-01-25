#ifndef __epoc_gps_module_hpp__
#define __epoc_gps_module_hpp__

#include "epoc-gps-observer.hpp"

#include "common/epoc-session.hpp"

#include <e32std.h>
#include <lbs.h> // link against lbs.lib
#include <lbssatellite.h>

NONSHARABLE_CLASS(CPosModuleStatAo) :
  public CActive
{
 public: 
  static CPosModuleStatAo* NewL(MObserverPosMod& aObserver);
  virtual ~CPosModuleStatAo();

 private:
  CPosModuleStatAo(MObserverPosMod& aObserver);
  void ConstructL();

 private:
  DEF_SESSION(RPositionServer, iPositionServer);
  MObserverPosMod& iObserver;
  TPositionModuleStatusEvent iPositionModuleStatusEvent;

 public:
  // All requests are single-shot, and never made automatically.
  void MakeRequest();

  TPositionModuleId ChooseBestPositionerL();

  RPositionServer& PositionServer()
  {
    return iPositionServer;
  }

 private:
  void PosModChange();

 private: // CActive
  virtual void DoCancel();
  virtual TInt RunError(TInt errCode);
  virtual void RunL();
};

#endif /* __epoc_gps_module_hpp__ */

/**

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


#ifndef __epoc_gps_observer_hpp__
#define __epoc_gps_observer_hpp__

#include <e32std.h>
#include <lbs.h> // link against lbs.lib

#include <glib.h>

// The GPS positioner class uses this interface for reporting events.
NONSHARABLE_CLASS(MObserver_gps)
{
 public:
  // This callback may either leave or set a GError to report an error
  // in the callback.
  // 
  // The implementation of this method may invoke StatusCode() on the
  // positioner to determine whether a position was successfully
  // acquired.
  virtual gboolean PositionerEventL(GError** error) = 0;
};

// The positioning module status observer uses this interface to report.
NONSHARABLE_CLASS(MObserverPosMod)
{
 public:
  virtual void PosModChangeL() = 0;
  virtual void PosModErrorL(TInt errCode) = 0;
  virtual void PosModLeave(TInt errCode) = 0;
  virtual TBool PosModIsCurrent(TPositionModuleId id) const = 0;
};

#endif /* __epoc_gps_observer_hpp__ */

/**

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

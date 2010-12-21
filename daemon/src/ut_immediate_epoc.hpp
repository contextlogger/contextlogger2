#ifndef __ut_immediate_epoc_hpp__
#define __ut_immediate_epoc_hpp__

#include <e32base.h>

class MImmediateObserver
{
public:
  virtual void HandleImmediateEvent() = 0;
  virtual TInt HandleImmediateError(TInt errCode);
  virtual TInt HandleImmediateLeave(TInt errCode);
};

NONSHARABLE_CLASS(CImmediateAo) : public CActive
{
 public:
  static CImmediateAo* NewL(MImmediateObserver& aObserver);
  ~CImmediateAo();

 private:
  CImmediateAo(MImmediateObserver& aObserver);

 public:
  void Complete();

 private:
  void DoCancel();
  TInt RunError(TInt aError);
  void RunL();

 private:
  MImmediateObserver& iObserver;
};

#endif /* __ut_immediate_epoc_hpp__ */

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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

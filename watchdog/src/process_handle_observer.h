// -*- c++ -*-

#ifndef __PROCESS_HANDLE_OBSERVER_H__
#define __PROCESS_HANDLE_OBSERVER_H__

#include <e32base.h>

// An active object and a callback interface for observing a process to
// find out when it dies.

/** A callback interface for CProcessHandleObserver.
 */
class MProcessHandleObserver
{
public:

  virtual void HandleProcessHandleEvent(TInt aError) = 0;
};

/** A fairly generic active object that delivers process handle events via a callback interface.
 */
class CProcessHandleObserver :
  public CActive
{
public:

  static CProcessHandleObserver *NewLC(MProcessHandleObserver &aInterface, TInt aPriority, RProcess &aProcess);

  static CProcessHandleObserver *NewL(MProcessHandleObserver &aInterface, TInt aPriority, RProcess &aProcess);

private:

  CProcessHandleObserver(MProcessHandleObserver &aInterface, TInt aPriority, RProcess &aProcess);

  void ConstructL();

public:

  ~CProcessHandleObserver();

  void MakeRequest();

private:

  void DoCancel();

  virtual void RunL();

  // Must implement RunError if HandleProcessHandleEvent may leave. As
  // per the current naming, we do not allow that. We could
  // have another variant of this class allowing for it, I
  // guess, and letting a RunError handler also be defined in
  // the interface.

  MProcessHandleObserver &iInterface;

  RProcess &iProcess;
};

#endif // __PROCESS_HANDLE_OBSERVER_H__

/** 
    Copyright 2008 Helsinki Institute for Information Technology (HIIT)
    and Tero Hasu <tero.hasu@hut.fi>. All rights reserved.

    This license applies:

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Alternatively, this license applies:

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
 */

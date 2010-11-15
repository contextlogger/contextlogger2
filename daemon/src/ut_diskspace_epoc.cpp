//
// Copyright (c) 2009-2009 HIIT and Tero Hasu
// Copyright (c) 2007-2009 Google Inc.
// Copyright (c) 2006-2007 Jaiku Ltd.
// Copyright (c) 2002-2006 Mika Raento and Renaud Petit
//
// This software is licensed at your choice under either 1 or 2 below.
//
// 1. MIT License
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// 2. Gnu General Public license 2.0
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

#include "ut_diskspace_epoc.hpp"

#include "er_errors.h"
#include "symbian_auto_ptr.hpp"

#if defined(__EPOC32__)

CDiskSpaceNotifier* CDiskSpaceNotifier::NewL(RFs& fs, TInt aDrive, 
					     MDiskSpace* aNotifier, TInt64 aThreshold)
{
  auto_ptr<CDiskSpaceNotifier> ret(new (ELeave) CDiskSpaceNotifier(fs, aDrive, 
								   aNotifier, aThreshold));
  ret->ConstructL();
  return ret.release();
}

CDiskSpaceNotifier::CDiskSpaceNotifier(RFs& fs, TInt aDrive, 
				       MDiskSpace* aNotifier, TInt64 aThreshold) : 
  CActive(EPriorityNormal), iNotifier(aNotifier), iFs(fs), iDrive(aDrive),
  iThreshold(aThreshold)
{
  CActiveScheduler::Add(this);
}

void CDiskSpaceNotifier::ConstructL()
{
  MakeRequest(); // activate at initialization already
}

void CDiskSpaceNotifier::MakeRequest()
{
  //iStatus=KRequestPending;
  iFs.NotifyDiskSpace(iThreshold, iDrive, iStatus);
  SetActive();
}

CDiskSpaceNotifier::~CDiskSpaceNotifier()
{
  Cancel();
}

void CDiskSpaceNotifier::RunL()
{
  TInt err = iStatus.Int();

  assert((err != KErrArgument) && "threshold value outside its limits");

  /*  xxx When could we get this? Not documented.
  if (err == KErrCorrupt) {
    return;
  }
  */

  // Make new request before notification, do nothing after the callback.
  MakeRequest();

  /*
  if (err != KErrNone) {
    logg("Unexpected, got error %d in DiskSpaceNotifier!", err);
  }
  */

  // Leave it to the caller to decide what to do about any possible error.
  iNotifier->DiskSpaceNotify(iDrive, err);
}

void CDiskSpaceNotifier::DoCancel()
{
  iFs.NotifyDiskSpaceCancel();
}

#endif /* defined(__EPOC32__) */

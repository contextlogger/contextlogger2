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

// http://www.newlc.com/article-275.html -- RTimer::Inactivity

#include "epoc-inactivity.hpp"

#if __INACTIVITY_ENABLED__

#include "er_errors.h"
#include "sa_sensor_list_log_db.h"

#define TIMEOUT_SECS TTimeIntervalSeconds(60)

CTOR_IMPL_CSensor_inactivity;

void CSensor_inactivity::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iTimer, iTimer.CreateLocal()); 
}

CSensor_inactivity::~CSensor_inactivity()
{
  if (IS_SESSION_OPEN(iTimer)) {
    Cancel();
    iTimer.Close();
  }
}

#define iLogDb ac_LogDb(iC)

gboolean CSensor_inactivity::StartL(GError** error)
{
  if (!IsActive()) {
    GetState();
    MakeRequest();
    log_db_log_status(iLogDb, NULL, "inactivity sensor started");
  }
  return TRUE;
}

void CSensor_inactivity::Stop()
{
  if ((IsActive())) {
    Cancel();
    log_db_log_status(iLogDb, NULL, "inactivity sensor stopped");
  }
}

void CSensor_inactivity::RunL()
{
  TInt errCode = iStatus.Int();

  if (errCode) {
    log_db_log_status(iLogDb, NULL, "INACTIVATE: inactivity: RTimer inactivity request error %d", errCode);
    return;
  }
  
  if (iCurrentState == EIdle) {
    iCurrentState = EActive;
  } else {
    iCurrentState = EIdle;
  }
  
  LogDb* logDb = ac_LogDb(iC);
  GError* localError = NULL;
  if (!log_db_log_inactivity(logDb, (iCurrentState == EActive), &localError)) {
    gx_txtlog_fatal_error_free(localError);
    return;
  }

  MakeRequest();
}

void CSensor_inactivity::DoCancel()
{
  iTimer.Cancel();
}

void CSensor_inactivity::MakeRequest()
{
  // This should not be dangerous even if there is some nasty process
  // that keeps resetting the timer. In that case we will end up in
  // the EActive state, and we will stay there since the timer resets
  // happen, and will not get timer events.
  if (iCurrentState == EIdle) {
    // Wait until there is some activity.
    iTimer.Inactivity(iStatus, 0);
  } else {
    // Wait until there is no activity and has not been for TIMEOUT_SECS.
    iTimer.Inactivity(iStatus, TIMEOUT_SECS);
  }

  SetActive();
}

void CSensor_inactivity::GetState()
{
  if (User::InactivityTime() > TIMEOUT_SECS) {
    iCurrentState = EIdle;
  } else {
    iCurrentState = EActive;
  }
}

#endif // __INACTIVITY_ENABLED__

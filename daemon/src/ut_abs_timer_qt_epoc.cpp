#include "ut_abs_timer_qt_epoc.hpp"

#include "er_errors.h"

#include "common/assertions.h"

#include <limits.h>

#include <QtDebug>

QAbsTimer::QAbsTimer(QObject *parent) :
  QObject(parent)
{
  TRAPD(errCode,
	iTimerAo = CTimerAo::NewL(*this, CActive::EPriorityLow));
  qt_symbian_throwIfError(errCode);
}

QAbsTimer::~QAbsTimer()
{
  delete iTimerAo;
}

bool QAbsTimer::isActive() const 
{
  return iTimerAo->IsActive();
}

void QAbsTimer::start(const QDateTime& aExpTime) 
{
  iAtTime = aExpTime.toUTC().toMSecsSinceEpoch();
  iTimerAo->AtUTC(iAtTime);
}

void QAbsTimer::stop() 
{
  iTimerAo->Cancel();
}

void QAbsTimer::HandleTimerEvent(CTimerAo *, TInt aError)
{
  switch (aError)
    {
    case KErrNone:
    case KErrUnderflow:
      {
	emit timeout();
        break;
      }
    case KErrAbort: // time may have changed
      {
	TTime now;
	now.UniversalTime();
	if (now < iAtTime) {
	  iTimerAo->AtUTC(iAtTime);
	} else {
	  emit timeout();
	}
        break;
      }
    default: // an actual error
      {
        er_log_symbian(er_FATAL, aError, "CTimerAo error");
        break;
      }
    }
}

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

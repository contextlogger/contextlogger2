#include "ut_abs_timer_qt_pure.hpp"

#include "common/assertions.h"

#include <limits.h>

#include <QtDebug>

QAbsTimer::QAbsTimer(QObject *parent) :
  QObject(parent)
{
  iTimerAo.setSingleShot(true);
  connect(&iTimerAo, SIGNAL(timeout()),
	  this, SLOT(handleTimeout()));
}

QAbsTimer::~QAbsTimer()
{
}

bool QAbsTimer::isActive() const 
{
  return iTimerAo.isActive();
}

void QAbsTimer::start(const QDateTime& aExpTime) 
{
  iExpTime = aExpTime;
  startOrSignal();
}

void QAbsTimer::startOrSignal()
{
  QDateTime now = QDateTime::currentDateTimeUtc();
  if (now >= iExpTime) {
    emit timeout();
  } else {
    qint64 diff = now.msecsTo(iExpTime);
    assert(diff > 0);
    
    //qDebug() << "timer expiration msecs from now" << diff;
    int diffInt = qBound((qint64)0, diff, (qint64)INT_MAX);
    //qDebug() << "timer expiration msecs from now" << diffInt;

    // The argument is also in msec, but the problem is that the int
    // type is narrower, and hence we must make sure that it is not
    // too large.
    iTimerAo.start(diffInt);
  }
}

void QAbsTimer::stop() 
{
  iTimerAo.stop();
}

void QAbsTimer::handleTimeout() 
{
  startOrSignal();
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

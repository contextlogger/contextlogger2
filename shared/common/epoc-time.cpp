#include "epoc-time.h"

TTime UnixEpochTime()
{
  TDateTime dateTime;
  dateTime.Set(1970, EJanuary, 0, 0, 0, 0, 0);
  return TTime(dateTime);
}

// "epocTime" is assumed to be in UTC.
time_t UtcEpocTimeToUnixTime(const TTime& epocTime)
{
  return (time_t)((epocTime.Int64() - UnixEpochTime().Int64()) / 1000000);
}

// "epocTime" is assumed to be local time.
time_t LocalEpocTimeToUnixTime(const TTime& epocTime)
{
  TLocale loc;
  TTimeIntervalSeconds utcOffset = loc.UniversalTimeOffset();
  return (UtcEpocTimeToUnixTime(epocTime) - utcOffset.Int());
}

void UnixTimeToUtcEpocTime(TTime& epocTime, time_t unixTime)
{
  epocTime = UnixEpochTime() + TTimeIntervalSeconds(unixTime);
}

void UnixTimeToLocalEpocTime(TTime& epocTime, time_t unixTime)
{
  TLocale loc;
  TTimeIntervalSeconds utcOffset = loc.UniversalTimeOffset();
  UnixTimeToUtcEpocTime(epocTime, unixTime);
  epocTime += utcOffset;
}

TTimeIntervalMicroSeconds32 SecsToUsecs(TInt secs)
{
  TInt64 us64 = TInt64(secs) * 1000000LL;
  // max time is around 35 minutes only
  TInt us = ((us64 > 2147483647LL) ? 2147483647 : (int)us64);
  return TTimeIntervalMicroSeconds32(us);
}

/**

epoc-time.cpp

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

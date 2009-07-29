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

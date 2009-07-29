#include "epoc-time.h"

TTimeIntervalMicroSeconds32 SecsToUsecs(TInt secs)
{
  TInt64 us64 = TInt64(secs) * 1000000LL;
  // max time is around 35 minutes only
  TInt us = ((us64 > 2147483647LL) ? 2147483647 : (int)us64);
  return TTimeIntervalMicroSeconds32(us);
}

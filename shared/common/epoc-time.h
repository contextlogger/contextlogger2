#ifndef __epoc_time_h__
#define __epoc_time_h__

#include <time.h>
#include <e32std.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#ifdef __cplusplus
TTime UnixEpochTime();

time_t LocalEpocTimeToUnixTime(const TTime& epocTime);

time_t UtcEpocTimeToUnixTime(const TTime& epocTime);

void UnixTimeToUtcEpocTime(TTime& epocTime, time_t unixTime);

void UnixTimeToLocalEpocTime(TTime& epocTime, time_t unixTime);

TTimeIntervalMicroSeconds32 SecsToUsecs(TInt secs);
#endif

#endif /* __epoc_time_h__ */

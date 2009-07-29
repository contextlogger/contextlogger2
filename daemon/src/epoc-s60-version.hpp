#ifndef __epoc_s60_version_hpp__
#define __epoc_s60_version_hpp__

#include <e32std.h>
#include <f32file.h>

struct TPlatformVersion {
  TUint iMajor;
  TUint iMinor;
};

void GetS60PlatformVersionL(RFs& aFs, TPlatformVersion& aVersion);

// internally creates a temporary RFs session, so not efficient
void GetS60PlatformVersionL(TPlatformVersion& aVersion);

#endif /* __epoc_s60_version_hpp__ */

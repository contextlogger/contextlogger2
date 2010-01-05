#include "kr_diskspace.h"

#if defined(__EPOC32__)

#include "ac_app_context.h"
#include "application_config.h"
#include "er_errors.h"

void CheckLoggingMediumReadyL(RFs& fs)
{
  TChar driveLetter = DATABASE_DRIVE_LETTER;
  TInt driveNum = 0;
  User::LeaveIfError(RFs::CharToDrive(driveLetter, driveNum));

  // Could use RFs::IsValidDrive to check whether the drive number is
  // valid, but presumably this is redundant. Surely we cannot
  // successfully get drive information for a drive number that is not
  // valid.
  TVolumeInfo volumeInfo;
  User::LeaveIfError(fs.Volume(volumeInfo, driveNum));
  TDriveInfo& driveInfo = volumeInfo.iDrive;

  if (driveInfo.iType == EMediaNotPresent) {
    logt("logging hardware not available");
    User::Leave(KErrHardwareNotAvailable);
  }
  // The C: drive seems to have the write protected attribute set (on
  // all the devices I have tried).
  if ((driveLetter != 'c') && 
      (driveInfo.iType & KMediaAttWriteProtected)) {
    logt("logging medium write protected");
    User::Leave(KErrAccessDenied);
  }

  int log_disk_threshold = ac_STATIC_GET(log_disk_threshold);
  if (volumeInfo.iFree < log_disk_threshold) {
    logt("logging medium (almost) full");
    User::Leave(KErrDiskFull);
  }
}

#if 1
// Optimization, use shared RFs session.

#include "ac_app_context.h"

static void CheckLoggingMediumReadyL()
{
  RFs& fs = ac_Fs(ac_get_global_AppContext());
  CheckLoggingMediumReadyL(fs);
}
#else
static void CheckLoggingMediumReadyL()
{
  RFs fs;
  User::LeaveIfError(fs.Connect());
  CleanupClosePushL(fs);

  CheckLoggingMediumReadyL(fs);

  CleanupStack::PopAndDestroy(); // fs
}
#endif

extern "C" gboolean check_logging_medium_ready(GError** error)
{
  TRAPD(errCode, CheckLoggingMediumReadyL());
  if (errCode) {
    if (error) {
      const char* fmt;
      if (errCode == KErrDiskFull)
	fmt = "Logging medium full: %s (%d)";
      else
	fmt = "Logging medium not ready: %s (%d)";
      *error = g_error_new(domain_symbian, errCode, fmt, plat_error_strerror(errCode), errCode);
    }
    return FALSE;
  }
  return TRUE;
}

#endif /* defined(__EPOC32__) */

/**

kr_diskspace.cpp

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

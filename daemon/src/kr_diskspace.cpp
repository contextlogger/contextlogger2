#include "kr_diskspace.h"

#if defined(__EPOC32__)

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
    User::Leave(KErrHardwareNotAvailable);
  }
  if (driveInfo.iType & KMediaAttWriteProtected) {
    User::Leave(KErrAccessDenied);
  }
  if (volumeInfo.iFree < DATABASE_VOLUME_THRESHOLD) {
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

#include "epoc-s60-version.hpp"

// from http://wiki.forum.nokia.com/index.php/CS000933_-_Detecting_S60_platform_version_at_run_time

_LIT(KS60ProductIDFile, "Series60v*.sis");
_LIT(KROMInstallDir, "z:\\system\\install\\");

void GetS60PlatformVersionL(RFs& aFs, TPlatformVersion& aVersion)
{
  TFindFile ff( aFs );
  CDir* result;
  User::LeaveIfError( ff.FindWildByDir( KS60ProductIDFile, KROMInstallDir, result ) );
  CleanupStack::PushL( result );
  User::LeaveIfError( result->Sort( ESortByName|EDescending ) );
  aVersion.iMajor = (*result)[0].iName[9] - '0';
  aVersion.iMinor = (*result)[0].iName[11] - '0';
  CleanupStack::PopAndDestroy(); // result
}

void GetS60PlatformVersionL(TPlatformVersion& aVersion)
{
  RFs fs;
  User::LeaveIfError(fs.Connect());
  CleanupClosePushL(fs);
  GetS60PlatformVersionL(fs, aVersion);
  CleanupStack::PopAndDestroy(); // fs
}

#include "epoc-gps-module.hpp"

#include "er_errors.h"
#include "utils_cl2.h"

CPosModuleStatAo* CPosModuleStatAo::NewL(MObserverPosMod& aObserver)
{
  CPosModuleStatAo* obj = new (ELeave) CPosModuleStatAo(aObserver);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop(obj);
  return obj;
}

CPosModuleStatAo::~CPosModuleStatAo()
{
  Cancel();
  SESSION_CLOSE_IF_OPEN(iPositionServer);
}

CPosModuleStatAo::CPosModuleStatAo(MObserverPosMod& aObserver) :
  CActive(CActive::EPriorityStandard), 
  iObserver(aObserver)
{
  CActiveScheduler::Add(this);
}

void CPosModuleStatAo::ConstructL()
{
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iPositionServer, iPositionServer.Connect());
  iPositionModuleStatusEvent.SetRequestedEvents(TPositionModuleStatusEventBase::EEventDeviceStatus|TPositionModuleStatusEventBase::EEventSystemModuleEvent);
}

void CPosModuleStatAo::MakeRequest()
{
  if (!IsActive()) {
    iPositionServer.NotifyModuleStatusEvent(iPositionModuleStatusEvent, iStatus);
    SetActive();
  }
}

void CPosModuleStatAo::DoCancel()
{
  // Ignoring the undocumented error code.
  iPositionServer.CancelRequest(EPositionServerNotifyModuleStatusEvent);
}

TInt CPosModuleStatAo::RunError(TInt aErrCode)
{
  TRAPD(errCode, iObserver.PosModErrorL(aErrCode));
  if (errCode)
    iObserver.PosModLeave(errCode);
  return KErrNone;
}

void CPosModuleStatAo::PosModChange()
{
  TRAPD(errCode, iObserver.PosModChangeL());
  if (errCode)
    iObserver.PosModLeave(errCode);
}

// Returns the module ID of the best (and good enough) available
// module. Returns KPositionNullModuleId if nothing suitable is found.
// The 'aModifiers' parameter(s) indicates what kind of a module is
// "best".
TPositionModuleId CPosModuleStatAo::ChooseBestPositionerL(TInt aModifiers)
{
  TUint numModules;
  User::LeaveIfError(iPositionServer.GetNumModules(numModules));
  TUint i;
  TPositionModuleInfo moduleInfo;
  int bestScore = -1;
  int bestIndex = -1;
  for (i=0; i<numModules; i++) {
    User::LeaveIfError(iPositionServer.GetModuleInfoByIndex(i, moduleInfo));

#if __DO_LOGGING__
    {
      TBuf<KPositionMaxModuleName> moduleName;
      moduleInfo.GetModuleName(moduleName);
      gchar* nameString = ConvToUtf8CStringL(moduleName);
      logg("considering positioning module '%s'", nameString);
      g_free(nameString);
    }
#endif

    TPositionModuleStatus moduleStatus;
    TInt moduleStatusError = iPositionServer.GetModuleStatus(moduleStatus, moduleInfo.ModuleId());
    
    // See lbscommon.h for interpretations for the values.
    logg("moduleInfo: %s stat=%d loc=%d tech=%02x caps=%04x",
	 moduleInfo.IsAvailable() ? "available" : "unavailable",
	 moduleStatus.DeviceStatus(),
	 moduleInfo.DeviceLocation(),
	 moduleInfo.TechnologyType(),
	 moduleInfo.Capabilities());

    if (moduleInfo.IsAvailable() &&
	!moduleStatusError && 
	(moduleStatus.DeviceStatus() != TPositionModuleStatus::EDeviceDisabled) &&
	(moduleInfo.Capabilities() & TPositionModuleInfo::ECapabilitySatellite) &&
	(moduleInfo.TechnologyType() != TPositionModuleInfo::ETechnologyUnknown) &&
        // We might want to allow network positioning (particularly
        // for WLAN based positioning on devices that support it), but
        // it is dangerous as it may involve frequent queries over the
        // network. Should at least make sure we are not roaming
        // before allowing it. xxx
	(moduleInfo.TechnologyType() != TPositionModuleInfo::ETechnologyNetwork) &&
	(moduleInfo.DeviceLocation() != TPositionModuleInfo::EDeviceUnknown)) {
      int score = 0;
      if (moduleInfo.DeviceLocation() == TPositionModuleInfo::EDeviceExternal) {
	/*
	  // Not required: Bluetooth GPS can be disabled globally in
          // the device settings. See the Position application in 
          // S60 3.0 devices, and the settings dialog in later devices.
	  if (iPlatformVersion.iMajor == 3 && iPlatformVersion.iMinor == 0)
	  // Avoid the BT device search dialog.
	  continue;
	*/
	if (aModifiers & KAllowExternal)
	  score += 0x100;
	else
	  continue;
      }
      if (moduleInfo.TechnologyType() == TPositionModuleInfo::ETechnologyAssisted) {
	if (aModifiers & KAllowAssisted)
	  score += 0x10;
	else
	  continue;
      }

      // Given the choice between 'Wi-Fi/Network' and 'Network based'
      // we would like to favor the former. But they both have the
      // same TechnologyType and Capabilities. We could consider
      // making better use of TPositionQuality, perhaps that would do
      // the trick. Not sure if this code is ever going to be of any
      // use.
      TPositionQuality quality;
      moduleInfo.GetPositionQuality(quality);
      TReal32 accuracy = quality.VerticalAccuracy();
      if (!Math::IsNaN(accuracy)) {
	//logg("vertical accuracy %g", accuracy);
	score += 0x1; // bonus for having some accuracy value
      }

      logg("module score is %d", score);
      if (score > bestScore) {
	bestScore = score;
	bestIndex = i;
      }
    }
  }
  if (bestIndex >= 0) {
    User::LeaveIfError(iPositionServer.GetModuleInfoByIndex(bestIndex, moduleInfo));

    {
      TBuf<KPositionMaxModuleName> moduleName;
      moduleInfo.GetModuleName(moduleName);
      gchar* nameString = ConvToUtf8CStringL(moduleName);
      dblogg("chose positioning module '%s'", nameString);
      guilogf("gps: chose module '%s'", nameString);
      g_free(nameString);
    }

    return moduleInfo.ModuleId();
  }
  return KPositionNullModuleId;
}

static TBool DeviceNotAvailable(TPositionModuleStatus::TDeviceStatus deviceStatus)
{
  return ((deviceStatus == TPositionModuleStatus::EDeviceUnknown) ||
	  (deviceStatus == TPositionModuleStatus::EDeviceDisabled) ||
	  (deviceStatus == TPositionModuleStatus::EDeviceError));
}

void CPosModuleStatAo::RunL()
{
  TInt errCode = iStatus.Int();
  logg("positioning module status event (%d)", errCode);

  if (errCode) {
    RunError(errCode);
    return;
  }

  // We are interested in these kinds of events:
  // * current module is removed
  // * currently used module stops working in some way
  // * some other (possibly better) module becomes available
  // * a new (possibly better) module is installed
  // Note also that there might not be a current module at all.

  TPositionModuleId moduleId = iPositionModuleStatusEvent.ModuleId();
  assert(moduleId != KPositionNullModuleId);

  TBool aboutCurrent = iObserver.PosModIsCurrent(moduleId);

  TPositionModuleStatus moduleStatus;
  iPositionModuleStatusEvent.GetModuleStatus(moduleStatus);
    
  TPositionModuleStatusEventBase::TModuleEvent occurredEvents = 
    iPositionModuleStatusEvent.OccurredEvents();
  logg("occurred position events: %d", (int)occurredEvents);

  if (occurredEvents & TPositionModuleStatusEventBase::EEventDeviceStatus) {
    TPositionModuleStatus::TDeviceStatus deviceStatus = 
      moduleStatus.DeviceStatus();
#if __DO_LOGGING__
    const char* deviceStatusStr;
    switch (deviceStatus)
      {
      case TPositionModuleStatus::EDeviceUnknown:
	{
	  deviceStatusStr = "EDeviceUnknown";
	  break;
	}
      case TPositionModuleStatus::EDeviceError:
	{
	  deviceStatusStr = "EDeviceError";
	  break;
	}
      case TPositionModuleStatus::EDeviceDisabled:
	{
	  deviceStatusStr = "EDeviceDisabled";
	  break;
	}
      case TPositionModuleStatus::EDeviceInactive:
	{
	  deviceStatusStr = "EDeviceInactive";
	  break;
	}
      case TPositionModuleStatus::EDeviceInitialising:
	{
	  deviceStatusStr = "EDeviceInitialising";
	  break;
	}
      case TPositionModuleStatus::EDeviceStandBy:
	{
	  deviceStatusStr = "EDeviceStandBy";
	  break;
	}
      case TPositionModuleStatus::EDeviceReady:
	{
	  deviceStatusStr = "EDeviceReady";
	  break;
	}
      case TPositionModuleStatus::EDeviceActive:
	{
	  deviceStatusStr = "EDeviceActive";
	  break;
	}
      default:
	{
	  deviceStatusStr = "<unknown>";
	  break;
	}
      }
      
    logg("%s device status now %d (%s)", 
	 aboutCurrent ? "current" : "other", 
	 deviceStatus, deviceStatusStr);
#endif
    if (aboutCurrent && DeviceNotAvailable(deviceStatus)) {
      PosModChange(); // current module unavailable
    } else if (!aboutCurrent && !DeviceNotAvailable(deviceStatus)) {
      // May not be mean that it actually is new, but recompute best
      // one anyway.
      PosModChange(); // new module available
    }
  }

  // Can it ever really happen that we get both a system module event
  // and a non-system module event? Doesn't matter, we handle it all
  // the same way.
  if (occurredEvents & TPositionModuleStatusEventBase::EEventSystemModuleEvent) {
    TPositionModuleStatusEventBase::TSystemModuleEvent systemModuleEvent = 
      iPositionModuleStatusEvent.SystemModuleEvent();
    logg("system module event was %d", (int)systemModuleEvent);
    if (aboutCurrent &&
	((systemModuleEvent == TPositionModuleStatusEventBase::ESystemError) || 
	 (systemModuleEvent == TPositionModuleStatusEventBase::ESystemModuleRemoved))) {
      PosModChange(); // current module unavailable
    } else if (!aboutCurrent &&
	       ((systemModuleEvent == TPositionModuleStatusEventBase::ESystemModuleInstalled))) {
      PosModChange(); // new module available
    }
  }
}

/**

Copyright 2009-2011 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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


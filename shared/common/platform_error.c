// Except as noted, this file is:
//
// Copyright 2008 Helsinki Institute for Information Technology (HIIT)
// and the authors. All rights reserved.
//
// Authors: Tero Hasu <tero.hasu@hut.fi>
//
// Licensed under the Apache License, Version 2.0 (the "License").

/*
* ====================================================================
* The symbian_error_strerror function is extracted and modified
* from symbian_python_ext_util.cpp
*  
*                Utilities for Symbian OS specific Python extensions.
* 
* The original code is:
*     
* Copyright (c) 2005 Nokia Corporation
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
* ====================================================================
*/

#include "common/platform_error.h"
#include "common/error_list.h"
#include "common/assertions.h"

EXTERN_C gboolean code_means_no_error(int errCode, const char* desc, GError** error)
{
  assert_error_unset(error);
  if (!errCode)
    return TRUE;
  if (error)
    *error = g_error_new(domain_shared, code_unspecified_error, "%s: %s (%d)", desc, plat_error_strerror(errCode), errCode);
  return FALSE;
}

#ifdef __EPOC32__

// Provides textual descriptions for Symbian-specific error codes.
EXTERN_C const char* symbian_error_strerror(int err)
{
  switch(err) {
  case 0:
    return "KErrNone";
    break;
  case (-1):
    return "KErrNotFound";
    break;
  case (-2):
    return "KErrGeneral";
    break;
  case (-3):
    return "KErrCancel";
    break;
  case (-4):
    return "KErrNoMemory";
    break;
  case (-5):
    return "KErrNotSupported";
    break;
  case (-6):
    return "KErrArgument";
    break;
  case (-7):
    return "KErrTotalLossOfPrecision";
    break;
  case (-8):
    return "KErrBadHandle";
    break;
  case (-9):
    return "KErrOverflow";
    break;
  case (-10):
    return "KErrUnderflow";
    break;
  case (-11):
    return "KErrAlreadyExists";
    break;
  case (-12):
    return "KErrPathNotFound";
    break;
  case (-13):
    return "KErrDied";
    break;
  case (-14):
    return "KErrInUse";
    break;
  case (-15):
    return "KErrServerTerminated";
    break;
  case (-16):
    return "KErrServerBusy";
    break;
  case (-17):
    return "KErrCompletion";
    break;
  case (-18):
    return "KErrNotReady";
    break;
  case (-19):
    return "KErrUnknown";
    break;
  case (-20):
    return "KErrCorrupt";
    break;
  case (-21):
    return "KErrAccessDenied";
    break;
  case (-22):
    return "KErrLocked";
    break;
  case (-23):
    return "KErrWrite";
    break;
  case (-24):
    return "KErrDisMounted";
    break;
  case (-25):
    return "KErrEof";
    break;
  case (-26):
    return "KErrDiskFull";
    break;
  case (-27):
    return "KErrBadDriver";
    break;
  case (-28):
    return "KErrBadName";
    break;
  case (-29):
    return "KErrCommsLineFail";
    break;
  case (-30):
    return "KErrCommsFrame";
    break;
  case (-31):
    return "KErrCommsOverrun";
    break;
  case (-32):
    return "KErrCommsParity";
    break;
  case (-33):
    return "KErrTimedOut";
    break;
  case (-34):
    return "KErrCouldNotConnect";
    break;
  case (-35):
    return "KErrCouldNotDisconnect";
    break;
  case (-36):
    return "KErrDisconnected";
    break;
  case (-37):
    return "KErrBadLibraryEntryPoint";
    break;
  case (-38):
    return "KErrBadDescriptor";
    break;
  case (-39):
    return "KErrAbort";
    break;
  case (-40):
    return "KErrTooBig";
    break;
  case (-41):
    return "KErrDivideByZero";
    break;
  case (-42):
    return "KErrBadPower";
    break;
  case (-43):
    return "KErrDirFull";
    break;
  case (-44):
    return "KErrHardwareNotAvailable";
    break;
  case (-45):
    return "KErrSessionClosed";
    break;
  case (-46):
    return "KErrPermissionDenied";
    break;
  default:
    return "(error description unavailable)";
    break;
  }
}

#endif // __EPOC32__

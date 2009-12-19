/*
 !concept {:name => "Resolving AP names",
   :desc => "Resolving an access point name to its ID on Symbian."}
*/

#include "epoc-iap.h"

#include "er_errors.h"

#include <commdb.h>
#include <utf.h>

#define MAX_IAP_NAME_LENGTH KCommsDbSvrMaxColumnNameLength

static TBool FindIapByNameL(const TDesC& aIapName, TUint32& aIapId)
{
  TBool found = EFalse;

  CCommsDatabase* commsDb = CCommsDatabase::NewL(EDatabaseTypeIAP);
  CleanupStack::PushL(commsDb);

  {
    CCommsDbTableView* tableView = 
      commsDb->OpenIAPTableViewMatchingBearerSetLC(ECommDbBearerGPRS | ECommDbBearerWLAN, ECommDbConnectionDirectionOutgoing);
    for (TInt errCode(tableView->GotoFirstRecord());
	 !errCode;
	 errCode = tableView->GotoNextRecord()) {
      TBuf<MAX_IAP_NAME_LENGTH> iapName;
      tableView->ReadTextL(TPtrC(COMMDB_NAME), iapName);
      if (iapName == aIapName) {
	tableView->ReadUintL(TPtrC(COMMDB_ID), aIapId);
	found = ETrue;
	break;
      }
    }
    CleanupStack::PopAndDestroy(tableView);
  }

  CleanupStack::PopAndDestroy(commsDb);

  return found;
}

extern "C" 
gboolean epoc_iap_by_name(const gchar* iapName, 
			  guint32* iapId, 
			  gboolean* found,
			  GError** error)
{
  *found = FALSE;

  TPtrC8 iapNameDes8((TUint8*)iapName);
  TBuf<MAX_IAP_NAME_LENGTH> iapNameDes;
  TInt errCode = CnvUtfConverter::ConvertToUnicodeFromUtf8(iapNameDes, iapNameDes8);
  if (errCode) {
    if (error)
      *error = g_error_new(domain_symbian, errCode, "Unicode conversion failure on IAP name: %s (%d)", plat_error_strerror(errCode), errCode);
    return FALSE;
  }

  TUint32 epocIapId;
  TRAP(errCode, *found = FindIapByNameL(iapNameDes, epocIapId));
  if (errCode) {
    if (error)
      *error = g_error_new(domain_symbian, errCode, "error resolving IAP name to ID: %s (%d)", plat_error_strerror(errCode), errCode);
    return FALSE;
  } else if (*found) {
    *iapId = epocIapId;
  }

  return TRUE;
}

/**

epoc-iap.cpp

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

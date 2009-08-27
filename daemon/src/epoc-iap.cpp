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

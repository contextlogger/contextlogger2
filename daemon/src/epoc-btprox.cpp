#include "epoc-btprox.hpp"

#if __BTPROX_ENABLED__

#include "cf_query.h"
#include "er_errors.h"
#include "kr_controller_private.h"
#include "ld_logging.h"
#include "utils_cl2.h"

#include "common/epoc-time.h"

#include <stdlib.h>
#include <string.h>

// might be related -- http://code.google.com/p/bt-proximity/

#define PRINT_ELEMENTS (__DO_LOGGING__ && 0)

// -------------------------------------------------------------------

#if PRINT_ELEMENTS
static void PrintElement(gpointer data, gpointer user_data)
{
  btprox_item* item = (btprox_item*)data;
  logg("element '%s' '%s'", item->address, item->name);
}

static void PrintElements(GPtrArray* array)
{
  g_ptr_array_foreach(array, &PrintElement, NULL);
}
#endif

static void FreeElement(gpointer data, gpointer user_data)
{
  btprox_item* item = (btprox_item*)data;
  g_free(item->address);
  g_free(item->name);
  g_free(item);
}

// Frees all elements, but does not modify the pointer array.
static void FreeElements(GPtrArray* array)
{
  g_ptr_array_foreach(array, &FreeElement, NULL);
}

static void FreeResult(GPtrArray* array)
{
  if (array) {
    FreeElements(array);
    g_ptr_array_free(array, TRUE);
  }
}

static void ClearResult(GPtrArray* array)
{
  FreeElements(array);
  g_ptr_array_set_size(array, 0);
}

static gint CmpString(const char* a, const char* b)
{
  if (!a && !b) return 0;
  if (a && !b) return 1;
  if (!a && b) return -1;
  return strcmp(a, b);
}

static gint CmpElement(gconstpointer a, gconstpointer b)
{
  btprox_item* ai = (btprox_item*)a;
  btprox_item* bi = (btprox_item*)b;
  return (CmpString(ai->address, bi->address) ||
	  CmpString(ai->name, bi->name));
}

static gint cmpInt(int a, int b)
{
  if (a == b)
    return 0;
  if (a < b)
    return -1;
  return 1;
}

// A good generic utility candidate. Assumes sorted arrays.
static gint g_ptr_array_cmp(GPtrArray* array0, GPtrArray* array1, GCompareFunc compareFunc)
{
  int i;
  int len = MIN(array0->len, array1->len);
  for (i=0; i<len; i++) {
    int res = (*compareFunc)(array0->pdata + i, array1->pdata + i);
    if (!res) return res;
  }
  return cmpInt(array0->len, array1->len);
}

static void SortResult(GPtrArray* array)
{
  g_ptr_array_sort(array, &CmpElement);
}

static gint CmpResults(GPtrArray* array0, GPtrArray* array1)
{
  return g_ptr_array_cmp(array0, array1, &CmpElement);
}

// -------------------------------------------------------------------

CSensor_btprox* CSensor_btprox::NewL(LogDb* aLogDb)
{
  CSensor_btprox* obj = new (ELeave) CSensor_btprox(aLogDb);
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop();
  return obj;
}

CSensor_btprox::~CSensor_btprox()
{
  Cancel();

  BtClose();
  SESSION_CLOSE_IF_OPEN(iSocketServ);
  SESSION_CLOSE_IF_OPEN(iTimer);

  FreeResult(iResult);
  FreeResult(iOldResult);
}

void CSensor_btprox::BtClose()
{
  SESSION_CLOSE_IF_OPEN(iHostResolver);
}

gboolean CSensor_btprox::StartL(GError** error)
{
  iNumScanFailures = 0;
  if (iState == EInactive) {
    TryStartScanning();
    log_db_log_status(iLogDb, NULL, "btprox sensor started");
  }
  return TRUE;
}

void CSensor_btprox::TryStartScanning()
{
  if (!EnsureBtInit()) {
    // It might be more optimal to observe BT on/off events, and
    // adjust scanning attempts accordingly; not sure if the plugin
    // API supports this.
    iNumScanFailures++;
    iState = ERetryWaiting;
  } else {
    iState = EScanWaiting;
  }
  SetTimer();
}

void CSensor_btprox::Stop()
{
  if (iState != EInactive) {
    Cancel();
    log_db_log_status(iLogDb, NULL, "btprox sensor stopped");
  }
}

CSensor_btprox::CSensor_btprox(LogDb* aLogDb) : 
  CActiveRunG(EPriorityStandard)
{
  iLogDb = aLogDb;
  CActiveScheduler::Add(this);
}

void CSensor_btprox::ConstructL()
{
  RefreshBaseScanIntervalSecs();

  SET_TRAP_OOM(User::LeaveNoMemory());
  iResult = g_ptr_array_sized_new(15); 
  iOldResult = g_ptr_array_sized_new(15);
  UNSET_TRAP_OOM();

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iTimer, iTimer.CreateLocal()); 

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iSocketServ, iSocketServ.Connect());
}

/* Returns true iff BT resolver is ready for use.
   Produces an error otherwise. */
TBool CSensor_btprox::EnsureBtInit()
{
  if (IS_SESSION_OPEN(iHostResolver)) {
    return TRUE;
  }

  TRAPD(errCode, TryBtInitL());
  if (errCode == KErrNone) {
    return TRUE;
  }

  dblogg("BT init failed in btprox scanner: %s (%d)", plat_error_strerror(errCode), errCode);
  return FALSE;
}

/* This can be expected to fail whenever Bluetooth is turned off. */
void CSensor_btprox::TryBtInitL()
{
  assert(!IS_SESSION_OPEN(iHostResolver) && "BT session already open");

  TProtocolName protocolName;
  _LIT(KBtLinkManager, "BTLinkManager");
  protocolName.Copy(KBtLinkManager);
  TProtocolDesc protocolDesc;
  User::LeaveIfError(iSocketServ.FindProtocol(protocolName, protocolDesc));
  
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iHostResolver, iHostResolver.Open(iSocketServ, protocolDesc.iAddrFamily, protocolDesc.iProtocol));
}

void CSensor_btprox::Reconfigure(const gchar* name, const gchar* value)
{
  if (strcmp(name, "sensor.btprox.interval") == 0)
    RefreshBaseScanIntervalSecs();
}

#define SCAN_INTERVAL_SECS (10 * 60) // 10 minutes

// This only takes effect the next time the timer is set.
void CSensor_btprox::RefreshBaseScanIntervalSecs()
{
  iBaseScanIntervalSecs = SCAN_INTERVAL_SECS;
  try_get_ConfigDb_int("sensor.btprox.interval",
		       &iBaseScanIntervalSecs,
		       NULL, NULL);
  dblogg("btprox scan interval set to %d secs", iBaseScanIntervalSecs);
}

void CSensor_btprox::SetTimer() 
{
  assert(iState == EScanWaiting || iState == ERetryWaiting);
  int secs = iBaseScanIntervalSecs * (1 + iNumScanFailures) + (rand() % 10);
  TTimeIntervalMicroSeconds32 interval = SecsToUsecs(secs);
  logg("btprox timer set to %d secs / %d usecs", secs, interval.Int());
  iTimer.After(iStatus, interval);
  SetActive();
}

#define SWAP(t,x,y) { t _tmp = x; x = y; y = _tmp; }

void CSensor_btprox::BtDiscover() 
{
  logt("starting btprox discovery");

  SWAP(GPtrArray*, iResult, iOldResult);
  ClearResult(iResult);

  iInquirySockAddr.SetIAC(KGIAC);
  iInquirySockAddr.SetAction(KHostResInquiry|KHostResName);
  // We should also specify KHostResIgnoreCache if we did not
  // want cache BT friendly names, but in most cases we do not
  // require genuine name discovery, and device discovery is
  // slow enough as it is.
  iHostResolver.GetByAddress(iInquirySockAddr, iNameEntry, iStatus);
  SetActive();
  iState = EDiscovering;
}

void CSensor_btprox::BtNext() 
{
  iHostResolver.Next(iNameEntry, iStatus);
  SetActive();
  iState = EDiscovering;
}

static void BtDevAddrToString(TDes8& aString, const TBTDevAddr& addr)
{
  // GetReadable() does not produce a "standard" result,
  // so have to construct a string manually.
  aString.Zero();
  _LIT8(KColon, ":");
  for (TInt i=0; i<6; i++) {
    const TUint8& val = addr[i];
    aString.AppendNumFixedWidthUC(val, EHex, 2);
    if (i < 5)
      aString.Append(KColon);
  }
}

gboolean CSensor_btprox::HandleScanEventL(TInt errCode, GError** error)
{
  if (errCode == KErrEof) // no more devices
    {
      //logt("no more bt devices");
      iNumScanFailures = 0;
      assert(iResult);
      SortResult(iResult);
      assert(iOldResult);
      if (CmpResults(iResult, iOldResult))
	{ // Log result.
#if PRINT_ELEMENTS
	  PrintElements(iResult);
#endif
	  if (!log_db_log_btprox(iLogDb, iResult, error)) 
	    {
	      return FALSE;
	    }
	}
      else 
	{
	  // There was some discussion as to whether something should be
	  // logged even when there was no change, but is that really
	  // necessary here; if there was no change, then the previous
	  // result still stands. Is it necessary to know the time
	  // of the attempt to scan a different set?
	  
	  //logt("bt device set unchanged");
	}
      iState = EScanWaiting;
      SetTimer(); // wait before scanning for more
    } 
  else if (errCode) // some error
    {
      iNumScanFailures++;
      dblogg("%dth consecutive failure in btprox: %s (%d)", 
	     iNumScanFailures, plat_error_strerror(errCode), errCode);
      iState = ERetryWaiting;
      SetTimer();
    } 
  else // no error
    {
      { // Add to result.
	TSockAddr& sockAddr = iNameEntry().iAddr;
	TBTDevAddr btDevAddr = static_cast<TBTSockAddr>(sockAddr).BTAddr();
	/*
	  TBuf<32> addrBuf;
	  btDevAddr.GetReadable(addrBuf); // has no colons
	*/
	TBuf8<6*2+5+1> addrBuf8;
	BtDevAddrToString(addrBuf8, btDevAddr);
	
	THostName& hostName = iNameEntry().iName;
	
	btprox_item* item = g_try_new0(btprox_item, 1);
	User::LeaveIfNull(item);
#define free_item_action FreeElement(item, NULL); User::Leave(KErrNoMemory);
	item->name = ConvToUtf8CString(hostName);
	if (!item->name) { free_item_action; }
	SET_TRAP_OOM(free_item_action);
	item->address = g_strdup((gchar*)(addrBuf8.PtrZ()));
	g_ptr_array_add(iResult, item);
	UNSET_TRAP_OOM();
	//logg("discovered bt device '%s' '%s'", item->address, item->name);
      }
      BtNext();
    }

  return TRUE;
}

gboolean CSensor_btprox::RunGL(GError** error)
{
  assert_error_unset(error);

  TInt errCode = iStatus.Int();
  TState oldState = iState;
  iState = EInactive;
  
  switch (oldState)
    {
    case EScanWaiting:
    case ERetryWaiting:
      {
	if (errCode) {
          // The timer expired with an error. This is rather strange
          // with interval timers.
	  if (error) {
            // If gx_error_new fails, *error will be set to NULL, which
            // we interpret as an out-of-memory error. This is can
            // only happen with the Symbian port, as normally there
            // will be an automatic abort().
	    *error = gx_error_new(domain_cl2app, code_timer, 
				  "timer failure in btprox sensor: %s (%d)", 
				  plat_error_strerror(errCode), errCode);
	  }
	  return FALSE;
	}

	if (oldState == EScanWaiting) {
	  BtDiscover();
	} else {
	  TryStartScanning();
        }

        break;
      }
    case EDiscovering:
      {
	if (!HandleScanEventL(errCode, error))
	  return FALSE;
        break;
      }
    default:
      {
	assert(0 && "unexpected state in btprox");
        break;
      }
    }
  
  return TRUE;
}

void CSensor_btprox::DoCancel()
{
  switch (iState)
    {
    case EScanWaiting:
    case ERetryWaiting:
      {
	iTimer.Cancel();
        break;
      }
    case EDiscovering:
      {
	iHostResolver.Cancel();
        break;
      }
    default:
      {
	assert(0 && "unexpected state in btprox");
      }
    }

  // Note that the state must never become anything else without
  // invoking SetActive at the same time.
  iState = EInactive;
}

const char* CSensor_btprox::Description()
{
  return "btprox";
}

#endif // __BTPROX_ENABLED__

/**

epoc-btprox.cpp

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

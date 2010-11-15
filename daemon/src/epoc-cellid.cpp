#include "epoc-cellid.hpp"

#if __CELLID_ENABLED__

#include "er_errors.h"
#include "ld_logging.h"
#include "sa_sensor_list_log_db.h"
#include "utils_cl2.h"

#include "common/utilities.h"

// -------------------------------------------------------------------
// the sensor object implementation...

CTOR_IMPL_CSensor_cellid;

static void DataChanged(bb_Blackboard* self, enum bb_DataType dt,
			gpointer data, int len, gpointer arg)
{
  (void)self;
  (void)dt;
  (void)len;
  CSensor_cellid* sensor = (CSensor_cellid*)arg;
  CTelephony::TNetworkInfoV1* info = (CTelephony::TNetworkInfoV1*)data;
  sensor->PostNewData(*info);
}

void CSensor_cellid::ConstructL()
{
  iClosure.changed = DataChanged;
  iClosure.arg = this;
  if (!bb_Blackboard_register(GetBlackboard(),
			      bb_dt_network_info,
			      iClosure,
			      NULL))
    User::LeaveNoMemory();
}

CSensor_cellid::~CSensor_cellid()
{
  Unregister();
}

void CSensor_cellid::Unregister()
{
  bb_Blackboard_unregister(GetBlackboard(), iClosure);
}

void CSensor_cellid::PostNewData(const CTelephony::TNetworkInfoV1& aData)
{
  TRAPD(errCode, PostNewDataL(aData));
  if (errCode) {
    er_log_symbian(er_FATAL, errCode, 
		   "failure processing network info in cellid sensor");
  }
}

void CSensor_cellid::PostNewDataL(const CTelephony::TNetworkInfoV1& aData)
{
  // Logging is not all that straightforward here, as some readings
  // can for instance indicate that some or all of the usual
  // (country_code, network_code, area_code, cell_id) information is
  // not available or has just become unavailable. Our present
  // solution is to log nothing unless all of that information is
  // available.
  if (!aData.iAccess) {
    //logg("cellid info: no network access: iAreaKnown=%d", (int)aData.iAreaKnown);
  } else {
    // Here we are assuming that the initial "zero" iOldData is not
    // a valid reading, and likely this is true as it would mean no
    // country code or network ID. And duplicates can still occur,
    // across sensor restarts. This should nonetheless help reduce
    // the amount of logged data quite a bit, as it seems that in
    // practice one gets around 2-4 cell ID events per minute even
    // when there is no cell change.
    if ((iOldData.iCountryCode == aData.iCountryCode) &&
	(iOldData.iNetworkId == aData.iNetworkId) &&
	(iOldData.iLocationAreaCode == aData.iLocationAreaCode) &&
	(iOldData.iCellId == aData.iCellId)) {
      // Same reading as previously.
      //logt("duplicate cell ID reading");
    } else {
      //logt("new cell ID reading");
      iOldData = aData;

      // In practice it seems that countryCode and networkCode are
      // decimal strings, but this may only apply to GSM networks, and
      // hence we are treating them as strings. Whoever parses the
      // database content may decide to do something different if they
      // see that all the data indeed is decimal strings.
      // http://en.wikipedia.org/wiki/List_of_mobile_country_codes
      // http://en.wikipedia.org/wiki/Mobile_Network_Code
      HBufC8* countryCode = ConvToUtf8ZL(aData.iCountryCode);
      CleanupStack::PushL(countryCode);
      HBufC8* networkCode = ConvToUtf8ZL(aData.iNetworkId);
      CleanupStack::PushL(networkCode);
      int areaCode = aData.iLocationAreaCode; // valid if iAccess is true
      int cellId = aData.iCellId; // valid if iAccess is true

      log_db_log_cellid(GetLogDb(), 
			(char*)(countryCode->Ptr()), 
			(char*)(networkCode->Ptr()), 
			areaCode, cellId, NULL);
      //logg("new cellid: (%s, %s, %d, %d)", (char*)(countryCode->Ptr()), (char*)(networkCode->Ptr()), areaCode, cellId);

      CleanupStack::PopAndDestroy(2); // networkCode, countryCode
    }
  }
}

#endif // __CELLID_ENABLED__

/**

epoc-cellid.cpp

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

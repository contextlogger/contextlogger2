//
// Copyright (c) 2009-2009 HIIT and Tero Hasu
// Copyright (c) 2007-2009 Google Inc.
// Copyright (c) 2006-2007 Jaiku Ltd.
// Copyright (c) 2002-2006 Mika Raento and Renaud Petit
//
// This software is licensed at your choice under either 1 or 2 below.
//
// 1. MIT License
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// 2. Gnu General Public license 2.0
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//

/*
 !concept {:name => "Observing SMS events",
   :desc => "Detecting and getting information about incoming and outgoing SMS messages."}
*/

#include "ut_sms_epoc.hpp"

#include "er_errors.h"
#include "jaiku_compat.hpp"
#include "symbian_auto_ptr.hpp"
#include "utils_cl2.h"

#include <mtclreg.h>                        // for CClientMtmRegistry 
#include <msvids.h>                         // for Message type IDs
#include <smscmds.h>
#include <smuthdr.h>
#include <smutset.h>
#include <msvuids.h>
#include <txtrich.h>
#include <smsclnt.h>
#include <mtmdef.h>
#include <gsmupdu.h>
#include <mmsconst.h>
#include <mmsclient.h>

#include <rsendas.h>
#include <rsendasmessage.h>

// --------------------------------------------------
// 
// --------------------------------------------------

void i_handle_received_sms::handle_reception(const TMsvId& entry_id, 
					     const TMsvId& folder_id, 
					     TUid aMtmUid, 
					     CBaseMtm* aMtm)
{
  if (aMtmUid == KUidMsgTypeSMS) {
    CSmsClientMtm* smsMtm = STATIC_CAST(CSmsClientMtm*, aMtm);
    handle_reception(entry_id, folder_id, 
		     smsMtm->SmsHeader().FromAddress(), 
		     // Body returns a value of type CRichText&. The
		     // value is empty for non-message contexts. The 0
		     // argument to Read is the starting position, and
		     // this only returns the first message fragment.
		     smsMtm->Body());
  }
}

void i_handle_received_sms::handle_sending(const TMsvId& entry_id, 
					   TUid aMtmUid, CBaseMtm* aMtm)
{
  if (aMtmUid == KUidMsgTypeSMS) {
    CSmsClientMtm* smsMtm = STATIC_CAST(CSmsClientMtm*, aMtm);
    handle_sending(entry_id,
		   smsMtm->SmsHeader().FromAddress(), 
		   smsMtm->Body());
  }
}

// --------------------------------------------------
// 
// --------------------------------------------------

// xxx The callback implementation needs fixing so that things will work if the callee does a "delete" on us. So there must be only one callback, and it must be the last thing that affects internal state in the handler.

CSmsEventNotifier* CSmsEventNotifier::NewL()
{
  CSmsEventNotifier* obj = new (ELeave) CSmsEventNotifier();
  CleanupStack::PushL(obj);
  obj->ConstructL();
  CleanupStack::Pop(obj);
  return obj;
}

CSmsEventNotifier::CSmsEventNotifier()
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("CSmsEventNotifier"));

}

CSmsEventNotifier::~CSmsEventNotifier()
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("~CSmsEventNotifier"));

  delete iReceiveMtm;
  delete iMMSMtm;
  delete iReceiveMtmReg;
  delete iReceiveSession;
}

void CSmsEventNotifier::ConstructL()
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("ConstructL"));

#if 1
  iReceiveSession = CMsvSession::OpenAsObserverL(*this);
#else
  iReceiveSession = CMsvSession::OpenSyncL(*this);
#endif

  iReceiveMtmReg = CClientMtmRegistry::NewL(*iReceiveSession);
  // iReceiveMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeSMS);
  // iMMSMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeMultimedia);
}

void CSmsEventNotifier::SetHandler(i_handle_received_sms* handler)
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("AddHandler"));

  iHandler = handler;
}

// See msvapi.h for the enum TMsvSessionEvent values.
//
// What will happen if we leave here? Do not know, but we won't.
void CSmsEventNotifier::HandleSessionEventL(TMsvSessionEvent aEvent, 
					    TAny* aArg1, TAny* aArg2, TAny* aArg3)
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("HandleSessionEventL"));
  //logg("TMsvSessionEvent %d", (int)aEvent);
  TRAPD(errCode, DoHandleSessionEventL(aEvent, aArg1, aArg2, aArg3));
  if (errCode)
    HandleErrorL(errCode);
}

void CSmsEventNotifier::DoHandleSessionEventL(TMsvSessionEvent aEvent, 
					      TAny* aArg1, TAny* aArg2, TAny* aArg3)
{
  DELETE_Z(iMMSMtm);
  DELETE_Z(iReceiveMtm);

  switch(aEvent) {
  case EMsvEntriesCreated:
    {
      if (!aArg1 || !aArg2 ) return;
      CMsvEntrySelection* entries=(CMsvEntrySelection *)aArg1;
      TMsvId id=*(TMsvId*)aArg2;
      if (id == KMsvGlobalInBoxIndexEntryId) {
	for (int i=0; i< entries->Count(); i++) {
	  HandleReceivedL(entries->At(i), id);
	}
      }
    }
    break;

  case EMsvEntriesMoved:      // this event is given when message entries are moved
    {
      // An entry has been moved to another parent
      // We are interested messages that have been moved to Sent folder
      if (!aArg1 || !aArg2 || !aArg3 ) return;
			
      TMsvId* toEntryId;
      TMsvId* fromEntryId;
      toEntryId = static_cast<TMsvId*>(aArg2); 
      fromEntryId = static_cast<TMsvId*>(aArg3);
			
      // We take the moved entries into a selection
      CMsvEntrySelection* entries = static_cast<CMsvEntrySelection*>(aArg1);

      // the entry has been moved into Sent folder
      if ( *toEntryId == KMsvSentEntryId )
	{
	  // Process each created entry, one at a time.
	  for(TInt i = 0; i < entries->Count(); i++)
	    {
	      HandleSentL(entries->At(i)); 
	    }
	}
    }
    break;

    // Apparently all clients should respond to these events.
  case EMsvCloseSession:
  case EMsvServerTerminated:
    {
      if (iHandler) iHandler->handle_close();
    }
    break;

  default:
    break;
  }
}

void CSmsEventNotifier::HandleErrorL(TInt aError)
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("HandleErrorL"));

  if (iHandler) iHandler->handle_error(aError);
}

// May also return KNullUid to indicate failure.
TUid CSmsEventNotifier::loadmessageL(const TMsvId& entry_id, TMsvEntry& entry)
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("loadmessageL"));

  TInt err;
	
  e_auto_ptr<CMsvEntry> realentry(0);
  CC_TRAP(err, realentry.reset(iReceiveSession->GetEntryL(entry_id)) );
  if (err != KErrNone) 
    {
      return KNullUid;
    }
  entry=realentry->Entry();

  if (entry.iMtm != KUidMsgTypeSMS && entry.iMtm != KUidMsgTypeMultimedia) 
    {
      // Not an SMS or MMS.
      return KNullUid;
    }

  CBaseMtm* mtm=0;
  // SetCurrentEntryL takes ownership of the CMsvEntry
  TMsvPartList validationFlags(KMsvMessagePartDescription|KMsvMessagePartOriginator);
  if (entry.iMtm == KUidMsgTypeSMS) {
    if (iReceiveMtm==0) iReceiveMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeSMS);
    mtm = iReceiveMtm;
    validationFlags |= KMsvMessagePartBody;
  } else {
    if (iMMSMtm==0) iMMSMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeMultimedia);
    mtm = iMMSMtm;
  }
  CC_TRAP(err, mtm->SetCurrentEntryL(realentry.get()));
  realentry.release();
  if (err!=KErrNone) 
    {
      return KNullUid;
    }
  if (mtm->ValidateMessage(validationFlags != 0)) 
    {
      // Not validated!
      return KNullUid;
    }
	
  // The message loading sometimes fails with KErrAccessDenied - the
  // message server is busy. Just retry loading.
  TInt retry_count=0; 
  const TInt MAX_RETRIES=5;
  while (retry_count<MAX_RETRIES) 
    {
      CC_TRAP(err, mtm->LoadMessageL());
      if (err==KErrAccessDenied || err==KErrInUse) {
	retry_count++;
	User::After(TTimeIntervalMicroSeconds32(100*1000)); //100 ms
      } else if (err==-1) {
	// can ignore
	return KNullUid;
      } else if (err != KErrNone) {
	return KNullUid;
      } else {
	break;
      }
    }
  if (retry_count==MAX_RETRIES) {
    return KNullUid;
  }
  return entry.iMtm;
}

void CSmsEventNotifier::HandleReceivedL(const TMsvId& entry_id, 
					const TMsvId& folder_id)
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("HandleReceivedL"));

  TMsvEntry entry;
  TUid mtmuid=loadmessageL(entry_id, entry);
  CBaseMtm* mtm = 0;
  if (mtmuid == KUidMsgTypeSMS ) {
    if (iReceiveMtm==0) iReceiveMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeSMS);
    mtm=iReceiveMtm;
  } else if ( mtmuid== KUidMsgTypeMultimedia ) {
    if (iMMSMtm==0) iMMSMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeMultimedia);
    mtm=iMMSMtm;
  } else {
    return;
  }

  if (iHandler) iHandler->handle_reception(entry_id, folder_id, mtmuid, mtm);
}

void CSmsEventNotifier::HandleSentL(const TMsvId& entry_id)
{
  CALLSTACKITEM_N(_CL("CSmsEventNotifier"), _CL("HandleSentL"));

  TMsvEntry entry;
  TUid mtmuid=loadmessageL(entry_id, entry);
		
  CBaseMtm* mtm = 0;
  if (mtmuid == KUidMsgTypeSMS ) {
    if (iReceiveMtm==0) iReceiveMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeSMS);
    mtm=iReceiveMtm;
  } else if ( mtmuid== KUidMsgTypeMultimedia ) {
    if (iMMSMtm==0) iMMSMtm = iReceiveMtmReg->NewMtmL(KUidMsgTypeMultimedia);
    mtm=iMMSMtm;
  } else {
    logt("sent: not an sms or mms");
    return;
  }

  if (iHandler) iHandler->handle_sending(entry_id, mtmuid, mtm);
}

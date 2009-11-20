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

#ifndef CL_SMS_H_INCLUDED
#define CL_SMS_H_INCLUDED

#include "application_config.h"

#include "symbian_auto_ptr.hpp"

#include <mtclbase.h>
#include <msvapi.h>
#include <mtclreg.h>

#include <f32file.h>

/*
 * now for handling both SMS and MMS
 * but compatible with old code that only handles SMS
 *
 */

NONSHARABLE_CLASS(i_handle_received_sms)
{
 public:
  virtual void handle_reception(const TMsvId& entry_id, const TMsvId& folder_id, 
				const TDesC& sender, const TDesC& body) = 0; 
  virtual void handle_reception(const TMsvId& entry_id, const TMsvId& folder_id, 
				TUid aMtmUid, CBaseMtm* aMtm);

  virtual void handle_error(TInt aError) = 0;
  virtual void handle_close() = 0;
  virtual void handle_sending(const TMsvId& entry_id, 
			      const TDesC& sender, const TDesC& body) = 0;
  virtual void handle_sending(const TMsvId& entry_id, 
			      TUid aMtmUid, CBaseMtm* aMtm);
};

NONSHARABLE_CLASS(CSmsEventNotifier) : 
  public CBase, MMsvSessionObserver
{
 public:
  static CSmsEventNotifier* NewL();
  CSmsEventNotifier();
  ~CSmsEventNotifier();
  void ConstructL();
  void SetHandler(i_handle_received_sms* handler);

 protected: // MMsvSessionObserver
  virtual void HandleSessionEventL(TMsvSessionEvent aEvent, 
				   TAny* aArg1, TAny* aArg2, TAny* aArg3);

 private:
  void DoHandleSessionEventL(TMsvSessionEvent aEvent, 
			     TAny* aArg1, TAny* aArg2, TAny* aArg3);

  void HandleReceivedL(const TMsvId& entry_id, const TMsvId& folder_id);
  void HandleErrorL(TInt aError);
  void HandleSentL(const TMsvId& entry_id);
	
 private:
  TUid loadmessageL(const TMsvId& entry_id, TMsvEntry& entry);

  CMsvSession* iReceiveSession;
  CClientMtmRegistry* iReceiveMtmReg;
  CBaseMtm	*iReceiveMtm, *iMMSMtm;
  TBuf<50> state;
		
  i_handle_received_sms*	iHandler;
};

#endif // CL_SMS_H_INCLUDED

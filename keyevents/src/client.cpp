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
//
// This file is part of the JaikuEngine mobile client.

#include "keyevents/client.h"
#include "keyevents/client_server.rh"

// The RKeyEventsAnim class is the client side class for CKeyEventsAnim
// animation class. No commands are needed (commands would normally be used in
// animation dlls to actually trigger animations/other functionality), the
// construction of this class will create the CKeyEventsAnim object
// server-side, which will notify of keypresses via the RProperty key given in
// the args as a TPckg<TKeyEventArgs>.

class RKeyEventsAnim : public RAnim {
 public:
  RKeyEventsAnim(RAnimDll* dll);
  TInt Construct(const RWindowBase& device, const TDesC8& args);

  // Inherited destructor is fine.
};

// *****************************************************************************
// RKeyEventsAnim

// The parameter dll will not be NULL, only called from CKeyEventsClient
RKeyEventsAnim::RKeyEventsAnim(RAnimDll *dll) : RAnim(*dll) {
}

TInt RKeyEventsAnim::Construct(const RWindowBase& device, const TDesC8& args) {
  // server side Anim doesn't care about parameters
  return RAnim::Construct(device, 0, args);
}

// *****************************************************************************
// CKeyEventsClient

EXPORT_C CKeyEventsClient* CKeyEventsClient::NewL(const TUid& category_uid,
    TUint key_id, RWsSession* ws_io, RWindowGroup* window_group_io) {
  if (!ws_io || !window_group_io) {
    User::Leave(KErrArgument);
  }
  CKeyEventsClient* ret = new (ELeave) CKeyEventsClient(ws_io);
  CleanupStack::PushL(ret);
  ret->ConstructL(category_uid, key_id, window_group_io);
  CleanupStack::Pop(ret);
  return ret;
}

EXPORT_C void CKeyEventsClient::OpenNotificationPropertyL(
    RProperty* property_out) {
  property_out->Attach(TUid::Uid(property_definition_.category_uid),
      property_definition_.key_id, EOwnerThread);
}

EXPORT_C CKeyEventsClient::~CKeyEventsClient() {
  // Destroy() does Close() and delete.
  if (keyevents_) keyevents_->Destroy();
  keyevents_dll_.Close();
  window_.Close();
}

CKeyEventsClient::CKeyEventsClient(RWsSession* ws_io)
    : keyevents_dll_(*ws_io), 
      window_(*ws_io),
      property_definition_pckg_(property_definition_) {
  // The parameter ws_io is checked for NULL in NewL.
}

// We use ProtServ to protect the property, as that is basically
// what the animation dll is required to have. We can't use
// SwEvent (at least not as a write policy) as the
// window server doesn't have it. Realistically, we could
// just have it world accessible.
static _LIT_SECURITY_POLICY_C1(kProtServPolicy, ECapabilityProtServ);

void CKeyEventsClient::ConstructL(const TUid& category_uid,
    TUint key_id, RWindowGroup* window_group_io) {
  // The parameter window_group_io is checked for NULL in NewL.

  property_definition_.category_uid = category_uid.iUid;
  property_definition_.key_id = key_id;

  TInt err = RProperty::Define(category_uid, key_id, RProperty::EInt,
                               kProtServPolicy, kProtServPolicy);
  if (err != KErrNone && err != KErrAlreadyExists) User::Leave(err);

  err = keyevents_dll_.Load(TPtrC(
      reinterpret_cast<const TUint16*>(
          KEYEVENTS_SERVER_DLL_NAME_STRING)));
  if (err != KErrNone)
    User::Leave(err);
  keyevents_ = new (ELeave) RKeyEventsAnim(&keyevents_dll_);
  err = window_.Construct(*window_group_io,
    reinterpret_cast<TUint32>(&window_));
  if (err != KErrNone)
    User::Leave(err);
  // By not Activate()ing the window, it stays invisible and gets no events
  err = keyevents_->Construct(window_, property_definition_pckg_);
  if (err != KErrNone)
    User::Leave(err);
}

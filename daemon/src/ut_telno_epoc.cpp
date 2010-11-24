/*
 !concept {:name => "Phone number to contact name",
   :desc => "Resolving a phone number to the name of a matching contact on Symbian."}
*/

#include "ut_telno_epoc.hpp"

#include "ac_app_context.h"
#include "er_errors.h"
#include "utils_cl2.h"

#include <cntdb.h>
#include <cntfield.h>
#include <cntfldst.h> 
#include <cntitem.h>

// The caller is to free any returned object.
static CContactItem* FindContactItemByPhoneNoL(const TDesC& phoneNo)
{
  CContactItem* item = NULL;

  if (phoneNo.Length() > 0)
  {
    // This ought to (typically) be enough for a single match. If not, too bad.
    const TInt KNumberOfDigitsToMatch = 8;

    //CContactDatabase* database = CContactDatabase::OpenL(); // xxx use a global copy
    //CleanupStack::PushL(database);

    CContactDatabase& database = ac_ContactDatabase(ac_get_global_AppContext());

    CContactIdArray* idArray = database.MatchPhoneNumberL(phoneNo, KNumberOfDigitsToMatch);
    CleanupStack::PushL(idArray);
    logg("number of contact matches: %d", idArray->Count());
    if (idArray->Count() == 1) {
      TContactItemId itemId = (*idArray)[0];
      TRAPD(errCode, item = database.ReadMinimalContactL(itemId));
      if (errCode) {
	if (errCode != KErrNotFound) User::Leave(errCode);
      }
    }
    CleanupStack::PopAndDestroy(idArray);

    //CleanupStack::PopAndDestroy(database);
  }

  return item; // may be NULL
}

static gchar* GetNameFromContactItemL(const CContactItem& item)
{
  CContactItemFieldSet& fieldSet = item.CardFields();

  TInt givenIx = fieldSet.Find(KUidContactFieldGivenName);
  TBool gotGiven = (givenIx != KErrNotFound);

  TInt familyIx = fieldSet.Find(KUidContactFieldFamilyName);
  TBool gotFamily = (familyIx != KErrNotFound);

  logg("got given name %d, surname %d", gotGiven, gotFamily);

  _LIT(KSpace, " ");

  TInt bufLen = 0;
  if (gotGiven)
    bufLen += fieldSet[givenIx].TextStorage()->Text().Length();
  if (gotGiven && gotFamily)
    bufLen += KSpace().Length();
  if (gotFamily)
    bufLen += (fieldSet[familyIx].TextStorage()->Text().Length());

  HBufC* buf = HBufC::NewLC(bufLen);
  TPtr s(buf->Des());

  if (gotGiven)
    s.Append(fieldSet[givenIx].TextStorage()->Text());
  if (gotGiven && gotFamily)
    s.Append(KSpace);
  if (gotFamily)
    s.Append(fieldSet[familyIx].TextStorage()->Text());

  gchar* ret = ConvToUtf8CStringL(s);

  CleanupStack::PopAndDestroy(buf);

  return ret;
}

// The caller is to free any returned buffer.
gchar* GetContactNameByPhoneNoL(const TDesC& phoneNo)
{
  gchar* name = NULL;
  CContactItem* item = FindContactItemByPhoneNoL(phoneNo);
  if (!item) return NULL;
  CleanupStack::PushL(item);
  name = GetNameFromContactItemL(*item);
  if (name) { logg("got contact name '%s'", name); }
  else { logt("got no contact name"); }
  CleanupStack::PopAndDestroy(item);
  return name;
}

gchar* GetContactNameByPhoneNo(const TDesC& phoneNo)
{
  gchar* name = NULL;
  TRAP_IGNORE(name = GetContactNameByPhoneNoL(phoneNo));
  return name;
}

/**

ut_telno_epoc.cpp

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

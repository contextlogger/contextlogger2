/*
 !concept {:name => "Multipart HTTP posting"}
*/

#include "up_poster_epoc.hpp"

#if __FEATURE_UPLOADER__

#include "cf_query.h" // for username

#include "common/assertions.h"
#include "common/epoc-time.h"
#include "common/logging.h"
#include "common/sh_utils.h"

#include <commdb.h>
#include <cdbpreftable.h>
#include <commdbconnpref.h>
#include <es_enum.h>
#include <f32file.h>

CTOR_IMPL_CPosterAo;

void CPosterAo::ConstructL()
{
  iFileDataSupplier = new (ELeave) CFileDataSupplier;

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iSocketServ, iSocketServ.Connect());
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iConnection, iConnection.Open(iSocketServ));

  TCommDbConnPref connPref;
  connPref.SetDialogPreference(ECommDbDialogPrefDoNotPrompt);
  connPref.SetIapId(iIapId);
  User::LeaveIfError(iConnection.Start(connPref));

  iHttpSession.OpenL(); SET_SESSION_OPEN(iHttpSession);

  // Set our RSocketServ and RConnection to the RHTTPSession instance.
  RStringPool strPool = iHttpSession.StringPool();
  RHTTPConnectionInfo connInfo = iHttpSession.ConnectionInfo();
  connInfo.SetPropertyL(strPool.StringF(HTTP::EHttpSocketServ, RHTTPSession::GetTable()), 
			THTTPHdrVal(iSocketServ.Handle()));
  connInfo.SetPropertyL(strPool.StringF(HTTP::EHttpSocketConnection, RHTTPSession::GetTable()),
			THTTPHdrVal(REINTERPRET_CAST(TInt, &(iConnection))));
}

CPosterAo::~CPosterAo()
{
  SESSION_CLOSE_IF_OPEN(iHttpTransaction);
  SESSION_CLOSE_IF_OPEN(iHttpSession);
  SESSION_CLOSE_IF_OPEN(iConnection);
  SESSION_CLOSE_IF_OPEN(iSocketServ);
  delete iFileDataSupplier;
}

// ----------------------------------------------------------------------------
// CClientEngine::SetHeaderL()
//
// Used to set header value to HTTP request
// ----------------------------------------------------------------------------
void CPosterAo::SetHeaderL(RHTTPHeaders aHeaders,
			   TInt aHdrField,
			   const TDesC8& aHdrValue)
{
  RStringF valStr = iHttpSession.StringPool().OpenFStringL(aHdrValue);
  CleanupClosePushL(valStr);
  THTTPHdrVal val(valStr);
  aHeaders.SetFieldL(iHttpSession.StringPool().
		     StringF(aHdrField,
			     RHTTPSession::GetTable()), val);
  CleanupStack::PopAndDestroy();  // valStr
}

void CPosterAo::PostComplete(TInt errCode)
{
  if (iState != EActive) return;
  logh();
  assert(iFileDataSupplier);
  iFileDataSupplier->CloseFile(); // to allow deletion
  iState = EDone;
  iObserver.PosterEvent(errCode);
}

void CPosterAo::PostFileL(const TDesC8& aUri,
			  const TDesC& aFileName)
{
  assert(iState == EReady);
  assert(iFileDataSupplier);
  iFileDataSupplier->OpenL(aFileName);
  SetBoundary(iFileDataSupplier->Boundary());
  PostGenericL(aUri, *iFileDataSupplier);
}

// ----------------------------------------------------------------------------
// CClientEngine::IssueHTTPPostL()
//
// Start a new HTTP POST transaction.
// ----------------------------------------------------------------------------

void CPosterAo::PostGenericL(const TDesC8& aUri,
			     MDataSupplier& aDataSupplier)
{
  // Parse string to URI
  TUriParser8 uri;
  uri.Parse(aUri);

  // Get request method string for HTTP POST
  RStringF method = iHttpSession.StringPool().StringF(HTTP::EPOST,
						      RHTTPSession::GetTable());

  // Open transaction with previous method and parsed URI. This class will
  // receive transaction events in MHFRunL and MHFRunError.
  SESSION_CLOSE_IF_OPEN(iHttpTransaction);
  iHttpTransaction = iHttpSession.OpenTransactionL(uri, *this, method);
  SET_SESSION_OPEN(iHttpTransaction);

  aDataSupplier.SetHttpTransaction(&iHttpTransaction);
  
  // Set headers for request; user agent, accepted content type and body's
  // content type.
  RHTTPHeaders hdr = iHttpTransaction.Request().GetHeaderCollection();
  _LIT8(KConnectionClose, "close");
  SetHeaderL(hdr, HTTP::EConnection, KConnectionClose);

  _LIT8(KContentTypeStart, "multipart/form-data, boundary=");
  TBuf8<KMultiPartBoundaryMaxLen + 30> contentType;
  contentType.Copy(KContentTypeStart);
  contentType.Append(iBoundary);
  SetHeaderL(hdr, HTTP::EContentType, contentType);

  // Set this class as an data supplier. Inherited MHTTPDataSupplier methods
  // are called when framework needs to send body data.
  iHttpTransaction.Request().SetBody(aDataSupplier);

  // Submit the transaction. After this the framework will give transaction
  // events via MHFRunL and MHFRunError.
  iHttpTransaction.SubmitL();

  iState = EActive;
}

// ----------------------------------------------------------------------------
// CClientEngine::MHFRunL()
//
// Inherited from MHTTPTransactionCallback
// Called by framework to pass transaction events.
// ----------------------------------------------------------------------------
void CPosterAo::MHFRunL(RHTTPTransaction aTransaction,
			const THTTPEvent& aEvent)
{
  TInt eventStatus = aEvent.iStatus;

  switch (eventStatus)
    {
    case THTTPEvent::EGotResponseHeaders:
      {
        // HTTP response headers have been received. Use
        // aTransaction.Response() to get the response. However, it's
        // not necessary to do anything with the response when this
        // event occurs.
        // 
        // We really only want the response code (to know whether the
        // post succeeded), but we shall read the complete response
        // nonetheless.

	// Get HTTP status code from header (e.g. 200)
	RHTTPResponse resp = aTransaction.Response();
	iHttpStatus = resp.StatusCode();
	logg("HTTP status %d", iHttpStatus);

        // xxx if we got a redirect we should really make a new
        // request, see http://mobbler.googlecode.com/

	/*
	// Get status text (e.g. "OK")
	TBuf<KStatustextBufferSize> statusText;
	statusText.Copy(resp.StatusText().DesC());
	*/
      }
      break;

    case THTTPEvent::EGotResponseBodyData:
      {
        // Part (or all) of response's body data received. Use
        // aTransaction.Response().Body()->GetNextDataPart() to get
        // the actual body data.
        // 
        // According to http://mikie.iki.fi/wordpress/?p=48, consuming
        // the body is NOT optional.

	// Get the body data supplier
	MHTTPDataSupplier* body = aTransaction.Response().Body();
	TPtrC8 dataChunk;

	// GetNextDataPart() returns ETrue, if the received part is the last
	// one.
	TBool isLast = body->GetNextDataPart(dataChunk);
	logg("body data chunk of %d bytes received", dataChunk.Length());

	// NOTE: isLast may not be ETrue even if last data part received.
	// (e.g. multipart response without content length field)
	// Use EResponseComplete to reliably determine when body is completely
	// received.
	if (isLast)
	  {
	    //logt("apparently the last chunk of the response");
	  }

	// Always remember to release the body data.
	body->ReleaseData();
      }
      break;

    case THTTPEvent::EResponseComplete:
      {
	// Indicates that header & body of response is completely received.
	// No further action here needed.
	logt("R_HTTP_TX_COMPLETE");
      }
      break;

    case THTTPEvent::ESucceeded:
      {
	// Indicates that transaction succeeded.
	logt("R_HTTP_TX_SUCCESSFUL");
        // Some codes might even indicate a permanent error, but it is
        // possible for such errors to appear as we do tweaking on the
        // server side.
	PostComplete((iHttpStatus == 200) ? POSTER_SUCCESS : POSTER_TRANSIENT_FAILURE);
      }
      break;

    case THTTPEvent::EFailed:
      {
	// Transaction completed with failure.
	logt("R_HTTP_TX_FAILED");
	PostComplete(POSTER_TRANSIENT_FAILURE); // guessing
      }
      break;

    default:
      {
	logg("THTTPEvent (%d)", eventStatus);
        // Any negative value presumably is a Symbian error, otherwise
        // we are just guessing here. And for progress events it is
        // just wrong to even complete the post.
	PostComplete((eventStatus < 0) ? eventStatus : POSTER_TRANSIENT_FAILURE);
      }
      break;
    }
}

// ----------------------------------------------------------------------------
// CClientEngine::MHFRunError()
//
// Inherited from MHTTPTransactionCallback
// Called by framework when *leave* occurs in handling of transaction event.
// These errors must be handled, or otherwise HTTP-CORE 6 panic is thrown.
// ----------------------------------------------------------------------------
TInt CPosterAo::MHFRunError(TInt aError,
			    RHTTPTransaction /*aTransaction*/,
			    const THTTPEvent& /*aEvent*/)
{
  logg("leave %d in HTTP handler", aError);
  PostComplete(aError);
  return KErrNone;
}

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// CFileDataSupplier
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

CFileDataSupplier::~CFileDataSupplier()
{
  Close();
}

void CFileDataSupplier::CloseFile()
{
  SESSION_CLOSE_IF_OPEN(iFile);
}

void CFileDataSupplier::Close()
{
  CloseFile();
  SESSION_CLOSE_IF_OPEN(iFs);
  iPrelude.Close();
  iEpilogue.Close();
}

// xxx need some metadata support here, probably, unless user identified in some other manner (now all we have is the filename encoded username, but could have a JSON part with that and more, say)

//_LIT8(KPrelude, "-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata\"; filename=\"" __USERNAME__ ".db\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\n");
//_LIT8(KEpilogue, "\r\n-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata_submit\"\r\n\r\nUpload\r\n-------AaB03xeql7dsxeql7ds--\r\n");

_LIT8(KSep, "--");
_LIT8(KCrLf, "\r\n");

_LIT8(KBoundary, "-----AaB03xeql7dsxeql7ds");

void CFileDataSupplier::OpenL(const TDesC& aFileName)
{
  Close();

  //logg("lens %d %d %d", KMaxFileName, iFileName.MaxLength(), aFileName.Length());
  iFileName = aFileName;

  const gchar* username = get_config_username();
  logg("uploader using username '%s'", username);

  iPrelude.CreateMax(200);
  iEpilogue.CreateMax(100);

  // _LIT8(KPrelude, "-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata\"; filename=\"" __USERNAME__ ".db\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\n");
  _LIT8(KPrelude1, "Content-Disposition: form-data; name=\"logdata\"; filename=\"");
  _LIT8(KPrelude2, ".db\"\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\n\r\n");
  iPrelude.CopyL(KSep);
  iPrelude.AppendL(KBoundary);
  iPrelude.AppendL(KCrLf);
  iPrelude.AppendL(KPrelude1);
  iPrelude.AppendL((const TUint8*)username, strlen(username));
  iPrelude.AppendL(KPrelude2);

  // _LIT8(KEpilogue, "\r\n-------AaB03xeql7dsxeql7ds\r\nContent-Disposition: form-data; name=\"logdata_submit\"\r\n\r\nUpload\r\n-------AaB03xeql7dsxeql7ds--\r\n");
  _LIT8(KEpilogueSubmit, "Content-Disposition: form-data; name=\"logdata_submit\"\r\n\r\nUpload\r\n");
  iEpilogue.CopyL(KCrLf);
  iEpilogue.AppendL(KSep);
  iEpilogue.AppendL(KBoundary);
  iEpilogue.AppendL(KCrLf);
  iEpilogue.AppendL(KEpilogueSubmit);
  iEpilogue.AppendL(KSep);
  iEpilogue.AppendL(KBoundary);
  iEpilogue.AppendL(KSep);
  iEpilogue.AppendL(KCrLf);

  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iFs, iFs.Connect());
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iFile, iFile.Open(iFs, iFileName, EFileStream|EFileShareAny|EFileRead));
  TInt fileSize = 0;
  User::LeaveIfError(iFile.Size(fileSize));
  //iDataLen = KPrelude().Length() + fileSize + KEpilogue().Length();
  iDataLen = iPrelude.Length() + fileSize + iEpilogue.Length();
  iPhase = 0;
}

const TDesC8& CFileDataSupplier::Boundary() const
{
  return KBoundary;
}

// It seems that this never gets invoked for the second time even when
// we return EFalse from here, not unless we invoke
// RHTTPTransaction::NotifyNewRequestBodyPartL().
// 
// HttpPostDataSupplier.cpp in the S60 WebKit port reveals that Nokia
// have despaired with this API as well, for example wrt how to handle
// leaves in here. Obviously this API has not been thought out well
// enough to actually robustly support incremental loading from any
// error-prone source.
TBool CFileDataSupplier::GetNextDataPart(TPtrC8& aDataPart) // May leave!
{
  //logg("next data part in phase %d", iPhase);
  switch (iPhase)
    {
    case 0:
      {
	//aDataPart.Set(KPrelude);
	aDataPart.Set(iPrelude);
	iPhase++;
        break;
      }
    case 1:
      {
	User::LeaveIfError(iFile.Read(iBuffer));
	//logg("read %d bytes of file data", iBuffer.Length());
	if (iBuffer.Length() > 0) {
	  aDataPart.Set(iBuffer);
	  break;
	} else {
	  iPhase++;
	  // fall through
	}
      }
    case 2:
      {
	//aDataPart.Set(KEpilogue);
	aDataPart.Set(iEpilogue);
	iPhase++;
	return ETrue; // last part
      }
    default:
      {
        assert(0 && "framework error");
        break;
      }
    }
  //logg("non-last data part has length %d", aDataPart.Length());
  return EFalse;
}

void CFileDataSupplier::ReleaseData() // May leave!
{
  // Nothing to do here, our buffer is static.
  //logt("ReleaseData invoked");
  if (iPhase < 3)
    iTransaction->NotifyNewRequestBodyPartL();
}

TInt CFileDataSupplier::Reset()
{
  //logt("Reset invoked");
  iPhase = 0;
  TInt pos; // set to new position on return
  return iFile.Seek(ESeekStart, pos);
}

TInt CFileDataSupplier::OverallDataSize()
{
  //logg("OverallDataSize is %d", iDataLen);
  return iDataLen;
}

#endif // __FEATURE_UPLOADER__

/*

Some of the code in this file has been derived from Nokia sample code,
and such code is used under and covered by the following license:


Copyright © 2006-2008 Nokia Corporation. All rights reserved.
Nokia and Nokia Connecting People are registered trademarks of Nokia Corporation. 
Java and all Java-based marks are trademarks or registered trademarks of 
Sun Microsystems, Inc. Other product and company names mentioned herein may be 
trademarks or trade names of their respective owners.


Subject to the conditions below, you may, without charge:

·  Use, copy, modify and/or merge copies of this software and 
   associated documentation files (the "Software")

·  Publish, distribute, sub-license and/or sell new software 
   derived from or incorporating the Software.



This file, unmodified, shall be included with all copies or substantial portions
of the Software that are distributed in source code form.

The Software cannot constitute the primary value of any new software derived 
from or incorporating the Software.

Any person dealing with the Software shall not misrepresent the source of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A 
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION 
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

/**

Modifications made by HIIT to the original source code are covered by
the following license:


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

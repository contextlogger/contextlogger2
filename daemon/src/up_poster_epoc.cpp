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

  //logt("doing poster init");
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iSocketServ, iSocketServ.Connect());
  //logt("socket server session open");
  LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(iConnection, iConnection.Open(iSocketServ));
  //logt("connection created");

  TCommDbConnPref connPref;
  connPref.SetDialogPreference(ECommDbDialogPrefDoNotPrompt);
  connPref.SetIapId(iIapId);
  User::LeaveIfError(iConnection.Start(connPref));
  //logt("connection preferences set");

  iHttpSession.OpenL(); SET_SESSION_OPEN(iHttpSession);
  //logt("http session opened");

  // Set our RSocketServ and RConnection to the RHTTPSession instance.
  RStringPool strPool = iHttpSession.StringPool();
  RHTTPConnectionInfo connInfo = iHttpSession.ConnectionInfo();
  connInfo.SetPropertyL(strPool.StringF(HTTP::EHttpSocketServ, RHTTPSession::GetTable()), 
			THTTPHdrVal(iSocketServ.Handle()));
  connInfo.SetPropertyL(strPool.StringF(HTTP::EHttpSocketConnection, RHTTPSession::GetTable()),
			THTTPHdrVal(REINTERPRET_CAST(TInt, &(iConnection))));
  //logt("http session set to use our connection");
}

CPosterAo::~CPosterAo()
{
  Cancel();
  SESSION_CLOSE_IF_OPEN(iHttpTransaction);
  SESSION_CLOSE_IF_OPEN(iHttpSession);
  SESSION_CLOSE_IF_OPEN(iConnection);
  SESSION_CLOSE_IF_OPEN(iSocketServ);
  iBufferDataSupplier.Close(); // safe when no data set
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
  aHeaders.SetFieldL(iHttpSession.StringPool().StringF(aHdrField,
						       RHTTPSession::GetTable()), val);
  CleanupStack::PopAndDestroy();  // valStr
}

// ----------------------------------------------------------------------------
// CClientEngine::CancelTransaction()
//
// Cancels currently running transaction and frees resources related to it.
// ----------------------------------------------------------------------------
void CPosterAo::Cancel()
{
  if(!iRunning)
    return;
  
  logh();

  iRunning = EFalse;

  // Close() also cancels transaction (Cancel() can also be used but
  // resources allocated by transaction must be still freed with Close())
  if (IS_SESSION_OPEN(iHttpTransaction)) {
    iHttpTransaction.Cancel();
  }

  if (iFileDataSupplier) {
    iFileDataSupplier->Close(); // important for closing any locked file
  }
}

void CPosterAo::PostFileL(const TDesC8& aUri,
			  const TDesC& aFileName)
{
  assert(!iRunning);

  iFileDataSupplier->OpenL(aFileName);
  SetMultiPart(iFileDataSupplier->Boundary());
  PostGenericL(aUri, *iFileDataSupplier);
}

void CPosterAo::PostBufferL(const TDesC8& aUri,
			    const TDesC8& aBody)
{
  assert(!iRunning);

  SetSinglePart();

  // Copy data to be posted into member variable; iPostData is used later in
  // methods inherited from MHTTPDataSupplier.
  iBufferDataSupplier.Set(aBody.AllocL()); // takes ownership

  PostGenericL(aUri, iBufferDataSupplier);
}

void CPosterAo::PostMultiPartBufferL(const TDesC8& aUri,
				     const TDesC8& aBody,
				     const TDesC8& aBoundary)
{
  assert(!iRunning);

  SetMultiPart(aBoundary);

  // Copy data to be posted into member variable; iPostData is used later in
  // methods inherited from MHTTPDataSupplier.
  iBufferDataSupplier.Set(aBody.AllocL()); // takes ownership

  PostGenericL(aUri, iBufferDataSupplier);
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

  if (iIsMultiPart) {
    _LIT8(KContentTypeStart, "multipart/form-data, boundary=");
    TBuf8<KMultiPartBoundaryMaxLen + 30> contentType;
    contentType.Copy(KContentTypeStart);
    contentType.Append(iBoundary);
    SetHeaderL(hdr, HTTP::EContentType, contentType);
  } else {
    _LIT8(KContentDisposition, "form-data; name=\"logdata\"; filename=\"teemuteekkari.db\"");
    _LIT8(KContentType, "application/octet-stream");
    _LIT8(KContentTransferEncoding, "binary");
    SetHeaderL(hdr, HTTP::EContentDisposition, KContentDisposition);
    SetHeaderL(hdr, HTTP::EContentType, KContentType);
    SetHeaderL(hdr, HTTP::ETransferEncoding, KContentTransferEncoding);
  }

  // Set this class as an data supplier. Inherited MHTTPDataSupplier methods
  // are called when framework needs to send body data.
  iHttpTransaction.Request().SetBody(aDataSupplier);

  // Submit the transaction. After this the framework will give transaction
  // events via MHFRunL and MHFRunError.
  iHttpTransaction.SubmitL();

  iRunning = ETrue;
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
	Cancel(); // transaction complete (important, must Close any file before posting event)
        // Some codes might even indicate a permanent error, but it is
        // possible for such errors to appear as we do tweaking on the
        // server side.
	iObserver.PosterEvent((iHttpStatus == 200) ? POSTER_SUCCESS : POSTER_TRANSIENT_FAILURE);
      }
      break;

    case THTTPEvent::EFailed:
      {
	// Transaction completed with failure.
	logt("R_HTTP_TX_FAILED");
	Cancel(); // transaction complete
	iObserver.PosterEvent(POSTER_TRANSIENT_FAILURE); // guessing
      }
      break;

    default:
      {
	logg("THTTPEvent (%d)", eventStatus);
	Cancel(); // xxx not right if for progress events
        // Any negative value presumably is a Symbian error, otherwise
        // we are just guessing here.
	iObserver.PosterEvent((eventStatus < 0) ? eventStatus : POSTER_TRANSIENT_FAILURE);
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
  Cancel(); // no more events if could not handle that one
  iObserver.PosterEvent(aError);
  return KErrNone;
}

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// RBufferDataSupplier
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

// ----------------------------------------------------------------------------
// CClientEngine::GetNextDataPart()
//
// Inherited from MHTTPDataSupplier
// Called by framework when next part of the body is needed. In this
// this provides data for HTTP post.
// ----------------------------------------------------------------------------
TBool RBufferDataSupplier::GetNextDataPart(TPtrC8& aDataPart)
{
  assert(iData);

  // Provide pointer to next chunk of data (return ETrue, if last chunk)
  // Usually only one chunk is needed, but sending big file could require
  // loading the file in small parts.
  aDataPart.Set(iData->Des());

  return ETrue;
}

// ----------------------------------------------------------------------------
// CClientEngine::ReleaseData()
//
// Inherited from MHTTPDataSupplier
// Called by framework. Allows us to release resources needed for previous
// chunk. (e.g. free buffers)
// ----------------------------------------------------------------------------
void RBufferDataSupplier::ReleaseData()
{
  // We have no "buffers" to release.
}

// ----------------------------------------------------------------------------
// CClientEngine::Reset()
//
// Inherited from MHTTPDataSupplier
// Called by framework to reset the data supplier. Indicates to the data
// supplier that it should return to the first part of the data.
// In practise an error has occured while sending data, and framework needs to
// resend data.
// ----------------------------------------------------------------------------
TInt RBufferDataSupplier::Reset()
{
  // Nothing needed since iPostData still exists and contains all the data.
  // (If a file is used and read in small parts we should seek to beginning
  // of file and provide the first chunk again in GetNextDataPart() )
  return KErrNone;
}

// ----------------------------------------------------------------------------
// CClientEngine::OverallDataSize()
//
// Inherited from MHTTPDataSupplier
// Called by framework. We should return the expected size of data to be sent.
// If it's not know we can return KErrNotFound (it's allowed and does not cause
// problems, since HTTP protocol allows multipart bodys without exact content
// length in header).
// ----------------------------------------------------------------------------
TInt RBufferDataSupplier::OverallDataSize()
{
  assert(iData);
  return iData->Length();
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

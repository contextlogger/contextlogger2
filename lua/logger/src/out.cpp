#include "out.h"

_LIT8(KIntFmt, "%d");
_LIT8(KErrorFmt, "error: %S: %d");

_LIT8(KUnknown_, "Unknown");
_LIT8(KErrNone_, "KErrNone");
_LIT8(KErrNotFound_, "KErrNotFound");
_LIT8(KErrGeneral_, "KErrGeneral");

const TPtrC8 errorMap[] = 
	{
	KUnknown_(),
	KErrNone_(),
	KErrNotFound_(),
	KErrGeneral_(),
	};

EXPORT_C TInt ROut::Create(MOut& aOut)
	{
	if (Dll::Tls() != NULL) 
		{
		return KErrInUse;
		}

	Dll::SetTls(&aOut);
	return KErrNone;
	}

EXPORT_C TInt ROut::Open()
	{
	iOut = Dll::Tls();

	return (iOut == NULL) ? KErrNotReady : KErrNone;
	}

EXPORT_C void ROut::Close()
	{
	iOut = NULL;
	}

EXPORT_C void ROut::PutChar(TChar aChar, TRequestStatus& aStatus)
	{
	if (iOut == NULL)
		{
		User::RequestComplete(&aStatus, KErrNoReady);
		return;
		}

	iOut->PutByte(aChar, aStatus);
	}

EXPORT_C void ROut::PutString(const TDesC16& aString, TRequestStatus& aStatus)
	{
	if (iOut == NULL)
		{
		User::RequestComplete(&aStatus, KErrNoReady);
		return;
		}

	iOut->PutData(reinterpret_cast<TUint8*>(aString.Ptr()), aString.Size(), aStatus);
	}

EXPORT_C void ROut::PutString(const TDesC8& aString, TRequestStatus& aStatus)
	{
	if (iOut == NULL)
		{
		User::RequestComplete(&aStatus, KErrNotReady);
		return;
		}	

	iOut->PutData(aString.Ptr(), aString.Size(), aStatus);
	}

EXPORT_C void ROut::PutInt(TInt aInt, TRequestStatus& aStatus)
	{
	if (iOut == NULL)
		{
		return KErrNotReady;
		}	
	
	iIntBuffer.Format(KIntFmt, aInt);
	iOut->PutData(iIntBuffer.Ptr(), iIntBuffer.Size(), aStatus);
	}

EXPORT_C void ROut::PutError(TInt aError, TRequestStatus& aStatus)
	{
	if (iOut == NULL)
		{
		return KErrNotReady;
		}	

	iErrorMsg.Format(KErrorFmt, MapError(aError), aError);
	iOut->PutData(iErrorMsg.Ptr(), iErrorMsg.Size(), aStatus);
	}

EXPORT_C void Printf(const TDesC8& aFmt, ...)
	{
	VA_LIST list;
	VA_START(list, aFmt);

	ROut out;
		
	if (out.Open() == KErrNone)
		{
		RBuf8 buffer;
		
		// Use 1000 characters output buffer
		if (buffer.CreateMax(1024) == KErrNone)
			{
			buffer.FormatList(aFmt, list);
		
			TRequestStatus status;

			out.PutString(buffer, status);
			User::WaitForRequest(status);
		
			buffer.Close();
			}

		out.Close();
		}

	VA_END(ap);
	}

EXPORT_C void PrintError(const TDesC8& aCtx, TInt aError)
	{
	ROut out;

	if (out.Open() == KErrNone)
		{	
		TRequestStatus status;

		out.PutString(aCtx, status);
		User::WaitForRequest(status);

		out.PutError(aError, status);
		User::WaitForRequest(status);

		out.PutChar(TChar('\n'), status);
		User::WaitForRequest(status);

		out.Close();
		}
	}

EXPORT_C const TDesC8* MapError(TInt aError)
	{
	switch(aError) 
		{
	case KErrNone:
		return &errorMap[1];
	case KErrNotFound:
		return &errorMap[2];
	case KErrGeneral:
		return &errorMap[3];
	default:
		return &errorMap[0];
		}
	// Yay, cw compiler licks balls, it's too stupid to see the switch case will always return
	return &errorMap[0];
	}


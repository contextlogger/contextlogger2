#include "fileout.h"

CFileOut* CFileOut::NewL(const TDesC& aFilename)
	{
	CFileOut* self = new (ELeave) CFileOut;
	CleanupStack::PushL(self);
	self->ConstructL(aFilename);
	CleanupStack::Pop(self);
	return self;
	}

CFileOut::CFileOut()
	{
	}

void CFileOut::ConstructL(const TDesC& aFilename)
	{
	User::LeaveIfError(iFs.Open());
	User::LeaveIfError(iFile.Replace(iFs, aFilename, EFileShareAny | EFileStreamText | EFileWrite));
	}

CFileOut::~CFileOut()
	{
	iFile.Close();
	iFs.Close();
	}

void CFileOut::PutByte(TUint8 aByte, TRequestStatus& aStatus)
	{
	iByteBuf.Copy(&aByte, sizeof(aByte));
	iFile.Write(iByteBuf, aStatus);
	}

void CFileOut::PutData(const TUint8* aData, TInt aSize, TRequestStatus& aStatus)
	{
	iDataPtr.Set(aData, aSize);
	iFile.Write(iDataPtr, aStatus);
	}


#ifndef OUT_H
#define OUT_H

#include <e32base.h>

namespace ALua
	{
	class MOut 
		{
	public:
		void PutByte(TUint8 aByte, TRequestStatus& aStatus) = 0;
		void PutData(const TUint8* aData, TInt aSize, TRequestStatus& aStatus) = 0;
		};

	NONSHARABLE_CLASS(ROut)
		{
	public:
		IMPORT_C static TInt Create(MOut& aOut);

		IMPORT_C TInt Open();
		IMPORT_C void Close();

		IMPORT_C void PutChar(TChar aChar, TRequestStatus& aStatus);
		IMPORT_C void PutString(const TDesC16& aString, TRequestStatus& aStatus);
		IMPORT_C void PutString(const TDesC8& aString, TRequestStatus& aStatus);
		IMPORT_C void PutInt(TInt aInt, TRequestStatus& aStatus);
		IMPORT_C void PutError(TInt aError, TRequestStatus& aStatus);

	private:
		MOut* iOut;
		TBuf8 iIntBuffer[10];
		TBuf8 iErrorMsg[64];
		};
	
	IMPORT_C void Puts(const char* aString);
	IMPORT_C void Printf(const TDesC8& aFmt, ...);
	IMPORT_C void PrintError(const TDesC8& aCtx, TInt aError);
	IMPORT_C const TDesC8* MapError(TInt aError);
	}

#endif // OUT_H


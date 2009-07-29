#ifndef FILEOUT_H
#define FILEOUT_H

#include "out.h"

namespace ALua
	{
	class CFileOut :
		public CBase
		public MOut
		{
	public:
		static CFileOut* NewL(const TDesC& aFilename);
		~CFileOut();

		void PutByte(TUint8 aByte, TRequestStatus& aStatus);
		void PutData(const TUint8* aData, TInt aSize, TRequestStatus& aStatus);

	private:
		CFileOut();
		void ConstructL(const TDesC& aFilename);

	private:
		RFs iFs;
		RFile iFile;
		TBuf8 iByteBuf[1];
		TPtrC8 iDataPtr;
		};
	}

#endif // FILEOUT_H


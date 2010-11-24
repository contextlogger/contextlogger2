#ifndef __epoc_rs_buf8_hpp__
#define __epoc_rs_buf8_hpp__

#include <e32std.h>

// An RBuf8 with more resizing support.
NONSHARABLE_CLASS(RRsBuf8) : 
  public RBuf8
{
 public:
  RRsBuf8() : iGranularity(16) {}
  RRsBuf8(TInt aGranularity) : iGranularity(aGranularity) {}

  void CopyL(const TDesC8 &aDes);
  void CopyL(const TUint8 *aZeroTerminatedString);
  void CopyL(const TUint8 *aBuf, TInt aLength);

  void AppendL(const TDesC8 &aDes);
  void AppendL(const TUint8 *aZeroTerminatedString);
  void AppendL(const TUint8 *aBuf, TInt aLength);
  
  const TUint8 *PtrZL();

 private:
  void ReserveL(TInt aMinRequiredCapacity);
  void ReserveFreeCapacityL(TInt aExtraSpaceLength);

 private:
   TInt iGranularity;
};

#endif /* __epoc_rs_buf8_hpp__ */

// Adapted from estring.h of EUserHL, hence this license applies.
//
// Copyright (c) 2008-2009 Nokia Corporation and/or its subsidiary(-ies).
// All rights reserved.
// This component and the accompanying materials are made available
// under the terms of "Eclipse Public License v1.0"
// which accompanies this distribution, and is available
// at the URL "http://www.eclipse.org/legal/epl-v10.html".

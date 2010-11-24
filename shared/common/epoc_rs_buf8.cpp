#include "common/epoc_rs_buf8.hpp"

// See Symbian's ossrv/lowlevellibsandfws/genericusabilitylib/ for the
// EUserHL code.

void RRsBuf8::CopyL(const TDesC8 &aDes) 
{
  ReserveL(aDes.Length());
  RBuf8::Copy(aDes);
}

void RRsBuf8::CopyL(const TUint8 *aZeroTerminatedString) 
{
  TInt len = User::StringLength(aZeroTerminatedString);
  ReserveL(len);
  RBuf8::Copy(aZeroTerminatedString, len);
}

void RRsBuf8::CopyL(const TUint8 *aBuf, TInt aLength) 
{
  ReserveL(aLength);
  RBuf8::Copy(aBuf, aLength);
}

void RRsBuf8::AppendL(const TDesC8 &aDes) 
{
  ReserveFreeCapacityL(aDes.Length());
  RBuf8::Append(aDes);
}

void RRsBuf8::AppendL(const TUint8 *aZeroTerminatedString) 
{
  TInt len = User::StringLength(aZeroTerminatedString);
  ReserveFreeCapacityL(len);
  RBuf8::Append(aZeroTerminatedString, len);
}

void RRsBuf8::AppendL(const TUint8 *aBuf, TInt aLength) 
{
  ReserveFreeCapacityL(aLength);
  RBuf8::Append(aBuf, aLength);
}
 
const TUint8 *RRsBuf8::PtrZL() 
{
  ReserveFreeCapacityL(1);
  return RBuf8::PtrZ();
}

/**
Aligns the supplied capacity to the nearest growth factor

For performance reasons the growth factor, KDefaultExpandSizeShift,
is expressed as an exponent of 2 so shifting can be used to achieve the
alignment. 

a KDefaultExpandSizeShift value of 4 is equivalent to 16; 
giving newCapacity = ((newCapacity / 16) + 1) * 16

@param aNewCapacity The size to be aligned

@return The new, aligned capacity
*/
static inline TInt AlignCapacity(TInt aNewCapacity)
{
  const TUint KDefaultExpandSizeShift = 4;
  
  return (TInt)((((TUint)aNewCapacity >> KDefaultExpandSizeShift) + 1) 
		<< KDefaultExpandSizeShift);
}

/**
Guarantees that MaxLength() is greater than or equal to the supplied
capacity, reallocating the supplied capacity if necessary.

The actual value of MaxLength() after a call may differ from the exact
value requested, but if it does differ it will always be greater. This
flexibility allows the implementation to manage heap buffers more
efficiently.

The string descriptor's heap buffer may be reallocated in order to
accommodate the new size. As a
result, MaxLength() and Ptr() may return different values afterwards,
and any existing raw pointers to into the descriptor data may be
invalidated.

@param aMinRequiredCapacity The minimum value of MaxLength() required

@leave KErrNoMemory if the underlying buffer needs to be
grown and there are insufficient resources to do so
*/
void RRsBuf8::ReserveL(TInt aMinRequiredCapacity)
{
  if (MaxLength() < aMinRequiredCapacity)
    {
      ReAllocL(AlignCapacity(aMinRequiredCapacity));
    }
}

/**
Ensures that the remaining unused space is more than the supplied value. 

May reallocate a larger storage space to meet the requirement.
As a result MaxLength() and Ptr() may return different values afterwards,
and any existing raw pointers to into the descriptor data may be
invalidated.

Typically, you use this method to reserve a known amount of required space
in one go instead of relying on the automatic growth pattern.

@param aExtraSpaceLength The extra space required.

@leave KErrNoMemory if the the buffer needs to be
reallocated and there are insufficient resources to do so.

@panic USER 11 if aLength is negative 
*/
void RRsBuf8::ReserveFreeCapacityL(TInt aExtraSpaceLength)
{
  ReserveL(Length() + aExtraSpaceLength);
}

// Adapted from estring.h of EUserHL, hence this license applies.
//
// Copyright (c) 2008-2009 Nokia Corporation and/or its subsidiary(-ies).
// All rights reserved.
// This component and the accompanying materials are made available
// under the terms of "Eclipse Public License v1.0"
// which accompanies this distribution, and is available
// at the URL "http://www.eclipse.org/legal/epl-v10.html".

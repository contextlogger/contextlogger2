//
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

#ifndef __AUTO_PTR_H__
#define __AUTO_PTR_H__  


#ifndef __E32BASE_H__
#include <e32base.h>
#endif

IMPORT_C void CloseCBaseIndirect(TAny* aPtr);
IMPORT_C void CloseHBufC16Indirect(TAny* aPtr);
IMPORT_C void CloseHBufC8Indirect(TAny* aPtr);

template<class Y> struct auto_ptr_ref
{
  Y* iPtr;
  auto_ptr_ref(Y* aPtr) : iPtr(aPtr) {}
};

template<class X> class auto_ptr
{
public:  
  typedef X element_type;

  auto_ptr(X* aPtr = 0): iBasePtr(aPtr)
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseCBaseIndirect, (void*)&iBasePtr));
#endif
  }  
  
  auto_ptr(auto_ptr& aPtr): iBasePtr(aPtr.release())
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseCBaseIndirect, (void*)&iBasePtr));
#endif
  }

  auto_ptr<X>& operator=(auto_ptr<X>& aRhs)
  {
    if (&aRhs != this)
    {
      delete iBasePtr;
      iBasePtr = aRhs.release();
    }
    return (*this); 
  }

  ~auto_ptr() 
  { 
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::Pop();
#endif
    delete iBasePtr;
  }

  X& operator *() const { return *(X*)iBasePtr; }
  X* operator ->() const { return (X*)iBasePtr; }
   
  X* get() const { return (X*)iBasePtr; }

  X* release()
  { 
    X* result = (X*)iBasePtr;
    iBasePtr = 0;
    return result; 
  }

  void reset(X* aPtr = 0) {
    if ( aPtr != iBasePtr) {
      delete iBasePtr;
      iBasePtr = aPtr;
    }
  }


  auto_ptr(auto_ptr_ref<X> aRef): iBasePtr(aRef.iBasePtr)
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseCBaseIndirect, (void*)&iBasePtr));
#endif
  }


  template <class Y> operator auto_ptr_ref<Y>() 
    { return auto_ptr_ref<Y>(this->release()); }

private:
  CBase* iBasePtr;
};

template<> class auto_ptr<HBufC16>
{
public:  

  auto_ptr(HBufC16* aPtr = 0): iPtr(aPtr)
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseHBufC16Indirect, (void*)&iPtr));
#endif
  }  
  
  auto_ptr(auto_ptr& aPtr): iPtr(aPtr.release())
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseHBufC16Indirect, (void*)&iPtr));
#endif
  }

  auto_ptr<HBufC16>& operator=(auto_ptr<HBufC16>& aRhs)
  {
    if (&aRhs != this)
    {
      delete iPtr;
      iPtr = iPtr = aRhs.release();
    }
    return (*this); 
  }

  ~auto_ptr() 
  { 
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::Pop();
#endif
    delete iPtr;
  }

  const TDesC16& operator *() const { return *iPtr; }
  HBufC16* operator ->() const { return iPtr; }
   
  HBufC16* get() const { return iPtr; }

  HBufC16* release()
 	{ 
    HBufC16* result = iPtr;
    iPtr = 0;
	return result; 
  }

  void reset(HBufC16* aPtr = 0) {
    if (aPtr != iPtr) {
      delete iPtr;
      iPtr = aPtr;
    }
  }


  auto_ptr(auto_ptr_ref<HBufC16> aRef): iPtr(aRef.iPtr)
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseHBufC16Indirect, (void*)&iPtr));
#endif
  }


  template <class Y> operator auto_ptr_ref<Y>() 
    { return auto_ptr_ref<Y>(this->release()); }
  
private:
  HBufC16* iPtr;  
};

template<> class auto_ptr<HBufC8>
{
public:  

  auto_ptr(HBufC8* aPtr = 0): iPtr(aPtr)
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseHBufC8Indirect, (void*)&iPtr));
#endif
  }  
  
  auto_ptr(auto_ptr& aPtr): iPtr(aPtr.release())
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseHBufC8Indirect, (void*)&iPtr));
#endif
  }

  auto_ptr<HBufC8>& operator=(auto_ptr<HBufC8>& aRhs)
  {
    if (&aRhs != this)
    {
      delete iPtr;
      iPtr = iPtr = aRhs.release();
    }
    return (*this); 
  }

  ~auto_ptr() 
  { 
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::Pop();
#endif
    delete iPtr;
  }

  const TDesC8& operator *() const { return *iPtr; }
  HBufC8* operator ->() const { return iPtr; }
   
  HBufC8* get() const { return iPtr; }

  HBufC8* release()
 	{ 
    HBufC8* result = iPtr;
    iPtr = 0;
	return result; 
  }

  void reset(HBufC8* aPtr = 0) {
    if (aPtr != iPtr) {
      delete iPtr;
      iPtr = aPtr;
    }
  }


  auto_ptr(auto_ptr_ref<HBufC8> aRef): iPtr(aRef.iPtr)
  {
#ifndef __LEAVE_EQUALS_THROW__
    CleanupStack::PushL(TCleanupItem(CloseHBufC8Indirect, (void*)&iPtr));
#endif
  }


  template <class Y> operator auto_ptr_ref<Y>() 
    { return auto_ptr_ref<Y>(this->release()); }
  
private:
  HBufC8* iPtr;  
};

#endif // __auto_set_to_zero_H__

#include "epocxplat.hpp"

#if (__S60_VERSION__ >= 30) && (__S60_VERSION__ < 50) 
#define HAVE_AHLECLIENT 1
#else
#define HAVE_AHLECLIENT 0
#endif

EXPORT_C TBool epocxplat::HasFeature(TFeature aFeature)
{
  switch (aFeature)
    {
    case EFeatureAhleBrowser:
      {
	return HAVE_AHLECLIENT;
      }
    default:
      {
	return EFalse;
      }
    }
}

using namespace epocxplat;

// --------------------------------------------------
// EFeatureAhleBrowser
// --------------------------------------------------
  
/*
http://wiki.forum.nokia.com/index.php/Adaptive_History_List_API
http://wiki.forum.nokia.com/index.php/Adaptive_History_List_API_for_5th_Edition
*/

EXPORT_C AhleBrowser::MNotifier* AhleBrowser::NewNotifierL()
{
#if HAVE_AHLECLIENT
  return NULL;
#else
  return NULL;
#endif
}

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
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

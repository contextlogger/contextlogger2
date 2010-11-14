#ifndef __ut_immediate_h__
#define __ut_immediate_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__SYMBIAN32__)

  // As we have an efficient Symbian-specific implementation, we use
  // it in the Symbian case.

  typedef struct _ut_Immediate ut_Immediate;

  // A non-NULL immediateError indicates an error.
  // Caller takes ownership of any immediateError.
  typedef void (ut_ImmediateCallback)(void* userdata, GError* immediateError);

  ut_Immediate* ut_Immediate_new(void* userdata, ut_ImmediateCallback* cb, 
				 GError** error);
  
  void ut_Immediate_destroy(ut_Immediate* self);

  gboolean ut_Immediate_complete(ut_Immediate* self, GError** error);

  void ut_Immediate_cancel(ut_Immediate* self);

  gboolean ut_Immediate_is_active(ut_Immediate* self);

#else

  // We have no other platform/framework specific implementations, and
  // shall instead use an implementation based on using a timer.

#include "ut_timer.h"

  typedef ut_Timer ut_Immediate;

  typedef ut_TimerCallback ut_ImmediateCallback;

#define ut_Immediate_new ut_Timer_new

#define ut_Immediate_destroy ut_Timer_destroy

#define ut_Immediate_complete(self, error) \
  ut_Timer_set_after(self, 0, error)

#define ut_Immediate_cancel ut_Timer_cancel

#define ut_Immediate_is_active ut_Timer_is_active

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __ut_immediate_h__ */

/**

ut_immediate.h

Copyright 2009-2010 Helsinki Institute for Information Technology
(HIIT) and the authors. All rights reserved.

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

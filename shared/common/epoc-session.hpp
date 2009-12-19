#ifndef __epoc_session_hpp__
#define __epoc_session_hpp__

#include <e32std.h>

#define DEF_SESSION_OPEN(x) TBool x##IsOpen
#define DEF_SESSION(t,x) t x; DEF_SESSION_OPEN(x)
#define IS_SESSION_OPEN(x) (x##IsOpen)
#define SET_SESSION_OPEN(x) x##IsOpen = ETrue
#define SET_SESSION_CLOSED(x) x##IsOpen = EFalse
#define SESSION_CLOSE_IF_OPEN(x) { if (IS_SESSION_OPEN(x)) { x.Close(); SET_SESSION_CLOSED(x); } }
#define LEAVE_IF_ERROR_OR_SET_SESSION_OPEN(x,act) { User::LeaveIfError(act); x##IsOpen = ETrue; }

#endif /* __epoc_session_hpp__ */

/**

epoc-session.hpp

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

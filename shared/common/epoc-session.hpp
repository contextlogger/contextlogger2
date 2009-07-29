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

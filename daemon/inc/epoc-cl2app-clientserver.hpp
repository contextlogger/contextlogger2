#ifndef __epoc_cl2app_clientserver_hpp__
#define __epoc_cl2app_clientserver_hpp__

// This file concerns the Symbian specific, Symbian client-server
// framework based interface that CL2 offers to local clients on
// Symbian.
//
// Note that this is an internal API that both the server and any
// clients include in their implementation. This header is not to be
// included in other header files, as it contains static data.

#include <e32std.h>

_LIT(KMyServerName, "cl2daemon");        // process name
const TUid KServerUid3 = {0xE0000000};   // xxx why such a strange uid?

enum TMyMessages
  {
    /*
    ESend,
    EReceive,
    ECancelReceive,
    */
    ETickCountFresh,
    ETryEvalScript, // testing
    EEvalGetResult,
    EEval,
    EGetResultLength,
    EGetResult
  };

#endif /* __epoc_cl2app_clientserver_hpp__ */

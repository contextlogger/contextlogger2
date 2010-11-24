// 
// This code is derived from Symbian Ltd provided and copyrighted
// example code from year 2000. Note, however, the boilerplate nature
// of most Symbian client-server code, and therefore the lack of
// originality in such code.
// 

#include "epoc-cliapi-server.hpp"

#include "epoc-cl2app-clientserver.hpp"

#include "lua_cl2.h"
#include "utils_cl2.h" // for string conversions
#include "symbian_auto_ptr.hpp"

#include "common/gxlowmem.h"
#include "common/logging.h"
#include "common/platform_error.h"
#include "common/sh_utils.h"

#include <glib.h>

// --------------------------------------------------
// utilities
// --------------------------------------------------

_LIT(KServerPanicCat, "cl2cliserv");

enum TMyPanic
  {
    EPanicBadDescriptor = 1,
    EPanicIllegalFunction,
    EPanicAlreadyReceiving
  };

static void PanicClient(const RMessagePtr2& aMessage, TMyPanic aPanicNum)
//
// RMessage::Panic() also completes the message. This is:
// (a) important for efficient cleanup within the kernel
// (b) a problem if the message is completed a second time
//
{
  aMessage.Panic(KServerPanicCat, aPanicNum);
}

// --------------------------------------------------
// session interface
// --------------------------------------------------

class CCliapiSession : public CSession2
{
public:
  CCliapiSession();
  void CreateL();
  void Send(const TDesC& aMessage);
private:
  ~CCliapiSession();
  inline CCliapiServer& Server();
  void ServiceL(const RMessage2& aMessage);
  void ServiceError(const RMessage2& aMessage,TInt aError);
  inline TBool ReceivePending() const;
private:
  RMessagePtr2 iReceiveMsg;
  TInt iReceiveLen;
};

// --------------------------------------------------
// server implementation
// --------------------------------------------------

TInt CCliapiServer::Start()
{
  return CServer2::Start(KMyServerName);
}

// This is private, so okay to define as "inline" here.
inline CCliapiServer::CCliapiServer() :
  CServer2(CActive::EPriorityStandard, ESharableSessions)
{}

CCliapiServer* CCliapiServer::NewLC()
{
  CCliapiServer* self = new (ELeave) CCliapiServer;
  CleanupStack::PushL(self);
  self->ConstructL();
  return self;
}

CCliapiServer* CCliapiServer::NewL()
{
  CCliapiServer* self = NewLC();
  CleanupStack::Pop();
  return self;
}

void CCliapiServer::ConstructL()
//
// 2nd phase construction - ensure the timer and server objects are running
//
{
  //User::LeaveIfError(StartL()); // no automatic start for us
}

CSession2* CCliapiServer::NewSessionL(const TVersion&,const RMessage2&) const
//
// Cretae a new client session. This should really check the version number.
//
{
  return new(ELeave) CCliapiSession();
}

void CCliapiServer::AddSession()
//
// A new session is being created
//
{
  ++iSessionCount;
}

void CCliapiServer::DropSession()
//
// A session is being destroyed
//
{
  iSessionCount--;
}

void CCliapiServer::Send(const TDesC& aMessage)
//
// Pass on the signal to all clients
//
{
  iSessionIter.SetToFirst();
  CSession2* s;
  while ((s=iSessionIter++)!=0)
    static_cast<CCliapiSession*>(s)->Send(aMessage);
}

// --------------------------------------------------
// session implementation
// --------------------------------------------------

inline CCliapiSession::CCliapiSession()
{}

inline CCliapiServer& CCliapiSession::Server()
{
  return *static_cast<CCliapiServer*>(const_cast<CServer2*>(CSession2::Server()));
}

inline TBool CCliapiSession::ReceivePending() const
{
  return !iReceiveMsg.IsNull();
}

void CCliapiSession::CreateL()
//
// 2nd phase construct for sessions - called by the CServer framework
//
{
  Server().AddSession();
}

CCliapiSession::~CCliapiSession()
{
  Server().DropSession();
}

void CCliapiSession::Send(const TDesC& aMessage)
//
// Deliver the message to the client, truncating if required
// If the write fails, panic the client, not the sender
//
{
  if (ReceivePending())
    {
      TPtrC m(aMessage);
      if (iReceiveLen < aMessage.Length())
	m.Set(m.Left(iReceiveLen));
      TInt r = iReceiveMsg.Write(0, m);
      if (r == KErrNone)
	iReceiveMsg.Complete(KErrNone);
      else
	PanicClient(iReceiveMsg, EPanicBadDescriptor);
    }
}

/*
class MaybeCharPtr
{
 public:
  MaybeCharPtr() : iPtr(NULL) {}
  ~MaybeCharPtr() { if (iPtr) g_free(iPtr); }
  char* iPtr;
};
*/

void CCliapiSession::ServiceL(const RMessage2& aMessage)
//
// Handle a client request.
// Leaving is handled by CCliapiServer::ServiceError() which reports
// the error code to the client
//
{
  switch (aMessage.Function())
    {
    case ETickCountFresh:
      {
	TUint tc = User::TickCount();
	TPckg<TUint> result(tc);
	aMessage.WriteL(0, result);
	aMessage.Complete(KErrNone);
	break;
      }

    case ETryEvalScript:
      {
        // It is somewhat of an issue if the client has to allocate a
        // "large enough" buffer for us to write the result to, but we
        // cannot really go and allocate memory for the client side,
        // can we now?

        TInt errCode = KErrNone;

        // No docs or examples about the use of text descriptors to be
        // found, but experimentation shows that the length is in
        // characters, not bytes, so it is the length of the original
        // descriptor, not that of the Pckg.
        TInt srcLen = aMessage.GetDesLengthL(0);

        TInt dstLen = aMessage.GetDesMaxLengthL(2);

        // Must allocate a srcLen size buffer into which to read the
        // data.
        HBufC* sc = HBufC::NewLC(srcLen);
        TPtr s(sc->Des());
        aMessage.ReadL(0, s);
        CleanupStack::PopAndDestroy(sc);

        // This service message is reserved for experiments. Now we
        // happen to do this instead of anything useful.
        TPckg<TInt> srcLenPk(dstLen);
        aMessage.WriteL(1, srcLenPk);

        aMessage.Complete(errCode);
        break;
      }

    case EEvalGetResult:
      {
	TInt errCode = KErrNone;

        // No docs or examples about the use of text descriptors to be
        // found, but experimentation shows that the length is in
        // characters, not bytes, so it is the length of the original
        // descriptor, not that of the Pckg.
	TInt srcLen = aMessage.GetDesLengthL(0);

        // It is somewhat of an issue if the client has to allocate a
        // "large enough" buffer for us to write the result to, but we
        // cannot really go and allocate memory for the client side,
        // can we now?
	TInt dstLen = aMessage.GetDesMaxLengthL(2);

        // Must allocate a srcLen size buffer into which to read the
        // data.
        HBufC* srcBuf = HBufC::NewLC(srcLen);
        TPtr srcDes(srcBuf->Des());
        aMessage.ReadL(0, srcDes);
	HBufC8* srcBuf8 = ConvToUtf8ZL(srcDes);
        CleanupStack::PopAndDestroy(srcBuf);

	e_auto_ptr<HBufC8> cleanupSrcBuf8(srcBuf8); // takes ownership
	const TUint8* srcU = srcBuf8->Ptr();
	const char* srcC = (const char*)srcU;
	logt(srcC);

        // Now must have a Lua VM evaluate the data. The expression
        // should evaluate to a string. Lua's strings are 8-bit clean,
        // so we can use UTF-8, and Unicode can hence appear in string
        // literals, but Unicode in variable names is likely to cause
        // a parse error.
	lua_State *L = cl_lua_new_libs();
	if (!L)
	  User::Leave(KErrNoMemory);
	lua_State_auto_ptr cleanupLuaState(L);

	const char* luaResult = NULL;
#define luaResultBuf_size 100
	gchar luaResultBuf[luaResultBuf_size];

        // When we don't get an actual Symbian error, we set the error
        // code KErrLuaErr; this way all the errors we write to the
        // client are Symbian style.
        //
        // Note that retrieving the error message when none is
        // available can in itself cause an exception, and a USER-EXEC
        // 3 panic.
	TInt evalErr = 0;
	TRAPD(leaveErr, evalErr = luaL_loadstring(L, srcC));
	if (leaveErr) {
	  logg("leave %d in luaL_loadstring!", leaveErr);
	  evalErr = leaveErr;
	  luaResult = "<Symbian exception in load>";
	} else if (evalErr) {
	  logg("luaL_loadstring error %d", evalErr);
	  switch (evalErr) {
	  case LUA_ERRSYNTAX:
	    {
	      luaResult = "<syntax error during precompilation>";
	      break;
	    }
	  case LUA_ERRMEM:
	    {
	      luaResult = "<out of memory>";
	      break;
	    }
	  default: 
	    {
	      if (!lua_isnone(L, -1)) // if acceptable index
		luaResult = lua_tostring(L, -1);
	      if (!luaResult)
		luaResult = "<unknown error in load>";
	      break;
	    }
	  }
	  evalErr = KErrLuaErr;
	} else /* load okay */ {
	  logt("luaL_loadstring ok");

          // We should get 0 (for success), or one of LUA_ERRRUN,
          // LUA_ERRMEM, or LUA_ERRERR (all positive values), or a
          // Symbian error code (a negative value). Plus if there was
          // an error we should have a string error message as well.
	  TRAP(leaveErr, evalErr = lua_pcall(L, 0, 1, 0));
	  if (leaveErr) {
	    // This should not happen.
	    logg("leave %d escaped lua_pcall!", leaveErr);
	    evalErr = leaveErr;
	    luaResult = "<escaped Symbian exception in eval>";
	  } else if (evalErr) {
	    logg("lua_pcall err %d", evalErr);
	    if (!lua_isnone(L, -1)) // if acceptable index
	      luaResult = lua_tostring(L, -1);
	    if (!luaResult) {
              // evalErr here may be a Symbian error as well. They are
              // negative, while the Lua errors are positive.
	      switch (evalErr) {
	      case LUA_ERRRUN:
		{
		  luaResult = "<runtime error>";
		  break;
		}
	      case LUA_ERRERR:
		{
		  luaResult = "<error handler error>";
		  break;
		}
	      case LUA_ERRMEM:
		{
		  luaResult = "<out of memory>";
		  break;
		}
	      default: 
		{
		  TRAP_OOM_FAIL({
		      g_snprintf(luaResultBuf, luaResultBuf_size,
				 "Symbian error in eval: %s (%d)",
				 symbian_error_strerror(evalErr), evalErr);
		    });
		  luaResult = luaResultBuf;
		  break;
		fail: 
		  luaResult = "<out of memory>";
		  break;
		}
	      }
	    }
	    evalErr = KErrLuaErr;
	  } else /* eval okay */ {
	    if (lua_isnone(L, -1)) {
	      luaResult = "<eval to no value>";
	    } else {
	      luaResult = lua_tostring(L, -1);
	      if (!luaResult) {
		evalErr = KErrLuaErr;
		luaResult = "<eval to non-string value>";
	      }
	    }
	  }
	}
	logt(luaResult);

	// Convert to Unicode descriptor.
	TPtrC8 resultDes8((TUint8*)luaResult);
	e_auto_ptr<HBufC> resultBuf16(ConvFromUtf8L(resultDes8)); // takes ownership
	TPtrC resultDes(*resultBuf16);

        // I think we shall return an overflow error if the result
        // does not fit into the client-side buffer, but must check
        // for that of course.
	if (resultDes.Length() > dstLen)
	  User::Leave(KErrOverflow);

        // We do not consider a Lua code evaluation error to be a
        // Symbian client-server session error as such, and we should
        // complete with KErrNone, and report the error code. An error
        // text will have been written if the error code is
        // KErrLuaErr.
        TPckg<TInt> evalErrPk(evalErr);
        aMessage.WriteL(1, evalErrPk);

	// Write the result string to the client side.
	aMessage.WriteL(2, resultDes);

	aMessage.Complete(errCode);
	break;
      }

    default:
      {
	PanicClient(aMessage, EPanicIllegalFunction);
	break;
      }
    }
}

void CCliapiSession::ServiceError(const RMessage2& aMessage,TInt aError)
//
// Handle an error from CCliapiSession::ServiceL()
// A bad descriptor error implies a badly programmed client, so panic it;
// otherwise use the default handling (report the error to the client)
//
{
  /*
  if (aError==KErrBadDescriptor)
    PanicClient(aMessage,EPanicBadDescriptor);
  */
  CSession2::ServiceError(aMessage,aError);
}

/**

epoc-cliapi-server.cpp

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

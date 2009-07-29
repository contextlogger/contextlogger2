// Basic framework code adapted from a Symbian example.

#include "sconfig.hrh"
#include "cl2cli.hpp"
#include "epoc-cl2app-clientserver.hpp"

const TInt KServerDefaultMessageSlots = 1;

// Connect to server. It must be running for this to succeed, as we
// will not attempt to start it up on demand.
EXPORT_C TInt RCl2Session::Connect()
{
  return CreateSession(KMyServerName, TVersion(0,0,0), KServerDefaultMessageSlots);
}

EXPORT_C TInt RCl2Session::TickCount(TUint& aResult)
{
  TPckg<TUint> result(aResult);
  return SendReceive(ETickCountFresh, TIpcArgs(&result));
}

EXPORT_C TInt RCl2Session::TryEval(const TDesC& aText, TInt& aResult)
{
  //TPckgC<TDesC> src(aText); // this packaged version cause ReadL problems at server end, although the length is detected just the same as with passing aText directly
  TPckg<TInt> result(aResult);
  TBuf<30> buf;
  return SendReceive(ETryEvalScript, TIpcArgs(&aText, &result, &buf));
}

// Here the problem is that the caller likely does not know how big a
// buffer to pass in. We can implement two alternative methods, one
// for evaling something and returning the result length, and another
// for retrieving data, possibly in pieces. The server side session
// can store the most recent result. But for small things where the
// result size is known (roughly at least) this variant should be
// easier.
EXPORT_C TInt RCl2Session::EvalGetResult(const TDesC& aScriptText, 
					 TInt& aEvalError,
					 TDes& aResultText)
{
  TPckg<TInt> evalErrorPk(aEvalError);
  return SendReceive(EEvalGetResult, 
		     TIpcArgs(&aScriptText, &evalErrorPk, &aResultText));
}

EXPORT_C TInt RCl2Session::Eval(const TDesC& aScriptText, 
				TInt& aEvalError,
				TInt& aResultTextSize) 
{
  TPckg<TInt> evalErrorPk(aEvalError);
  return SendReceive(EEval, 
		     TIpcArgs(&aScriptText, &evalErrorPk, &aResultTextSize));
}

EXPORT_C TInt RCl2Session::GetResultLength(TInt& aLen) 
{
  TPckg<TInt> lenPk(aLen);
  return SendReceive(EGetResultLength, TIpcArgs(&lenPk));
}

// Retrieves some of the previous Eval or EvalGetResult result.
EXPORT_C TInt RCl2Session::GetResult(TDes& aBuffer,
				     TInt aOffset,
				     TInt aLen) 
{
  return SendReceive(EGetResult, TIpcArgs(&aBuffer, &aOffset, &aLen));
}

#ifndef EKA2
GLDEF_C TInt E32Dll(TDllReason)
{
  return KErrNone;
}
#endif /*EKA2*/

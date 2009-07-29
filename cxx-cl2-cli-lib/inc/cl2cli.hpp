// -*- c++ -*-

#ifndef CL2CLI_H
#define CL2CLI_H

#include <e32std.h>

class RCl2Session : public RSessionBase
{
 public:
  IMPORT_C TInt Connect();

  IMPORT_C TInt TickCount(TUint& aResult); // dummy test thing

  IMPORT_C TInt TryEval(const TDesC& aText, TInt& aRes); // dummy test thing

  // Writes result (or error string) to aResultText, which must be
  // large enough. The return value of this function is KErrOverflow
  // when the result does not fit.
  IMPORT_C TInt EvalGetResult(const TDesC& aScriptText, 
			      TInt& aEvalError,
			      TDes& aResultText);

  // Only writes the result text size. This may be non-zero even when
  // there is an error, in which case the result string will be an
  // error string.
  IMPORT_C TInt Eval(const TDesC& aScriptText, 
		     TInt& aEvalError,
		     TInt& aResultTextSize);

  IMPORT_C TInt GetResultLength(TInt& aLen);

  // Retrieves some of the previous Eval or EvalGetResult result.
  IMPORT_C TInt GetResult(TDes& aBuffer,
			  TInt aOffset,
			  TInt aLen);
};

#endif // CL2CLI_H

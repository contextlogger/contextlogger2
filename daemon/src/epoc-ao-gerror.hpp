#ifndef __epoc_ao_gerror_hpp__
#define __epoc_ao_gerror_hpp__

#include "application_config.h"

#include "common/error_list.h"

#include <e32base.h>

#include <glib.h>

#if __IS_APPLICATION__
#include <coemain.h>
#endif

#define KGError (-5555)

// Otherwise like CActive, but provides a RunError implementation such
// that the error is logged, and then the application is exited.
NONSHARABLE_CLASS(CActiveLogErr) : public CActive
{
 protected:
  CActiveLogErr(TInt aPriority) : CActive(aPriority) {}

 protected: // CActive
  virtual TInt RunError(TInt aError);

 protected:
  virtual const char* Description() = 0;
};

// Otherwise like CActiveLogErr, but provides the RunL implementer
// with the option of erroring out with a GError value rather than a
// leave code.
NONSHARABLE_CLASS(CActiveLogErrG) : public CActiveLogErr
{
 protected:
  CActiveLogErrG(TInt aPriority) : CActiveLogErr(aPriority) {}

  virtual ~CActiveLogErrG();

 protected: // CActive
  /** As in CActiveLogErr, but supports the logging of a GError value. */
  virtual TInt RunError(TInt aError);

 protected:
  void Leave(GError* aError);

  void LeaveWithError() { User::Leave(KGError); }

 protected:
  GError* iError;
};

// This subclass of CActive supports a GLib style RunL implementation,
// one that may either set a GError, or leave.
NONSHARABLE_CLASS(CActiveRunG) : public CActiveLogErrG
{
 protected:
  CActiveRunG(TInt aPriority) : CActiveLogErrG(aPriority) {}

 protected:
  /** This function may either leave or return a GError. */
  virtual gboolean RunGL(GError** error) = 0;

 private:
  /**
     Implements RunL based on RunGL.
  */
  virtual void RunL();
};

#endif /* __epoc_ao_gerror_hpp__ */

/**

epoc-ao-gerror.hpp

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

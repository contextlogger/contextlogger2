#include "er_errors.h"

#if defined(__SYMBIAN32__)

/*

  Documentation about resolving and displaying errors can be found from:

  http://wiki.forum.nokia.com/index.php/CS000966_-_Using_class_CTextResolver_to_resolve_error_texts

  http://wiki.forum.nokia.com/index.php/CS000965_-_Using_class_CErrorUI_to_display_error_notes

*/

#include <errorui.h>

extern "C" void ex_show_error(int errCode)
{
  TRAP_IGNORE(
	CErrorUI* ui = CErrorUI::NewLC();
	ui->ShowGlobalErrorNoteL(errCode);
	CleanupStack::PopAndDestroy();
	);
}

extern "C" void ex_show_default_error()
{
  ex_show_error(KErrGeneral);
}

extern "C" void ex_show_nomem_error()
{
  ex_show_error(KErrNoMemory);
}

#endif /* __SYMBIAN32__ */

/**

er_errors_cxx.cpp

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

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
  // xxx some ignoring trap macro exists right?
  TRAPD(ignore,
	CErrorUI* ui = CErrorUI::NewLC();
	ui->ShowGlobalErrorNoteL(errCode);
	CleanupStack::PopAndDestroy();
	);
}

extern "C" void ex_show_default_error()
{
  ex_show_error(KErrGeneral);
}

#endif /* __SYMBIAN32__ */

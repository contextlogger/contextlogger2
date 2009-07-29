#include "utils_cl2.h"

#ifdef __EPOC32__
#ifdef __cplusplus

#include <utf.h> // CnvUtfConverter

// The caller must free the returned buffer. The buffer is guaranteed
// to have a zero terminator.
HBufC8* ConvToUtf8ZL(const TDesC& name16)
{
  // Note that there is no non-leaving variant of this function, such
  // that the result has no maximum size.
  // 
  // Do not know what happens if any 0 values appear in "name16"; this
  // is not documented.
  HBufC8* name8 = CnvUtfConverter::ConvertFromUnicodeToUtf8L(name16);

  // Ensure there is a zero terminator. Quite a lot of work to do this
  // efficiently.
  TPtr8 ptr = name8->Des();
  if (ptr.Length() < ptr.MaxLength()) {
    ptr.PtrZ(); // modifies HBufC8 content
  } else {
    HBufC8* bigger8 = name8->ReAlloc(ptr.Length() + 1);
    if (bigger8) {
      name8 = bigger8; // name8 already deleted by ReAlloc
      name8->Des().PtrZ(); // modifies HBufC8 content
    } else {
      delete name8;
      User::Leave(KErrNoMemory);
    }
  }

  return name8;
}

// The caller must free the returned buffer. The buffer is guaranteed
// to have a zero terminator. Returns NULL in an out-of-memory
// situation.
HBufC8* ConvToUtf8(const TDesC& name16)
{
  HBufC8* res = NULL;
  TRAPD(errCode, res = ConvToUtf8ZL(name16));
  return res;
}

// We really just have this here for the convenience of not having to
// include yet another header file when doing charset conversions. Yet
// one case where we sacrifice performance for implementation
// convenience. [language problem]
HBufC* ConvFromUtf8L(const TDesC8& name8)
{
  HBufC16* name16 = CnvUtfConverter::ConvertToUnicodeFromUtf8L(name8);
  return name16;
}

// The caller must free the return value with g_free.
gchar* ConvToUtf8CStringL(const TDesC& name16)
{
  HBufC8* des = ConvToUtf8ZL(name16);
  int strLen = des->Length() + 1;
  char* str = (char*)(g_try_malloc(strLen));
  if (!str) {
    delete des;
    User::Leave(KErrNoMemory);
  }
  memcpy(str, des->Ptr(), strLen);
  delete des;
  return str;
}

// Returns NULL if fails to allocate.  
gchar* ConvToUtf8CString(const TDesC& name16)
{
  gchar* value = NULL;
  TRAPD(errCode, value = ConvToUtf8CStringL(name16));
  return value;
}

// Returns a Symbian error code, or number of unconverted characters
// if the provided buffer is too small. The buffer will be set to the
// empty string if the return value is negative.
int ConvToUtf8CString(gchar* buf, int bufLen, const TDesC& name16)
{
  TPtr8 text((TUint8*)buf, bufLen - 1);
  TInt errCode = CnvUtfConverter::ConvertFromUnicodeToUtf8(text, name16);
  if (errCode < 0) {
    *(buf) = '\0';
    return errCode;
  }
  *(buf + text.Length()) = '\0';
  return errCode;
}

#endif // __cplusplus
#endif // __EPOC32__

#if PRINTF_DOUBLE_BUGGY
// This is only an approximation. The semantics are not exactly the
// same, and hence we are using a different name.
extern "C" gint g_snprintf_fix(gchar *string,
			       gulong n,
			       gchar const *format,
			       ...)
{
  VA_LIST argp;
  VA_START(argp, format);

  TPtr8 buf((TUint8*)string, n - 1); // length set to 0
  TPtrC8 fmt((TUint8*)format); // like strlen for length
  buf.FormatList(fmt, argp);
  buf.PtrZ();
  
  return buf.Length();
}
#endif // PRINTF_DOUBLE_BUGGY


#if PRINTF_DOUBLE_BUGGY
// This is only an approximation. The semantics are not exactly the
// same, and hence we are using a different name.
//
// Note that this implementation is not general purpose, since the
// size of the internal buffer is limited.
extern "C" void g_string_append_printf_fix(GString *gs,
					   const gchar *format,
					   ...)
{
  static const int bufSize = 100;
  TBuf8<bufSize> buf;

  VA_LIST argp;
  VA_START(argp, format);

  TPtrC8 fmt((TUint8*)format); // like strlen for length
  buf.FormatList(fmt, argp);

  g_string_append(gs, (gchar*)buf.PtrZ());

#if 0
  char buf[bufSize];

  va_list argp;
  va_start(argp, fmt);
  // vsnprintf would be safer but itself buggy.
  int ret = vsprintf(buf, fmt, argp);
  va_end(argp); // any cleanup

  if (ret >= 0 && ret < bufSize) {
    // No error, and everything did fit.
    g_string_append(gs, buf);
  } else {
    assert(ret < bufSize && "likely memory corruption");
  }
#endif
}
#endif

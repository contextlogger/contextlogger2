#include "common/utilities.h"
#include "platform_config.h"
#include <glib.h>
#include <glib/gprintf.h>

#if defined(__EPOC32__) && __HAS_THREAD_STACK_INFO__
// This functionality is inspired by RLogMan.
EXTERN_C char* get_stack_info_string()
{
  // Seeing as we are dealing with TUint32 values, converting these to
  // Python integers is non-trivial, as Py_BuildValue does not take
  // unsigned int values. So we better calculate values that ought to
  // fit into a signed integer.
  TLinAddr sp = 0; // TLinAddr == TUint32, TLinAddr in v9-up
  sp = (TLinAddr)&sp;
  RThread thread;
  TThreadStackInfo threadStackInfo;
  thread.StackInfo(threadStackInfo);
  TUint32 cFree = sp - threadStackInfo.iLimit;
  TUint32 cUsed = threadStackInfo.iBase - sp;
  TUint32 cTotal = threadStackInfo.iBase - threadStackInfo.iLimit;
  return g_strdup_printf("stack free:%u used:%u total:%u", 
			 static_cast<unsigned int>(cFree), 
			 static_cast<unsigned int>(cUsed), 
			 static_cast<unsigned int>(cTotal)); 
}
#else
EXTERN_C char* get_stack_info_string()
{
  return g_strdup("<stack info unavailable>");
}
#endif

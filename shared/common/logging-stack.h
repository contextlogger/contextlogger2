// logging utilities with stack info logging
#ifndef __logging_stack_h__
#define __logging_stack_h__

#include "common/logging.h"

#if __DO_LOGGING__

#include "common/utilities.h" // get_stack_info_string()
#define log_stack_info(f) { char* _s = get_stack_info_string(); log_text(f,_s); g_free(_s); }

#else

#define log_stack_info(f) ((void)0)

#endif

#define logst log_stack_info(PRIMARY_LOG_FILENAME)

#endif /* __logging_stack_h__ */

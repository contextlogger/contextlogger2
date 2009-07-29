#ifndef __UTILITIES_H__
#define __UTILITIES_H__

#include "common/sh_utils.h"

// The caller must free the buffer
EXTERN_C char* get_stack_info_string();

#ifdef __EPOC32__
#ifdef __cplusplus

#include "common/epoc-utilities.hpp"

#endif // __cplusplus
#endif // __EPOC32__

// Forms a JSON String text out of the given UTF-8 text.
// The string will be quoted, and still UTF-8.
EXTERN_C char* utf8ToJsonString(const char* text);

#endif // __UTILITIES_H__

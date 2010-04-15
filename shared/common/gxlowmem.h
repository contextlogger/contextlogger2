#ifndef __gxlowmem_h__
#define __gxlowmem_h__

#if defined(__SYMBIAN32__)
#include "common/glowmem_action.h"
#else
#define SET_LOW_MEMORY_TRAP_VOID()
#define SET_LOW_MEMORY_TRAP(failure_value)
#define SET_LOW_MEMORY_TRAP_ACTION(action)
#define REMOVE_LOW_MEMORY_TRAP()
#endif /* not __SYMBIAN32__ */

#endif /* __gxlowmem_h__ */


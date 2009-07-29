#ifndef __panic_h__
#define __panic_h__

#ifdef __cplusplus
extern "C" {
#endif

// code:: Consider using one of the integer values defined in
//        error_list.h.
void Panic(int code);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __panic_h__ */

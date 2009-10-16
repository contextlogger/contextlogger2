#ifndef __sh_utils_h__
#define __sh_utils_h__

#include <string.h> // memset

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

#define memzero(x) memset(&(x), 0, sizeof(x))

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif

#ifdef __cplusplus
#define DEFINE_CLEANUP_CLASS(name,type,free) \
class name \
{ \
 public: \
 name(type aObj) : iObj(aObj) {} \
  ~##name() { free; }		 \
 private: \
  type iObj; \
}
#endif

#if defined(__SYMBIAN32__)
#define IF_SYMBIAN_EXPR(x,y) (x)
#define IF_SYMBIAN(x,y) { x ; }
#define WHEN_SYMBIAN(stmt) { stmt ; }
#define UNLESS_SYMBIAN(stmt)
#else
#define IF_SYMBIAN_EXPR(x,y) (y)
#define IF_SYMBIAN(x,y) { y ; }
#define WHEN_SYMBIAN(stmt)
#define UNLESS_SYMBIAN(stmt) { stmt ; }
#endif /* __SYMBIAN32__ */

#if defined(__SYMBIAN32__) && defined(__cplusplus)
#define DEFINE_FOR_SYMBIAN_CXX(def) def
#define WHEN_SYMBIAN_CXX(stmt) { stmt ; }
#define UNLESS_SYMBIAN_CXX(stmt)
#else
#define DEFINE_FOR_SYMBIAN_CXX(def)
#define WHEN_SYMBIAN_CXX(stmt)
#define UNLESS_SYMBIAN_CXX(stmt) { stmt ; }
#endif

#endif /* __sh_utils_h__ */

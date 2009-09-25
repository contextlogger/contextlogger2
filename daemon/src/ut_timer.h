#ifndef __ut_timer_h__
#define __ut_timer_h__

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _ut_Timer ut_Timer;

  // A non-NULL timerError indicates an error.
  // Caller takes ownership of any timerError.
  typedef void (ut_TimerCallback)(void* userdata, GError* timerError);

  ut_Timer* ut_Timer_new(void* userdata, ut_TimerCallback* cb, GError** error);
  
  void ut_Timer_destroy(ut_Timer* self);

  gboolean ut_Timer_set_after(ut_Timer* self, int secs, GError** error);

  void ut_Timer_cancel(ut_Timer* self);

  gboolean ut_Timer_is_active(ut_Timer* self);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __ut_timer_h__ */

#include "common/panic.h"
#include "application_config.h"
#include <e32std.h> // User

extern "C" void Panic(int code)
{
  DEFINE_PANIC_CATEGORY;
  User::Panic(KPanicCategory, code);
}

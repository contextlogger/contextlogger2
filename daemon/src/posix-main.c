#include "client-run.h"

int main()
{
  cl2GlobalInit();
  return cl2RunOnceGetExitCode();
}

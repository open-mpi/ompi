/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"


int lam_abort(int status, char *fmt, ...)
{
  va_list arglist;

  /* If there was a message, output it */

#if __STDC__
  va_start(arglist, fmt);
#else
  va_start(arglist);
#endif
  if (NULL != fmt) {
    lam_output(0, fmt);
  }
  va_end(arglist);

  /* Shut down and exit */

  lam_finalize();
  exit(status);
}

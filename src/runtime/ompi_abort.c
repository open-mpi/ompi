/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"


int ompi_abort(int status, char *fmt, ...)
{
  va_list arglist;

  /* If there was a message, output it */

  va_start(arglist, fmt);
  if (NULL != fmt) {
    ompi_output(0, fmt);
  }
  va_end(arglist);

  /* Shut down and exit */

  ompi_finalize();
  exit(status);
}

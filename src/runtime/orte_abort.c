/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"


int orte_abort(int status, char *fmt, ...)
{
  va_list arglist;

  /* If there was a message, output it */

  va_start(arglist, fmt);
  if (NULL != fmt) {
    ompi_output(0, fmt);
  }
  va_end(arglist);

  /* Shut down and exit */

  orte_finalize();
  exit(status);
}

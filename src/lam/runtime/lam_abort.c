/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "lam/util/output.h"


int lam_abort(int status, char *message)
{
  if (NULL != message) {
    lam_output(0, message);
  }

  lam_finalize();

  exit(status);
}

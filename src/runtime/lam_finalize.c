/*
 * $HEADER$
 */

#include "lam_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "mem/malloc.h"


int lam_finalize(void)
{
    /* Shut down malloc debug stuff */
    lam_malloc_finalize();
  
    /* Shut down the output streams */
    lam_output_finalize();

    return LAM_SUCCESS;
}

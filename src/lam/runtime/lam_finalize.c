/*
 * $HEADER$
 */

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "lam/util/output.h"
#include "mpi/group/group.h"

int lam_finalize(void)
{
    /* free group resources */
    lam_group_finalize();

    /* Shut down malloc debug stuff */
    lam_malloc_finalize();
  
    /* Shut down the output streams */
    lam_output_finalize();

    return LAM_SUCCESS;
}

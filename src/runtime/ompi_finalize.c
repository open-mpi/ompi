/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "mem/malloc.h"


int ompi_finalize(void)
{
    /* Shut down malloc debug stuff */
    ompi_malloc_finalize();
  
    /* Shut down the output streams */
    ompi_output_finalize();

    return OMPI_SUCCESS;
}

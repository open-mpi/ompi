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

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/malloc.h"
#include "util/output.h"


int ompi_finalize(void)
{
    /* Shut down malloc debug stuff */
    ompi_malloc_finalize();
  
    /* Shut down the output streams */
    ompi_output_finalize();

    return OMPI_SUCCESS;
}

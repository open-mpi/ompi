/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"

#include "include/orte_constants.h"
#include "util/malloc.h"
#include "opal/util/output.h"
#include "mca/base/base.h"

#include "runtime/opal.h"

/**
 * Initialize the OPAL utilities
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 */
int opal_init(void)
{

    /* initialize the memory allocator */
    ompi_malloc_init();

    /* initialize the output system */
    opal_output_init();
    
    /* initialize the mca */
    mca_base_open();

    return ORTE_SUCCESS;
}


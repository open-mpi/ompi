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

#include "orte/include/orte_constants.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"
#include "opal/memory/memory.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/mca/memory/base/base.h"

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
    opal_malloc_init();

    /* initialize the memory manager / tracker */
    opal_mem_free_init();

    /* initialize the output system */
    opal_output_init();
    
    /* initialize the mca */
    mca_base_open();

    /* open the memory manager components.  Memory hooks may be
       triggered before this (any time after mem_free_init(),
       actually).  This is a hook available for memory manager hooks
       without good initialization routine support */
    opal_memory_base_open();

    return ORTE_SUCCESS;
}


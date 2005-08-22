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

#include "opal/util/malloc.h"
#include "opal/util/output.h"
#include "opal/memory/memory.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/timer/base/base.h"
#include "opal/include/constants.h"
#include "opal/util/error.h"

 
static const char *
opal_err2str(int errnum)
{
    const char *retval;

    switch (errnum) {
    case OPAL_SUCCESS:
        retval = "Success";
        break;
    case OPAL_ERROR:
        retval = "Error";
        break;
    case OPAL_ERR_OUT_OF_RESOURCE:
        retval = "Out of resource";
        break;
    case OPAL_ERR_NOT_FOUND:
        retval = "Not found";
        break;
    case OPAL_ERR_BAD_PARAM:
        retval = "Bad parameter";
        break;
    default: 
        retval = NULL;
    }

    return retval;
}


/**
 * Initialize the OPAL utilities
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
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

    /* register handler for errnum -> string converstion */
    opal_error_register(opal_err2str);
    
    /* initialize the mca */
    mca_base_open();

    /* open the processor affinity base */
    opal_paffinity_base_open();
    opal_paffinity_base_select();

    /* open the memory manager components.  Memory hooks may be
       triggered before this (any time after mem_free_init(),
       actually).  This is a hook available for memory manager hooks
       without good initialization routine support */
    opal_memory_base_open();

    opal_timer_base_open();

    return OPAL_SUCCESS;
}


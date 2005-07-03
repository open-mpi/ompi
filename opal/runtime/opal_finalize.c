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
#include "opal/class/opal_object.h"
#include "util/output.h"
#include "util/malloc.h"
#include "mca/base/base.h"
#include "runtime/opal.h"

/**
 * Finalize the OPAL utilities
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 */
int opal_finalize(void)
{

    /* finalize the mca */
    mca_base_close();

    /* finalize the output system */
    ompi_output_finalize();
    
    /* finalize the class/object system */
    opal_class_finalize();

    /* finalize the memory allocator */
    ompi_malloc_finalize();

    return ORTE_SUCCESS;
}

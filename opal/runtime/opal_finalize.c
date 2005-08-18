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

#include "opal/class/opal_object.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/memory/memory.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "orte/include/orte_constants.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/timer/base/base.h"

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
    opal_timer_base_close();

    /* close the memory manager components.  Registered hooks can
       still be fired any time between now and the call to
       opal_mem_free_finalize(), and callbacks from the memory manager
       hooks to the bowels of the mem_free code can still occur any
       time between now and end of application (even post main()!) */
    opal_memory_base_close();

    /* finalize the mca */
    mca_base_close();

    /* finalize the output system */
    opal_output_finalize();

    /* finalize the memory manager / tracker */
    opal_mem_free_finalize();
    
    /* finalize the class/object system */
    opal_class_finalize();

    /* finalize the memory allocator */
    opal_malloc_finalize();

    return ORTE_SUCCESS;
}

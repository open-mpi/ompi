/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#define OMPI_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/runtime/params.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/constants.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "ompi/mca/mpool/base/static-components.h"

#include "mpool_base_tree.h"
/*
 * Global variables
 */
int mca_mpool_base_output = -1;

/* should we attempt to use the available memory hooks */
int mca_mpool_base_use_mem_hooks = 0; 
/* should we attempt to use mallopt to disable free() returning memory
   to OS? */
int mca_mpool_base_mallopt_disable_free = 0;

uint32_t mca_mpool_base_page_size; 
uint32_t mca_mpool_base_page_size_log;

opal_list_t mca_mpool_base_components;
opal_list_t mca_mpool_base_modules;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_mpool_base_open(void)
{
    /* Open up all available components - and populate the
       mca_mpool_base_components list */
    
    int use_mem_hooks;
    int no_mallopt;
    
    if (OMPI_SUCCESS != 
        mca_base_components_open("mpool", 0, mca_mpool_base_static_components, 
                               &mca_mpool_base_components, true)) {
        return OMPI_ERROR;
    }
  
     /* Initialize the list so that in mca_mpool_base_close(), we can
        iterate over it (even if it's empty, as in the case of ompi_info) */

    OBJ_CONSTRUCT(&mca_mpool_base_modules, opal_list_t);
  
    /* 
     * check for use_mem_hooks (for diagnostics/testing) 
     * however if leave_pinned is set we force this to be enabled
     */
    mca_base_param_reg_int_name("mpool", 
                                "base_use_mem_hooks", 
                                "use memory hooks for deregistering freed memory",
                                false, 
                                false, 
                                0,
                                &mca_mpool_base_use_mem_hooks); 
    
    mca_base_param_reg_int_name("mpool", 
                                "use_mem_hooks", 
                                "(deprecated, use mpool_base_use_mem_hooks)",
                                false, 
                                false, 
                                0,
                                &use_mem_hooks); 
    
    mca_mpool_base_use_mem_hooks = use_mem_hooks || mca_mpool_base_use_mem_hooks;

    
#if OMPI_MPOOL_BASE_HAVE_LINUX_MALLOPT
    mca_base_param_reg_int_name("mpool", 
                                "base_disable_mallopt",
                                "do not use mallopt to disable returning memory to "
                                "the OS when leave_pinned is active and no memory "
                                "components are found.",
                                false, 
                                false, 
                                0,
                                &no_mallopt);
#else
    no_mallopt = 1;
#endif

    /* force mem hooks if leave_pinned or leave_pinned_pipeline is enabled */
    if (ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline) {
        mca_mpool_base_use_mem_hooks = 1;
    }

    /* enable mallopt if we're using leave pinned, there is support
       for intercepting munmap, and the user didn't tell us not to use
       mallopt */
    if ((ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline) &&
        (0 != (OPAL_MEMORY_MUNMAP_SUPPORT & opal_mem_hooks_support_level())) &&
        0 == no_mallopt) {
        mca_mpool_base_mallopt_disable_free = 1;        
    }
    
    /* get the page size for this architecture*/ 
    mca_mpool_base_page_size = sysconf(_SC_PAGESIZE); 
    mca_mpool_base_page_size_log = my_log2(mca_mpool_base_page_size); 

    /* setup tree for tracking MPI_Alloc_mem */ 
    mca_mpool_base_tree_init();
    
    return OMPI_SUCCESS;
}


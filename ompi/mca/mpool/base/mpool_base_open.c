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


#include "ompi_config.h"
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/runtime/params.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/constants.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif  /* HAVE_UNISTD_H */

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
int mca_mpool_base_use_mem_hooks = 0; 

#if MPOOL_BASE_CAN_DISABLE_SBRK
int mca_mpool_base_disable_sbrk = 0;
#endif

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
#if MPOOL_BASE_CAN_DISABLE_SBRK
    int disable_sbrk;
#endif  /* MPOOL_BASE_CAN_DISABLE_SBRK */
    
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

    
#if MPOOL_BASE_CAN_DISABLE_SBRK
    mca_base_param_reg_int_name("mpool", 
                                "base_disable_sbrk", 
                                "use mallopt to override calling sbrk (doesn't return memory to OS!)",
                                false, 
                                false, 
                                0,
                                &mca_mpool_base_disable_sbrk);

    mca_base_param_reg_int_name("mpool", 
                                "disable_sbrk", 
                                "(deprecated, use mca_mpool_base_disable_sbrk)",
                                false, 
                                false, 
                                0,
                                &disable_sbrk);
    
    mca_mpool_base_disable_sbrk = disable_sbrk || mca_mpool_base_disable_sbrk;

#endif

    /* force mem hooks if leave_pinned or leave_pinned_pipeline is enabled */
#if MPOOL_BASE_CAN_DISABLE_SBRK
    if(0 == mca_mpool_base_use_mem_hooks && 0 == mca_mpool_base_disable_sbrk) {
#else 
    if(0 == mca_mpool_base_use_mem_hooks ) {
#endif
        if (ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline) {
            mca_mpool_base_use_mem_hooks = 1;
        }
    }
    
    /* get the page size for this architecture*/ 
    mca_mpool_base_page_size = sysconf(_SC_PAGESIZE); 
    mca_mpool_base_page_size_log = my_log2(mca_mpool_base_page_size); 

    /* setup tree for tracking MPI_Alloc_mem */ 
    mca_mpool_base_tree_init();
    
    return OMPI_SUCCESS;
}


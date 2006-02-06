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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"
#include "ompi/include/constants.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif  /* HAVE_UNISTD_H */

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "ompi/mca/mpool/base/static-components.h"

/*
 * Global variables
 */
int mca_mpool_base_output = -1;
int mca_mpool_base_use_mem_hooks = 0; 

#ifdef HAVE_MALLOC_H
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
                              "use_mem_hooks", 
                              "use memory hooks for deregistering freed memory",
                              false, 
                              false, 
                              0,
                              &mca_mpool_base_use_mem_hooks); 
        
#ifdef HAVE_MALLOC_H
    mca_base_param_reg_int_name("mpool", 
                                "disable_sbrk", 
                                "use mallopt to override calling sbrk (doesn't return memory to OS!)",
                                false, 
                                false, 
                                0,
                                &mca_mpool_base_disable_sbrk); 
#endif
    
    /* force mem hooks if leave_pinned is enabled */
    if(0 == mca_mpool_base_use_mem_hooks) {
         int param;
         mca_base_param_register_int("mpi", NULL, "leave_pinned", "leave_pinned", 0);
         param = mca_base_param_find("mpi", NULL, "leave_pinned");
         mca_base_param_lookup_int(param, &mca_mpool_base_use_mem_hooks);
    } 
    
    /* get the page size for this architecture*/ 
    mca_mpool_base_page_size = sysconf(_SC_PAGESIZE); 
    mca_mpool_base_page_size_log = my_log2(mca_mpool_base_page_size); 
    return OMPI_SUCCESS;
}


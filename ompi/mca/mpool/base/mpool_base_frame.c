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
 * Copyright (c) 2007-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif  /* HAVE_UNISTD_H */

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/mca/mpool/base/base.h"
#include "mpool_base_mem_cb.h"
#include "ompi/constants.h"
#include "opal/util/sys_limits.h"

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

/* whether we actually used the mem hooks or not */
int mca_mpool_base_used_mem_hooks = 0;

uint32_t mca_mpool_base_page_size; 
uint32_t mca_mpool_base_page_size_log;

opal_list_t mca_mpool_base_modules;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_mpool_base_open(mca_base_open_flag_t flags)
{
    /* Open up all available components - and populate the
       ompi_mpool_base_framework.framework_components list */
    if (OMPI_SUCCESS != 
        mca_base_framework_components_open(&ompi_mpool_base_framework, flags)) {
        return OMPI_ERROR;
    }
  
     /* Initialize the list so that in mca_mpool_base_close(), we can
        iterate over it (even if it's empty, as in the case of ompi_info) */

    OBJ_CONSTRUCT(&mca_mpool_base_modules, opal_list_t);
  
    /* get the page size for this architecture*/ 
    mca_mpool_base_page_size = opal_getpagesize();
    mca_mpool_base_page_size_log = my_log2(mca_mpool_base_page_size); 

    /* setup tree for tracking MPI_Alloc_mem */ 
    mca_mpool_base_tree_init();
    
    return OMPI_SUCCESS;
}

static int mca_mpool_base_close(void)
{
  opal_list_item_t *item;
  mca_mpool_base_selected_module_t *sm;
  int32_t modules_length;

  /* Need the initial length in order to know if some of the initializations
   * are done in the open function.
   */
  modules_length = opal_list_get_size(&mca_mpool_base_modules);

  /* Finalize all the mpool components and free their list items */

  while(NULL != (item = opal_list_remove_first(&mca_mpool_base_modules))) {
    sm = (mca_mpool_base_selected_module_t *) item;

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore).  Note that it's legal for the module to have NULL for
       the finalize function. */

    if (NULL != sm->mpool_module->mpool_finalize) {
        sm->mpool_module->mpool_finalize(sm->mpool_module);
    }
    OBJ_RELEASE(sm);
  }

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */
  (void) mca_base_framework_components_close(&ompi_mpool_base_framework, NULL);

  /* deregister memory free callback */
  if( (modules_length > 0) && mca_mpool_base_used_mem_hooks && 
     0 != (OPAL_MEMORY_FREE_SUPPORT & opal_mem_hooks_support_level())) {
      opal_mem_hooks_unregister_release(mca_mpool_base_mem_cb);
  }

  mca_mpool_base_tree_fini();
  /* All done */

  return OMPI_SUCCESS;
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, mpool, NULL, NULL, mca_mpool_base_open,
                           mca_mpool_base_close, mca_mpool_base_static_components, 0);

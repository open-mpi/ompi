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
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved
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
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/constants.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/rcache/base/static-components.h"


/*
 * Global variables
 */
opal_list_t mca_rcache_base_modules;


OBJ_CLASS_INSTANCE(mca_rcache_base_selected_module_t, opal_list_item_t, NULL, NULL);

static int mca_rcache_base_close(void)
{
  opal_list_item_t *item;
  mca_rcache_base_selected_module_t *sm;

  /* Finalize all the rcache components and free their list items */

  for (item = opal_list_remove_first(&mca_rcache_base_modules);
       NULL != item; 
       item = opal_list_remove_first(&mca_rcache_base_modules)) {
    sm = (mca_rcache_base_selected_module_t *) item;

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore).  Note that it's legal for the module to have NULL for
       the finalize function. */

    if (NULL != sm->rcache_module->rcache_finalize) {
        sm->rcache_module->rcache_finalize(sm->rcache_module);
    }
    OBJ_RELEASE(sm);
  }

  /* Close all remaining available components */
    return mca_base_framework_components_close(&ompi_rcache_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_rcache_base_open(mca_base_open_flag_t flags)
{
  /* Initialize the list so that in mca_rcache_base_close(), we can
     iterate over it (even if it's empty, as in the case of the ompi_info-tool) */

  OBJ_CONSTRUCT(&mca_rcache_base_modules, opal_list_t);

     /* Open up all available components */
    return mca_base_framework_components_open(&ompi_rcache_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, rcache, "OMPI Rcache", NULL,
                           mca_rcache_base_open, mca_rcache_base_close,
                           mca_rcache_base_static_components, 0);


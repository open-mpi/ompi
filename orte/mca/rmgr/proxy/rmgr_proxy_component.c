/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "include/orte_constants.h"
#include "util/proc_info.h"
#include "opal/util/output.h"
#include "mca/errmgr/errmgr.h"

#include "mca/rds/base/base.h"
#include "mca/ras/base/base.h"
#include "mca/rmaps/base/base.h"
#include "mca/pls/base/base.h"
#include "rmgr_proxy.h"

/*
 * Local functions
 */

static int orte_rmgr_proxy_open(void);
static int orte_rmgr_proxy_close(void);
static orte_rmgr_base_module_t* orte_rmgr_proxy_init(int *priority);


orte_rmgr_proxy_component_t mca_rmgr_proxy_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RMGR_BASE_VERSION_1_0_0,

        "proxy", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        orte_rmgr_proxy_open,  /* component open  */
        orte_rmgr_proxy_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_rmgr_proxy_init
    }
};


/**
  * component open/close/init function
  */
static int orte_rmgr_proxy_open(void)
{
    return ORTE_SUCCESS;
}


static orte_rmgr_base_module_t *orte_rmgr_proxy_init(int* priority)
{
    *priority = 1;
    return &orte_rmgr_proxy_module;
}


/**
 *  Close all subsystems.
 */
static int orte_rmgr_proxy_close(void)
{
    return ORTE_SUCCESS;
}

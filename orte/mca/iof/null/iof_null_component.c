/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "orte/mca/rml/rml.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "iof_null.h"

/*
 * Local functions
 */
static int orte_iof_null_open(void);
static orte_iof_base_module_t* orte_iof_null_init(
    int* priority, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads);


orte_iof_null_component_t mca_iof_null_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_IOF_BASE_VERSION_1_0_0,

        "null", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_iof_null_open,  /* component open  */
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      orte_iof_null_init
    },
    false,
    /*{{NULL, 0}}*/
};

/*
 * component open/init function
 */
static int orte_iof_null_open(void)
{
    mca_base_param_reg_int(&mca_iof_null_component.super.iof_version,
                           "override",
                           "Whether to use the null IOF component or not",
                           false, false, 0, 
                           &mca_iof_null_component.null_override);
    return ORTE_SUCCESS;
}


static orte_iof_base_module_t* 
orte_iof_null_init(int* priority, bool *allow_multi_user_threads, 
                   bool *have_hidden_threads)
{
    /* Only be used in a PBS environment -- this component is
       currently *only* for debugging */

    if (0 != mca_iof_null_component.null_override &&
        (NULL != getenv("PBS_ENVIRONMENT") &&
         NULL != getenv("PBS_JOBID"))) {
        *priority = 50;
        *allow_multi_user_threads = true;
        *have_hidden_threads = false;

        return &orte_iof_null_module;
    } 

    return NULL;
}

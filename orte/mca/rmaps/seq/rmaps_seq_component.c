/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/mca/rmaps/rmaps.h"
#include "rmaps_seq.h"

/*
 * Local functions
 */

static int orte_rmaps_seq_open(void);
static int orte_rmaps_seq_close(void);
static orte_rmaps_base_module_t* orte_rmaps_seq_init(int* priority);


orte_rmaps_base_component_t mca_rmaps_seq_component = {
      {
        /* Indicate that we are a rmaps v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_RMAPS_BASE_VERSION_1_3_0,

        "seq", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmaps_seq_open,  /* component open  */
        orte_rmaps_seq_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      orte_rmaps_seq_init
};


/**
  * component open/close/init function
  */
static int orte_rmaps_seq_open(void)
{
    return ORTE_SUCCESS;
}


static orte_rmaps_base_module_t* 
orte_rmaps_seq_init(int *priority)
{
    /* the RMAPS framework is -only- opened on HNP's,
     * so no need to check for that here
     */
    
    *priority = 0; /* only select if specified */
    return &orte_rmaps_seq_module;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_seq_close(void)
{
    return ORTE_SUCCESS;
}



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
 * Copyright (c) 2008      Voltaire. All rights reserved
 *  
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/ras/ras_types.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file_lex.h"

/*
 * Local functions
 */

static int orte_rmaps_rank_file_open(void);
static int orte_rmaps_rank_file_close(void);
static orte_rmaps_base_module_t* orte_rmaps_rank_file_init(int* priority);
char *orte_mca_rmaps_rank_file_slot_list = NULL;

orte_rmaps_rank_file_component_t mca_rmaps_rank_file_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rmaps v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_RMAPS_BASE_VERSION_1_3_0,

        "rank_file", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmaps_rank_file_open,  /* component open  */
        orte_rmaps_rank_file_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      orte_rmaps_rank_file_init
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_rank_file_open(void)
{
    int index, paffinity_alone;

    mca_base_param_reg_int(&mca_rmaps_rank_file_component.super.rmaps_version, "priority",
                           "Selection priority for Rank File RMAPS component",
                           false, false, 1,
                           &mca_rmaps_rank_file_component.priority);

    mca_base_param_reg_string(&mca_rmaps_rank_file_component.super.rmaps_version,
                              "path",
                              "The path to the rank mapping file",
                              false, false, NULL, &orte_rmaps_rank_file_path);
    if (NULL != orte_rmaps_rank_file_path) {
        index = mca_base_param_find("rank_file",NULL,NULL); 
        if ( OPAL_ERROR != index) {
            mca_base_param_set_string(index,"rank_file");
        }
        mca_base_param_lookup_string(index, &orte_rmaps_rank_file_path);
        /* if rankfile path is present than set higher priority to this component */
        mca_rmaps_rank_file_component.priority = 1000000;
    } else {
        mca_rmaps_rank_file_component.priority = 0;
    }

    index = mca_base_param_find("opal", NULL, "paffinity_slot_list");
    if (index >= 0) {
        if (OPAL_SUCCESS == mca_base_param_lookup_string(index, &orte_mca_rmaps_rank_file_slot_list)) {
            if (NULL != orte_mca_rmaps_rank_file_slot_list) {
                mca_rmaps_rank_file_component.priority = 1000000;
            }
        }
    } else {
        mca_rmaps_rank_file_component.priority = 0;
    }

    index = mca_base_param_find("opal", NULL, "paffinity_alone");
    if (index >= 0) { 
        if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &paffinity_alone)) {
            if ( 1000000 == mca_rmaps_rank_file_component.priority && paffinity_alone ){
                 opal_output(0, "WARNING: paffinity_alone cannot be set with paffinity_slot_list or rank_file\nTherefore mca_rmaps_rank_file_component.priority set to 0\n");
                mca_rmaps_rank_file_component.priority = 0;
            }
        }
    }
    return ORTE_SUCCESS;
}

static orte_rmaps_base_module_t* 
orte_rmaps_rank_file_init(int *priority)
{
    /* the RMAPS framework is -only- opened on HNP's,
     * so no need to check for that here
     */

    *priority = mca_rmaps_rank_file_component.priority;

    return &orte_rmaps_rank_file_module;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_rank_file_close(void)
{
    return ORTE_SUCCESS;
}


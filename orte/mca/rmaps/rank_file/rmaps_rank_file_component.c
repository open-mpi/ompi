/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/mca/ras/ras_types.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file_lex.h"

/*
 * Local functions
 */

static int orte_rmaps_rank_file_open(void);
static int orte_rmaps_rank_file_close(void);
static int orte_rmaps_rank_file_query(mca_base_module_t **module, int *priority);

static int my_priority;

orte_rmaps_rf_component_t mca_rmaps_rank_file_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        {
            ORTE_RMAPS_BASE_VERSION_2_0_0,

            "rank_file", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_rmaps_rank_file_open,  /* component open  */
            orte_rmaps_rank_file_close, /* component close */
            orte_rmaps_rank_file_query  /* component query */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_rank_file_open(void)
{
    mca_base_component_t *c = &mca_rmaps_rank_file_component.super.base_version;
    int tmp;

    mca_base_param_reg_int(c, "priority",
                           "Priority of the rank_file rmaps component",
                           false, false, 0,
                           &my_priority);
    
    /* did the user provide a slot list? */
    tmp = mca_base_param_reg_string(c, "slot_list",
                           "List of processor IDs to bind MPI processes to (e.g., used in conjunction with rank files) [default=NULL]",
                           false, false, NULL, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "base_slot_list", false);
    mca_base_param_lookup_string(tmp, &mca_rmaps_rank_file_component.slot_list);

    /* ensure we flag mapping by user */
    if (NULL != mca_rmaps_rank_file_component.slot_list ||
        NULL != orte_rankfile) {
        ORTE_ADD_MAPPING_POLICY(ORTE_MAPPING_BYUSER);
        /* make us first */
        my_priority = 10000;
    }
    
    return ORTE_SUCCESS;
}

static int orte_rmaps_rank_file_query(mca_base_module_t **module, int *priority)
{
    *priority = my_priority;
    *module = (mca_base_module_t *)&orte_rmaps_rank_file_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_rank_file_close(void)
{
    return ORTE_SUCCESS;
}

static void rf_map_construct(orte_rmaps_rank_file_map_t *ptr)
{
    ptr->node_name = NULL;
    memset(ptr->slot_list, (char)0x00, 64);
}
static void rf_map_destruct(orte_rmaps_rank_file_map_t *ptr)
{
    if (NULL != ptr->node_name) free(ptr->node_name);
}
OBJ_CLASS_INSTANCE(orte_rmaps_rank_file_map_t,
                   opal_object_t,
                   rf_map_construct,
                   rf_map_destruct);

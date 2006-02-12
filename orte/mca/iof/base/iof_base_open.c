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


#include "orte_config.h"
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_header.h"
#include "orte/mca/iof/base/iof_base_fragment.h"
#include "opal/util/output.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public orte_base_component_t struct.
 */

#include "orte/mca/iof/base/static-components.h"


/*
 * Global variables
 */

orte_iof_base_t orte_iof_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_iof_base_open(void)
{
    int id;
    int int_value;
    char* str_value;

    /* Initialize globals */
    OBJ_CONSTRUCT(&orte_iof_base.iof_components_opened, opal_list_t);
    OBJ_CONSTRUCT(&orte_iof_base.iof_endpoints, opal_list_t);
    OBJ_CONSTRUCT(&orte_iof_base.iof_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_iof_base.iof_condition, opal_condition_t);
    OBJ_CONSTRUCT(&orte_iof_base.iof_fragments, opal_free_list_t);
    orte_iof_base.iof_waiting = 0;
    orte_iof_base.iof_flush = false;

    /* lookup common parameters */
    id = mca_base_param_register_int("iof","base","window_size",NULL,ORTE_IOF_BASE_MSG_MAX << 1);
    mca_base_param_lookup_int(id,&int_value);
    orte_iof_base.iof_window_size = int_value;

    id = mca_base_param_register_string("iof","base","service",NULL,"0.0.0");
    mca_base_param_lookup_string(id,&str_value);
    orte_ns.convert_string_to_process_name(&orte_iof_base.iof_service, str_value);
    free(str_value);

    /* Debugging / verbose output */

    id = mca_base_param_reg_int_name("iof_base", "verbose", 
                                     "Verbosity level for the iof framework",
                                     false, false, 0, &int_value);
    if (int_value != 0) {
        orte_iof_base.iof_output = opal_output_open(NULL);
    } else {
        orte_iof_base.iof_output = -1;
    }

    /* initialize free list */
    opal_free_list_init(
        &orte_iof_base.iof_fragments,
        sizeof(orte_iof_base_frag_t),
        OBJ_CLASS(orte_iof_base_frag_t),
        0,   /* number to initially allocate */
        -1,  /* maximum elements to allocate */
        32);  /* number per allocation */

    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("iof", orte_iof_base.iof_output,
                                 mca_iof_base_static_components, 
                                 &orte_iof_base.iof_components_opened,
                                 true)) {
        return ORTE_ERROR;
    }
  
    /* All done */
    return ORTE_SUCCESS;
}


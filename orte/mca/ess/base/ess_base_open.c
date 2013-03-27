/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/ess/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/ess/base/static-components.h"

opal_list_t orte_ess_base_components_available;
orte_ess_base_module_t orte_ess = {
    NULL,  /* init */
    NULL,  /* finalize */
    NULL,  /* abort */
    NULL   /* ft_event */
};
int orte_ess_base_output;
int orte_ess_base_std_buffering = -1;
int orte_ess_base_num_procs = -1;
char *orte_ess_base_jobid = NULL;
char *orte_ess_base_vpid = NULL;

static mca_base_var_enum_value_t stream_buffering_values[] = {
  {-1, "default"},
  {0, "unbuffered"},
  {1, "line_buffered"},
  {2, "fully_buffered"},
  {0, NULL}
};

static int
orte_ess_base_register(void)
{
    mca_base_var_enum_t *new_enum;
    int ret;

    orte_ess_base_std_buffering = -1;
    (void) mca_base_var_enum_create("ess_base_stream_buffering", stream_buffering_values, &new_enum);
    (void) mca_base_var_register("orte", "ess", "base", "stream_buffering",
                                 "Adjust buffering for stdout/stderr "
                                "[-1 system default] [0 unbuffered] [1 line buffered] [2 fully buffered] "
                                 "(Default: -1)",
                                 MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &orte_ess_base_std_buffering);
    OBJ_RELEASE(new_enum);

    orte_ess_base_jobid = NULL;
    ret = mca_base_var_register("orte", "ess", "base", "jobid", "Process jobid",
                                MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                MCA_BASE_VAR_FLAG_INTERNAL,
                                OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY, &orte_ess_base_jobid);
    mca_base_var_register_synonym(ret, "orte", "orte", "ess", "jobid", 0);

    orte_ess_base_vpid = NULL;
    ret = mca_base_var_register("orte", "ess", "base", "vpid", "Process vpid",
                                MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                MCA_BASE_VAR_FLAG_INTERNAL,
                                OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY, &orte_ess_base_vpid);
    mca_base_var_register_synonym(ret, "orte", "orte", "ess", "vpid", 0);

    orte_ess_base_num_procs = -1;
    ret = mca_base_var_register("orte", "ess", "base", "num_procs",
                                "Used to discover the number of procs in the job",
                                MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                MCA_BASE_VAR_FLAG_INTERNAL,
                                OPAL_INFO_LVL_9,
                                MCA_BASE_VAR_SCOPE_READONLY, &orte_ess_base_num_procs);
    mca_base_var_register_synonym(ret, "orte", "orte", "ess", "num_procs", 0);

    return ORTE_SUCCESS;
}

int
orte_ess_base_open(void)
{
    (void) orte_ess_base_register();

    orte_ess_base_output = opal_output_open(NULL);
    
    OBJ_CONSTRUCT(&orte_ess_base_components_available, opal_list_t);

    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("ess", orte_ess_base_output, mca_ess_base_static_components, 
                                 &orte_ess_base_components_available,
                                 true)) {
        /* error message emitted by fn above */
        return ORTE_ERR_SILENT;
    }

    return ORTE_SUCCESS;
}

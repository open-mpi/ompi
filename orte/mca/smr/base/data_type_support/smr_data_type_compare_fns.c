/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/smr/base/smr_private.h"

/*
 * EXIT CODE
 */
int orte_smr_base_compare_exit_code(orte_exit_code_t *value1,
                                    orte_exit_code_t *value2,
                                    orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

/*
 * NODE STATE
 */
int orte_smr_base_compare_node_state(orte_node_state_t *value1,
                                     orte_node_state_t *value2,
                                     orte_node_state_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

/*
 * PROC STATE
 */
int orte_smr_base_compare_proc_state(orte_proc_state_t *value1,
                                     orte_proc_state_t *value2,
                                     orte_proc_state_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

/*
 * JOB STATE
 */
int orte_smr_base_compare_job_state(orte_job_state_t *value1,
                                    orte_job_state_t *value2,
                                    orte_job_state_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

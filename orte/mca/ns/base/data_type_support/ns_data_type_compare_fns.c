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
#include "orte/orte_constants.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/ns_private.h"

/*
 * NUMERIC COMPARE FUNCTIONS
 */
int orte_ns_base_compare_name(orte_process_name_t *value1,
                              orte_process_name_t *value2,
                              orte_data_type_t type)
{
    if (NULL == value1 && NULL == value2) {
    return ORTE_EQUAL;
    } else if (NULL == value1) {
    return ORTE_VALUE2_GREATER;
    } else if (NULL == value2) {
       return ORTE_VALUE1_GREATER;
    }

    /* If any of the fields are wildcard,
     * then we want to just ignore that one field. In the case
     * of ORTE_NAME_WILDCARD (where ALL of the fields are wildcard), this
     * will automatically result in ORTE_EQUAL for any name in the other
     * value - a totally useless result, but consistent in behavior.
     */

    /** check the cellids - if one of them is WILDCARD, then ignore
    * this field since anything is okay
    */
    if (value1->cellid != ORTE_CELLID_WILDCARD &&
        value2->cellid != ORTE_CELLID_WILDCARD) {
        if (value1->cellid < value2->cellid) {
            return ORTE_VALUE2_GREATER;
        } else if (value1->cellid > value2->cellid) {
            return ORTE_VALUE1_GREATER;
        }
    }
    
    /** check the jobids - if one of them is WILDCARD, then ignore
    * this field since anything is okay
    */
    if (value1->jobid != ORTE_JOBID_WILDCARD &&
        value2->jobid != ORTE_JOBID_WILDCARD) {
        if (value1->jobid < value2->jobid) {
            return ORTE_VALUE2_GREATER;
        } else if (value1->jobid > value2->jobid) {
            return ORTE_VALUE1_GREATER;
        }
    }
    
    /** check the vpids - if one of them is WILDCARD, then ignore
    * this field since anything is okay
    */
    if (value1->vpid != ORTE_VPID_WILDCARD &&
        value2->vpid != ORTE_VPID_WILDCARD) {
        if (value1->vpid < value2->vpid) {
            return ORTE_VALUE2_GREATER;
        } else if (value1->vpid > value2->vpid) {
            return ORTE_VALUE1_GREATER;
        }
    }
    
    /** only way to get here is if all fields are equal or WILDCARD */
    return ORTE_EQUAL;
}

int orte_ns_base_compare_vpid(orte_vpid_t *value1,
                              orte_vpid_t *value2,
                              orte_data_type_t type)

{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORTE_VPID_WILDCARD ||
        *value2 == ORTE_VPID_WILDCARD) return ORTE_EQUAL;
    
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

int orte_ns_base_compare_jobid(orte_jobid_t *value1,
                               orte_jobid_t *value2,
                               orte_data_type_t type)
{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORTE_JOBID_WILDCARD ||
        *value2 == ORTE_JOBID_WILDCARD) return ORTE_EQUAL;
    
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

int orte_ns_base_compare_cellid(orte_cellid_t *value1,
                                orte_cellid_t *value2,
                                orte_data_type_t type)
{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORTE_CELLID_WILDCARD ||
        *value2 == ORTE_CELLID_WILDCARD) return ORTE_EQUAL;
    
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

int orte_ns_base_compare_nodeid(orte_nodeid_t *value1,
                                orte_nodeid_t *value2,
                                orte_data_type_t type)
{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORTE_NODEID_WILDCARD ||
        *value2 == ORTE_NODEID_WILDCARD) return ORTE_EQUAL;
    
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;
    
    if (*value2 > *value1) return ORTE_VALUE2_GREATER;
    
    return ORTE_EQUAL;
}

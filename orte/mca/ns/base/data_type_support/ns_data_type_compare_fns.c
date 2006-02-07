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
#include "orte/include/orte_constants.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/base.h"

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

    /* for this generic compare, go through the progression */
    if (value1->cellid < value2->cellid) {
        return ORTE_VALUE2_GREATER;
    } else if (value1->cellid > value2->cellid) {
        return ORTE_VALUE1_GREATER;
    }

    /* get here if jobid's are equal - now check process group */
    if (value1->jobid < value2->jobid) {
        return ORTE_VALUE2_GREATER;
    } else if (value1->jobid > value2->jobid) {
        return ORTE_VALUE1_GREATER;
    }

    /* get here if cellid's and jobid's are equal - now check vpid */
    if (value1->vpid < value2->vpid) {
        return ORTE_VALUE2_GREATER;
    } else if (value1->vpid > value2->vpid) {
        return ORTE_VALUE1_GREATER;
    }

    /* only way to get here is if all fields are equal */
    return ORTE_EQUAL;
}

int orte_ns_base_compare_vpid(orte_vpid_t *value1,
                              orte_vpid_t *value2,
                              orte_data_type_t type)

{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_ns_base_compare_jobid(orte_jobid_t *value1,
                               orte_jobid_t *value2,
                               orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

int orte_ns_base_compare_cellid(orte_cellid_t *value1,
                                orte_cellid_t *value2,
                                orte_data_type_t type)
{
    if (*value1 > *value2) return ORTE_VALUE1_GREATER;

    if (*value2 > *value1) return ORTE_VALUE2_GREATER;

    return ORTE_EQUAL;
}

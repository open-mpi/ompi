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
/** @file:
 *
 */

#include "orte_config.h"

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/mca/mca.h"

#include "orte/mca/schema/schema_types.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/ns_private.h"

/*
 * "not available" functions
 */
int
orte_ns_base_create_jobid_not_available(orte_jobid_t *jobid, opal_list_t *attrs)
{
    *jobid = ORTE_JOBID_INVALID;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_get_vpid_range_not_available(orte_jobid_t job,
                                       orte_vpid_t range,
                                       orte_vpid_t *startvpid)
{
    *startvpid = ORTE_VPID_INVALID;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int orte_ns_base_get_job_descendants_not_available(orte_jobid_t** descendants,
                                                   orte_std_cntr_t *num_desc,
                                                   orte_jobid_t job)
{
    *descendants = NULL;
    *num_desc = 0;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int orte_ns_base_get_job_children_not_available(orte_jobid_t** children,
                                                orte_std_cntr_t *num_childs,
                                                orte_jobid_t job)
{
    *children = NULL;
    *num_childs = 0;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int orte_ns_base_get_root_job_not_available(orte_jobid_t *root_job, orte_jobid_t job)
{
    *root_job = ORTE_JOBID_INVALID;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int orte_ns_base_get_parent_job_not_available(orte_jobid_t *parent, orte_jobid_t job)
{
    *parent = ORTE_JOBID_INVALID;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}


/****    JOB STRING FUNCTIONS    ****/
int orte_ns_base_get_jobid_string(char **jobid_string, const orte_process_name_t* name)
{
    if (NULL == name) { /* got an error */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        *jobid_string = NULL;
       return ORTE_ERR_BAD_PARAM;
    }

    /* check for wildcard value - handle appropriately */
    if (ORTE_JOBID_WILDCARD == name->jobid) {
        *jobid_string = strdup(ORTE_SCHEMA_WILDCARD_STRING);
        return ORTE_SUCCESS;
    }

    if (0 > asprintf(jobid_string, "%ld", (long) name->jobid)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_jobid_to_string(char **jobid_string, const orte_jobid_t jobid)
{
    /* check for wildcard value - handle appropriately */
    if (ORTE_JOBID_WILDCARD == jobid) {
        *jobid_string = strdup(ORTE_SCHEMA_WILDCARD_STRING);
        return ORTE_SUCCESS;
    }
    
    if (0 > asprintf(jobid_string, "%ld", (long) jobid)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_string_to_jobid(orte_jobid_t *jobid, const char* jobidstring)
{
    long int tmpint;

    if (NULL == jobidstring) {  /* got an error */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        *jobid = ORTE_JOBID_INVALID;
        return ORTE_ERR_BAD_PARAM;
    }

    tmpint = strtoul(jobidstring, NULL, 10);

    /* check for invalid value */
    if (ORTE_JOBID_INVALID == tmpint) {
        *jobid = ORTE_JOBID_INVALID;
        return ORTE_SUCCESS;
    }

    if (ORTE_JOBID_MAX >= tmpint && ORTE_JOBID_MIN <= tmpint) {
       *jobid = (orte_jobid_t)tmpint;
    } else {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
       *jobid = ORTE_JOBID_INVALID;
       return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;
}



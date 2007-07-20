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
orte_ns_base_create_nodeids_not_available(orte_nodeid_t **nodeids, orte_std_cntr_t *nnodes, char **nodename)
{
    *nodeids = NULL;
    *nnodes = 0;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_get_node_info_not_available(char ***nodenames, orte_std_cntr_t num_nodeids, orte_nodeid_t *nodeids)
{
    *nodenames = NULL;
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}


/****    NODEID STRING FUNCTIONS     ****/
int orte_ns_base_convert_nodeid_to_string(char **string, const orte_nodeid_t nodeid)
{
    *string = NULL;
    
    /* check for wildcard value - handle appropriately */
    if (ORTE_NODEID_WILDCARD == nodeid) {
        *string = strdup(ORTE_SCHEMA_WILDCARD_STRING);
        return ORTE_SUCCESS;
    }
    
    /* check for invalid value - handle appropriately */
    if (ORTE_NODEID_INVALID == nodeid) {
        *string = strdup(ORTE_SCHEMA_INVALID_STRING);
        return ORTE_SUCCESS;
    }
    
    if (0 > asprintf(string, "%ld", (long)nodeid)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    return ORTE_SUCCESS;
}


int orte_ns_base_convert_string_to_nodeid(orte_nodeid_t *nodeid, const char* string)
{
    long int tmpint;
    
    if (NULL == string) {  /* got an error */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        *nodeid = ORTE_NODEID_INVALID;
        return ORTE_ERR_BAD_PARAM;
    }

    /** check for wildcard character - handle appropriately */
    if (0 == strcmp(ORTE_SCHEMA_WILDCARD_STRING, string)) {
        *nodeid = ORTE_NODEID_WILDCARD;
        return ORTE_SUCCESS;
    }

    /* check for invalid value */
    if (0 == strcmp(ORTE_SCHEMA_INVALID_STRING, string)) {
        *nodeid = ORTE_NODEID_INVALID;
        return ORTE_SUCCESS;
    }

    tmpint = strtol(string, NULL, 10);

    if (ORTE_NODEID_MAX >= tmpint && ORTE_NODEID_MIN <= tmpint) {
        *nodeid = (orte_nodeid_t)tmpint;
    } else {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        *nodeid = ORTE_NODEID_INVALID;
        return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;
}


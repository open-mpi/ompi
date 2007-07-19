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

#include "opal/threads/mutex.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/base.h"
#include "ns_replica.h"

/*
 * functions
 */

/*
 * NODEID
 */
int orte_ns_replica_create_nodeids(orte_nodeid_t **nodeids, orte_std_cntr_t *nnodes, char **nodenames)
{
    orte_nodeid_t *nds, nid, m;
    orte_std_cntr_t k, n, num_nodes;
    char **nodes;
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);
    
    num_nodes = opal_argv_count(nodenames);
    if (0 == num_nodes) { /** no nodenames provided - just return */
        *nodeids = NULL;
        *nnodes = 0;
        return ORTE_SUCCESS;
    }

    nds = (orte_nodeid_t*)malloc(num_nodes * sizeof(orte_nodeid_t));
    if (NULL == nds) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    nodes = (char**)(orte_ns_replica.nodenames->addr);
    for (n=0; n < num_nodes; n++) {
        for (k=0, m=0; m < orte_ns_replica.next_nodeid &&
                       k < (orte_ns_replica.nodenames)->size; k++) {
            if (NULL != nodes[k]) {
                m++;
                if (strcmp(nodenames[n], nodes[k]) == 0) { /** found same name */
                    nid = m;
                    goto ASSIGN;
                }
            }
        }
        /** get here if we don't find this nodename - add it */
        nid = orte_ns_replica.next_nodeid++;

ASSIGN:
        nds[n] = nid;
    } /** for n */

    *nodeids = nds;
    *nnodes = num_nodes;

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}

int orte_ns_replica_get_node_info(char ***nodenames, orte_std_cntr_t num_nodes, orte_nodeid_t *nodeids)
{
    char **names;
    orte_std_cntr_t n;
    char **nodes;
        
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);
    
    if (0 == num_nodes) {
        *nodenames = NULL;
        return ORTE_SUCCESS;
    }
    
    /** allocate an extra space for the NULL termination */
    names = (char**)malloc((num_nodes+1) * sizeof(char*));
    if (NULL == names) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    names[num_nodes] = NULL;  /** NULL-terminate the list */
    
    nodes = (char**)(orte_ns_replica.nodenames->addr);
    for (n=0; n < num_nodes; n++) {
        if (nodeids[n] >= orte_ns_replica.next_nodeid) {
            names[n] = strdup("invalid nodeid");
        } else if (NULL != nodes[nodeids[n]]) {
            names[n] = strdup(nodes[nodeids[n]]);
        } else {
            names[n] = strdup("unknown nodeid");
        }
    }

    *nodenames = names;

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}


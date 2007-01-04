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

int orte_ns_replica_create_cellid(orte_cellid_t *cellid, char *site, char *resource)
{
    orte_ns_replica_cell_tracker_t *new_cell, **cell;
    int rc;
    orte_std_cntr_t i, j, index;

    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    *cellid = ORTE_CELLID_INVALID;

    /* check for error */
    if (NULL == site || NULL == resource) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_BAD_PARAM;                
    }
    
    /* is this a known cellid? */
    cell = (orte_ns_replica_cell_tracker_t**)(orte_ns_replica.cells)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_cells &&
         i < (orte_ns_replica.cells)->size; i++) {
        if (NULL != cell[i]) {
            j++;
            if (0 == strcmp(site, cell[i]->site) &&
                0 == strcmp(resource, cell[i]->resource)) {
                *cellid = cell[i]->cell;
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return ORTE_SUCCESS;                
            }
        }
    }
    
    /* new cell - check if cellid is available */
    if (ORTE_CELLID_MAX-1 < orte_ns_replica.num_cells) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
     }

    new_cell = OBJ_NEW(orte_ns_replica_cell_tracker_t);
    if (NULL == new_cell) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&index,
                                orte_ns_replica.cells, new_cell))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    new_cell->site = strdup(site);
    new_cell->resource = strdup(resource);

    new_cell->cell = orte_ns_replica.num_cells;
    *cellid = new_cell->cell;
    (orte_ns_replica.num_cells)++;

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}

int orte_ns_replica_get_cell_info(orte_cellid_t cellid,
                                char **site, char **resource)
{
    orte_std_cntr_t i;
    orte_cellid_t j;
    orte_ns_replica_cell_tracker_t **cell;

    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    cell = (orte_ns_replica_cell_tracker_t**)(orte_ns_replica.cells)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_cells &&
                   i < (orte_ns_replica.cells)->size; i++) {
        if (NULL != cell[i]) {
            j++;
            if (cellid == cell[i]->cell) {
                *site = strdup(cell[i]->site);
                *resource = strdup(cell[i]->resource);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return ORTE_SUCCESS;
            }
         }
     }
    
    /* it isn't an error to not find the cell - so do NOT
     * report it via ORTE_ERROR_LOG
     */

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_ERR_NOT_FOUND;
}

/*
 * NODEID
 */
int orte_ns_replica_create_nodeids(orte_nodeid_t **nodeids, orte_std_cntr_t *nnodes,
                                   orte_cellid_t cellid, char **nodenames)
{
    orte_ns_replica_cell_tracker_t **cell, *cptr;
    orte_ns_replica_nodeid_tracker_t **nodes, *node;
    orte_nodeid_t *nds, nid;
    orte_std_cntr_t i, j, k, m, n, num_nodes;
    
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

    /** find the cell */
    cell = (orte_ns_replica_cell_tracker_t**)(orte_ns_replica.cells)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_cells &&
         i < (orte_ns_replica.cells)->size; i++) {
        if (NULL != cell[i]) {
            j++;
            if (cellid == cell[i]->cell) {
                /** found the specified cell - check to see if nodename has already been
                * defined. if so, just return the nodeid. if not, create a new one
                */
                cptr = cell[i];
                goto PROCESS;
            }
        }
    }
    /** get here if we didn't find the cell */
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    free(nds);
    *nodeids = NULL;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_ERR_NOT_FOUND;

PROCESS:
    nodes = (orte_ns_replica_nodeid_tracker_t**)(cptr->nodeids->addr);
    for (n=0; n < num_nodes; n++) {
        for (k=0, m=0; m < cptr->next_nodeid &&
             k < (cptr->nodeids)->size; k++) {
            if (NULL != nodes[k]) {
                m++;
                if (strcmp(nodenames[n], nodes[k]->nodename) == 0) { /** found same name */
                    nid = nodes[k]->nodeid;
                    goto ASSIGN;
                }
            }
        }
        /** get here if we don't find this nodename - add one */
        node = OBJ_NEW(orte_ns_replica_nodeid_tracker_t);
        if (NULL == node) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(nds);
            *nodeids = NULL;
            OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        node->nodename = strdup(nodenames[n]);
        node->nodeid = cptr->next_nodeid;
        cptr->next_nodeid++;
        nid = node->nodeid;

ASSIGN:
        nds[n] = nid;
    } /** for n */

    *nodeids = nds;
    *nnodes = num_nodes;

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}

int orte_ns_replica_get_node_info(char ***nodenames, orte_cellid_t cellid,
                                  orte_std_cntr_t num_nodes, orte_nodeid_t *nodeids)
{
    char **names, *nm;
    orte_ns_replica_cell_tracker_t **cell, *cptr;
    orte_ns_replica_nodeid_tracker_t **nodes;
    orte_std_cntr_t i, j, k, m, n;
    char *err_name = "NODE_NOT_FOUND";
        
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
    
    /** find the cell */
    cell = (orte_ns_replica_cell_tracker_t**)(orte_ns_replica.cells)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_cells &&
         i < (orte_ns_replica.cells)->size; i++) {
        if (NULL != cell[i]) {
            j++;
            if (cellid == cell[i]->cell) {
                /** found the specified cell - check to see if nodename has already been
                * defined. if so, just return the nodeid. if not, create a new one
                */
                cptr = cell[i];
                goto PROCESS;
            }
        }
    }
    /** get here if we didn't find the cell */
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    free(names);
    *nodenames = NULL;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_ERR_NOT_FOUND;
    
PROCESS:
    nodes = (orte_ns_replica_nodeid_tracker_t**)(cell[i]->nodeids->addr);
    for (n=0; n < num_nodes; n++) {
        for (k=0, m=0; m < cell[i]->next_nodeid &&
             k < (cell[i]->nodeids)->size; k++) {
            if (NULL != nodes[k]) {
                m++;
                if (nodeids[n] == nodes[k]->nodeid) { /** found it */
                    nm = nodes[k]->nodename;
                    goto ASSIGN;
                }
            }
        }
        /** node not found - set name to error name. Can't set it to NULL since
        * the list is a NULL-terminated one
        */
        nm = err_name;
        
ASSIGN:
        names[n] = strdup(nm);
    }

    *nodenames = names;

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}


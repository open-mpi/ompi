/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

/*
 * JOB_MAP
 */
int orte_rmaps_base_pack_map(orte_buffer_t *buffer, void *src,
                             orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i;
    orte_job_map_t **maps;
    opal_list_item_t *item;
    orte_mapped_node_t *srcnode;

    /* array of pointers to orte_job_map_t objects - need to pack the objects a set of fields at a time */
    maps = (orte_job_map_t**) src;

    for (i=0; i < num_vals; i++) {
        /* pack the jobid this map is for */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(maps[i]->job), 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the mapping mode used to generate it */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(maps[i]->mapping_mode), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the starting vpid */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(maps[i]->vpid_start), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the range */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(maps[i]->vpid_range), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number of app_contexts */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(maps[i]->num_apps), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the app_contexts */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, maps[i]->apps, maps[i]->num_apps, ORTE_APP_CONTEXT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of nodes */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(maps[i]->num_nodes), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the nodes list */
        if (0 < maps[i]->num_nodes) {
            for (item = opal_list_get_first(&(maps[i]->nodes));
                 item != opal_list_get_end(&(maps[i]->nodes));
                 item = opal_list_get_next(item)) {
                srcnode = (orte_mapped_node_t*)item;
                if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, (void*)srcnode,
                                                           1, ORTE_MAPPED_NODE))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
    }
        
    return ORTE_SUCCESS;
}


/*
 * MAPPED_PROC
 */
int orte_rmaps_base_pack_mapped_proc(orte_buffer_t *buffer, void *src,
                                     orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i;
    orte_mapped_proc_t **procs;
    
    /* array of pointers to orte_mapped_proc_t objects - need to pack the objects a set of fields at a time */
    procs = (orte_mapped_proc_t**) src;
    
    for (i=0; i < num_vals; i++) {
        /* pack the proc name */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, (void*)(&(procs[i]->name)),
                                                       1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the rank */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(procs[i]->rank), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the pid */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(procs[i]->pid), 1, ORTE_PID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the app_idx */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(procs[i]->app_idx), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;        
}


/*
 * MAPPED_NODE
 */
int orte_rmaps_base_pack_mapped_node(orte_buffer_t *buffer, void *src,
                                     orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i;
    orte_mapped_node_t **nodes;
    opal_list_item_t *item;
    orte_mapped_proc_t *srcproc;
    
    /* array of pointers to orte_mapped_node_t objects - need to pack the objects a set of fields at a time */
    nodes = (orte_mapped_node_t**) src;
    
    for (i=0; i < num_vals; i++) {
        /* pack the cellid */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(nodes[i]->cell), 1, ORTE_CELLID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the nodename */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(nodes[i]->nodename), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the launch id */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(nodes[i]->launch_id), 1, ORTE_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the username */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(nodes[i]->username), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the daemon's name */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(nodes[i]->daemon), 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the oversubscribed flag */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(nodes[i]->oversubscribed), 1, ORTE_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
                
        /* pack the number of procs */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &(nodes[i]->num_procs), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the procs list */
        if (0 < nodes[i]->num_procs) {
            for (item = opal_list_get_first(&(nodes[i]->procs));
                 item != opal_list_get_end(&(nodes[i]->procs));
                 item = opal_list_get_next(item)) {
                srcproc = (orte_mapped_proc_t*)item;
                if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, (void*)srcproc,
                                                               1, ORTE_MAPPED_PROC))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
    }
    return ORTE_SUCCESS;        
}


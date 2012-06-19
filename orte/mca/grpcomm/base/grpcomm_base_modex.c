/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "opal/class/opal_hash_table.h"
#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/db/db.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/grpcomm/grpcomm.h"

orte_grpcomm_coll_id_t orte_grpcomm_base_get_coll_id(void)
{
    orte_grpcomm_coll_id_t id;

    /* assign the next collective id */
    id = orte_grpcomm_base.coll_id;
    /* rotate to the next value */
    orte_grpcomm_base.coll_id++;
    return id;
}


/***************  MODEX SECTION **************/
int orte_grpcomm_base_modex(orte_grpcomm_collective_t *modex)
{
    int rc;
    orte_local_rank_t local_rank;
    orte_node_rank_t node_rank;
    orte_namelist_t *nm;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:base:modex: performing modex",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* record the collective */
    modex->active = true;
    modex->next_cbdata = modex;
    opal_list_append(&orte_grpcomm_base.active_colls, &modex->super);

    /* put our process name in the buffer so it can be unpacked later */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    if (0 == opal_list_get_size(&modex->participants)) {
        /* add a wildcard name to the participants so the daemon knows
         * that everyone in my job must participate
         */
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
        nm->name.vpid = ORTE_VPID_WILDCARD;
        opal_list_append(&modex->participants, &nm->super);
        modex->next_cb = orte_grpcomm_base_store_modex;
    } else {
        /* this is not amongst our peers, but rather between a select
         * group of processes - e.g., during a connect/accept operation.
         * Thus, this requires we send additional info
         */
        modex->next_cb = orte_grpcomm_base_store_peer_modex;

        /* pack our hostname */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &orte_process_info.nodename, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    
        /* pack our daemon's vpid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &ORTE_PROC_MY_DAEMON->vpid, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    
        /* pack our node rank */
        node_rank = orte_ess.get_node_rank(ORTE_PROC_MY_NAME);
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &node_rank, 1, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    
        /* pack our local rank */
        local_rank = orte_ess.get_local_rank(ORTE_PROC_MY_NAME);
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &local_rank, 1, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    
#if OPAL_HAVE_HWLOC
        /* pack our binding info so other procs can determine our locality */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &orte_process_info.bind_level, 1, OPAL_HWLOC_LEVEL_T))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &orte_process_info.bind_idx, 1, OPAL_UINT))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
#endif
    }

    /* pack the entries we have received */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_pack_modex_entries(&modex->buffer))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:base:full:modex: executing allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* execute the allgather */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.allgather(modex))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:base:modex: modex posted",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return ORTE_SUCCESS;

 cleanup:
    return rc;
}

void orte_grpcomm_base_store_peer_modex(opal_buffer_t *rbuf, void *cbdata)
{
    int rc, n, cnt;
    orte_process_name_t proc_name;
    char *hostname;
    orte_vpid_t daemon;
    orte_node_rank_t node_rank;
    orte_local_rank_t local_rank;
    orte_nid_t *nid;
    orte_jmap_t *jmap;
    orte_pmap_t *pmap;
    orte_grpcomm_collective_t *modex = (orte_grpcomm_collective_t*)cbdata;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s STORING PEER MODEX DATA",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the process name */
    cnt=1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(rbuf, &proc_name, &cnt, ORTE_NAME))) {
        /* unpack the hostname */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &hostname, &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* unpack the daemon vpid */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &daemon, &cnt, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* unpack the node rank */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &node_rank, &cnt, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* unpack the local rank */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &local_rank, &cnt, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* UPDATE THE NIDMAP/PIDMAP TO SUPPORT DYNAMIC OPERATIONS */
        
        /* find this proc's node in the nidmap */
        nid = NULL;
        for (n=0; NULL != (nid = (orte_nid_t *) opal_pointer_array_get_item(&orte_nidmap, n)); n++) {
            if (0 == strcmp(hostname, nid->name)) {
                break;
            }
        }
        if (NULL == nid) {
            /* node wasn't found - let's add it */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:base:full:modex no nidmap entry for node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (NULL == hostname) ? "NULL" : hostname));
            nid = OBJ_NEW(orte_nid_t);
            nid->name = strdup(hostname);
            nid->daemon = daemon;
            nid->index = opal_pointer_array_add(&orte_nidmap, nid);
        }
        
        /* see if we have this job in a jobmap */
        if (NULL == (jmap = orte_util_lookup_jmap(proc_name.jobid))) {
            /* proc wasn't found - let's add it */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:base:full:modex no jobmap entry for job %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(proc_name.jobid)));
            jmap = OBJ_NEW(orte_jmap_t);
            jmap->job = proc_name.jobid;
            /* unfortunately, job objects cannot be stored
             * by index number as the jobid is a constructed
             * value. So we have to just add it to the end
             * of the array
             */
            opal_pointer_array_add(&orte_jobmap, jmap);
            jmap->num_procs = 1;
            /* have to add the pidmap entry too, but this
             * can be done at the specific site corresponding
             * to the proc's vpid
             */
            pmap = OBJ_NEW(orte_pmap_t);
            pmap->node = nid->index;
            pmap->local_rank = local_rank;
            pmap->node_rank = node_rank;
            opal_pointer_array_set_item(&jmap->pmap, proc_name.vpid, pmap);
        } else {
            /* see if we have this proc in a pidmap */
            if (NULL == (pmap = orte_util_lookup_pmap(&proc_name))) {
                /* proc wasn't found - let's add it */
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s grpcomm:base:full:modex no pidmap entry for proc %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc_name)));
                pmap = OBJ_NEW(orte_pmap_t);
                pmap->node = nid->index;
                pmap->local_rank = local_rank;
                pmap->node_rank = node_rank;
                /* this can be done at the specific site corresponding
                 * to the proc's vpid
                 */
                opal_pointer_array_set_item(&jmap->pmap, proc_name.vpid, pmap);
                /* account for the proc entry in the jmap */
                jmap->num_procs++;
            }
        }

#if OPAL_HAVE_HWLOC
        {
            opal_hwloc_level_t bind_level;
            unsigned int bind_idx;

            /* unpack the locality info */
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &bind_level, &cnt, OPAL_HWLOC_LEVEL_T))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &bind_idx, &cnt, OPAL_UINT))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                                 "%s grpcomm:base:modex setting proc %s level %s idx %u",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name),
                                 opal_hwloc_base_print_level(bind_level), bind_idx));

            /* store on the pmap */
            if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &proc_name, ORTE_PROC_MY_NAME)) {
                /* if this data is from myself, then set locality to all */
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s grpcomm:base:modex setting proc %s locale ALL",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc_name)));
                pmap->locality = OPAL_PROC_ALL_LOCAL;
            } else if (daemon != ORTE_PROC_MY_DAEMON->vpid) {
                /* this is on a different node, then mark as non-local */
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s grpcomm:base:modex setting proc %s locale NONLOCAL",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc_name)));
                pmap->locality = OPAL_PROC_NON_LOCAL;
            } else if (OPAL_HWLOC_NODE_LEVEL == orte_process_info.bind_level ||
                       OPAL_HWLOC_NODE_LEVEL == bind_level) {
                /* one or both of us is not bound, so all we can say is we are on the
                 * same node
                 */
                pmap->locality = OPAL_PROC_ON_NODE;
            } else {
                /* determine relative location on our node */
                pmap->locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                       orte_process_info.bind_level,
                                                                       orte_process_info.bind_idx,
                                                                       bind_level, bind_idx);
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s grpcomm:base:modex setting proc %s locale %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc_name),
                                     opal_hwloc_base_print_locality(pmap->locality)));
            }
        }
#else
        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &proc_name, ORTE_PROC_MY_NAME)) {
            /* if this data is from myself, then set locality to all */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:base:modex setting proc %s locale ALL",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name)));
            pmap->locality = OPAL_PROC_ALL_LOCAL;
        } else if (daemon != ORTE_PROC_MY_DAEMON->vpid) {
            /* this is on a different node, then mark as non-local */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:base:modex setting proc %s locale NONLOCAL",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name)));
            pmap->locality = OPAL_PROC_NON_LOCAL;
        } else {
            /* must be on our node */
            pmap->locality = OPAL_PROC_ON_NODE;
        }
#endif

        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:base:full:modex: adding modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc_name)));
        
        /* update the modex database */
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_update_modex_entries(&proc_name, rbuf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }            
    }    

 cleanup:
    /* flag the collective as complete */
    modex->active = false;
    /* cleanup the list, but don't release the
     * collective object as it was passed into us
     */
    opal_list_remove_item(&orte_grpcomm_base.active_colls, &modex->super);
    /* notify that the modex is complete */
    if (NULL != modex->cbfunc) {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s CALLING MODEX RELEASE",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        modex->cbfunc(NULL, modex->cbdata);
    }
}

void orte_grpcomm_base_store_modex(opal_buffer_t *rbuf, void *cbdata)
{
    orte_std_cntr_t cnt;
    orte_process_name_t proc_name;
    int rc=ORTE_SUCCESS;
    orte_grpcomm_collective_t *modex = (orte_grpcomm_collective_t*)cbdata;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s STORING MODEX DATA",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the process name */
    cnt=1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(rbuf, &proc_name, &cnt, ORTE_NAME))) {
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:base:store_modex adding modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc_name)));
        
        /* update the modex database */
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_update_modex_entries(&proc_name, rbuf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
    }

 cleanup:
    /* flag the modex as complete */
    modex->active = false;
    /* cleanup */
    opal_list_remove_item(&orte_grpcomm_base.active_colls, &modex->super);
    /* execute user callback, if requested */
    if (NULL != modex->cbfunc) {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s CALLING MODEX RELEASE",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        modex->cbfunc(NULL, modex->cbdata);
    }
}



int orte_grpcomm_base_update_modex_entries(orte_process_name_t *proc_name,
                                           opal_buffer_t *rbuf)
{
    int rc = ORTE_SUCCESS;
    int32_t num_recvd_entries;
    orte_std_cntr_t cnt;
    orte_std_cntr_t j;

    /* unpack the number of entries for this proc */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &num_recvd_entries, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:update_modex_entries: adding %d entries for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_recvd_entries,
                         ORTE_NAME_PRINT(proc_name)));
    
    /*
     * Extract the attribute names and values
     */
    for (j = 0; j < num_recvd_entries; j++) {
        int32_t num_bytes;
        void *bytes = NULL;
        char *attr_name;
        
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &attr_name, &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &num_bytes, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (0 < num_bytes) {
            if (NULL == (bytes = malloc(num_bytes))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            cnt = (orte_std_cntr_t)num_bytes;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, bytes, &cnt, OPAL_BYTE))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            num_bytes = cnt;
            /* store it in the database */
            if (ORTE_SUCCESS != (rc = orte_db.store(proc_name, attr_name, (void*)bytes, num_bytes))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }
    
 cleanup:
    return rc;
}

int orte_grpcomm_base_set_proc_attr(const char *attr_name,
                                    const void *data,
                                    size_t size)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:set_proc_attr: setting attribute %s data size %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         attr_name, (unsigned long)size));

    rc = orte_db.store(ORTE_PROC_MY_NAME, attr_name, data, size);
    
    return rc;
}

int orte_grpcomm_base_get_proc_attr(const orte_process_name_t *proc,
                                    const char *attribute_name, void **val, 
                                    size_t *size)
{
    orte_db_keyval_t *kv;
    opal_list_t data;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:get_proc_attr: searching for attr %s on proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attribute_name,
                         ORTE_NAME_PRINT(proc)));

    /* set defaults */
    *val = NULL;
    *size = 0;

    /* fetch the data */
    OBJ_CONSTRUCT(&data, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_db.fetch(proc, attribute_name, &data))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* this interface to the MPI layer only supports returning one value */
    if (1 < opal_list_get_size(&data)) {
        return ORTE_ERROR;
    }
    kv = (orte_db_keyval_t*)opal_list_remove_first(&data);
    /* transfer the data */
    *val = kv->value.bytes;
    *size = kv->value.size;
    kv->value.bytes = NULL;
    /* cleanup */
    OBJ_RELEASE(kv);

 cleanup:
    OBJ_DESTRUCT(&data);
    return rc;
} 

int orte_grpcomm_base_purge_proc_attrs(void)
{
    return orte_db.remove(NULL, NULL);
}

int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf)
{
    int rc;
    int32_t num_entries;
    orte_db_keyval_t *kv;
    opal_list_t data;

    /* fetch our data */
    OBJ_CONSTRUCT(&data, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_db.fetch(ORTE_PROC_MY_NAME, NULL, &data))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    num_entries = opal_list_get_size(&data);

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:pack_modex: reporting %d entries",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_entries));
    
    /* put the number of entries into the buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &num_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if there are entries, store them */
    while (NULL != (kv = (orte_db_keyval_t*)opal_list_remove_first(&data))) {
        if (ORTE_SUCCESS != (opal_dss.pack(buf, &kv->key, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            break;
        }
        if (ORTE_SUCCESS != (opal_dss.pack(buf, &(kv->value.size), 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            break;
        }
        if (0 < kv->value.size) {
            if (ORTE_SUCCESS != (opal_dss.pack(buf, kv->value.bytes, kv->value.size, OPAL_BYTE))) {
                ORTE_ERROR_LOG(rc);
                break;
            }
        }
        OBJ_RELEASE(kv);
    }

 cleanup:
    while (NULL != (kv = (orte_db_keyval_t*)opal_list_remove_first(&data))) {
        OBJ_RELEASE(kv);
    }
    OBJ_DESTRUCT(&data);

    return rc;
}

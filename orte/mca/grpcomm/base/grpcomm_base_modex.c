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
    orte_namelist_t *nm, *nm2;
    opal_list_item_t *item, *itm;
    bool found;
    orte_grpcomm_collective_t *cptr;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:base:modex: performing modex",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (0 == opal_list_get_size(&modex->participants)) {
        /* record the collective */
        modex->active = true;
        modex->next_cbdata = modex;
        opal_list_append(&orte_grpcomm_base.active_colls, &modex->super);

        /* put our process name in the buffer so it can be unpacked later */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* add a wildcard name to the participants so the daemon knows
         * the jobid that is involved in this collective
         */
        nm = OBJ_NEW(orte_namelist_t);
        nm->name.jobid = ORTE_PROC_MY_NAME->jobid;
        nm->name.vpid = ORTE_VPID_WILDCARD;
        opal_list_append(&modex->participants, &nm->super);
        modex->next_cb = orte_grpcomm_base_store_modex;
    } else {
        /* see if the collective is already present - a race condition
         * exists where other participants may have already sent us their
         * contribution. This would place the collective on the global
         * array, but leave it marked as "inactive" until we call
         * modex with the list of participants
         */
        found = false;
        for (item = opal_list_get_first(&orte_grpcomm_base.active_colls);
             item != opal_list_get_end(&orte_grpcomm_base.active_colls);
             item = opal_list_get_next(item)) {
            cptr = (orte_grpcomm_collective_t*)item;
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s CHECKING COLL id %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 cptr->id));
            
            if (modex->id == cptr->id) {
                found = true;
                /* remove the old entry - we will replace it
                 * with the modex one
                 */
                opal_list_remove_item(&orte_grpcomm_base.active_colls, item);
                break;
            }
        }
        if (found) {
            /* since it already exists, the list of
             * targets contains the list of procs
             * that have already sent us their info. Cycle
             * thru the targets and move those entries to
             * the modex object
             */
            while (NULL != (item = opal_list_remove_first(&cptr->targets))) {
                opal_list_append(&modex->targets, item);
            }
            /* copy the previously-saved data across */
            opal_dss.copy_payload(&modex->local_bucket, &cptr->local_bucket);
            /* cleanup */
            OBJ_RELEASE(cptr);
        }
        /* now add the modex to the global list of active collectives */
        modex->next_cb = orte_grpcomm_base_store_peer_modex;
        modex->next_cbdata = modex;
        modex->active = true;
        opal_list_append(&orte_grpcomm_base.active_colls, &modex->super);

        /* this is not amongst our peers, but rather between a select
         * group of processes - e.g., during a connect/accept operation.
         * Thus, this requires we send additional info
         */

        /* pack the collective id */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &modex->id, 1, ORTE_GRPCOMM_COLL_ID_T))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* pack our name */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

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
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &orte_process_info.my_node_rank, 1, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    
        /* pack our local rank */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, &orte_process_info.my_local_rank, 1, ORTE_LOCAL_RANK))) {
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
    int rc, cnt;
    orte_process_name_t proc_name;
    char *hostname;
    orte_vpid_t daemon;
    orte_node_rank_t node_rank;
    orte_local_rank_t local_rank;
    orte_grpcomm_collective_t *modex = (orte_grpcomm_collective_t*)cbdata;
    opal_hwloc_locality_t locality;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s STORING PEER MODEX DATA",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the process name */
    cnt=1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(rbuf, &proc_name, &cnt, ORTE_NAME))) {
        /* unpack and store the hostname */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &hostname, &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc_name, ORTE_DB_HOSTNAME, hostname, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* unpack and store the daemon vpid */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &daemon, &cnt, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc_name, ORTE_DB_DAEMON_VPID, &daemon, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* unpack and store the node rank */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &node_rank, &cnt, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc_name, ORTE_DB_NODERANK, &node_rank, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* unpack the local rank */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &local_rank, &cnt, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc_name, ORTE_DB_LOCALRANK, &local_rank, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        /* compute the locality and store in the database */
#if OPAL_HAVE_HWLOC
        {
            opal_hwloc_level_t bind_level;
            unsigned int bind_idx;

            /* unpack and store the locality info */
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &bind_level, &cnt, OPAL_HWLOC_LEVEL_T))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc_name, ORTE_DB_BIND_LEVEL, &bind_level, OPAL_HWLOC_LEVEL_T))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            cnt = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &bind_idx, &cnt, OPAL_UINT))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            if (ORTE_SUCCESS != (rc = orte_db.store(&proc_name, ORTE_DB_BIND_INDEX, &bind_idx, OPAL_UINT))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                                 "%s store:peer:modex setting proc %s level %s idx %u",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name),
                                 opal_hwloc_base_print_level(bind_level), bind_idx));

            if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &proc_name, ORTE_PROC_MY_NAME)) {
                /* if this data is from myself, then set locality to all */
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s store:peer:modex setting proc %s locale ALL",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc_name)));
                locality = OPAL_PROC_ALL_LOCAL;
            } else if (daemon != ORTE_PROC_MY_DAEMON->vpid) {
                /* this is on a different node, then mark as non-local */
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s store:peer:modex setting proc %s locale NONLOCAL",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc_name)));
                locality = OPAL_PROC_NON_LOCAL;
            } else if (OPAL_HWLOC_NODE_LEVEL == orte_process_info.bind_level ||
                       OPAL_HWLOC_NODE_LEVEL == bind_level) {
                /* one or both of us is not bound, so all we can say is we are on the
                 * same node
                 */
                locality = OPAL_PROC_ON_NODE;
            } else {
                /* determine relative location on our node */
                locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                 orte_process_info.bind_level,
                                                                 orte_process_info.bind_idx,
                                                                 bind_level, bind_idx);
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s store:peer:modex setting proc %s locale %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc_name),
                                     opal_hwloc_base_print_locality(locality)));
            }
        }
#else
        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &proc_name, ORTE_PROC_MY_NAME)) {
            /* if this data is from myself, then set locality to all */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:base:modex setting proc %s locale ALL",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name)));
            locality = OPAL_PROC_ALL_LOCAL;
        } else if (daemon != ORTE_PROC_MY_DAEMON->vpid) {
            /* this is on a different node, then mark as non-local */
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s store:peer:modex setting proc %s locale NONLOCAL",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_name)));
            locality = OPAL_PROC_NON_LOCAL;
        } else {
            /* must be on our node */
            locality = OPAL_PROC_ON_NODE;
        }
#endif
        if (ORTE_SUCCESS != (rc = orte_db.store(&proc_name, ORTE_DB_LOCALITY, &locality, OPAL_HWLOC_LOCALITY_T))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s store:peer:modex: adding modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc_name)));
        
        /* update the modex database */
        if (ORTE_SUCCESS != (rc = orte_grpcomm_base_update_modex_entries(&proc_name, rbuf))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }            
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s store:peer:modex: completed modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc_name)));
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
    } else {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s store:peer:modex NO MODEX RELEASE CBFUNC",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
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
        opal_value_t *kv;
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &kv, &cnt, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        /* if this is me, dump the data - we already have it in the db */
        if (ORTE_PROC_MY_NAME->jobid == proc_name->jobid &&
            ORTE_PROC_MY_NAME->vpid == proc_name->vpid) {
            OBJ_RELEASE(kv);
        } else {
            /* store it in the database */
            if (ORTE_SUCCESS != (rc = orte_db.store_pointer(proc_name, kv))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* do not release the kv - the db holds that pointer */
        }
    }
    
 cleanup:
    return rc;
}

int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf)
{
    int rc;
    int32_t num_entries;
    opal_value_t *kv;
    opal_list_t data;
    opal_list_item_t *item, *next;

    /* fetch our data */
    OBJ_CONSTRUCT(&data, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_db.fetch_multiple(ORTE_PROC_MY_NAME, NULL, &data))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* count the number of entries we will send, purging the rest */
    num_entries = 0;
    item = opal_list_get_first(&data);
    while (item != opal_list_get_end(&data)) {
        kv = (opal_value_t*)item;
        next = opal_list_get_next(item);
        /* if this is an entry we get from the nidmap, then don't include it here */
        if (0 == strcmp(kv->key, ORTE_DB_HOSTNAME) ||
            0 == strcmp(kv->key, ORTE_DB_DAEMON_VPID) ||
            0 == strcmp(kv->key, ORTE_DB_NODERANK) ||
            0 == strcmp(kv->key, ORTE_DB_LOCALRANK) ||
            0 == strcmp(kv->key, ORTE_DB_BIND_LEVEL) ||
            0 == strcmp(kv->key, ORTE_DB_BIND_INDEX)) {
            opal_list_remove_item(&data, item);
        } else {
            num_entries++;
        }
        item = next;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:pack_modex: reporting %d entries",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_entries));
    
    /* put the number of entries into the buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &num_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if there are entries, store them */
    while (NULL != (kv = (opal_value_t*)opal_list_remove_first(&data))) {
        if (ORTE_SUCCESS != (opal_dss.pack(buf, &kv, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            break;
        }
        OBJ_RELEASE(kv);
    }

 cleanup:
    while (NULL != (kv = (opal_value_t*)opal_list_remove_first(&data))) {
        OBJ_RELEASE(kv);
    }
    OBJ_DESTRUCT(&data);

    return rc;
}

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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
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
#include "opal/mca/db/db.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
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
void orte_grpcomm_base_modex(int fd, short args, void *cbdata)
{
    orte_grpcomm_caddy_t *caddy = (orte_grpcomm_caddy_t*)cbdata;
    orte_grpcomm_collective_t *modex = caddy->op;
    int rc;
    orte_namelist_t *nm;
    opal_list_item_t *item;
    bool found;
    orte_grpcomm_collective_t *cptr;
    opal_scope_t scope;

    OBJ_RELEASE(caddy);

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:modex: performing modex",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we are a singleton and routing isn't enabled,
     * then we have nobody with which to communicate, so
     * we can just declare success
     */
    if ((orte_process_info.proc_type & ORTE_PROC_SINGLETON) &&
        !orte_routing_is_enabled) {
        if (NULL != modex->cbfunc) {
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                                 "%s CALLING MODEX RELEASE",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            modex->cbfunc(NULL, modex->cbdata);
        }
        /* flag the collective as complete */
        modex->active = false;
        return;
    }

    if (0 == opal_list_get_size(&modex->participants)) {
        /* record the collective */
        modex->next_cbdata = modex;
        opal_list_append(&orte_grpcomm_base.active_colls, &modex->super);

        /* put our process name in the buffer so it can be unpacked later */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&modex->buffer, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* this is between our peers, so only collect info marked for them */
        scope = OPAL_SCOPE_PEER;

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
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
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
        modex->next_cb = orte_grpcomm_base_store_modex;
        modex->next_cbdata = modex;
        opal_list_append(&orte_grpcomm_base.active_colls, &modex->super);

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

        /* this is not amongst our peers, but rather between a select
         * group of processes - e.g., during a connect/accept operation.
         * Thus, we need to include the non-peer info as well as our peers
         * since we can't tell what the other participants may already have
         */
        scope = OPAL_SCOPE_GLOBAL;

    }

    /* pack the requested entries */
    if (ORTE_SUCCESS != (rc = orte_grpcomm_base_pack_modex_entries(&modex->buffer, scope))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:full:modex: executing allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* execute the allgather */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.allgather(modex))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:modex: modex posted",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return;

 cleanup:
    return;
}

void orte_grpcomm_base_store_modex(opal_buffer_t *rbuf, void *cbdata)
{
    int rc, j, cnt;
    int32_t num_recvd_entries;
    orte_process_name_t pname;
    orte_grpcomm_collective_t *modex = (orte_grpcomm_collective_t*)cbdata;

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                         "%s STORING MODEX DATA",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the process name */
    cnt=1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(rbuf, &pname, &cnt, ORTE_NAME))) {
        /* unpack the number of entries for this proc */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, &num_recvd_entries, &cnt, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:base:update_modex_entries: adding %d entries for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_recvd_entries,
                             ORTE_NAME_PRINT(&pname)));
        
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
            OPAL_OUTPUT_VERBOSE((10, orte_grpcomm_base_framework.framework_output,
                                 "%s STORING MODEX DATA FROM %s FOR %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&pname), kv->key));
            /* if this is me, dump the data - we already have it in the db */
            if (ORTE_PROC_MY_NAME->jobid == pname.jobid &&
                ORTE_PROC_MY_NAME->vpid == pname.vpid) {
                OBJ_RELEASE(kv);
            } else {
                /* store it in the database */
                if (ORTE_SUCCESS != (rc = opal_db.store_pointer((opal_identifier_t*)&pname, kv))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* do not release the kv - the db holds that pointer */
            }
        }

        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s store:peer:modex: completed modex entry for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&pname)));
    }

 cleanup:
    if (NULL == cbdata) {
        return;
    }
    /* cleanup the list, but don't release the
     * collective object as it was passed into us
     */
    opal_list_remove_item(&orte_grpcomm_base.active_colls, &modex->super);
    /* notify that the modex is complete */
    if (NULL != modex->cbfunc) {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                             "%s CALLING MODEX RELEASE",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        modex->cbfunc(NULL, modex->cbdata);
    } else {
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base_framework.framework_output,
                             "%s store:peer:modex NO MODEX RELEASE CBFUNC",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }
    /* flag the collective as complete */
    modex->active = false;
}

int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf, opal_scope_t scope)
{
    int rc;
    int32_t num_entries;
    opal_value_t *kv;
    opal_list_t data;

    /* fetch any global or local data */
    OBJ_CONSTRUCT(&data, opal_list_t);
    if (ORTE_SUCCESS != (rc = opal_db.fetch_multiple((opal_identifier_t*)ORTE_PROC_MY_NAME,
                                                     scope, NULL, &data))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    num_entries = opal_list_get_size(&data);

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:base:pack_modex: reporting %d entries",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), num_entries));
    
    /* put the number of entries into the buffer */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &num_entries, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* if there are entries, store them */
    while (NULL != (kv = (opal_value_t*)opal_list_remove_first(&data))) {
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:base:pack_modex: packing entry for %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), kv->key));
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &kv, 1, OPAL_VALUE))) {
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

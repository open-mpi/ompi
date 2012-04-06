/* -*- C -*-
 *
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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

/*
 * includes
 */
#include "orte_config.h"


#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/error_strings.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/grpcomm/base/base.h"

static bool recv_issued=false;
static void daemon_local_recv(int status, orte_process_name_t* sender,
                              opal_buffer_t* buffer, orte_rml_tag_t tag,
                              void* cbdata);
static void daemon_coll_recv(int status, orte_process_name_t* sender,
                             opal_buffer_t* buffer, orte_rml_tag_t tag,
                             void* cbdata);
static void app_recv(int status, orte_process_name_t* sender,
                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                     void* cbdata);
static void coll_id_req(int status, orte_process_name_t* sender,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata);

int orte_grpcomm_base_comm_start(void)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:receive start comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (!recv_issued) {
        if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
            if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                              ORTE_RML_TAG_COLLECTIVE,
                                                              ORTE_RML_PERSISTENT,
                                                              daemon_local_recv, NULL))) {
                ORTE_ERROR_LOG(rc);
                recv_issued = false;
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                              ORTE_RML_TAG_XCAST,
                                                              ORTE_RML_PERSISTENT,
                                                              orte_grpcomm_base_xcast_recv, NULL))) {
                ORTE_ERROR_LOG(rc);
                recv_issued = false;
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                              ORTE_RML_TAG_DAEMON_COLL,
                                                              ORTE_RML_PERSISTENT,
                                                              daemon_coll_recv, NULL))) {
                ORTE_ERROR_LOG(rc);
                recv_issued = false;
                return rc;
            }
            if (ORTE_PROC_IS_HNP) {
                if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                                  ORTE_RML_TAG_COLL_ID_REQ,
                                                                  ORTE_RML_PERSISTENT,
                                                                  coll_id_req, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    recv_issued = false;
                    return rc;
                }
            }
            recv_issued = true;
        } else if (ORTE_PROC_IS_APP) {
            if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                              ORTE_RML_TAG_COLLECTIVE,
                                                              ORTE_RML_PERSISTENT,
                                                              app_recv, NULL))) {
                ORTE_ERROR_LOG(rc);
                recv_issued = false;
                return rc;
            }
            recv_issued = true;
        }
    }

    return ORTE_SUCCESS;
}


void orte_grpcomm_base_comm_stop(void)
{
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:receive stop comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    if (recv_issued) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_COLLECTIVE);
        if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
            orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_XCAST);
            orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON_COLL);
        }
        if (ORTE_PROC_IS_HNP) {
            orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_COLL_ID_REQ);
        }
        recv_issued = false;
    }
}

static void coll_id_req(int status, orte_process_name_t* sender,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    orte_grpcomm_coll_id_t id;
    opal_buffer_t *relay;
    int rc;
    /* collective - only the HNP ever gets this message, but check
     * in case a developer makes a mistake!
     */
    id = orte_grpcomm_base_get_coll_id();
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:receive proc %s requested coll id - returned id %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender), id));
    relay = OBJ_NEW(opal_buffer_t);
    if (ORTE_SUCCESS != (rc = opal_dss.pack(relay, &id, 1, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(relay);
        return;
    }
    if (0 > (rc = orte_rml.send_buffer_nb(sender, relay, ORTE_RML_TAG_COLL_ID, 0,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(relay);
        return;
    }
}


/* process incoming coll returns */
static void app_recv(int status, orte_process_name_t* sender,
                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                     void* cbdata)
{
    orte_grpcomm_collective_t *coll;
    opal_list_item_t *item;
    int n, rc;
    orte_grpcomm_coll_id_t id;

    /* get the collective id */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &id, &n, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:receive processing collective return for id %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), id));

    /* search my list of active collectives */
    for (item = opal_list_get_first(&orte_grpcomm_base.active_colls);
         item != opal_list_get_end(&orte_grpcomm_base.active_colls);
         item = opal_list_get_next(item)) {
        coll = (orte_grpcomm_collective_t*)item;
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s CHECKING COLL id %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             coll->id));

        if (id == coll->id) {
            /* see if the collective needs another step */
            if (NULL != coll->next_cb) {
                /* have to go here next */
                coll->next_cb(buffer, coll->next_cbdata);
                break;
            }
            /* flag the collective as complete */
            coll->active = false;
            /* cleanup */
            opal_list_remove_item(&orte_grpcomm_base.active_colls, item);
            /* callback the specified function */
            if (NULL != coll->cbfunc) {
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s grpcomm:base:receive executing callback",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

                coll->cbfunc(buffer, coll->cbdata);
            }
            break;
        }
    }
}

/****    DAEMON COLLECTIVE SUPPORT    ****/
/* recv for collective messages sent from a daemon's local procs */
static void daemon_local_recv(int status, orte_process_name_t* sender,
                              opal_buffer_t* buffer, orte_rml_tag_t tag,
                              void* cbdata)
{
    int32_t rc, n;
    orte_vpid_t nprocs;
    orte_job_t *jdata;
    orte_grpcomm_collective_t *coll;
    orte_process_name_t proc;
    orte_namelist_t *nm;
    bool keep;
    orte_vpid_t i;
    orte_grpcomm_coll_id_t id;
    bool do_progress=true;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s COLLECTIVE RECVD FROM %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* unpack the collective id */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &id, &n, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s WORKING COLLECTIVE %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), id));

    /* setup the collective for this id - if it's already present,
     * then this will just return the existing structure
     */
    coll = orte_grpcomm_base_setup_collective(id);

    /* record this proc's participation and its data */
    coll->num_local_recvd++;

    /* unpack the number of participants */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nprocs, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    /* do we already have the names of all participants in this collective */
    keep = true;
    if (0 < opal_list_get_size(&coll->participants)) {
        /* we already have it, so don't bother saving the data */
        keep = false;
    }

    /* even if we don't need the names, we still have to
     * unpack them to get to the data
     */
    for (i=0; i < nprocs; i++) {
        /* unpack the name of this participant */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &proc, &n, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (keep) {
            /* add the name to the list */
            nm = OBJ_NEW(orte_namelist_t);
            nm->name.jobid = proc.jobid;
            nm->name.vpid = proc.vpid;
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s ADDING %s TO PARTICIPANTS",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc)));
            opal_list_append(&coll->participants, &nm->super);
        }
        /*  find this job */
        if (NULL == (jdata = orte_get_job_data_object(proc.jobid))) {
            /* if we can't find it, then we haven't processed the
             * launch msg for this job yet - can't happen with
             * our own local procs, but this could involve a proc
             * running remotely that we don't know about yet
             */
            do_progress = false;
        }
    }

    /* what remains in the buffer is solely the data payload, so
     * add it to the collective
     */
    opal_dss.copy_payload(&coll->local_bucket, buffer);

    /* if all involved jobs are known, then progress collectives */
    if (do_progress) {
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s PROGRESSING COLLECTIVE %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), id));
        orte_grpcomm_base_progress_collectives();
    }
}

void orte_grpcomm_base_pack_collective(opal_buffer_t *relay,
                                       orte_grpcomm_collective_t *coll,
                                       orte_grpcomm_internal_stage_t stg)
{
    orte_vpid_t nprocs;
    orte_namelist_t *nm;
    opal_list_item_t *itm;

    opal_dss.pack(relay, &coll->id, 1, ORTE_GRPCOMM_COLL_ID_T);
    nprocs = opal_list_get_size(&coll->participants);
    opal_dss.pack(relay, &nprocs, 1, ORTE_VPID);
    if (0 < nprocs) {
        for (itm = opal_list_get_first(&coll->participants);
             itm != opal_list_get_end(&coll->participants);
             itm = opal_list_get_next(itm)) {
            nm = (orte_namelist_t*)itm;
            opal_dss.pack(relay, &nm->name, 1, ORTE_NAME);
        }
    }
    if (ORTE_GRPCOMM_INTERNAL_STG_LOCAL == stg) {
        opal_dss.pack(relay, &coll->num_local_recvd, 1, ORTE_VPID);
        opal_dss.copy_payload(relay, &coll->local_bucket);
    } else if (ORTE_GRPCOMM_INTERNAL_STG_APP == stg) {
        opal_dss.copy_payload(relay, &coll->buffer);
    } else if (ORTE_GRPCOMM_INTERNAL_STG_GLOBAL == stg) {
        opal_dss.pack(relay, &coll->num_global_recvd, 1, ORTE_VPID);
        opal_dss.copy_payload(relay, &coll->buffer);
    } else {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
    }
}


void orte_grpcomm_base_progress_collectives(void)
{
    opal_list_item_t *item, *itm;
    orte_grpcomm_collective_t *coll;
    orte_namelist_t *nm;
    orte_job_t *jdata;
    orte_vpid_t nlp, vpid;
    opal_buffer_t *relay;
    int rc;

    /* cycle thru all known collectives - any collective on the list
     * must have come from either a local proc or receiving a global
     * collective. Either way, the number of required recipients
     * should have been set
     */
    item = opal_list_get_first(&orte_grpcomm_base.active_colls);
    while (item != opal_list_get_end(&orte_grpcomm_base.active_colls)) {
        coll = (orte_grpcomm_collective_t*)item;
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s PROGRESSING COLL id %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             coll->id));
        /* if this collective is already locally complete, then ignore it */
        if (coll->locally_complete) {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s COLL %d IS LOCALLY COMPLETE",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 coll->id));
            goto next_coll;
        }
        /* setup to count number of local participants */
        nlp = 0;
        /* check all participants */
        for (itm = opal_list_get_first(&coll->participants);
             itm != opal_list_get_end(&coll->participants);
             itm = opal_list_get_next(itm)) {
            nm = (orte_namelist_t*)itm;
            /* get the job object for this participant */
            if (NULL == (jdata = orte_get_job_data_object(nm->name.jobid))) {
                /* if the job object isn't found, then we can't progress
                 * this collective
                 */
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s COLL %d JOBID %s NOT FOUND",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     coll->id, ORTE_JOBID_PRINT(nm->name.jobid)));
                goto next_coll;
            }
            /* if the job object is found, then we know about this
             * job - count its local participants
             */
            if (ORTE_VPID_WILDCARD == nm->name.vpid) {
                /* all local procs from this job are required to participate */
                OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                     "%s ALL LOCAL PROCS CONTRIBUTE %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)jdata->num_local_procs));
                nlp += jdata->num_local_procs;
            } else {
                /* see if this is a local proc */
                if (ORTE_VPID_INVALID == (vpid = orte_ess.proc_get_daemon(&nm->name))) {
                    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                         "%s COLL %d VPID %s NONLOCAL",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         coll->id, ORTE_VPID_PRINT(nm->name.vpid)));
                    continue;
                }
                if (vpid == ORTE_PROC_MY_NAME->vpid) {
                    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                         "%s grpcomm:prog:collectives Counting %s as local participant",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&nm->name)));
                    nlp++;
                }
            }
        }
        /* see if all reqd participants are done */
        if (nlp == coll->num_local_recvd) {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s COLLECTIVE %d LOCALLY COMPLETE - SENDING TO GLOBAL COLLECTIVE",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), coll->id));
            /* mark it as locally complete */
            coll->locally_complete = true;
            /* pack the collective */
            relay = OBJ_NEW(opal_buffer_t);
            orte_grpcomm_base_pack_collective(relay, coll, ORTE_GRPCOMM_INTERNAL_STG_LOCAL);
            /* send it to our global collective handler */
            if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_NAME, relay,
                                                  ORTE_RML_TAG_DAEMON_COLL, 0,
                                                  orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(relay);
            }
        }

    next_coll:
        item = opal_list_get_next(item);
    }
}

static void daemon_coll_recv(int status, orte_process_name_t* sender,
                             opal_buffer_t* data, orte_rml_tag_t tag,
                             void* cbdata)
{
    orte_job_t *jdata;
    orte_std_cntr_t n;
    opal_list_item_t *item;
    orte_vpid_t np, nprocs, total_local_np;
    int rc;
    orte_grpcomm_collective_t *coll;
    orte_namelist_t *nm;
    orte_grpcomm_coll_id_t id;
    bool keep, do_progress;
    orte_process_name_t proc;
    opal_buffer_t *relay;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:daemon_coll: daemon collective recvd from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* get the collective id */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &id, &n, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:daemon_coll: WORKING COLLECTIVE %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), id));

    /* setup the collective for this id - if it's already present,
     * then this will just return the existing structure
     */
    coll = orte_grpcomm_base_setup_collective(id);

    /* record that we received a bucket */
    coll->num_peer_buckets++;

    /* unpack the number of procs involved */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &nprocs, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* do we need to keep the participants? */
    keep = true;
    if (0 < opal_list_get_size(&coll->participants)) {
        /* already have it */
        keep = false;
    }

    do_progress = true;
    total_local_np = 0;
    for (np=0; np < nprocs; np++) {
        /* unpack the name of this participant */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &proc, &n, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        if (keep) {
            /* add the name to the list */
            nm = OBJ_NEW(orte_namelist_t);
            nm->name.jobid = proc.jobid;
            nm->name.vpid = proc.vpid;
            opal_list_append(&coll->participants, &nm->super);
        }
        /*  find this job */
        if (NULL == (jdata = orte_get_job_data_object(proc.jobid))) {
            /* if we can't find it, then we haven't processed the
             * launch msg for this job yet - can't happen with
             * our own local procs, but this could involve a proc
             * running remotely that we don't know about yet
             */
            do_progress = false;
        }
        total_local_np += jdata->num_local_procs;
    }
    if (do_progress && 0 == total_local_np) {
        coll->locally_complete = true;
    }

    /* unpack the number of contributors involved in the incoming data */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &np, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                         "%s grpcomm:base:daemon_coll: NUM CONTRIBS: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_VPID_PRINT(np)));
    /* add it to the number of global recvd */
    coll->num_global_recvd += np;

    /* transfer the data */
    opal_dss.copy_payload(&coll->buffer, data);

    /* are we done? */
    if (!do_progress || !coll->locally_complete) {
        /* can't continue - missing at least one launch msg
         * or not locally complete
         */
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:base:daemon_coll: CANNOT PROGRESS",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return;
    }

    /* determine how many buckets we should receive from others
     * involved in this collective - need to know the number
     * of total contributors from all buckets being relayed
     * thru us
     */
    orte_routed.get_routing_list(ORTE_GRPCOMM_COLL_PEERS, coll);
    np = 1;  /* account for our own bucket */
    while (NULL != (item = opal_list_remove_first(&coll->targets))) {
        nm = (orte_namelist_t*)item;
        if (ORTE_VPID_WILDCARD == nm->name.vpid) {
            /* wait for input from all daemons */
            np = orte_process_info.num_procs;
            break;
        } else {
            np++;
        }
    }
    /* clear the list for reuse */
    while (NULL != (nm = (orte_namelist_t*)opal_list_remove_first(&coll->targets))) {
        OBJ_RELEASE(nm);
    }

    /* relay the data, if required */
    if (np == coll->num_peer_buckets) {
        orte_routed.get_routing_list(ORTE_GRPCOMM_COLL_RELAY, coll);

        while (NULL != (nm = (orte_namelist_t*)opal_list_remove_first(&coll->targets))) {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                                 "%s grpcomm:base:daemon_coll: RELAYING COLLECTIVE TO %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&nm->name)));        
            relay = OBJ_NEW(opal_buffer_t);
            orte_grpcomm_base_pack_collective(relay, coll, ORTE_GRPCOMM_INTERNAL_STG_GLOBAL);
            if (ORTE_VPID_WILDCARD == nm->name.vpid) {
                /* this is going to everyone in this job, so use xcast */
                orte_grpcomm.xcast(nm->name.jobid, relay, ORTE_RML_TAG_DAEMON_COLL);
                OBJ_RELEASE(relay);
            }
            /* otherwise, send to each member, but don't send it back to the
             * sender as that can create an infinite loop
             */
            if (nm->name.vpid == sender->vpid) {
                OBJ_RELEASE(relay);
            } else {
                if (0 > orte_rml.send_buffer_nb(&nm->name, relay, ORTE_RML_TAG_DAEMON_COLL, 0,
                                                orte_rml_send_callback, NULL)) {
                    ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                    OBJ_RELEASE(relay);
                }
            }
            OBJ_RELEASE(nm);
        }
    }
    /* clear the list for reuse */
    while (NULL != (nm = (orte_namelist_t*)opal_list_remove_first(&coll->targets))) {
        OBJ_RELEASE(nm);
    }

    /* determine how many contributors we need to recv - we know
     * that all job objects were found, so we can skip that test
     * while counting
     */
    np = 0;
    for (item = opal_list_get_first(&coll->participants);
         item != opal_list_get_end(&coll->participants);
         item = opal_list_get_next(item)) {
        nm = (orte_namelist_t*)item;
        /* get the job object for this participant */
        jdata = orte_get_job_data_object(nm->name.jobid);
        if (ORTE_VPID_WILDCARD == nm->name.vpid) {
            /* all procs from this job are required to participate */
            np += jdata->num_procs;
        } else {
            np++;
        }
    }

    /* are we done? */
    if (np != coll->num_global_recvd) {
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base.output,
                             "%s grpcomm:base:daemon_coll: MISSING CONTRIBUTORS: np %s ngr %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_VPID_PRINT(np),
                             ORTE_VPID_PRINT(coll->num_global_recvd)));
        return;
    }

    /* find out where, if anywhere, to send the results */
    orte_routed.get_routing_list(ORTE_GRPCOMM_COLL_COMPLETE, coll);

    /* pass the result */
    while (NULL != (item = opal_list_remove_first(&coll->targets))) {
        nm = (orte_namelist_t*)item;
        relay = OBJ_NEW(opal_buffer_t);
        opal_dss.pack(relay, &coll->id, 1, ORTE_GRPCOMM_COLL_ID_T);
        opal_dss.copy_payload(relay, &coll->buffer);
       if (ORTE_VPID_WILDCARD == nm->name.vpid) {
            /* all procs from this job get it */
            orte_grpcomm.xcast(nm->name.jobid, relay, ORTE_RML_TAG_COLLECTIVE);
            OBJ_RELEASE(relay);
        } else {
            /* send it to this proc */
            if (0 > orte_rml.send_buffer_nb(&nm->name, relay, ORTE_RML_TAG_COLLECTIVE, 0,
                                            orte_rml_send_callback, NULL)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                OBJ_RELEASE(relay);
            }
        }
        OBJ_RELEASE(nm);
    }

    /* remove this collective */
    opal_list_remove_item(&orte_grpcomm_base.active_colls, &coll->super);
    OBJ_RELEASE(coll);
}

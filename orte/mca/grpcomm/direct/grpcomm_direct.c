/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
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

#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_direct.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *buf);
static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *buf);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_direct_module = {
    init,
    finalize,
    xcast,
    allgather
};

/* internal functions */
static void xcast_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t* buffer, orte_rml_tag_t tag,
                       void* cbdata);
static void allgather_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata);
static allgather_trk_t* get_trk(oorte_grpcomm_coll_t *coll);

/* internal variables */
static opal_list_t tracker;

/**
 * Initialize the module
 */
static int init(void)
{
    OBJ_CONSTRUCT(&tracker, opal_list_t);

    /* post the receives */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_XCAST,
                            ORTE_RML_PERSISTENT,
                            xcast_recv, NULL);
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_ALLGATHER,
                            ORTE_RML_PERSISTENT,
                            allgather_recv, NULL);

    return OPAL_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    /* cancel the recv */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_XCAST);

    OPAL_LIST_DESTRUCT(&tracker);
    return;
}

static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *buf)
{
    int rc;

    /* send it to the HNP (could be myself) for relay */
    OBJ_RETAIN(buf);  // we'll let the RML release it
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_XCAST,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return rc;
    }
    return ORTE_SUCCESS;
}

static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *buf)
{
    /* the base functions pushed us into the event library
     * before calling us, so we can safely access global data
     * at this point */

    /* if we are the HNP and nobody else is participating,
     * then just execute the xcast */
    if (ORTE_PROC_IS_HNP && 1 == coll->ndmns) {
        xcast(coll->dmns, coll->ndmns, buf);
        opal_list_remove_item(&orte_grpcomm_base.ongoing, &coll->super);
        OBJ_RELEASE(coll);
        return ORTE_SUCCESS;
    }
    /* send the info to the HNP for tracking */
    OBJ_RETAIN(buf);
    orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, ORTE_RML_TAG_ALLGATHER,
                            buf, orte_rml_send_callback, NULL);
}

static void allgather_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata)
{
    int32_t cnt;
    uint64_t id;
    int rc;
    orte_process_name_t name;
    size_t sz;
    opal_identifier_t *sig = NULL;
    orte_vpid_t nprocs;
    opal_buffer_t *reply;

    /* unpack the id of the proc involved in case the signature is NULL*/
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &id, &cnt, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    memcpy(&name, &id, sizeof(orte_process_name_t));

    /* unpack the #procs in this collective - if it's a zero, then
     * that means everyone in the job is participating. If nonzero,
     * then we have an array of proc names that are involved */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &sz, &cnt, OPAL_SIZE))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    if (0 < sz) {
        /* allocate space for the array */
        sig = (opal_identifier_t*)malloc(sz * sizeof(opal_identifier_t));
        /* unpack the array - the array is our signature for the collective */
        cnt = sz;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &sig, &cnt, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            if (NULL != sig) {
                free(sig);
            }
            return;
        }
    }
    /* check for the tracker and create it if not found */
    if (NULL == (trk = get_trk(name.jobid, sig, sz))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        if (NULL != sig) {
            free(sig);
        }
        return;
    }

    /* unpack the number of procs reported in this message */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &nprocs, &cnt, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* increment nprocs reported for collective */
    trk->nprocs_reported += nprocs;

    /* if all procs reported */
    if (trk->nprocs_reqd == trk->nprocs_reported) {
        reply = OBJ_NEW(opal_buffer_t);
        /* pack the id of the sender in case the signature is NULL */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &id, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            if (NULL != sig) {
                free(sig);
            }
            return;
        }
        /* pack the size of the signature */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &sz, 1, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            if (NULL != sig) {
                free(sig);
            }
            return;
        }
        if (0 < sz) {
            /* pack the signature, if given */
            if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, sig, sz, OPAL_UINT64))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(reply);
                free(sig);
                return;
            }
            free(sig);
        }
        /* send the release via xcast */
        (void)orte_grpcomm.xcast((orte_process_name_t*)sig, sz,
                                 ORTE_RML_TAG_COLL_RELEASE, reply);
        OBJ_RELEASE(reply);
    }
}

static void xcast_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t* buffer, orte_rml_tag_t tg,
                       void* cbdata)
{
    opal_list_item_t *item;
    orte_namelist_t *nm;
    int ret, cnt;
    opal_buffer_t *relay, *rly;
    orte_daemon_cmd_flag_t command;
    opal_buffer_t wireup;
    opal_byte_object_t *bo;
    int8_t flag;
    orte_job_t *jdata;
    orte_proc_t *rec;
    opal_list_t coll;
    size_t nprocs;
    orte_process_name_t *targets;
    orte_rml_tag_t tag;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct:xcast:recv: with %d bytes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)buffer->bytes_used));

    /* we need a passthru buffer to send to our children */
    rly = OBJ_NEW(opal_buffer_t);
    opal_dss.copy_payload(rly, buffer);

    /* get the number of target procs */
    cnt=1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &nprocs, &cnt, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        ORTE_FORCED_TERMINATE(ret);
        return;
    }
    if (0 < nprocs) {
        targets = (orte_process_name_t*)malloc(nprocs * sizeof(orte_process_name_t));
        cnt=nprocs;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, targets, &cnt, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            ORTE_FORCED_TERMINATE(ret);
            return;
        }
    }
    /* get the target tag */
    cnt=1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &tag, &cnt, ORTE_RML_TAG))) {
        ORTE_ERROR_LOG(ret);
        ORTE_FORCED_TERMINATE(ret);
        return;
    }

    /* setup a buffer we can pass to ourselves - this just contains
     * the initial message, minus the headers inserted by xcast itself */
    relay = OBJ_NEW(opal_buffer_t);
    opal_dss.copy_payload(relay, buffer);

    /* if this is headed for the daemon command processor,
     * then we first need to check for add_local_procs
     * as that command includes some needed wireup info */
    if (ORTE_RML_TAG_DAEMON == tag) {
        /* peek at the command */
        cnt=1;
        if (ORTE_SUCCESS == (ret = opal_dss.unpack(buffer, &command, &cnt, ORTE_DAEMON_CMD))) {
            /* if it is add_procs, then... */
            if (ORTE_DAEMON_ADD_LOCAL_PROCS == command) {
                OBJ_RELEASE(relay);
                relay = OBJ_NEW(opal_buffer_t);
                /* repack the command */
                if (OPAL_SUCCESS != (ret = opal_dss.pack(relay, &command, 1, ORTE_DAEMON_CMD))) {
                    ORTE_ERROR_LOG(ret);
                    goto relay;
                }
                /* extract the byte object holding the daemonmap */
                cnt=1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
                    ORTE_ERROR_LOG(ret);
                    goto relay;
                }
                
                /* update our local nidmap, if required - the decode function
                 * knows what to do - it will also free the bytes in the byte object
                 */
                if (ORTE_PROC_IS_HNP) {
                    /* no need - already have the info */
                    if (NULL != bo->bytes) {
                        free(bo->bytes);
                    }
                } else {
                    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                         "%s grpcomm:direct:xcast updating daemon nidmap",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
                    if (ORTE_SUCCESS != (ret = orte_util_decode_daemon_nodemap(bo))) {
                        ORTE_ERROR_LOG(ret);
                        goto relay;
                    }
                }

                /* update the routing plan */
                orte_routed.update_routing_plan();
    
                /* see if we have wiring info as well */
                cnt=1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &flag, &cnt, OPAL_INT8))) {
                    ORTE_ERROR_LOG(ret);
                    goto relay;
                }
                if (0 == flag) {
                    /* no - just return */
                    goto relay;
                }

                /* unpack the byte object */
                cnt=1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
                    ORTE_ERROR_LOG(ret);
                    goto relay;
                }
                if (0 < bo->size) {
                    /* load it into a buffer */
                    OBJ_CONSTRUCT(&wireup, opal_buffer_t);
                    opal_dss.load(&wireup, bo->bytes, bo->size);
                    /* pass it for processing */
                    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, &wireup))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_DESTRUCT(&wireup);
                        goto relay;
                    }
                    /* done with the wireup buffer - dump it */
                    OBJ_DESTRUCT(&wireup);
                }
                /* copy the remainder of the payload */
                opal_dss.copy_payload(relay, buffer);
            }
        } else {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
    }

 relay:
    /* setup the relay list */
    OBJ_CONSTRUCT(&coll, opal_list_t);

    /* get the list of next recipients from the routed module */
    orte_routed.get_routing_list(&coll);

    /* if list is empty, no relay is required */
    if (opal_list_is_empty(&coll)) {
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:direct:send_relay - recipient list is empty!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto CLEANUP;
    }
    
    /* send the message to each recipient on list, deconstructing it as we go */
    while (NULL != (item = opal_list_remove_first(&coll))) {
        nm = (orte_namelist_t*)item;

        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:direct:send_relay sending relay msg to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&nm->name)));
        OBJ_RETAIN(relay);
        /* check the state of the recipient - no point
         * sending to someone not alive
         */
        jdata = orte_get_job_data_object(nm->name.jobid);
        if (NULL == (rec = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, nm->name.vpid))) {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:direct:send_relay proc %s not found - cannot relay",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&nm->name)));
            OBJ_RELEASE(rly);
            continue;
        }
        if (ORTE_PROC_STATE_RUNNING < rec->state) {
            OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:direct:send_relay proc %s not running - cannot relay",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&nm->name)));
            OBJ_RELEASE(relay);
            continue;
        }
        if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(&nm->name, rly, ORTE_RML_TAG_XCAST,
                                                           orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(relay);
            continue;
        }
    }
    OBJ_RELEASE(relay);  // maintain accounting

 CLEANUP:
    /* cleanup */
    OBJ_DESTRUCT(&coll);

    /* now send the rly buffer to myself for processing */
    if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_NAME, relay, tag,
                                                       orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(relay);
    }
}

static allgather_trk_t* get_trk(orte_jobid_t jobid,
                                opal_identifier_t *sig,
                                size_t sz)
{
    allgather_trk_t *trk;
    orte_job_t *jdata;
    orte_proc_t *proc;
    orte_process_name_t name;

    OPAL_LIST_FOREACH(trk, &tracker, allgather_trk_t) {
        if (NULL == sig) {
            /* see if the jobid matches */
            if (jobid == trk->jobid) {
                /* got it */
                opal_output_verbose(2, pmix_server_output,
                                    "%s direct:allgather found tracker for job %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_JOBID_PRINT(jobid));
                return trk;
            }
        } else if (sz == trk->nprocs_reqd) {
            if (0 == memcmp(sig, trk->signature, sz)) {
                /* got it */
                opal_output_verbose(2, pmix_server_output,
                                    "%s direct:allgather found tracker for signature",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                return trk;
            }
        }
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s direct:allgather adding new tracker for job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jobid));

    /* get here if tracker not found */
    trk = OBJ_NEW(pmix_server_trk_t);
    trk->jobid = jobid;
    opal_list_append(&tracker, &trk->super);
    if (NULL != sig) {
        trk->signature = (opal_identifier_t*)malloc(sz * sizeof(opal_identifier_t));
        memcpy(trk->signature, sig, sz * sizeof(opal_identifier_t));
        trk->nprocs_reqd = sz;
    } else {
        /* get the job object */
        if (NULL == (jdata = orte_get_job_data_object(jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        trk->nprocs_reqd = jdata->num_procs;
    }
    opal_output_verbose(2, pmix_server_output,
                        "%s direct:allgather tracker requires %s procs",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_VPID_PRINT(trk->nprocs_reqd));
    return trk;
}

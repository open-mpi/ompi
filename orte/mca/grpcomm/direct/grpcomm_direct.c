/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "opal/class/opal_list.h"

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
static void barrier_release(int status, orte_process_name_t* sender,
                            opal_buffer_t* buffer, orte_rml_tag_t tag,
                            void* cbdata);

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
                            ORTE_RML_TAG_ALLGATHER_DIRECT,
                            ORTE_RML_PERSISTENT,
                            allgather_recv, NULL);
    /* setup recv for barrier release */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_COLL_RELEASE,
                            ORTE_RML_PERSISTENT,
                            barrier_release, NULL);

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
    int rc, ret;
    opal_buffer_t *relay;
    orte_job_t *jdata;
    uint64_t nprocs;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct: allgather",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* the base functions pushed us into the event library
     * before calling us, so we can safely access global data
     * at this point */

    relay = OBJ_NEW(opal_buffer_t);
    /* pack the signature */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(relay, &coll->sig, 1, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(relay);
        return rc;
    }

    /* if we are the HNP and nobody else is participating,
     * then just execute the xcast */
    if (ORTE_PROC_IS_HNP && 1 == coll->ndmns) {
        /* pack the status - success since the allgather completed. This
         * would be an error if we timeout instead */
        ret = ORTE_SUCCESS;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(relay, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(relay);
            return rc;
        }
        /* pack the number of procs involved in the collective
         * so the recipients can unpack any collected data */
        if (1 == coll->sig->sz) {
            /* get the job object for this entry */
            if (NULL == (jdata = orte_get_job_data_object(coll->sig->signature[0].jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OBJ_RELEASE(relay);
                return ORTE_ERR_NOT_FOUND;
            }
            nprocs = jdata->num_procs;
        } else {
            nprocs = coll->sig->sz;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(relay, &nprocs, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(relay);
            return rc;
        }
        /* pass along the payload */
        opal_dss.copy_payload(relay, buf);
        orte_grpcomm.xcast(coll->sig, ORTE_RML_TAG_COLL_RELEASE, relay);
        OBJ_RELEASE(relay);
        return ORTE_SUCCESS;
    }

    /* pass along the payload */
    opal_dss.copy_payload(relay, buf);

    /* otherwise, we need to send this to the HNP for
     * processing */
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct:allgather sending to HNP",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* send the info to the HNP for tracking */
    rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, relay,
                                 ORTE_RML_TAG_ALLGATHER_DIRECT,
                                 orte_rml_send_callback, NULL);
    return rc;
}

static void allgather_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata)
{
    int32_t cnt;
    int rc, ret;
    orte_grpcomm_signature_t *sig;
    opal_buffer_t *reply;
    orte_grpcomm_coll_t *coll;
    orte_job_t *jdata;
    uint64_t nprocs;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct allgather recvd from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* unpack the signature */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &sig, &cnt, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* check for the tracker and create it if not found */
    if (NULL == (coll = orte_grpcomm_base_get_tracker(sig, true))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OBJ_RELEASE(sig);
        return;
    }

    /* increment nprocs reported for collective */
    coll->nreported++;
    /* capture any provided content */
    opal_dss.copy_payload(&coll->bucket, buffer);

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct allgather recv ndmns %d nrep %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)coll->ndmns, (int)coll->nreported));

    /* if all participating daemons have reported */
    if (coll->ndmns == coll->nreported) {
        reply = OBJ_NEW(opal_buffer_t);
        /* pack the signature */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &sig, 1, ORTE_SIGNATURE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_RELEASE(sig);
            return;
        }
        /* pack the status - success since the allgather completed. This
         * would be an error if we timeout instead */
        ret = ORTE_SUCCESS;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_RELEASE(sig);
            return;
        }
        /* pack the number of procs involved in the collective
         * so the recipients can unpack any collected data */
        if (1 == sig->sz) {
            /* get the job object for this entry */
            if (NULL == (jdata = orte_get_job_data_object(sig->signature[0].jobid))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                OBJ_RELEASE(reply);
                OBJ_RELEASE(sig);
                return;
            }
            nprocs = jdata->num_procs;
        } else {
            nprocs = sig->sz;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &nprocs, 1, OPAL_UINT64))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(reply);
            OBJ_RELEASE(sig);
            return;
        }
        /* transfer the collected bucket */
        opal_dss.copy_payload(reply, &coll->bucket);

        /* send the release via xcast */
        (void)orte_grpcomm.xcast(sig, ORTE_RML_TAG_COLL_RELEASE, reply);
        OBJ_RELEASE(reply);
    }
    OBJ_RELEASE(sig);
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
    orte_grpcomm_signature_t *sig;
    orte_rml_tag_t tag;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct:xcast:recv: with %d bytes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)buffer->bytes_used));

    /* we need a passthru buffer to send to our children */
    rly = OBJ_NEW(opal_buffer_t);
    opal_dss.copy_payload(rly, buffer);

    /* get the signature that we do not need */
    cnt=1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &sig, &cnt, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(ret);
        ORTE_FORCED_TERMINATE(ret);
        return;
    }
    OBJ_RELEASE(sig);

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
                    if (NULL != bo) {
                        if (NULL != bo->bytes) {
                            free(bo->bytes);
                        }
                        free(bo);
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
                    /* copy the remainder of the payload */
                    opal_dss.copy_payload(relay, buffer);
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
                free(bo);
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
        OBJ_RELEASE(rly);
        goto CLEANUP;
    }
    
    /* send the message to each recipient on list, deconstructing it as we go */
    while (NULL != (item = opal_list_remove_first(&coll))) {
        nm = (orte_namelist_t*)item;

        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:direct:send_relay sending relay msg of %d bytes to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)rly->bytes_used,
                             ORTE_NAME_PRINT(&nm->name)));
        OBJ_RETAIN(rly);
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
            OBJ_RELEASE(rly);
            continue;
        }
        if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(&nm->name, rly, ORTE_RML_TAG_XCAST,
                                                           orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(rly);
            continue;
        }
    }
    OBJ_RELEASE(rly);  // retain accounting

 CLEANUP:
    /* cleanup */
    OBJ_DESTRUCT(&coll);

    /* now send the relay buffer to myself for processing */
    if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_NAME, relay, tag,
                                                       orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(relay);
    }
}

static void barrier_release(int status, orte_process_name_t* sender,
                            opal_buffer_t* buffer, orte_rml_tag_t tag,
                            void* cbdata)
{
    int32_t cnt;
    int rc, ret;
    orte_grpcomm_signature_t *sig;
    orte_grpcomm_coll_t *coll;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:direct: barrier release called",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the signature */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &sig, &cnt, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the return status */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* check for the tracker - it is not an error if not
     * found as that just means we wre not involved
     * in the collective */
    if (NULL == (coll = orte_grpcomm_base_get_tracker(sig, false))) {
        OBJ_RELEASE(sig);
        return;
    }

    /* execute the callback */
    if (NULL != coll->cbfunc) {
        coll->cbfunc(ret, buffer, coll->cbdata);
    }
    opal_list_remove_item(&orte_grpcomm_base.ongoing, &coll->super);
    OBJ_RELEASE(coll);
}


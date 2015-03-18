/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
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
#include "orte/runtime/orte_wait.h"

#include <math.h>
#include <string.h>

#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_brks.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *buf);
static void brks_allgather_process_data(orte_grpcomm_coll_t *coll, uint32_t distance);
static int brks_allgather_send_dist(orte_grpcomm_coll_t *coll, orte_process_name_t *peer, uint32_t distance);
static void brks_allgather_recv_dist(int status, orte_process_name_t* sender,
                                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                                     void* cbdata);
static int brks_finalize_coll(orte_grpcomm_coll_t *coll, int ret);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_brks_module = {
    init,
    finalize,
    NULL,
    allgather
};

/**
 * Initialize the module
 */
static int init(void)
{
    /* setup recv for distance data */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_ALLGATHER_BRKS,
                            ORTE_RML_PERSISTENT,
                            brks_allgather_recv_dist, NULL);
    return OPAL_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    /* cancel the recv */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER_BRKS);

    return;
}

static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *sendbuf)
{
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:bruck algo employed for %d processes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)coll->ndmns));

    /* record that we contributed */
    coll->nreported = 1;

    /* mark local data received */
    coll->distance_mask_recv = (uint32_t *)calloc(sizeof(uint32_t), (coll->ndmns - 1));

    /* start by seeding the collection with our own data */
    opal_dss.copy_payload(&coll->bucket, sendbuf);

    /* process data */
    brks_allgather_process_data(coll, 1);

    return ORTE_SUCCESS;
}

static int brks_allgather_send_dist(orte_grpcomm_coll_t *coll, orte_process_name_t *peer, uint32_t distance) {
    opal_buffer_t *send_buf;
    int rc;

    send_buf = OBJ_NEW(opal_buffer_t);

    /* pack the signature */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(send_buf, &coll->sig, 1, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    }
    /* pack the current distance */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(send_buf, &distance, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    }
    /* pack the data */
    if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(send_buf, &coll->bucket))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:brks SENDING TO %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));


    if (0 > (rc = orte_rml.send_buffer_nb(peer, send_buf,
                                          ORTE_RML_TAG_ALLGATHER_BRKS,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    };

    return ORTE_SUCCESS;
}

static void brks_allgather_process_data(orte_grpcomm_coll_t *coll, uint32_t distance) {
    /* Communication step:
     At every step i, rank r:
     - doubles the distance
     - sends message containing all data collected so far to rank r - distance
     - receives message containing all data collected so far from rank (r + distance)
     */
    orte_process_name_t peer;
    orte_vpid_t nv, rank;
    int rc;

    peer.jobid = ORTE_PROC_MY_NAME->jobid;

    /* get my own rank */
    rank = ORTE_VPID_INVALID;
    for (orte_vpid_t nv = 0; nv < coll->ndmns; nv++) {
        if (coll->dmns[nv] == ORTE_PROC_MY_NAME->vpid) {
            rank = nv;
            break;
        }
    }
    /* check for bozo case */
    if (ORTE_VPID_INVALID == rank) {
        OPAL_OUTPUT((orte_grpcomm_base_framework.framework_output,
                     "Peer not found"));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        brks_finalize_coll(coll, ORTE_ERR_NOT_FOUND);
        return;
    }

    while (distance < coll->ndmns) {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
             "%s grpcomm:coll:brks process distance %u)",
              ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));

        /* first send my current contents */
        nv = (coll->ndmns + rank - distance) % coll->ndmns;
        peer.vpid = coll->dmns[nv];

        brks_allgather_send_dist(coll, &peer, distance);

        /* check whether data for next distance is available*/
        if ((NULL != coll->buffers) && (coll->buffers[distance - 1] != NULL)) {
            OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:coll:brks %u distance data found",
                                  ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));
            if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(&coll->bucket, coll->buffers[distance - 1]))) {
                ORTE_ERROR_LOG(rc);
                brks_finalize_coll(coll, rc);
                return;
            }
            coll->nreported += distance;
            orte_grpcomm_base_mark_distance_recv(coll, distance);
            OBJ_RELEASE(coll->buffers[distance - 1]);
            coll->buffers[distance - 1] = NULL;
            distance = distance << 1;
            continue;
        }
        break;
    }
    OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                        "%s grpcomm:coll:brks reported %lu process from %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (unsigned long)coll->nreported,
                        (unsigned long)coll->ndmns));

    /* if we are done, then complete things */
    if (coll->nreported >= coll->ndmns){
        brks_finalize_coll(coll, ORTE_SUCCESS);
    }
    return;
}

static void brks_allgather_recv_dist(int status, orte_process_name_t* sender,
                                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                                     void* cbdata)
{
    int32_t cnt;
    int rc;
    orte_grpcomm_signature_t *sig;
    orte_grpcomm_coll_t *coll;
    uint32_t distance;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:brks RECEIVING FROM %s",
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
    /* unpack the distance */
    distance = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &distance, &cnt, OPAL_INT32))) {
        OBJ_RELEASE(sig);
        ORTE_ERROR_LOG(rc);
        brks_finalize_coll(coll, rc);
        return;
    }
    assert(0 == orte_grpcomm_base_check_distance_recv(coll, distance));

    /* Check whether we can process next distance */
    if (orte_grpcomm_base_check_distance_recv(coll, (distance >> 1))) {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                     "%s grpcomm:coll:brks data from %d distance received, "
                     "Process the next distance.",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));
        /* capture any provided content */
        if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(&coll->bucket, buffer))) {
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            brks_finalize_coll(coll, rc);
            return;
        }
        coll->nreported += distance;
        orte_grpcomm_base_mark_distance_recv(coll, distance);
        brks_allgather_process_data(coll, (uint32_t)(distance << 1));
    } else {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:coll:brks data from %d distance received, "
                             "still waiting for data.",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));
        if (NULL == coll->buffers) {
            if (NULL == (coll->buffers = (opal_buffer_t **)calloc(sizeof(opal_buffer_t *), coll->ndmns - 1))) {
                rc = OPAL_ERR_OUT_OF_RESOURCE;
                OBJ_RELEASE(sig);
                ORTE_ERROR_LOG(rc);
                brks_finalize_coll(coll, rc);
                return;
            }
        }
        if (NULL == (coll->buffers[distance - 1] = OBJ_NEW(opal_buffer_t))) {
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            brks_finalize_coll(coll, rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(coll->buffers[distance - 1], buffer))) {
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            brks_finalize_coll(coll, rc);
            return;
        }
    }

    OBJ_RELEASE(sig);

    return;
}

static int brks_finalize_coll(orte_grpcomm_coll_t *coll, int ret) {
    opal_buffer_t *reply;
    int rc;
    orte_job_t *jdata;
    uint64_t nprocs;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:brks declared collective complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* pack the number of procs involved in the collective
     * so the recipients can unpack any collected data */
    if (1 == coll->sig->sz) {
        /* get the job object for this entry */
        if (NULL == (jdata = orte_get_job_data_object(coll->sig->signature[0].jobid))) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            return ORTE_ERROR;
        }
        nprocs = jdata->num_procs;
    } else {
        nprocs = coll->sig->sz;
    }

    reply = OBJ_NEW(opal_buffer_t);
    if (NULL == reply) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &nprocs, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        return rc;
    }

    /* transfer the collected bucket */
    opal_dss.copy_payload(reply, &coll->bucket);

    /* execute the callback */
    if (NULL != coll->cbfunc) {
        coll->cbfunc(ret, reply, coll->cbdata);
    }

    opal_list_remove_item(&orte_grpcomm_base.ongoing, &coll->super);

    OBJ_RELEASE(reply);

    return ORTE_SUCCESS;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
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
#include "grpcomm_brucks.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *buf);
static void brucks_allgather_process_data(orte_grpcomm_coll_t *coll, uint32_t distance);
static int brucks_allgather_send_dist(orte_grpcomm_coll_t *coll, orte_process_name_t *peer, uint32_t distance);
static void brucks_allgather_recv_dist(int status, orte_process_name_t* sender,
                                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                                     void* cbdata);
static int brucks_finalize_coll(orte_grpcomm_coll_t *coll, int ret);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_brucks_module = {
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
                            ORTE_RML_TAG_ALLGATHER_BRUCKS,
                            ORTE_RML_PERSISTENT,
                            brucks_allgather_recv_dist, NULL);
    return OPAL_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    /* cancel the recv */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER_BRUCKS);
}

static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *sendbuf)
{
    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:brucks algo employed for %d processes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)coll->ndmns));
    /* get my own rank */
    coll->my_rank = ORTE_VPID_INVALID;
    for (orte_vpid_t nv = 0; nv < coll->ndmns; nv++) {
        if (coll->dmns[nv] == ORTE_PROC_MY_NAME->vpid) {
            coll->my_rank = nv;
            break;
        }
    }

    /* check for bozo case */
    if (ORTE_VPID_INVALID == coll->my_rank) {
        OPAL_OUTPUT((orte_grpcomm_base_framework.framework_output,
                     "Peer not found"));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        brucks_finalize_coll(coll, ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* record that we contributed */
    coll->nreported = 1;

    /* mark local data received */
    if (coll->ndmns > 1) {
        opal_bitmap_init (&coll->distance_mask_recv, (uint32_t) log2 (coll->ndmns) + 1);
    }

    /* start by seeding the collection with our own data */
    opal_dss.copy_payload(&coll->bucket, sendbuf);

    /* process data */
    brucks_allgather_process_data (coll, 0);

    return ORTE_SUCCESS;
}

static int brucks_allgather_send_dist(orte_grpcomm_coll_t *coll, orte_process_name_t *peer, uint32_t distance) {
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
    /* pack the number of daemons included in the payload */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(send_buf, &coll->nreported, 1, OPAL_SIZE))) {
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
                         "%s grpcomm:coll:brucks SENDING TO %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));


    if (0 > (rc = orte_rml.send_buffer_nb(peer, send_buf,
                                          ORTE_RML_TAG_ALLGATHER_BRUCKS,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    };

    return ORTE_SUCCESS;
}

static int brucks_allgather_process_buffered (orte_grpcomm_coll_t *coll, uint32_t distance) {
    opal_buffer_t *buffer;
    size_t nreceived;
    int32_t cnt = 1;
    int rc;

    /* check whether data for next distance is available*/
    if (NULL == coll->buffers || NULL == coll->buffers[distance]) {
        return 0;
    }

    buffer = coll->buffers[distance];
    coll->buffers[distance] = NULL;

    OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:brucks %u distance data found",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));
    rc = opal_dss.unpack (buffer, &nreceived, &cnt, OPAL_SIZE);
    if (OPAL_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        brucks_finalize_coll(coll, rc);
        return rc;
    }

    if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(&coll->bucket, buffer))) {
        ORTE_ERROR_LOG(rc);
        brucks_finalize_coll(coll, rc);
        return rc;
    }

    coll->nreported += nreceived;
    orte_grpcomm_base_mark_distance_recv (coll, distance);
    OBJ_RELEASE(buffer);

    return 1;
}

static void brucks_allgather_process_data(orte_grpcomm_coll_t *coll, uint32_t distance) {
    /* Communication step:
     At every step i, rank r:
     - doubles the distance
     - sends message containing all data collected so far to rank r - distance
     - receives message containing all data collected so far from rank (r + distance)
     */
    uint32_t log2ndmns = (uint32_t) log2 (coll->ndmns);
    uint32_t last_round;
    orte_process_name_t peer;
    orte_vpid_t nv;
    int rc;

    /* NTH: calculate in which round we should send the final data. this is the first
     * round in which we have data from at least (coll->ndmns - (1 << log2ndmns))
     * daemons. alternatively we could just send when distance reaches log2ndmns but
     * that could end up sending more data than needed */
    last_round = (uint32_t) ceil (log2 ((double) (coll->ndmns - (1 << log2ndmns))));

    peer.jobid = ORTE_PROC_MY_NAME->jobid;

    while (distance < log2ndmns) {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
             "%s grpcomm:coll:brucks process distance %u)",
              ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));

        /* first send my current contents */
        nv = (coll->ndmns + coll->my_rank - (1 << distance)) % coll->ndmns;
        peer.vpid = coll->dmns[nv];

        brucks_allgather_send_dist(coll, &peer, distance);

        if (distance == last_round) {
            /* have enough data to send the final round now */
            nv = (coll->ndmns + coll->my_rank - (1 << log2ndmns)) % coll->ndmns;
            peer.vpid = coll->dmns[nv];
            brucks_allgather_send_dist(coll, &peer, log2ndmns);
        }

        rc = brucks_allgather_process_buffered (coll, distance);
        if (!rc) {
            break;
        } else if (rc < 0) {
            return;
        }

        ++distance;
    }

    if (distance == log2ndmns) {
        if (distance == last_round) {
            /* need to send the final round now */
            nv = (coll->ndmns + coll->my_rank - (1 << log2ndmns)) % coll->ndmns;
            peer.vpid = coll->dmns[nv];
            brucks_allgather_send_dist(coll, &peer, log2ndmns);
        }

        /* check if the final message is already queued */
        rc = brucks_allgather_process_buffered (coll, distance);
        if (rc < 0) {
            return;
        }
    }

    OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                        "%s grpcomm:coll:brucks reported %lu process from %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (unsigned long)coll->nreported,
                        (unsigned long)coll->ndmns));

    /* if we are done, then complete things. we may get data from more daemons than expected */
    if (coll->nreported >= coll->ndmns){
        brucks_finalize_coll(coll, ORTE_SUCCESS);
    }
}

static void brucks_allgather_recv_dist(int status, orte_process_name_t* sender,
                                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                                     void* cbdata)
{
    int32_t cnt;
    int rc;
    orte_grpcomm_signature_t *sig;
    orte_grpcomm_coll_t *coll;
    uint32_t distance;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:brucks RECEIVING FROM %s",
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
        brucks_finalize_coll(coll, rc);
        return;
    }
    assert(0 == orte_grpcomm_base_check_distance_recv(coll, distance));

    /* Check whether we can process next distance */
    if (coll->nreported && (!distance || orte_grpcomm_base_check_distance_recv(coll, distance - 1))) {
        size_t nreceived;
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                     "%s grpcomm:coll:brucks data from %d distance received, "
                     "Process the next distance.",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));
        /* capture any provided content */
        rc = opal_dss.unpack (buffer, &nreceived, &cnt, OPAL_SIZE);
        if (OPAL_SUCCESS != rc) {
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            brucks_finalize_coll(coll, rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(&coll->bucket, buffer))) {
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            brucks_finalize_coll(coll, rc);
            return;
        }
        coll->nreported += nreceived;
        orte_grpcomm_base_mark_distance_recv(coll, distance);
        brucks_allgather_process_data(coll, distance + 1);
    } else {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:coll:brucks data from %d distance received, "
                             "still waiting for data.",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance));
        if (NULL == coll->buffers) {
            if (NULL == (coll->buffers = (opal_buffer_t **) calloc ((uint32_t) log2 (coll->ndmns) + 1, sizeof(opal_buffer_t *)))) {
                rc = OPAL_ERR_OUT_OF_RESOURCE;
                OBJ_RELEASE(sig);
                ORTE_ERROR_LOG(rc);
                brucks_finalize_coll(coll, rc);
                return;
            }
        }
        if (NULL == (coll->buffers[distance] = OBJ_NEW(opal_buffer_t))) {
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            brucks_finalize_coll(coll, rc);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(coll->buffers[distance], buffer))) {
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            brucks_finalize_coll(coll, rc);
            return;
        }
    }

    OBJ_RELEASE(sig);
}

static int brucks_finalize_coll(orte_grpcomm_coll_t *coll, int ret)
{
     OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:brucks declared collective complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* execute the callback */
    if (NULL != coll->cbfunc) {
        coll->cbfunc(ret, &coll->bucket, coll->cbdata);
    }

    opal_list_remove_item(&orte_grpcomm_base.ongoing, &coll->super);

    return ORTE_SUCCESS;
}

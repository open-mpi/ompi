/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
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
#include "grpcomm_rcd.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *msg);
static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *buf);
static void rcd_allgather_process_data(orte_grpcomm_coll_t *coll);
static int rcd_allgather_send_dist(orte_grpcomm_coll_t *coll, orte_process_name_t *peer, uint32_t distance);
static void rcd_allgather_recv_dist(int status, orte_process_name_t* sender,
                                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                                     void* cbdata);
static int rcd_finalize_coll(orte_grpcomm_coll_t *coll, int ret);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_rcd_module = {
    init,
    finalize,
    xcast,
    allgather
};

/**
 * Initialize the module
 */
static int init(void)
{
    /* setup recv for distance data */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_ALLGATHER_RCD,
                            ORTE_RML_PERSISTENT,
                            rcd_allgather_recv_dist, NULL);
    return OPAL_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    /* cancel the recv */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_ALLGATHER_RCD);
    return;
}

static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *msg)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int allgather(orte_grpcomm_coll_t *coll,
                     opal_buffer_t *sendbuf)
{
    /* check the number of involved daemons - if it is not a power of two,
     * then we cannot do it */
    if (0 == ((coll->ndmns != 0) && !(coll->ndmns & (coll->ndmns - 1)))) {
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:recdub algo employed for %d daemons",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)coll->ndmns));

    /* record that we contributed */
    coll->nreported += 1;

    /* mark local data received */
    coll->distance_mask_recv |= 1;

    /* start by seeding the collection with our own data */
    opal_dss.copy_payload(&coll->bucket, sendbuf);

    /* process data */
    rcd_allgather_process_data(coll);

    return ORTE_SUCCESS;
}

static int rcd_allgather_send_dist(orte_grpcomm_coll_t *coll, orte_process_name_t *peer, uint32_t distance) {
    opal_buffer_t *send_buf;
    int rc;

    send_buf = OBJ_NEW(opal_buffer_t);

    /* pack the signature */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(send_buf, &coll->sig, 1, ORTE_SIGNATURE))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    }
    /* pack the distance */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(send_buf, &distance, 1, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    }
    /* pack the number of daemons whose data we are including in this message */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(send_buf, &coll->nreported, 1, OPAL_INT32))) {
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
                         "%s grpcomm:coll:recdub SENDING TO %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));

    if (0 > (rc = orte_rml.send_buffer_nb(peer, send_buf,
                                          ORTE_RML_TAG_ALLGATHER_RCD,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(send_buf);
        return rc;
    };
    return ORTE_SUCCESS;
}

static void rcd_allgather_process_data(orte_grpcomm_coll_t *coll) {
    /* Communication step:
     At every step i, rank r:
     - exchanges message containing all data collected so far with rank peer = (r ^ 2^i).
     */
    orte_process_name_t peer;
    orte_vpid_t nv, rank;
    uint32_t distance, distance_prev, distance_bit;
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
        rcd_finalize_coll(coll, ORTE_ERR_NOT_FOUND);
        return;
    }

    distance_prev = coll->distance_send;
    distance = (0 == distance_prev) ? 1 : (distance_prev << 1);
    while(distance < coll->ndmns) {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
             "%s grpcomm:coll:recdub all data from previous %d distance received, "
             "process distance %u (mask recv: 0x%x)",
              ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance_prev, distance,
              coll->distance_mask_recv));

        /* first send my current contents */
        nv = rank ^ distance;
        peer.vpid = coll->dmns[nv];

        rcd_allgather_send_dist(coll, &peer, distance);

        coll->distance_send = distance;

        /* check whether data for next distance is available*/
        distance_bit = (uint32_t)(1 << distance);
        if ((coll->distance_mask_recv & distance_bit) != distance_bit) {
            OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:coll:recdub don't have information from distance %u, "
                                 "still waiting for data (mask recv: 0x%x)",
                                  ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance, coll->distance_mask_recv));
            break;
        } else {
            distance_prev = distance;
            distance = distance_prev << 1;

            orte_grpcomm_data_t *data;
            opal_list_item_t *item, *next_item;

            OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                                 "%s grpcomm:coll:recdub information from %u distance is already available, "
                                 "get them from list (mask recv: 0x%x)",
                                  ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance_prev, coll->distance_mask_recv));

            for(item = opal_list_get_first(&coll->buckets_recv);
                item != opal_list_get_end(&coll->buckets_recv);
                item = next_item ) {
                data = (orte_grpcomm_data_t*)item;
                next_item = opal_list_get_next(item);
                if (data->distance == distance_prev) {
                    OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                                         "%s grpcomm:coll:recdub %u distance data found",
                                          ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance_prev));
                    coll->nreported += data->nreported;;
                    /* capture any provided content */
                    if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(&coll->bucket, &data->bucket))) {
                        ORTE_ERROR_LOG(rc);
                        rcd_finalize_coll(coll, rc);
                        return;
                    }
                    opal_list_remove_item(&coll->buckets_recv, item);
                    OBJ_RELEASE(item);
                }
            }
        }
    }
    OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:recdub reported %lu process from %lu, sent distance %u",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (unsigned long)coll->nreported,
                         (unsigned long)coll->ndmns, coll->distance_send));

    /* if we are done, then complete things */
    if ((coll->nreported >= coll->ndmns) && ((uint32_t)(coll->distance_send << 1) >= coll->ndmns)){
        rcd_finalize_coll(coll, ORTE_SUCCESS);
    }
    return;
}

static void rcd_allgather_recv_dist(int status, orte_process_name_t* sender,
                                    opal_buffer_t* buffer, orte_rml_tag_t tag,
                                    void* cbdata)
{
    orte_grpcomm_data_t *item;
    uint32_t distance, nreported;
    int32_t cnt;
    int rc;
    orte_grpcomm_signature_t *sig;
    orte_grpcomm_coll_t *coll;
    uint32_t distance_bit;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:recdub RECEIVING FROM %s",
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
    distance = 0;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &distance, &cnt, OPAL_UINT32))) {
        OBJ_RELEASE(sig);
        ORTE_ERROR_LOG(rc);
        rcd_finalize_coll(coll, rc);
        return;
    }
    distance_bit = (uint32_t)(1 << distance);
    assert((coll->distance_mask_recv & distance_bit) != distance_bit);
    coll->distance_mask_recv |= (uint32_t)(1 << distance);

    /* unpack number of reported */
    nreported = 0;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &nreported, &cnt, OPAL_INT32))) {
        OBJ_RELEASE(sig);
        ORTE_ERROR_LOG(rc);
        rcd_finalize_coll(coll, rc);
        return;
    }

    /* Check whether we can process next distance */
    if (coll->distance_send != distance) {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                             "%s grpcomm:coll:recdub data from %d distance received, "
                             "still waiting for data (mask recv: 0x%x).",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance, coll->distance_mask_recv));
        item = OBJ_NEW(orte_grpcomm_data_t);
        item->distance = distance;
        item->nreported = nreported;
        /* capture any provided content */
        if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(&item->bucket, buffer))) {
            OBJ_RELEASE(sig);
            OBJ_RELEASE(item);
            ORTE_ERROR_LOG(rc);
            rcd_finalize_coll(coll, rc);
            return;
        }
        opal_list_append(&coll->buckets_recv, &item->super);
    } else {
        OPAL_OUTPUT_VERBOSE((80, orte_grpcomm_base_framework.framework_output,
                     "%s grpcomm:coll:recdub data from %d distance received, "
                     "Process the next distance (mask recv: 0x%x).",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), distance, coll->distance_mask_recv));
        coll->nreported += nreported;
        /* capture any provided content */
        if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(&coll->bucket, buffer))) {
            OBJ_RELEASE(sig);
            ORTE_ERROR_LOG(rc);
            rcd_finalize_coll(coll, rc);
            return;
        }
        rcd_allgather_process_data(coll);
    }

    OBJ_RELEASE(sig);

    return;
}

static int rcd_finalize_coll(orte_grpcomm_coll_t *coll, int ret) {
    opal_buffer_t *reply;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output,
                         "%s grpcomm:coll:recdub declared collective complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    reply = OBJ_NEW(opal_buffer_t);

    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &coll->nreported, 1, OPAL_UINT64))) {
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

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
#include <string.h>

#include "orte/orte_constants.h"
#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/dss/dss.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/soh/soh.h"
#include "orte/runtime/runtime.h"


/**
 *  A "broadcast-like" function over the specified set of peers.
 *  @param  root    The process acting as the root of the broadcast.
 *  @param  peers   The list of processes receiving the broadcast (excluding root).
 *  @param  buffer  The data to broadcast - only significant at root.
 *  @param  cbfunc  Callback function on receipt of data - not significant at root.
 *
 *  Note that the callback function is provided so that the data can be
 *  received and interpreted by the application prior to the broadcast
 *  continuing to forward data along the distribution tree.
 */


struct mca_oob_xcast_t {
    opal_object_t super;
    opal_mutex_t mutex;
    opal_condition_t cond;
    orte_std_cntr_t counter;
};
typedef struct mca_oob_xcast_t mca_oob_xcast_t;

static void mca_oob_xcast_construct(mca_oob_xcast_t* xcast)
{
    OBJ_CONSTRUCT(&xcast->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&xcast->cond, opal_condition_t);
}

static void mca_oob_xcast_destruct(mca_oob_xcast_t* xcast)
{
    OBJ_DESTRUCT(&xcast->mutex);
    OBJ_DESTRUCT(&xcast->cond);
}

static OBJ_CLASS_INSTANCE(
    mca_oob_xcast_t,
    opal_object_t,
    mca_oob_xcast_construct,
    mca_oob_xcast_destruct);

static void mca_oob_xcast_cb(int status, orte_process_name_t* peer, orte_buffer_t* buffer, int tag, void* cbdata)
{
    mca_oob_xcast_t* xcast = (mca_oob_xcast_t*)cbdata;
    OPAL_THREAD_LOCK(&xcast->mutex);
    if(--xcast->counter == 0) {
        opal_condition_signal(&xcast->cond);
    }
    OPAL_THREAD_UNLOCK(&xcast->mutex);
}


int mca_oob_xcast(
    orte_process_name_t* root,
    orte_process_name_t* peers,
    orte_std_cntr_t num_peers,
    orte_buffer_t* buffer,
    orte_gpr_trigger_cb_fn_t cbfunc)
{
    orte_std_cntr_t i;
    int rc;
    int tag = MCA_OOB_TAG_XCAST;
    int cmpval;
    int status;
    orte_proc_state_t state;

    /* check to see if I am the root process name */
    cmpval = orte_ns.compare(ORTE_NS_CMP_ALL, root, orte_process_info.my_name);
    if(NULL != root && 0 == cmpval) {
        mca_oob_xcast_t *xcast = OBJ_NEW(mca_oob_xcast_t);
        xcast->counter = num_peers;
        for(i=0; i<num_peers; i++) {
            /* check status of peer to ensure they are alive */
            if (ORTE_SUCCESS != (rc = orte_soh.get_proc_soh(&state, &status, peers+i))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (state != ORTE_PROC_STATE_TERMINATED && state != ORTE_PROC_STATE_ABORTED) {
                rc = mca_oob_send_packed_nb(peers+i, buffer, tag, 0, mca_oob_xcast_cb, xcast);
                if (rc < 0) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }

        /* wait for all non-blocking operations to complete */
        OPAL_THREAD_LOCK(&xcast->mutex);
        while(xcast->counter > 0) {
            opal_condition_wait(&xcast->cond, &xcast->mutex);
        }
        OPAL_THREAD_UNLOCK(&xcast->mutex);
        OBJ_RELEASE(xcast);

    } else {
        orte_buffer_t rbuf;
        orte_gpr_notify_message_t *msg;

        OBJ_CONSTRUCT(&rbuf, orte_buffer_t);
        rc = mca_oob_recv_packed(MCA_OOB_NAME_ANY, &rbuf, tag);
        if(rc < 0) {
            OBJ_DESTRUCT(&rbuf);
            return rc;
        }
        if (cbfunc != NULL) {
            msg = OBJ_NEW(orte_gpr_notify_message_t);
            if (NULL == msg) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            i=1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(&rbuf, &msg, &i, ORTE_GPR_NOTIFY_MSG))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(msg);
                return rc;
            }
            cbfunc(msg);
            OBJ_RELEASE(msg);
        }
        OBJ_DESTRUCT(&rbuf);
    }
    return ORTE_SUCCESS;
}



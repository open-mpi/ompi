/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *               2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "oob_ud_qp.h"
#include "oob_ud.h"

static void mca_oob_ud_qp_constructor (mca_oob_ud_qp_t *qp);
static void mca_oob_ud_qp_destructor  (mca_oob_ud_qp_t *qp);

OBJ_CLASS_INSTANCE(mca_oob_ud_qp_t, opal_free_list_item_t,
                   mca_oob_ud_qp_constructor,
                   mca_oob_ud_qp_destructor);

static inline int mca_oob_ud_qp_process_send_completions (mca_oob_ud_qp_t *qp,
                                                          int num_completions);

#define MCA_OOB_UD_CLEAR_CQ(cq)                                        \
    do {                                                               \
        if (NULL == (cq)->channel) {                                   \
            struct ibv_wc wc;                                          \
            while (ibv_poll_cq ((cq), 1, &wc));                        \
        }                                                              \
    } while (0);                                                       \

int mca_oob_ud_qp_init (mca_oob_ud_qp_t *qp, struct mca_oob_ud_port_t *port,
                        struct ibv_comp_channel *recv_channel,
                        struct ibv_comp_channel *send_channel, bool onecq)
{
    struct ibv_qp_init_attr init_attr;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:qp_init creating UD QP on port %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), port->port_num);

    /* create a UD queue pair */
    memset(&init_attr, 0, sizeof(init_attr));

    init_attr.qp_type = IBV_QPT_UD;

    qp->ib_recv_cq = ibv_create_cq (port->device->ib_context, 16384,
                                    port, recv_channel, 0);
    if (NULL == qp->ib_recv_cq) {
        opal_output(orte_oob_base_framework.framework_output,
                    "%s oob:ud:qp_init could not create recv completion queue. errno = %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }
    if (false == onecq) {
        qp->ib_send_cq = ibv_create_cq (port->device->ib_context, 16384,
                                        port, send_channel, 0);
        if (NULL == qp->ib_send_cq) {
            opal_output(orte_oob_base_framework.framework_output,
                        "%s oob:ud:qp_init could not create send completion queue. errno = %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
            return ORTE_ERROR;
        }
    } else {
        qp->ib_send_cq = qp->ib_recv_cq;
    }

    init_attr.send_cq = qp->ib_send_cq;
    init_attr.recv_cq = qp->ib_recv_cq;

    init_attr.cap.max_send_sge    = 32;
    init_attr.cap.max_recv_sge    = 32; /* GRH, data */
    init_attr.cap.max_inline_data = 0; /* don't use inline data for now */
    /* NTH: fix these */
    init_attr.cap.max_recv_wr     = 4096;
    init_attr.cap.max_send_wr     = 4096;

    qp->ib_qp = ibv_create_qp (port->device->ib_pd, &init_attr);
    if (NULL == qp->ib_qp) {
        opal_output_verbose(1, orte_oob_base_framework.framework_output,
                             "%s oob:ud:qp_init could not create queue pair. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }
    /* end: create the UD queue pair */

    qp->port = port;

    return ORTE_SUCCESS;
}

int mca_oob_ud_qp_to_reset (mca_oob_ud_qp_t *qp)
{
    struct ibv_qp_attr attr;

    /* move the QP into the ERR state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_ERR;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, IBV_QP_STATE)) {
        opal_output(0, "%s oob:ud:qp_to_reset error modifying qp to ERR. errno = %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    /* poll thread/event will clear failed work requests */
    MCA_OOB_UD_CLEAR_CQ(qp->ib_send_cq);
    MCA_OOB_UD_CLEAR_CQ(qp->ib_recv_cq);

    /* move the QP into the RESET state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_RESET;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, IBV_QP_STATE)) {
        opal_output(0, "%s oob:ud:qp_to_reset error modifying qp to RESET. errno = %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

int mca_oob_ud_qp_to_rts (mca_oob_ud_qp_t *qp)
{
    struct mca_oob_ud_port_t *port = qp->port;
    int attr_mask;
    struct ibv_qp_attr attr;

    /* move the QP into the INIT state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state   = IBV_QPS_INIT;
    attr.pkey_index = 0; /* NTH: might need to modify the pkey index later */
    attr.port_num   = port->port_num;
    attr.qkey       = 0;

    attr_mask = IBV_QP_STATE | IBV_QP_PKEY_INDEX | IBV_QP_PORT | IBV_QP_QKEY;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, attr_mask)) {
        opal_output(0, "%s oob:ud:qp_to_reset error modifying qp to INIT. errno = %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    /* Move QP to RTR */
    attr.qp_state = IBV_QPS_RTR;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, IBV_QP_STATE)) {
        opal_output(0, "%s oob:ud:qp_to_reset error modifying qp to RTR. errno = %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    /* Setup attributes */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_RTS;
    attr.sq_psn = 0;
    attr_mask = IBV_QP_STATE | IBV_QP_SQ_PSN;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, attr_mask)) {
        opal_output(0, "%s oob:ud:qp_to_reset error modifying qp to RTS. errno = %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

/* purge all work requests on a qp */
int mca_oob_ud_qp_purge (mca_oob_ud_qp_t *qp)
{
    int rc;

    rc = mca_oob_ud_qp_to_reset (qp);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    return mca_oob_ud_qp_to_rts (qp);
}

static void mca_oob_ud_qp_constructor (mca_oob_ud_qp_t *qp)
{
    memset ((char *)qp + sizeof(qp->super), 0, sizeof (*qp) - sizeof (qp->super));
}

static void mca_oob_ud_qp_destructor (mca_oob_ud_qp_t *qp)
{
    int rc;

    if (NULL != qp->ib_qp) {
        /* clear qp and move to reset */
        (void) mca_oob_ud_qp_to_reset (qp);

        /* destroy qp */
        rc = ibv_destroy_qp (qp->ib_qp);
        if (0 != rc) {
            opal_output (0, "IBV_DESTROY_QP FAILED! rc = %d, errno = %d", rc, errno);
        }
    }

    if (NULL != qp->ib_send_cq) {
        (void) ibv_destroy_cq (qp->ib_send_cq);
    }

    if (NULL != qp->ib_recv_cq && qp->ib_recv_cq != qp->ib_send_cq) {
        (void) ibv_destroy_cq (qp->ib_recv_cq);
    }
}

static inline int mca_oob_ud_qp_process_send_completions (mca_oob_ud_qp_t *qp,
                                                          int num_completions)
{
    struct ibv_wc wc[1];
    int count, rc, ret, i;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:qp_process_send_completions polling for %d completions",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         num_completions);

    rc = ORTE_SUCCESS;

    for (count = 0 ; count < num_completions ; ) {
        ret = ibv_poll_cq (qp->ib_send_cq, 1, wc);
        if (ret < 0) {
            opal_output (0, "%s oob:ud:qp_process_send_completions error polling for completions. "
                         "errno = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
            return ORTE_ERROR;
        }
        for (i = 0 ; i < ret ; ++i) {
            if (IBV_WC_SUCCESS != wc[i].status) {
                opal_output (0, "wc status = %d", wc[i].status);
                rc = ORTE_ERROR;
            }
        }
        count += ret;
    }

    return rc;
}

int mca_oob_ud_qp_post_send (mca_oob_ud_qp_t *qp, struct ibv_send_wr *wr,
                             int num_completions) {
    struct ibv_send_wr *bad_wr;
    int rc;

    rc = ibv_post_send (qp->ib_qp, wr, &bad_wr);
    if (0 != rc) {
        opal_output (0, "%s oob:ud:qp_post_send ibv_post_send failed. errno = %d",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }
    return mca_oob_ud_qp_process_send_completions (qp, num_completions);
}

int mca_oob_ud_qp_post_recv (mca_oob_ud_qp_t *qp, struct ibv_recv_wr *wr) {
    struct ibv_recv_wr *bad_wr;
    int rc;

    rc = ibv_post_recv (qp->ib_qp, wr, &bad_wr);
    if (0 != rc) {
        opal_output (0, "%s oob:ud:qp_post_recv failed. errno = %d",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

int mca_oob_ud_qp_data_aquire (struct mca_oob_ud_port_t *port, mca_oob_ud_qp_t **qp_ptr) {
    int rc;
    opal_free_list_item_t *item;

    do {
        item = opal_free_list_get_st (&port->data_qps);
        if (NULL == item) {
            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:qp_data_aquire error allocating new data qp",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            break;
        }

        *qp_ptr = (mca_oob_ud_qp_t *) item;

        if (NULL == (*qp_ptr)->ib_qp) {
            rc = mca_oob_ud_qp_init (*qp_ptr, port, NULL, NULL, true);
            if (ORTE_SUCCESS != rc) {
                break;
            }

            rc = mca_oob_ud_qp_to_rts (*qp_ptr);
        }
    } while (0);

    return rc;
}

int mca_oob_ud_qp_data_release (mca_oob_ud_qp_t *qp) {
    int rc;
    rc = mca_oob_ud_qp_purge (qp);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    opal_free_list_return_st (&qp->port->data_qps, &qp->super);

    return ORTE_SUCCESS;
}

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

#include "oob_ud_component.h"
#include "oob_ud_qp.h"
#include "oob_ud.h"
#include "orte/util/show_help.h"

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
    int max_cqe = min(port->device->attr.max_cqe, 16384);

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:qp_init creating UD QP on port %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), port->port_num);

    /* create a UD queue pair */
    memset(&init_attr, 0, sizeof(init_attr));

    init_attr.qp_type = IBV_QPT_UD;

    qp->ib_recv_cq = ibv_create_cq (port->device->ib_context, max_cqe,
                                    port, recv_channel, 0);
    if (NULL == qp->ib_recv_cq) {
        orte_show_help("help-oob-ud.txt", "create-cq-failed", true,
                       orte_process_info.nodename, max_cqe, strerror(errno));
        return ORTE_ERROR;
    }
    if (false == onecq) {
        qp->ib_send_cq = ibv_create_cq (port->device->ib_context, max_cqe,
                                        port, send_channel, 0);
        if (NULL == qp->ib_send_cq) {
            orte_show_help("help-oob-ud.txt", "create-cq-failed", true,
                       orte_process_info.nodename, max_cqe, strerror(errno));
            return ORTE_ERROR;
        }
    } else {
        qp->ib_send_cq = qp->ib_recv_cq;
    }

    init_attr.send_cq = qp->ib_send_cq;
    init_attr.recv_cq = qp->ib_recv_cq;

    mca_oob_ud_device_t *device = (mca_oob_ud_device_t *) opal_list_get_first (&mca_oob_ud_component.ud_devices);
    opal_output_verbose(80, orte_oob_base_framework.framework_output,
                        "%s oob:ud:qp_init create queue pair for device: device->attr.max_sge = %d, device->attr.max_qp_wr = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), device->attr.max_sge, device->attr.max_qp_wr);

    init_attr.cap.max_send_sge    = mca_oob_ud_component.ud_qp_max_send_sge;
    init_attr.cap.max_recv_sge    = mca_oob_ud_component.ud_qp_max_recv_sge; /* GRH, data */
    init_attr.cap.max_inline_data = mca_oob_ud_component.ud_qp_max_inline_data;
    init_attr.cap.max_recv_wr     = min(mca_oob_ud_component.ud_qp_max_recv_wr, device->attr.max_qp_wr);
    init_attr.cap.max_send_wr     = min(mca_oob_ud_component.ud_qp_max_send_wr, device->attr.max_qp_wr);

    qp->ib_qp = ibv_create_qp (port->device->ib_pd, &init_attr);
    if (NULL == qp->ib_qp) {
        orte_show_help("help-oob-ud.txt", "create-qp-failed", true,
                       orte_process_info.nodename, init_attr.cap.max_send_sge, init_attr.cap.max_recv_sge,
                       init_attr.cap.max_send_wr, init_attr.cap.max_recv_wr, init_attr.cap.max_inline_data,
                       strerror(errno));
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
        orte_show_help("help-oob-ud.txt", "modify-qp-failed", true,
                       orte_process_info.nodename, IBV_QP_STATE, strerror(errno));
        return ORTE_ERROR;
    }

    /* poll thread/event will clear failed work requests */
    MCA_OOB_UD_CLEAR_CQ(qp->ib_send_cq);
    MCA_OOB_UD_CLEAR_CQ(qp->ib_recv_cq);

    /* move the QP into the RESET state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_RESET;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, IBV_QP_STATE)) {
        orte_show_help("help-oob-ud.txt", "modify-qp-failed", true,
                       orte_process_info.nodename, IBV_QP_STATE, strerror(errno));
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
        orte_show_help("help-oob-ud.txt", "modify-qp-failed", true,
                       orte_process_info.nodename, attr_mask, strerror(errno));
        return ORTE_ERROR;
    }

    /* Move QP to RTR */
    attr.qp_state = IBV_QPS_RTR;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, IBV_QP_STATE)) {
        orte_show_help("help-oob-ud.txt", "modify-qp-failed", true,
                       orte_process_info.nodename, attr_mask, strerror(errno));
        return ORTE_ERROR;
    }

    /* Setup attributes */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_RTS;
    attr.sq_psn = 0;
    attr_mask = IBV_QP_STATE | IBV_QP_SQ_PSN;

    if (0 != ibv_modify_qp(qp->ib_qp, &attr, attr_mask)) {
        orte_show_help("help-oob-ud.txt", "modify-qp-failed", true,
                       orte_process_info.nodename, attr_mask, strerror(errno));
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
            orte_show_help("help-oob-ud.txt", "destroy-qp-failed", true,
                       orte_process_info.nodename, strerror(errno));
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
            orte_show_help("help-oob-ud.txt", "poll-cq-failed", true,
                       orte_process_info.nodename, 1, strerror(errno));
            return ORTE_ERROR;
        }
        for (i = 0 ; i < ret ; ++i) {
            if (IBV_WC_SUCCESS != wc[i].status) {
                orte_show_help("help-oob-ud.txt", "poll-cq-failed-wc", true,
                       orte_process_info.nodename, 1, i, wc[i].status);
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
        orte_show_help("help-oob-ud.txt", "post-send-failed", true,
                       orte_process_info.nodename, strerror(errno));
        return ORTE_ERROR;
    }
    return mca_oob_ud_qp_process_send_completions (qp, num_completions);
}

int mca_oob_ud_qp_post_recv (mca_oob_ud_qp_t *qp, struct ibv_recv_wr *wr) {

    struct ibv_recv_wr *bad_wr;
    int rc;

    rc = ibv_post_recv (qp->ib_qp, wr, &bad_wr);
    if (0 != rc) {
        orte_show_help("help-oob-ud.txt", "post-recv-failed", true,
                       orte_process_info.nodename, strerror(errno));
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

int mca_oob_ud_qp_data_aquire (struct mca_oob_ud_port_t *port, mca_oob_ud_qp_t **qp_ptr) {
    int rc = ORTE_SUCCESS;
    opal_free_list_item_t *item;

    do {
        item = opal_free_list_get_st (&port->data_qps);
        if (NULL == item) {
            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:qp_data_aquire error allocating new data qp. error = %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rc);
            rc = ORTE_ERR_TEMP_OUT_OF_RESOURCE;
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

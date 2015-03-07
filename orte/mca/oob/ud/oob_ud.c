/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *               2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/proc_info.h"

#include "orte/mca/routed/routed.h"

#include "oob_ud.h"
#include "oob_ud_send.h"

#define min(a,b) ((a) < (b) ? (a) : (b))

static int mca_oob_ud_module_init (void);
static void mca_oob_ud_module_fini (mca_oob_ud_peer_t **peer);
static int mca_oob_ud_set_addr (const orte_process_name_t *name, const char *uri);
static void mca_oob_ud_send_nb(orte_rml_send_t *msg);
static void mca_oob_ud_ping(const orte_process_name_t *proc);

mca_oob_ud_module_t mca_oob_ud_module = {
    {
        mca_oob_ud_module_init,
        mca_oob_ud_module_fini,

        mca_oob_ud_set_addr,

        mca_oob_ud_ping,

        mca_oob_ud_send_nb
    }
};

static void mca_oob_ud_send_nb(orte_rml_send_t *msg) {
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s oob:ud:send_nb to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&msg->dst));

    /* push this into our event base for processing */
    ORTE_ACTIVATE_UD_POST_SEND(msg, mca_oob_ud_process_send_nb);
}

static void mca_oob_ud_ping(const orte_process_name_t *proc) {
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s oob:ud:ping proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(proc));

    /* push this into our event base for processing */
    ORTE_ACTIVATE_UD_PING(proc, mca_oob_ud_process_ping);
}

/* uri must be at least 27 bytes in size */
void mca_oob_ud_port_get_uri (mca_oob_ud_port_t *port, char *uri)
{
    sprintf (uri, "ud://%u.%u.%u", port->listen_qp.ib_qp->qp_num,
             port->lid, port->port_num);
}

static int mca_oob_ud_set_addr (const orte_process_name_t *name, const char *uri)
{
    mca_oob_ud_peer_t *peer = NULL;
    int rc;

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:set_addr: setting location for peer %s from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(name), uri);

    (void) mca_oob_ud_peer_lookup (name, &peer);

    if (NULL == uri) {
        if (NULL != peer) {
            mca_oob_ud_peer_release (peer);
        }

        peer = NULL;
    } else if (NULL == peer) {
        peer = mca_oob_ud_peer_from_uri (uri);
        if (NULL == peer) {
            return ORTE_ERR_BAD_PARAM;
        }
    } else {
        rc = mca_oob_ud_peer_update_with_uri (peer, uri);

        if (ORTE_SUCCESS != rc) {
            return rc;
        }
    }

    if (NULL != peer) {
        peer->peer_name = *name;
        peer->needs_notification = true;
    }

    opal_proc_table_set_value(&mca_oob_ud_module.peers,
                              *name, (void *)peer);

    return ORTE_SUCCESS;
}

int mca_oob_ud_port_post_one_recv (mca_oob_ud_port_t *port, int msg_num)
{
    char *grh_buf = port->grh_buf.ptr + msg_num * sizeof (struct ibv_grh);
    char *msg_buf = port->msg_buf.ptr + msg_num * port->mtu;
    struct ibv_recv_wr wr;
    struct ibv_sge sge[2];

    /* GRH */
    mca_oob_ud_fill_sge(sge, grh_buf, sizeof (struct ibv_grh), port->grh_buf.mr->lkey);

    /* message */
    mca_oob_ud_fill_sge(sge + 1, msg_buf, port->mtu, port->msg_buf.mr->lkey);

    mca_oob_ud_fill_recv_wr (&wr, sge, 2);
    wr.wr_id   = MCA_OOB_UD_RECV_WR | (uint64_t)msg_num;

    return mca_oob_ud_qp_post_recv (&port->listen_qp, &wr);
}

static bool module_has_been_inited = false;

static int mca_oob_ud_module_init (void)
{
    /* protect against repeat inits */
    if (module_has_been_inited) {
        return ORTE_SUCCESS;
    }
    module_has_been_inited = true;

    OBJ_CONSTRUCT(&mca_oob_ud_module.peers, opal_proc_table_t);
    opal_proc_table_init (&mca_oob_ud_module.peers, 16, 1024);

    return ORTE_SUCCESS;
}

static void mca_oob_ud_module_fini (mca_oob_ud_peer_t **peer)
{
    opal_process_name_t key;
    void *node1, *node2;
    int rc;

    rc = opal_proc_table_get_first_key (&mca_oob_ud_module.peers, &key,
                                        (void **) peer, &node1, &node2);
    if (OPAL_SUCCESS == rc) {
        do {
            if (NULL != *peer) {
                mca_oob_ud_peer_release (*peer);
            }
            rc = opal_proc_table_get_next_key (&mca_oob_ud_module.peers, &key,
                                               (void **) peer, node1, &node1, node2, &node2);
        } while (OPAL_SUCCESS == rc);
    }

    opal_proc_table_remove_all(&mca_oob_ud_module.peers);

    OBJ_DESTRUCT(&mca_oob_ud_module.peers);

    return;
}

int mca_oob_ud_register_iov (struct iovec *iov, int count, struct ibv_mr **ib_mr,
                             struct ibv_pd *ib_pd, unsigned int mtu, int *sge_countp,
                             int *wr_countp, int *data_lenp)
{
    int data_len, iov_index, sge_count;
    unsigned int packet_size = 0;

    opal_output_verbose (80, orte_oob_base_framework.framework_output,
                         "%s oob:ud:register_iov registering memory", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    *wr_countp  = 0;
    *data_lenp  = 0;
    *sge_countp = 0;

    for (iov_index = 0, data_len = 0, sge_count = 0 ; iov_index < count ; ++iov_index) {
        unsigned int iov_left = iov[iov_index].iov_len;

        data_len += iov_left;

        sge_count++;

        do {
            unsigned int to_trans = min (iov_left, mtu - packet_size);

            packet_size = (to_trans < iov_left) ? 0 : packet_size + to_trans;
            iov_left    -= to_trans;

            if (0 == packet_size && iov_left) {
                sge_count++;
            }
        } while (iov_left);

        /* register buffers */
        if (NULL == ib_mr[iov_index]) {
            ib_mr[iov_index] = ibv_reg_mr (ib_pd,
                                           iov[iov_index].iov_base,
                                           iov[iov_index].iov_len,
                                           IBV_ACCESS_LOCAL_WRITE |
                                           IBV_ACCESS_REMOTE_WRITE);
            if (NULL == ib_mr[iov_index]) {
                /* Ruh-roh */
                opal_output (0, "%s oob:ud:register_iov error registering memory. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    *wr_countp  = (data_len + mtu - 1) / mtu;
    *sge_countp = sge_count;
    *data_lenp  = data_len;

    return ORTE_SUCCESS;
}

int mca_oob_ud_register_buf (char *buf, int size, struct ibv_mr **ib_mr_buf,
                             struct ibv_pd *ib_pd, unsigned int mtu, int *sge_countp, int *wr_countp)
{
    int sge_count = 0;
    unsigned int packet_size = 0;

    opal_output_verbose (80, orte_oob_base_framework.framework_output,
                         "%s oob:ud:mca_oob_ud_register_buf registering memory", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    *wr_countp  = 0;
    *sge_countp = 0;

    unsigned int iov_left = size;

    sge_count++;

    do {
        unsigned int to_trans = min (iov_left, mtu - packet_size);

        packet_size = (to_trans < iov_left) ? 0 : packet_size + to_trans;
        iov_left    -= to_trans;

        if (0 == packet_size && iov_left) {
            sge_count++;
        }
    } while (iov_left);

    /* register buffers */
    if (NULL == *ib_mr_buf) {
        *ib_mr_buf = ibv_reg_mr (ib_pd, buf, size,
                                IBV_ACCESS_LOCAL_WRITE |
                                IBV_ACCESS_REMOTE_WRITE);
        if (NULL == *ib_mr_buf) {
            opal_output (0, "%s oob:ud:mca_oob_ud_register_buf error registering memory. errno = %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }

    *wr_countp  = (size + mtu - 1) / mtu;
    *sge_countp = sge_count;

    return ORTE_SUCCESS;
}

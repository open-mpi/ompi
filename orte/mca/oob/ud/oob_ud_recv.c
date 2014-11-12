/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
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

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "math.h"

#include "oob_ud_component.h"

#define min(a,b) ((a) < (b) ? (a) : (b))

/* Caller MUST hold the matching lock before calling */
static inline int mca_oob_ud_find_recv (opal_list_t *list, const orte_process_name_t name,
                                        const int tag, mca_oob_ud_req_t **req)
{
    opal_list_item_t *item;
    int rc = ORTE_ERR_NOT_FOUND;

    *req = NULL;

    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_match_lock);

    for (item = opal_list_get_first (list) ; item != opal_list_get_end (list) ;
         item = opal_list_get_next (item)) {
        mca_oob_ud_req_t *recv_req = (mca_oob_ud_req_t *) item;

        opal_output_verbose(15, orte_oob_base_framework.framework_output,
                             "%s oob:ud:find_recv matching against "
                             "peer: %s, tag: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&recv_req->req_origin), recv_req->req_tag);

        if (OPAL_EQUAL == opal_dss.compare (&name, &recv_req->req_origin, ORTE_NAME) &&
            tag == recv_req->req_tag) {
            *req = recv_req;
            rc = ORTE_SUCCESS;
            break;
        }
    }

    opal_output_verbose(15, orte_oob_base_framework.framework_output,
                         "%s oob:ud:find_recv %sfound",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_SUCCESS != rc ? "not " : "");


    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_match_lock);

    if (ORTE_SUCCESS == rc) {
        mca_oob_ud_req_append_to_list (*req, NULL);
    }

    return rc;
}

int mca_oob_ud_get_recv_req (const orte_process_name_t name, const int tag,
                             mca_oob_ud_req_t **reqp, bool iovec_used) {
    mca_oob_ud_req_t *req;

    opal_output_verbose(15, orte_oob_base_framework.framework_output,
                         "%s oob:ud:get_recv_req create receive request against: %s, tag: %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), tag);

    *reqp = req = OBJ_NEW(mca_oob_ud_req_t);

    req->req_origin       = name;
    req->req_tag          = tag;

    /* this receive was not expected */
    req->type             = MCA_OOB_UD_REQ_RECV;

    /* let mca_oob_ud_recv_alloc alloc memory for the receive */
    if (iovec_used) {
        req->req_data.iov.uiov   = calloc (1, sizeof (struct iovec));
        req->req_data_type = MCA_OOB_UD_REQ_IOV;
    } else {
        req->req_data_type = MCA_OOB_UD_REQ_BUF;
    }
    req->req_data.iov.count  = 1;

    return ORTE_SUCCESS;
}

static inline int mca_oob_ud_find_active_recv (const orte_process_name_t name, const int tag,
                                               mca_oob_ud_req_t **req) {
    opal_output_verbose(15, orte_oob_base_framework.framework_output,
                         "%s oob:ud:recv_match active receive request "
                         "against: %s, tag: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), tag);

    return mca_oob_ud_find_recv (&mca_oob_ud_component.ud_active_recvs, name, tag, req);
}

static void mca_oob_ud_recv_try_to (int fd, short event, void *data)
{
    (void) mca_oob_ud_recv_try ((mca_oob_ud_req_t *) data);
}

int mca_oob_ud_recv_try (mca_oob_ud_req_t *recv_req)
{
    int rc, data_len;
    int wr_count, sge_count, wr_index, sge_index, iov_index;
    unsigned int iov_left, iov_offset, packet_size;
    const unsigned int mtu = recv_req->req_mtu;
    struct timeval aquire_timeout = {0, 500000};
    mca_oob_ud_msg_t *rep_msg = NULL;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:recv_try receiving from %s. recv_req = %p. rem ctx = %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&recv_req->req_peer->peer_name),
                         (void *)recv_req, (void *)recv_req->req_rem_ctx);

    do {
        if (NULL == recv_req->req_qp) {
            rc = mca_oob_ud_qp_data_aquire (recv_req->req_port, &recv_req->req_qp);
            if (ORTE_SUCCESS != rc) {
                break;
            }
        }

        (void) mca_oob_ud_qp_purge (recv_req->req_qp);

        rc = mca_oob_ud_msg_get (recv_req->req_port, recv_req, &recv_req->req_port->listen_qp,
                                 recv_req->req_peer, NULL, &rep_msg);
        if (ORTE_SUCCESS != rc) {
            break;
        }

        if (MCA_OOB_UD_REQ_IOV == recv_req->req_data_type) {
            if (NULL == recv_req->req_data.iov.mr) {
                /* allocate space for memory registers */
                recv_req->req_data.iov.mr = (struct ibv_mr **) calloc (recv_req->req_data.iov.count, sizeof (struct ibv_mr *));
                if (NULL == recv_req->req_data.iov.mr) {
                    opal_output (0, "%s oob:ud:recv_try error allocating space for memory registers. errno = %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                    rc = ORTE_ERR_OUT_OF_RESOURCE;
                    break;
                }
            }

            rc = mca_oob_ud_register_iov (recv_req->req_data.iov.uiov, recv_req->req_data.iov.count,
                                          recv_req->req_data.iov.mr, recv_req->req_port->device->ib_pd,
                                          mtu, &sge_count, &wr_count, &data_len);

            if (ORTE_SUCCESS != rc) {
                break;
            }
        } else {
            data_len = recv_req->req_data.buf.size;
            rc = mca_oob_ud_register_buf (recv_req->req_data.buf.p, recv_req->req_data.buf.size,
                                          &recv_req->req_data.buf.mr, recv_req->req_port->device->ib_pd,
                                          mtu, &sge_count, &wr_count);

            if (ORTE_SUCCESS != rc) {
                break;
            }
        }

        data_len = min(data_len, recv_req->req_rem_data_len);
        if (data_len < recv_req->req_rem_data_len) {
            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                     "%s oob:ud:recv_try receive buffers are not big. this is probably an error condition."
                     "data_len = %d, recv_req->req_rem_data_len = %d.",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), data_len, recv_req->req_rem_data_len);
            rc = ORTE_ERR_BAD_PARAM;
            break;
        }

        wr_count = (data_len + mtu - 1) / mtu;
        sge_count += wr_count;

        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                             "%s oob:ud:recv_try receiving %d bytes in %d "
                             "work requests, %d sges", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), data_len,
                             wr_count, sge_count);

        recv_req->req_packet_count = wr_count;

        if (NULL == recv_req->req_wr.recv) {
            /* allocate work requests */
            recv_req->req_wr.recv  = (struct ibv_recv_wr *) calloc (wr_count, sizeof (struct ibv_recv_wr));
            if (NULL == recv_req->req_wr.recv) {
                opal_output (0, "%s oob:ud:recv_try error allocating work requests. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                break;
            }
        }

        if (NULL == recv_req->req_sge) {
            /* allocate scatter-gather lists. we need more to hold the grh */
            recv_req->req_sge = (struct ibv_sge *) calloc (sge_count, sizeof (struct ibv_sge));
            if (NULL == recv_req->req_sge) {
                opal_output (0, "%s oob:ud:recv_try error allocating sges. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                break;
            }
        }

        if (NULL == recv_req->req_grh) {
            /* allocate grh buffers */
            recv_req->req_grh = (struct ibv_grh *) calloc (wr_count, sizeof (struct ibv_grh));
            if (NULL == recv_req->req_grh) {
                opal_output (0, "%s oob:ud:recv_try error allocating space for GRHs. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                break;
            }
        }

        if (NULL == recv_req->req_grh_mr) {
            /* register grh buffers */
            recv_req->req_grh_mr = ibv_reg_mr (recv_req->req_port->device->ib_pd, recv_req->req_grh,
                                               wr_count * sizeof (struct ibv_grh),
                                               IBV_ACCESS_LOCAL_WRITE);
            if (NULL == recv_req->req_grh_mr) {
                opal_output (0, "%s oob:ud:recv_try error allocating registering GRH memory. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                /* could not register memory */
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                break;
            }
        }

        rc = ORTE_SUCCESS;

        if (MCA_OOB_UD_REQ_IOV == recv_req->req_data_type) {
            iov_left   = recv_req->req_data.iov.uiov[0].iov_len;
            iov_offset = 0;
            iov_index  = 0;

            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud: recv_req->req_data.iov.uiov[0].iov_len = %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)recv_req->req_data.iov.uiov[0].iov_len);

            for (wr_index = 0, sge_index = 0 ; wr_index < wr_count ; ++wr_index) {
                int sge_first = sge_index;

                packet_size = 0;

                /* grh */
                mca_oob_ud_fill_sge(recv_req->req_sge + sge_index++,
                                    recv_req->req_grh + wr_index,
                                    sizeof (struct ibv_grh),
                                    recv_req->req_grh_mr->lkey);

                do {
                    int to_recv = min (iov_left, mtu - packet_size);

                    mca_oob_ud_fill_sge(recv_req->req_sge + sge_index++,
                                        (char *)recv_req->req_data.iov.uiov[iov_index].iov_base + iov_offset,
                                        to_recv, recv_req->req_data.iov.mr[iov_index]->lkey);

                    iov_offset += to_recv;
                    iov_left   -= to_recv;
                    packet_size += to_recv;

                    if (0 == iov_left) {
                        iov_index++;
                        iov_offset = 0;

                        if (iov_index < recv_req->req_data.iov.count) {
                            iov_left = recv_req->req_data.iov.uiov[iov_index].iov_len;
                        }
                    }
                } while ((packet_size < mtu) && (iov_left > 0));

                mca_oob_ud_fill_recv_wr(recv_req->req_wr.recv + wr_index,
                        recv_req->req_sge + sge_first,
                        sge_index - sge_first);

                if (wr_index + 1 < wr_count) {
                    recv_req->req_wr.recv[wr_index].next = recv_req->req_wr.recv + wr_index + 1;
                }
            }
        } else {
            unsigned int buffer_left   = recv_req->req_data.buf.size;
            unsigned int buffer_offset = 0;

            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:recv_try recv_req->req_data.buf.size = %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), recv_req->req_data.buf.size);

            for (wr_index = 0, sge_index = 0 ; wr_index < wr_count ; ++wr_index) {
                int sge_first = sge_index;

                packet_size = 0;

                /* grh */
                mca_oob_ud_fill_sge(recv_req->req_sge + sge_index++,
                                    recv_req->req_grh + wr_index,
                                    sizeof (struct ibv_grh),
                                    recv_req->req_grh_mr->lkey);

                do {
                    int to_recv = min (buffer_left, mtu - packet_size);

                    mca_oob_ud_fill_sge(recv_req->req_sge + sge_index++,
                                        (char *)recv_req->req_data.buf.p + buffer_offset,
                                        to_recv, recv_req->req_data.buf.mr->lkey);

                    buffer_offset += to_recv;
                    buffer_left   -= to_recv;
                    packet_size += to_recv;
                } while ((packet_size < mtu) && (buffer_left > 0));

                mca_oob_ud_fill_recv_wr(recv_req->req_wr.recv + wr_index,
                    recv_req->req_sge + sge_first,
                    sge_index - sge_first);

                if (wr_index + 1 < wr_count) {
                    recv_req->req_wr.recv[wr_index].next = recv_req->req_wr.recv + wr_index + 1;
                }
            }
        }

        rc = mca_oob_ud_qp_post_recv (recv_req->req_qp, recv_req->req_wr.recv);
        if (ORTE_SUCCESS != rc) {
            break;
        }

        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                             "%s oob:ud:recv_try posting reply message",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

        /* ok, we have a data queue pair */
        rep_msg->hdr->msg_type = MCA_OOB_UD_MSG_REPLY;
        rep_msg->hdr->msg_lcl_ctx = recv_req->req_rem_ctx;
        rep_msg->hdr->msg_rem_ctx = recv_req;

        rep_msg->hdr->msg_data.rep.qpn = recv_req->req_qp->ib_qp->qp_num;
        rep_msg->hdr->msg_data.rep.data_len = data_len;
        rep_msg->hdr->msg_data.rep.mtu = mtu;

        rc = mca_oob_ud_msg_post_send (rep_msg);

        /* post send already returned the message */
        rep_msg = NULL;
    } while (0);

    if (ORTE_ERR_TEMP_OUT_OF_RESOURCE == rc) {
        mca_oob_ud_req_timer_set (recv_req, &aquire_timeout, 1, mca_oob_ud_recv_try_to);
        rc = ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != rc) {
        /* bad stuff happened */
        mca_oob_ud_req_complete (recv_req, rc);

        if (mca_oob_ud_req_is_in_list(recv_req, &mca_oob_ud_component.ud_active_recvs)) {
            opal_list_remove_item (&mca_oob_ud_component.ud_active_recvs, (opal_list_item_t *) recv_req);
        }
        OBJ_RELEASE(recv_req);
        return rc;
    }

    recv_req->state = MCA_OOB_UD_REQ_ACTIVE;

    return rc;
}

int mca_oob_ud_recv_complete (mca_oob_ud_req_t *recv_req)
{
    mca_oob_ud_msg_t *dataok;
    int i, j, rc = ORTE_SUCCESS;
    uint32_t expected;
    bool error = false, out_of_order = false;
#if defined(HAVE_VALGRIND)
    int iov_index;
#endif

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:recv_complete req = %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) recv_req);

    if (false == recv_req->req_is_eager) {
        for (i = 0, expected = 0 ; i < recv_req->req_packet_count ; ) {
            struct ibv_wc wc[10];

            rc = ibv_poll_cq (recv_req->req_qp->ib_recv_cq, 10, wc);
            for (j = 0 ; j < rc ; ++j) {
                if (wc[j].imm_data != expected) {
                    out_of_order = true;
                }
                if (IBV_WC_SUCCESS != wc[j].status) {
                    error = true;
                }

                opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                     "%s oob:ud:recv_complete wc status = %d. imm data = %u. len = %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wc[j].status, wc[j].imm_data,
                                     wc[j].byte_len);

                expected++;
            }

            if (rc <= 0) {
                break;
            }

            i += rc;
        }

        if (i != recv_req->req_packet_count || error || out_of_order) {
            /* retry */
            recv_req->state = MCA_OOB_UD_REQ_PENDING;

            opal_output_verbose(5, orte_oob_base_framework.framework_output,
                                 "%s oob:ud:recv_complete receive incomplete. error: %d, "
                                 "out_of_order: %d packets: %d/%d. rc = %d, errno = %d.",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), error, out_of_order, i,
                                 recv_req->req_packet_count, rc, errno);
            mca_oob_ud_recv_try (recv_req);

            return ORTE_SUCCESS;
        }

        opal_output_verbose(10, orte_oob_base_framework.framework_output,
                             "%s oob:ud:recv_complete data received ok!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

        /* send data ok and wait for ack */
        rc = mca_oob_ud_msg_get (recv_req->req_port, recv_req, &recv_req->req_port->listen_qp,
                                 recv_req->req_peer, false, &dataok);
        if (ORTE_SUCCESS != rc) {
            return rc;
        }

        dataok->hdr->msg_type = MCA_OOB_UD_MSG_DATA_OK;
        dataok->hdr->msg_lcl_ctx = recv_req->req_rem_ctx;

        rc = mca_oob_ud_msg_post_send (dataok);
        if (ORTE_SUCCESS != rc) {
            return rc;
        }
    }

#if defined(HAVE_VALGRIND)
    for (iov_index = 0 ; iov_index < recv_req->req_count ; ++iov_index) {
        VALGRIND_MAKE_MEM_DEFINED(recv_req->req_uiov[iov_index].iov_base,
                                  recv_req->req_uiov[iov_index].iov_len);
    }
#endif

    mca_oob_ud_req_complete (recv_req, rc);

    return ORTE_SUCCESS;
}

int mca_oob_ud_recv_match_send (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr,
                                mca_oob_ud_req_t **reqp)
{
    char *data = (msg_hdr->msg_data.req.data_follows ? (char *)(msg_hdr + 1) : NULL);
    mca_oob_ud_req_t *req;
    int rc, i;

    *reqp = NULL;

    opal_output_verbose(10, orte_oob_base_framework.framework_output,
                         "%s oob:ud:recv_incoming_send matching incoming "
                         "send from peer %s with tag %d (data_follows = %d, data = %p, iovec_use = %d)", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&msg_hdr->msg_origin), msg_hdr->msg_data.req.tag,
                         msg_hdr->msg_data.req.data_follows, (void *)data, msg_hdr->msg_data.req.data_iovec_used);

    rc = mca_oob_ud_get_recv_req (msg_hdr->msg_origin, msg_hdr->msg_data.req.tag, &req,  msg_hdr->msg_data.req.data_iovec_used);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "%s oob:ud:recv_start mca_oob_ud_get_recv_req failed %d",
             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rc);
        return rc;
    }

    req->req_rem_ctx      = msg_hdr->msg_rem_ctx;
    req->req_port         = port;
    req->req_mtu          = min(port->mtu, msg_hdr->msg_data.req.mtu);
    req->req_origin       = msg_hdr->msg_origin;
    req->req_target       = msg_hdr->msg_target;
    req->req_rem_data_len = msg_hdr->msg_data.req.data_len;

    do {
        rc = mca_oob_ud_recv_alloc (req);
        if (ORTE_SUCCESS != rc) {
            opal_output (0, "%s oob:ud:recv_start malloc failed!", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            free (req->req_data.iov.uiov);
            OBJ_RELEASE(req);
            req = NULL;
            break;
        }
        req->req_peer = peer;
        OBJ_RETAIN(req->req_peer);

        if (NULL == data) {
            req->state = MCA_OOB_UD_REQ_ACTIVE;
            opal_output_verbose(10, orte_oob_base_framework.framework_output,
                                "%s oob:ud:recv_incoming_send request still active",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            break;
        }

       opal_output_verbose(10, orte_oob_base_framework.framework_output,
                             "%s oob:ud:recv_incoming_send send was eager",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

        req->req_is_eager = true;

        if (msg_hdr->msg_data.req.data_iovec_used) {
            for (i = 0 ; i < req->req_data.iov.count; ++i) {
                memcpy (req->req_data.iov.uiov[i].iov_base, data, req->req_data.iov.uiov[i].iov_len);
                data += req->req_data.iov.uiov[i].iov_len;
            }
        } else {
            memcpy(req->req_data.buf.p, data, msg_hdr->msg_data.req.data_len);
        }

        req->state = MCA_OOB_UD_REQ_COMPLETE;
    } while (0);

    *reqp = req;

    return rc;
}

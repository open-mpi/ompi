/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
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

#include "oob_ud.h"

#define min(a,b) ((a) < (b) ? (a) : (b))

static int mca_oob_ud_recv_unex_complete (mca_oob_ud_req_t *req);

static int mca_oob_ud_recv_copy (mca_oob_ud_req_t *dst, mca_oob_ud_req_t *src)
{
    int rc, i;

    dst->req_rem_data_len = src->req_rem_data_len;

    rc = mca_oob_ud_recv_alloc (dst);

    if (ORTE_SUCCESS == rc) {
        unsigned char *dptr = src->req_uiov[0].iov_base;

        for (i = 0 ; i < dst->req_count ; ++i) {
            memcpy (dst->req_uiov[i].iov_base, dptr, dst->req_uiov[i].iov_len);
            dptr += dst->req_uiov[i].iov_len;
        }
    }

    mca_oob_ud_req_complete (dst, (ORTE_SUCCESS == rc) ? dst->req_rem_data_len : rc);

    /* free io vector data */
    free (src->req_uiov[0].iov_base);
    free (src->req_uiov);
    src->req_uiov = NULL;

    OBJ_RELEASE(src);

    return rc;
}

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

        OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:find_recv matching against "
                             "peer: %s, tag: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&recv_req->req_origin), recv_req->req_tag));

        if (OPAL_EQUAL == opal_dss.compare (&name, &recv_req->req_origin, ORTE_NAME) &&
            tag == recv_req->req_tag) {
            *req = recv_req;
            rc = ORTE_SUCCESS;
            break;
        }
    }

    OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:find_recv %sfound",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_SUCCESS != rc ? "not " : ""));


    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_match_lock);

    if (ORTE_SUCCESS == rc) {
        mca_oob_ud_req_append_to_list (*req, NULL);
    }

    return rc;
}

static int mca_oob_ud_find_pending_recv (const orte_process_name_t name, const int tag,
                                         mca_oob_ud_req_t **reqp) {
    return mca_oob_ud_find_recv (&mca_oob_ud_component.ud_pending_recvs, name, tag, reqp);
}

int mca_oob_ud_get_recv_req (const orte_process_name_t name, const int tag,
                             mca_oob_ud_req_t **reqp) {
    mca_oob_ud_req_t *req;
    int rc;

    OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:get_recv_req pending receive request "
                         "against: %s, tag: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), tag));

    rc = mca_oob_ud_find_recv (&mca_oob_ud_component.ud_pending_recvs, name, tag, reqp);
    if (ORTE_SUCCESS != rc) {
        *reqp = req = OBJ_NEW(mca_oob_ud_req_t);

        OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:get_recv_req no matching receive. "
                             "created unexpected recv %p for tag %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (void *) req, tag));

        req->req_origin       = name;
        req->req_tag          = tag;

        /* this receive was not expected */
        req->type             = MCA_OOB_UD_REQ_UNEX;

        /* let mca_oob_ud_recv_alloc alloc memory for the receive */
        req->req_uiov   = calloc (1, sizeof (struct iovec));
        req->req_flags  = ORTE_RML_ALLOC;

        req->req_count  = 1;
    } else {
        OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:get_recv_req recv %p matched",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) *reqp));
    }

    return ORTE_SUCCESS;
}

static inline int mca_oob_ud_find_active_recv (const orte_process_name_t name, const int tag,
                                               mca_oob_ud_req_t **req) {
    OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:recv_match active receive request "
                         "against: %s, tag: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), tag));

    return mca_oob_ud_find_recv (&mca_oob_ud_component.ud_active_recvs, name, tag, req);
}

static inline int mca_oob_ud_find_unexpected_recv (const orte_process_name_t name, const int tag,
                                                   mca_oob_ud_req_t **req) {
    OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:recv_match unexpected receive request "
                         "against: %s, tag: %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&name), tag));

    return mca_oob_ud_find_recv (&mca_oob_ud_component.ud_unexpected_recvs, name, tag, req);
}


int mca_oob_ud_recv_match (mca_oob_ud_req_t *recv_req) {
    mca_oob_ud_req_t *urecv;
    int rc;

    OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:recv_match posting receive. req = %p ",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) recv_req));

    rc = mca_oob_ud_find_unexpected_recv (recv_req->req_origin, recv_req->req_tag, &urecv);

    OPAL_OUTPUT_VERBOSE((15, mca_oob_base_output, "%s oob:ud:recv_match posting receive. found = %p ",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) urecv));

    if (ORTE_SUCCESS == rc) {
        recv_req->state = MCA_OOB_UD_REQ_COMPLETE;
        return mca_oob_ud_recv_copy (recv_req, urecv);
    }

    recv_req->state      = MCA_OOB_UD_REQ_PENDING;
    mca_oob_ud_req_append_to_list (recv_req, &mca_oob_ud_component.ud_pending_recvs);

    return ORTE_SUCCESS;
}

/*
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User supplied tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error.
 */
int mca_oob_ud_recv_nb(
    orte_process_name_t* peer, 
    struct iovec* iov, 
    int count,
    int tag,
    int flags, 
    orte_rml_callback_fn_t cbfunc,
    void* cbdata)
{
    mca_oob_ud_req_t *recv_req;

    OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:recv_nb posting recieve. peer = %s, "
                         "tag = %d, count = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag, count));

    /* validate params */
    if(NULL == iov || 0 == count) {
        return ORTE_ERR_BAD_PARAM;
    }

    recv_req = OBJ_NEW(mca_oob_ud_req_t);
    if (NULL == recv_req) {
        opal_output(0, "oob:ud:recv_nb malloc failed! errno = %d", errno);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    recv_req->req_origin = *peer;
    recv_req->req_uiov   = iov;
    recv_req->req_count  = count;
    recv_req->req_tag    = tag;
    recv_req->req_flags  = flags;
    recv_req->req_cbfunc = cbfunc;
    recv_req->req_cbdata = cbdata;
    recv_req->req_rc     = 0;
    recv_req->req_peer   = NULL;

    recv_req->type       = MCA_OOB_UD_REQ_RECV;

    return mca_oob_ud_recv_match (recv_req);
}

int mca_oob_ud_recv_cancel(orte_process_name_t *name, int tag)
{
    mca_oob_ud_req_t *recv_req;
    bool matched = false;

    OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:recv_cancel canceling receive requests "
                         "with name = %s, tag = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(name), tag));

    /* cancel pending receives */
    while (ORTE_SUCCESS == mca_oob_ud_find_pending_recv (*name, tag, &recv_req)) {
        mca_oob_ud_req_abort (recv_req);
        matched = true;
    }

    /* cancel active receives */
    while (ORTE_SUCCESS == mca_oob_ud_find_active_recv (*name, tag, &recv_req)) {
        mca_oob_ud_req_abort (recv_req);
        matched = true;
    }

    return matched ? ORTE_SUCCESS : ORTE_ERR_NOT_FOUND;
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
 
    OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:recv_try receiving from %s. rem ctx = %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&recv_req->req_peer->peer_name),
                         recv_req->req_rem_ctx));

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

        if (NULL == recv_req->req_mr) {
            /* allocate space for memory registers */
            recv_req->req_mr = (struct ibv_mr **) calloc (recv_req->req_count, sizeof (struct ibv_mr *));
            if (NULL == recv_req->req_mr) {
                opal_output (0, "%s oob:ud:recv_try error allocating space for memory registers. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                break;
            }
        }

        rc = mca_oob_ud_register_iov (recv_req->req_uiov, recv_req->req_count,
                                      recv_req->req_mr, recv_req->req_port->device->ib_pd,
                                      mtu, &sge_count, &wr_count, &data_len);
        if (ORTE_SUCCESS != rc) {
            break;
        }

        data_len = min(data_len, recv_req->req_rem_data_len);
        if (data_len < recv_req->req_rem_data_len && !(recv_req->req_flags & ORTE_RML_TRUNC)) {
            /* receive buffers are not big enough and ORTE_RML_TRUNC was not specified.
               this is probably an error condition */
            rc = ORTE_ERR_BAD_PARAM;
            break;
        }

        wr_count = (data_len + mtu - 1) / mtu;
        sge_count += wr_count;

        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:recv_try receiving %d bytes in %d "
                             "work requests, %d sges", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), data_len,
                             wr_count, sge_count));

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

        iov_left   = recv_req->req_uiov[0].iov_len;
        iov_offset = 0;
        iov_index  = 0;

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
                                    (char *)recv_req->req_uiov[iov_index].iov_base + iov_offset,
                                    to_recv, recv_req->req_mr[iov_index]->lkey);

                iov_offset += to_recv;
                iov_left   -= to_recv;
                packet_size += to_recv;

                if (0 == iov_left) {
                    iov_index++;
                    iov_offset = 0;

                    if (iov_index < recv_req->req_count) {
                        iov_left = recv_req->req_uiov[iov_index].iov_len;
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

        rc = mca_oob_ud_qp_post_recv (recv_req->req_qp, recv_req->req_wr.recv);
        if (ORTE_SUCCESS != rc) {
            break;
        }

        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:recv_try posting reply message",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

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
        if (MCA_OOB_UD_REQ_UNEX != recv_req->type) {
            mca_oob_ud_req_complete (recv_req, rc);
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

    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:recv_complete req = %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) recv_req));

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

                OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:recv_complete wc status = %d. imm data = %d. "
                                     "len = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), wc[j].status, wc[j].imm_data,
                                     wc[j].byte_len));

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

            OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:recv_complete receive incomplete. error: %d, "
                                 "out_of_order: %d packets: %d/%d. rc = %d, errno = %d. flags = %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), error, out_of_order, i,
                                 recv_req->req_packet_count, rc, errno, recv_req->req_flags));
            mca_oob_ud_recv_try (recv_req);

            return ORTE_SUCCESS;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:recv_complete data received ok!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

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

    if (MCA_OOB_UD_REQ_UNEX != recv_req->type) {
        mca_oob_ud_req_complete (recv_req, (ORTE_SUCCESS == rc) ? recv_req->req_rem_data_len : rc);
    } else {
        mca_oob_ud_recv_unex_complete (recv_req);
    }

    return ORTE_SUCCESS;
}

static int mca_oob_ud_recv_unex_complete (mca_oob_ud_req_t *req)
{
    mca_oob_ud_req_t *recv_req;
    int rc;

    rc = mca_oob_ud_find_pending_recv (req->req_origin, req->req_tag, &recv_req);

    if (ORTE_SUCCESS == rc) {
        return mca_oob_ud_recv_copy (recv_req, req);
    }

    mca_oob_ud_req_append_to_list (req, &mca_oob_ud_component.ud_unexpected_recvs);

    return ORTE_SUCCESS;
}

int mca_oob_ud_recv_match_send (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer, mca_oob_ud_msg_hdr_t *msg_hdr,
                                mca_oob_ud_req_t **reqp)
{
    char *data = (msg_hdr->msg_data.req.data_follows ? (char *)(msg_hdr + 1) : NULL);
    mca_oob_ud_req_t *req;
    int rc, i;

    *reqp = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:recv_incoming_send matching incoming "
                         "send from peer %s with tag %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&msg_hdr->msg_origin), msg_hdr->msg_data.req.tag));

    rc = mca_oob_ud_get_recv_req (msg_hdr->msg_origin, msg_hdr->msg_data.req.tag, &req);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    req->req_rem_ctx      = msg_hdr->msg_rem_ctx;
    req->req_port         = port;
    req->req_mtu          = min(port->mtu, msg_hdr->msg_data.req.mtu);
    req->req_target       = msg_hdr->ra.name;
    req->req_rem_data_len = msg_hdr->msg_data.req.data_len;

    do {
        rc = mca_oob_ud_recv_alloc (req);
        if (ORTE_SUCCESS != rc) {
            opal_output (0, "%s oob:ud:recv_start malloc failed!", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            if (MCA_OOB_UD_REQ_UNEX == req->type) {
                free (req->req_uiov);
                OBJ_RELEASE(req);
            }
            req = NULL;
            break;
        }

        req->req_peer = peer;
        OBJ_RETAIN(req->req_peer);

        if (NULL == data) {
            req->state = MCA_OOB_UD_REQ_ACTIVE;
            break;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_oob_base_output, "%s oob:ud:recv_incoming_send send was eager",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        req->req_is_eager = true;

        for (i = 0 ; i < req->req_count; ++i) {
            memcpy (req->req_uiov[i].iov_base, data, req->req_uiov[i].iov_len);
            data += req->req_uiov[i].iov_len;
        }

        req->state = MCA_OOB_UD_REQ_COMPLETE;
    } while (0);

    *reqp = req;

    return rc;
}

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

#if !defined(MCA_OOB_UD_REQ_H)
#define MCA_OOB_UD_REQ_H

#include "oob_ud_peer.h"

#include "orte_config.h"
#include "orte/types.h"
#include "opal/threads/condition.h"
#include "opal/mca/event/event.h"
#include "opal/class/opal_free_list.h"
#include "orte/mca/rml/rml.h"

#include <infiniband/verbs.h>

#include "oob_ud_qp.h"

struct mca_oob_ud_peer_t;

enum mca_oob_ud_req_type_t {
    MCA_OOB_UD_REQ_RECV,
    MCA_OOB_UD_REQ_SEND
};
typedef enum mca_oob_ud_req_type_t mca_oob_ud_req_type_t;

enum mca_oob_ud_req_state_t {
    MCA_OOB_UD_REQ_ACTIVE,
    MCA_OOB_UD_REQ_PENDING,
    MCA_OOB_UD_REQ_COMPLETE
};
typedef enum mca_oob_ud_req_state_t mca_oob_ud_req_state_t;

enum mca_oob_ud_req_data_type_t {
    MCA_OOB_UD_REQ_IOV,
    MCA_OOB_UD_REQ_BUF,
    MCA_OOB_UD_REQ_TR
};
typedef enum mca_oob_ud_req_data_type_t mca_oob_ud_req_data_type_t;

enum mca_oob_ud_msg_type_t {
    MCA_OOB_UD_MSG_REQUEST  = 37,
    MCA_OOB_UD_MSG_REPLY    = 38,
    MCA_OOB_UD_MSG_COMPLETE = 39,
    MCA_OOB_UD_MSG_PING     = 40,
    MCA_OOB_UD_MSG_ACK      = 41,
    MCA_OOB_UD_MSG_NACK     = 42,
    MCA_OOB_UD_MSG_DATA_OK  = 43,
    MCA_OOB_UD_MSG_END      = 44
};
typedef enum mca_oob_ud_msg_type_t mca_oob_ud_msg_type_t;

struct mca_oob_ud_msg_hdr_t {
    mca_oob_ud_msg_type_t  msg_type;

    void *msg_rem_ctx;
    void *msg_lcl_ctx;

    orte_process_name_t msg_origin;
    orte_process_name_t msg_target;

    uint64_t msg_id;

    struct {
        /* the receiver can get the qpn and lid from the work completion */
        uint32_t            qkey;
        orte_process_name_t name;
        uint8_t             port_num;
    } ra;

    union {
        struct {
            int tag;
            int data_len;
            int mtu;
            bool data_follows;
            bool data_iovec_used;
        } req;
        struct {
            uint32_t qpn;
            int data_len;
            int tag;
            int mtu;
        } rep;
    } msg_data;
};
typedef struct mca_oob_ud_msg_hdr_t mca_oob_ud_msg_hdr_t;

struct mca_oob_ud_req_t {
    opal_list_item_t super;

    mca_oob_ud_req_type_t  type;
    mca_oob_ud_req_state_t state;

    union {
        struct ibv_send_wr *send;
        struct ibv_recv_wr *recv;
    } req_wr;

    /* storage for ib grh */
    struct ibv_grh         *req_grh;
    struct ibv_mr          *req_grh_mr;

    struct ibv_sge         *req_sge;

    /* negotiated mtu */
    int                     req_mtu;
    uint32_t                req_rem_qpn;
    int                     req_rem_data_len;

    int                     req_packet_count;

    struct mca_oob_ud_peer_t *req_peer;
    struct mca_oob_ud_port_t *req_port;
    struct mca_oob_ud_qp_t   *req_qp;

    /* remote context (request or response) */
    void                   *req_rem_ctx;

    /* retry timer */
    struct {
        opal_event_t   event;
        struct timeval value;
    } timer;

    /* user request */
    orte_process_name_t     req_target;
    orte_process_name_t     req_origin;

    mca_oob_ud_req_data_type_t req_data_type;
    union {
        struct {
            struct ibv_mr         **mr;
            struct iovec           *uiov;
            int                     count;
        }iov;
        struct {
            struct ibv_mr          *mr;
            char                    *p;
            int                     size;
        }buf;
    }req_data;

    int                     req_tag;
    int                     req_rc;

    void                    *req_cbdata;

    /* what list is this request in */
    opal_list_t            *req_list;

    bool                    req_is_eager;

    orte_rml_send_t        *rml_msg;
};

typedef struct mca_oob_ud_req_t mca_oob_ud_req_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_req_t);

enum mca_oob_ud_status_t {
    /* message posted */
    MCA_OOB_UD_MSG_STATUS_POSTED,
    /* remote side receive the message (ack'd) */
    MCA_OOB_UD_MSG_STATUS_COMPLETE,
    /* request message timed out */
    MCA_OOB_UD_MSG_STATUS_TIMEOUT,
    /* other failure */
    MCA_OOB_UD_MSG_STATUS_ERROR
};
typedef enum mca_oob_ud_status_t mca_oob_ud_status_t;

struct mca_oob_ud_msg_t {
    opal_free_list_item_t   super;

    struct ibv_send_wr      wr;
    struct ibv_sge          sge;
    mca_oob_ud_msg_hdr_t   *hdr;
    struct ibv_mr          *mr;

    /* qp this request was sent over */
    struct mca_oob_ud_qp_t   *qp;
    struct mca_oob_ud_port_t *port;

    opal_mutex_t            lock;
    opal_condition_t        status_changed;
    mca_oob_ud_status_t     status;

    bool                    persist;
    mca_oob_ud_req_t       *req;

    void (*cbfunc) (struct mca_oob_ud_msg_t *, int);

    struct mca_oob_ud_peer_t *peer;
};
typedef struct mca_oob_ud_msg_t mca_oob_ud_msg_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_msg_t);

static inline int mca_oob_ud_recv_alloc (mca_oob_ud_req_t *recv_req)
{
    int iov_index;

    size_t alloc_size = recv_req->req_rem_data_len;
    if (MCA_OOB_UD_REQ_IOV == recv_req->req_data_type) {
        for (iov_index = 0 ; iov_index < recv_req->req_data.iov.count - 1 ; ++iov_index) {
            alloc_size -= recv_req->req_data.iov.uiov[iov_index].iov_len;
        }

        recv_req->req_data.iov.uiov[iov_index].iov_len  = alloc_size;
        recv_req->req_data.iov.uiov[iov_index].iov_base = calloc (alloc_size, 1);

        if (NULL == recv_req->req_data.iov.uiov[iov_index].iov_base) {
            return ORTE_ERROR;
        }
    } else {
        recv_req->req_data.buf.p = (char *)calloc(recv_req->req_rem_data_len, sizeof(char));
        if (NULL == recv_req->req_data.buf.p) {
            return ORTE_ERROR;
        }
        recv_req->req_data.buf.size = recv_req->req_rem_data_len;
    }
    return ORTE_SUCCESS;
}

#define MCA_OOB_UD_REQ_DEREG_MR(req)                               \
    if (MCA_OOB_UD_REQ_IOV == req->req_data_type) {                \
        if (req->req_data.iov.mr) {                                \
            for (i = 0 ; i < req->req_data.iov.count ; ++i) {      \
                if (req->req_data.iov.mr[i]) {                     \
                    (void) ibv_dereg_mr (req->req_data.iov.mr[i]); \
                    req->req_data.iov.mr[i] = NULL;                \
                }                                                  \
            }                                                      \
            free (req->req_data.iov.mr);                           \
            req->req_data.iov.mr = NULL;                           \
        }                                                          \
    } else {                                                       \
        if (req->req_data.buf.mr) {                                \
            (void) ibv_dereg_mr (req->req_data.buf.mr);            \
            req->req_data.buf.mr = NULL;                           \
        }                                                          \
    }

int mca_oob_ud_msg_get (struct mca_oob_ud_port_t *port, mca_oob_ud_req_t *req,
                        mca_oob_ud_qp_t *qp, mca_oob_ud_peer_t *peer, bool persist,
                        mca_oob_ud_msg_t **msgp);
int mca_oob_ud_msg_init (opal_free_list_item_t *item, void *context);
void mca_oob_ud_msg_return (mca_oob_ud_msg_t *msg);


void mca_oob_ud_req_timer_set (mca_oob_ud_req_t *req, const struct timeval *timeout,
                               int max_tries, void (*cb)(evutil_socket_t, short, void *));

int mca_oob_ud_msg_post_send (mca_oob_ud_msg_t *msg);
int mca_oob_ud_msg_wait (mca_oob_ud_msg_t *msg);

int mca_oob_ud_msg_status_update (mca_oob_ud_msg_t *msg, mca_oob_ud_status_t status);

void mca_oob_ud_req_complete (mca_oob_ud_req_t *req, int rc);
void mca_oob_ud_req_abort (mca_oob_ud_req_t *req);

void mca_oob_ud_req_append_to_list (mca_oob_ud_req_t *req, opal_list_t *list);
bool mca_oob_ud_req_is_in_list (mca_oob_ud_req_t *req, opal_list_t *list);

#endif

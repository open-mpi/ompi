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

#if !defined(MCA_OOB_UD_REQ_H)
#define MCA_OOB_UD_REQ_H

#include "orte_config.h"
#include "orte/types.h"
#include "opal/threads/condition.h"
#include "opal/mca/event/event.h"
#include "orte/mca/rml/rml.h"

#include <infiniband/verbs.h>

#include "oob_ud_qp.h"

struct mca_oob_ud_peer_t;

enum mca_oob_ud_req_type_t {
    MCA_OOB_UD_REQ_RECV,
    MCA_OOB_UD_REQ_SEND,
    MCA_OOB_UD_REQ_UNEX
};
typedef enum mca_oob_ud_req_type_t mca_oob_ud_req_type_t;

enum mca_oob_ud_req_state_t {
    MCA_OOB_UD_REQ_ACTIVE,
    MCA_OOB_UD_REQ_PENDING,
    MCA_OOB_UD_REQ_COMPLETE
};

typedef enum mca_oob_ud_req_state_t mca_oob_ud_req_state_t;

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

    /* memory register for iovec memory */
    struct ibv_mr         **req_mr;

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
    struct iovec           *req_uiov;
    int                     req_count;
    int                     req_tag;
    int                     req_flags;
    int                     req_rc;

    orte_rml_callback_fn_t  req_cbfunc;
    void                   *req_cbdata;

    /* what list is this request in */
    opal_list_t            *req_list;

    bool                    req_is_eager;
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

    if (recv_req->req_flags & ORTE_RML_ALLOC) {
        size_t alloc_size = recv_req->req_rem_data_len;

        for (iov_index = 0 ; iov_index < recv_req->req_count - 1 ; ++iov_index) {
            alloc_size -= recv_req->req_uiov[iov_index].iov_len;
        }

        recv_req->req_uiov[iov_index].iov_len  = alloc_size;
        recv_req->req_uiov[iov_index].iov_base = calloc (alloc_size, 1);                

        if (NULL == recv_req->req_uiov[iov_index].iov_base) {
            return ORTE_ERROR;
        }
    }

    return ORTE_SUCCESS;
}

int mca_oob_ud_msg_get (struct mca_oob_ud_port_t *port, mca_oob_ud_req_t *req,
                        mca_oob_ud_qp_t *qp, mca_oob_ud_peer_t *peer, bool persist,
                        mca_oob_ud_msg_t **msgp);
int mca_oob_ud_msg_init (mca_oob_ud_msg_t *msg, struct mca_oob_ud_port_t *port,
                         char *buf, struct ibv_mr *mr);
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

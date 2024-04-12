/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_TCP_SENDRECV_H_
#define _MCA_OOB_TCP_SENDRECV_H_

#include "prte_config.h"

#include "src/class/pmix_list.h"
#include "src/util/pmix_string_copy.h"

#include "oob_tcp.h"
#include "oob_tcp_hdr.h"
#include "src/rml/rml.h"
#include "src/threads/pmix_threads.h"

/* forward declare */
struct prte_oob_tcp_peer_t;

/* tcp structure for sending a message */
typedef struct {
    pmix_list_item_t super;
    prte_event_t ev;
    struct prte_oob_tcp_peer_t *peer;
    bool activate;
    prte_oob_tcp_hdr_t hdr;
    prte_rml_send_t *msg;
    char *data;
    bool hdr_sent;
    int iovnum;
    char *sdptr;
    size_t sdbytes;
} prte_oob_tcp_send_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_send_t);

/* tcp structure for recving a message */
typedef struct {
    pmix_list_item_t super;
    prte_oob_tcp_hdr_t hdr;
    bool hdr_recvd;
    char *data;
    char *rdptr;
    size_t rdbytes;
} prte_oob_tcp_recv_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_recv_t);

/* Queue a message to be sent to a specified peer. The macro
 * checks to see if a message is already in position to be
 * sent - if it is, then the message provided is simply added
 * to the peer's message queue. If not, then the provided message
 * is placed in the "ready" position
 *
 * If the provided boolean is true, then the send event for the
 * peer is checked and activated if not already active. This allows
 * the macro to either immediately send the message, or to queue
 * it as "pending" for later transmission - e.g., after the
 * connection procedure is completed
 *
 * p => pointer to prte_oob_tcp_peer_t
 * s => pointer to prte_oob_tcp_send_t
 * f => true if send event is to be activated
 */
#define MCA_OOB_TCP_QUEUE_MSG(p, s, f)                                          \
    do {                                                                        \
        (s)->peer = (struct prte_oob_tcp_peer_t *) (p);                         \
        (s)->activate = (f);                                                    \
        PRTE_PMIX_THREADSHIFT((s), prte_event_base, prte_oob_tcp_queue_msg);    \
    } while (0)

/* queue a message to be sent by one of our modules - must
 * provide the following params:
 *
 * m - the RML message to be sent
 * p - the final recipient
 */
#define MCA_OOB_TCP_QUEUE_SEND(m, p)                                                           \
    do {                                                                                       \
        prte_oob_tcp_send_t *_s;                                                               \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                       \
                            "%s:[%s:%d] queue send to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT(&((m)->dst)));                 \
        _s = PMIX_NEW(prte_oob_tcp_send_t);                                                    \
        /* setup the header */                                                                 \
        PMIX_XFER_PROCID(&_s->hdr.origin, &(m)->origin);                                       \
        PMIX_XFER_PROCID(&_s->hdr.dst, &(m)->dst);                                             \
        _s->hdr.type = MCA_OOB_TCP_USER;                                                       \
        _s->hdr.tag = (m)->tag;                                                                \
        _s->hdr.seq_num = (m)->seq_num;                                                        \
        /* point to the actual message */                                                      \
        _s->msg = (m);                                                                         \
        /* set the total number of bytes to be sent */                                         \
        _s->hdr.nbytes = (m)->dbuf->bytes_used;                                                 \
        /* prep header for xmission */                                                         \
        MCA_OOB_TCP_HDR_HTON(&_s->hdr);                                                        \
        /* start the send with the header */                                                   \
        _s->sdptr = (char *) &_s->hdr;                                                         \
        _s->sdbytes = sizeof(prte_oob_tcp_hdr_t);                                              \
        /* add to the msg queue for this peer */                                               \
        MCA_OOB_TCP_QUEUE_MSG((p), _s, true);                                                  \
    } while (0)

/* queue a message to be sent by one of our modules upon completing
 * the connection process - must provide the following params:
 *
 * m - the RML message to be sent
 * p - the final recipient
 */
#define MCA_OOB_TCP_QUEUE_PENDING(m, p)                                                           \
    do {                                                                                          \
        prte_oob_tcp_send_t *_s;                                                                  \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                          \
                            "%s:[%s:%d] queue pending to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT(&((m)->dst)));                    \
        _s = PMIX_NEW(prte_oob_tcp_send_t);                                                       \
        /* setup the header */                                                                    \
        PMIX_XFER_PROCID(&_s->hdr.origin, &(m)->origin);                                          \
        PMIX_XFER_PROCID(&_s->hdr.dst, &(m)->dst);                                                \
        _s->hdr.type = MCA_OOB_TCP_USER;                                                          \
        _s->hdr.tag = (m)->tag;                                                                   \
        _s->hdr.seq_num = (m)->seq_num;                                                           \
        /* point to the actual message */                                                         \
        _s->msg = (m);                                                                            \
        /* set the total number of bytes to be sent */                                            \
        _s->hdr.nbytes = (m)->dbuf->bytes_used;                                                    \
        /* prep header for xmission */                                                            \
        MCA_OOB_TCP_HDR_HTON(&_s->hdr);                                                           \
        /* start the send with the header */                                                      \
        _s->sdptr = (char *) &_s->hdr;                                                            \
        _s->sdbytes = sizeof(prte_oob_tcp_hdr_t);                                                 \
        /* add to the msg queue for this peer */                                                  \
        MCA_OOB_TCP_QUEUE_MSG((p), _s, false);                                                    \
    } while (0)

/* queue a message for relay by one of our modules - must
 * provide the following params:
 *
 * m = the prte_oob_tcp_recv_t that was received
 * p - the next hop
 */
#define MCA_OOB_TCP_QUEUE_RELAY(m, p)                                                           \
    do {                                                                                        \
        prte_oob_tcp_send_t *_s;                                                                \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                        \
                            "%s:[%s:%d] queue relay to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT(&((p)->name)));                 \
        _s = PMIX_NEW(prte_oob_tcp_send_t);                                                     \
        /* setup the header */                                                                  \
        PMIX_XFER_PROCID(&_s->hdr.origin, &(m)->hdr.origin);                                    \
        PMIX_XFER_PROCID(&_s->hdr.dst, &(m)->hdr.dst);                                          \
        _s->hdr.type = MCA_OOB_TCP_USER;                                                        \
        _s->hdr.tag = (m)->hdr.tag;                                                             \
        (void) pmix_string_copy(_s->hdr.routed, (m)->hdr.routed, PRTE_MAX_RTD_SIZE);            \
        /* point to the actual message */                                                       \
        _s->data = (m)->data;                                                                   \
        /* set the total number of bytes to be sent */                                          \
        _s->hdr.nbytes = (m)->hdr.nbytes;                                                       \
        /* prep header for xmission */                                                          \
        MCA_OOB_TCP_HDR_HTON(&_s->hdr);                                                         \
        /* start the send with the header */                                                    \
        _s->sdptr = (char *) &_s->hdr;                                                          \
        _s->sdbytes = sizeof(prte_oob_tcp_hdr_t);                                               \
        /* add to the msg queue for this peer */                                                \
        MCA_OOB_TCP_QUEUE_MSG((p), _s, true);                                                   \
    } while (0)

/* State machine for processing message */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    prte_rml_send_t *msg;
} prte_oob_tcp_msg_op_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_msg_op_t);

#define PRTE_ACTIVATE_TCP_POST_SEND(ms, cbfunc)                                               \
    do {                                                                                      \
        prte_oob_tcp_msg_op_t *mop;                                                           \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                      \
                            "%s:[%s:%d] post send to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT(&((ms)->dst)));               \
        mop = PMIX_NEW(prte_oob_tcp_msg_op_t);                                                \
        mop->msg = (ms);                                                                      \
        PRTE_PMIX_THREADSHIFT(mop, prte_event_base, (cbfunc));                                \
    } while (0);

typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    prte_rml_send_t *rmsg;
    prte_oob_tcp_send_t *snd;
    pmix_proc_t hop;
} prte_oob_tcp_msg_error_t;
PMIX_CLASS_DECLARATION(prte_oob_tcp_msg_error_t);

#define PRTE_ACTIVATE_TCP_MSG_ERROR(s, r, h, cbfunc)                                               \
    do {                                                                                           \
        prte_oob_tcp_msg_error_t *mop;                                                             \
        prte_oob_tcp_send_t *snd;                                                                  \
        prte_oob_tcp_recv_t *proxy;                                                                \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                           \
                            "%s:[%s:%d] post msg error to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT((h)));                             \
        mop = PMIX_NEW(prte_oob_tcp_msg_error_t);                                                  \
        if (NULL != (s)) {                                                                         \
            mop->snd = (s);                                                                        \
        } else if (NULL != (r)) {                                                                  \
            /* use a proxy so we can pass NULL into the macro */                                   \
            proxy = (r);                                                                           \
            /* create a send object for this message */                                            \
            snd = PMIX_NEW(prte_oob_tcp_send_t);                                                   \
            mop->snd = snd;                                                                        \
            /* transfer and prep the header */                                                     \
            snd->hdr = proxy->hdr;                                                                 \
            MCA_OOB_TCP_HDR_HTON(&snd->hdr);                                                       \
            /* point to the data */                                                                \
            snd->data = proxy->data;                                                               \
            /* start the message with the header */                                                \
            snd->sdptr = (char *) &snd->hdr;                                                       \
            snd->sdbytes = sizeof(prte_oob_tcp_hdr_t);                                             \
            /* protect the data */                                                                 \
            proxy->data = NULL;                                                                    \
        }                                                                                          \
        PMIX_XFER_PROCID(&mop->hop, (h));                                                          \
        /* this goes to the OOB framework, so use that event base */                               \
        PRTE_PMIX_THREADSHIFT(mop, prte_event_base, (cbfunc));                                     \
    } while (0)

#define PRTE_ACTIVATE_TCP_NO_ROUTE(r, h, c)                                                       \
    do {                                                                                          \
        prte_oob_tcp_msg_error_t *mop;                                                            \
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,                          \
                            "%s:[%s:%d] post no route to %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), \
                            __FILE__, __LINE__, PRTE_NAME_PRINT((h)));                            \
        mop = PMIX_NEW(prte_oob_tcp_msg_error_t);                                                 \
        mop->rmsg = (r);                                                                          \
        PMIX_XFER_PROCID(&mop->hop, (h));                                                         \
        /* this goes to the component, so use the framework                                       \
         * event base */                                                                          \
        PRTE_PMIX_THREADSHIFT(mop, prte_event_base, (c));                                         \
    } while (0)

#endif /* _MCA_OOB_TCP_SENDRECV_H_ */

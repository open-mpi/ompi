/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Runtime Messaging Layer (RML) Communication Interface
 *
 * The Runtime Messaging Layer (RML) provices basic point-to-point
 * communication between PRTE processes.  The system is available for
 * most architectures, with some exceptions (the Cray XT3/XT4, for example).
 */

#ifndef PRTE_RML_H_
#define PRTE_RML_H_

#include "prte_config.h"
#include "types.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/rml/rml_types.h"
#include "src/pmix/pmix-internal.h"

BEGIN_C_DECLS
/**
 * Send a buffer non-blocking message
 *
 * Send a buffer to the specified peer.  The call
 * will return immediately, although the buffer may not be modified
 * until the completion callback is triggered.  The buffer *may* be
 * passed to another call to send_nb before the completion callback is
 * triggered.  The callback being triggered does not give any
 * indication of remote completion.
 *
 * @param[in] peer   Name of receiving process
 * @param[in] buffer Pointer to buffer to be sent
 * @param[in] tag    User defined tag for matching send/recv
 * @param[in] cbfunc Callback function on message comlpetion
 * @param[in] cbdata User data to provide during completion callback
 *
 * @retval PRTE_SUCCESS The message was successfully started
 * @retval PRTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval PRTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval PRTE_ERROR  An unspecified error occurred
 */
PRTE_EXPORT int prte_rml_send_buffer_nb(pmix_rank_t rank,
                                        pmix_data_buffer_t *buffer,
                                        prte_rml_tag_t tag);

#define PRTE_RML_SEND(_r, r, b, t)                              \
    do {                                                        \
        pmix_output_verbose(2, prte_rml_base.rml_output,        \
                            "RML-SEND(%s:%d): %s:%s:%d",        \
                            PMIX_RANK_PRINT(r), t,              \
                            __FILE__, __func__, __LINE__);      \
        (_r) = prte_rml_send_buffer_nb(r, b, t);                \
    } while(0)

/**
 * Purge the RML/OOB of contact info and pending messages
 * to/from a specified process. Used when a process aborts
 * and is to be restarted
 */
PRTE_EXPORT void prte_rml_purge(pmix_proc_t *peer);

#define PRTE_RML_PURGE(p)                                       \
    do {                                                        \
        pmix_output_verbose(2, prte_rml_base.rml_output,            \
                            "RML-PURGE(%s): %s:%s:%d",          \
                            PMIX_NAME_PRINT(p),                 \
                            __FILE__, __func__, __LINE__);      \
        prte_rml_purge(p);                                      \
    } while(0)

/**
 * Receive a buffer non-blocking message
 *
 * @param[in]  peer    Peer process or PRTE_NAME_WILDCARD for wildcard receive
 * @param[in]  tag     User defined tag for matching send/recv
 * @param[in] persistent Boolean flag indicating whether or not this is a one-time recv
 * @param[in] cbfunc   Callback function on message comlpetion
 * @param[in] cbdata   User data to provide during completion callback
 */
PRTE_EXPORT void prte_rml_recv_buffer_nb(pmix_proc_t *peer, prte_rml_tag_t tag,
                                         bool persistent,
                                         prte_rml_buffer_callback_fn_t cbfunc,
                                         void *cbdata);

#define PRTE_RML_RECV(p, t, prs, c, cb)                         \
    do {                                                        \
        pmix_output_verbose(2, prte_rml_base.rml_output,            \
                            "RML-RECV(%d): %s:%s:%d",           \
                            t, __FILE__, __func__, __LINE__);   \
        prte_rml_recv_buffer_nb(p, t, prs, c, cb);              \
    } while(0)


/**
 * Cancel a posted non-blocking receive
 *
 * Attempt to cancel a posted non-blocking receive.
 *
 * @param[in] peer    Peer process or PRTE_NAME_WILDCARD, exactly as passed
 *                    to the non-blocking receive call
 * @param[in] tag     Posted receive tag
 */
PRTE_EXPORT void prte_rml_recv_cancel(pmix_proc_t *peer, prte_rml_tag_t tag);

#define PRTE_RML_CANCEL(p, t)                                   \
    do {                                                        \
        pmix_output_verbose(2, prte_rml_base.rml_output,            \
                            "RML-CANCEL(%d): %s:%s:%d",         \
                            t, __FILE__, __func__, __LINE__);   \
        prte_rml_recv_cancel(p, t);                             \
    } while(0)

typedef struct {
    int rml_output;
    int routed_output;
    int max_retries;
    pmix_list_t posted_recvs;
    pmix_list_t unmatched_msgs;
    pmix_rank_t lifeline;
    pmix_list_t children;
    int radix;
    bool static_ports;
} prte_rml_base_t;

PRTE_EXPORT extern prte_rml_base_t prte_rml_base;

PRTE_EXPORT void prte_rml_register(void);
PRTE_EXPORT void prte_rml_close(void);
PRTE_EXPORT void prte_rml_open(void);
/* common implementations */
PRTE_EXPORT void prte_rml_base_post_recv(int sd, short args, void *cbdata);
PRTE_EXPORT void prte_rml_base_process_msg(int fd, short flags, void *cbdata);
PRTE_EXPORT void prte_rml_send_callback(int status, pmix_proc_t *peer,
                                        pmix_data_buffer_t *buffer,
                                        prte_rml_tag_t tag, void *cbdata);
PRTE_EXPORT void prte_rml_compute_routing_tree(void);
PRTE_EXPORT int prte_rml_get_num_contributors(pmix_rank_t *dmns, size_t ndmns);
PRTE_EXPORT int prte_rml_route_lost(pmix_rank_t route);
PRTE_EXPORT pmix_rank_t prte_rml_get_route(pmix_rank_t target);

#define PRTE_RML_POST_MESSAGE(p, t, s, b, l)                                                    \
    do {                                                                                        \
        prte_rml_recv_t *msg;                                                                   \
        pmix_status_t _rc;                                                                      \
        pmix_byte_object_t _bo;                                                                 \
        pmix_output_verbose(5, prte_rml_base.rml_output,                                            \
                            "%s Message posted at %s:%d for tag %d",                            \
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__, (t));       \
        msg = PMIX_NEW(prte_rml_recv_t);                                                        \
        PMIX_XFER_PROCID(&msg->sender, (p));                                                    \
        msg->tag = (t);                                                                         \
        msg->seq_num = (s);                                                                     \
        _bo.bytes = (char *) (b);                                                               \
        _bo.size = (l);                                                                        \
        PMIX_DATA_BUFFER_CREATE(msg->dbuf);                                                     \
        _rc = PMIx_Data_load(msg->dbuf, &_bo);                                                 \
        if (PMIX_SUCCESS != _rc) {                                                              \
            PMIX_ERROR_LOG(_rc);                                                                \
        }                                                                                       \
        /* setup the event */                                                                   \
        prte_event_set(prte_event_base, &msg->ev, -1, PRTE_EV_WRITE,                            \
                       prte_rml_base_process_msg, msg);                                         \
        prte_event_active(&msg->ev, PRTE_EV_WRITE, 1);                                          \
    } while (0);

#define PRTE_RML_ACTIVATE_MESSAGE(m)                                                            \
    do {                                                                                        \
        /* setup the event */                                                                   \
        prte_event_set(prte_event_base, &(m)->ev, -1, PRTE_EV_WRITE,                            \
                       prte_rml_base_process_msg, (m));                                         \
        prte_event_active(&(m)->ev, PRTE_EV_WRITE, 1);                                          \
    } while (0);

#define PRTE_RML_SEND_COMPLETE(m)                                                             \
    do {                                                                                      \
        pmix_output_verbose(5, prte_rml_base.rml_output,                                          \
                            "%s-%s Send message complete at %s:%d",                           \
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&((m)->dst)), \
                            __FILE__, __LINE__);                                              \
            /* non-blocking buffer send */                                                    \
        prte_rml_send_callback((m)->status, &((m)->dst),                                      \
                               (m)->dbuf, (m)->tag, (m)->cbdata);                            \
        PMIX_RELEASE(m);                                                                      \
    } while (0);


END_C_DECLS

#endif

/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
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
 * Data packing subsystem.
 */

#ifndef PMIX_PTL_H_
#define PMIX_PTL_H_

#include "src/include/pmix_config.h"

#include "src/include/pmix_types.h"

#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/bfrops/bfrops_types.h"
#include "src/mca/mca.h"

#include "ptl_types.h"

BEGIN_C_DECLS

/* forward declaration */
struct pmix_peer_t;

/* The overall objective of this framework is to provide transport
 * options by which a server can communicate with a client:
 *
 * (a) across different versions of the library - e.g., when the
 *     connection handshake changes.
 *
 * (b) using different transports as necessitated by different
 *     environments.
 *
 * This is a single-select framework - i.e., only one component
 * is selected and "active" at a time. The intent is
 * to have one component for each use-case, with the
 * expectation that the community will do its best not to revise
 * communications in manners that expand components to support (a).
 * Thus, new variations should be rare, and only a few components
 * will exist.
 */

/****    MODULE INTERFACE DEFINITION    ****/

/* initialize the active plugin */
typedef pmix_status_t (*pmix_ptl_init_fn_t)(void);

/* finalize the active plugin */
typedef void (*pmix_ptl_finalize_fn_t)(void);

/* (ONE-WAY) register a persistent recv */
typedef pmix_status_t (*pmix_ptl_recv_fn_t)(struct pmix_peer_t *peer, pmix_ptl_cbfunc_t cbfunc,
                                            pmix_ptl_tag_t tag);

/* Cancel a persistent recv */
typedef pmix_status_t (*pmix_ptl_cancel_fn_t)(struct pmix_peer_t *peer, pmix_ptl_tag_t tag);

/* connect to a peer - this is a blocking function
 * to establish a connection to a peer*/
typedef pmix_status_t (*pmix_ptl_connect_to_peer_fn_t)(struct pmix_peer_t *peer, pmix_info_t info[],
                                                       size_t ninfo);

/* query available servers on the local node */
typedef void (*pmix_ptl_query_servers_fn_t)(char *dirname, pmix_list_t *servers);

/* define an API for establishing a
 * communication rendezvous point for local procs. The active component
 * is given an opportunity to register a listener with the
 * PTL base */
typedef pmix_status_t (*pmix_ptl_setup_listener_fn_t)(pmix_info_t info[], size_t ninfo);

/* define an API for obtaining any envars that are to
 * be passed to client procs upon fork */
typedef pmix_status_t (*pmix_ptl_setup_fork_fn_t)(const pmix_proc_t *proc, char ***env);

/**
 * Base structure for a PTL module
 */
struct pmix_ptl_module_t {
    char *name;
    pmix_ptl_init_fn_t init;
    pmix_ptl_finalize_fn_t finalize;
    pmix_ptl_recv_fn_t recv;
    pmix_ptl_cancel_fn_t cancel;
    pmix_ptl_connect_to_peer_fn_t connect_to_peer;
    pmix_ptl_query_servers_fn_t query_servers;
    pmix_ptl_setup_listener_fn_t setup_listener;
    pmix_ptl_setup_fork_fn_t setup_fork;
};
typedef struct pmix_ptl_module_t pmix_ptl_module_t;

/*****    MACROS FOR EXECUTING PTL FUNCTIONS    *****/

/* (TWO-WAY) send a message to the peer, and get a response delivered
 * to the specified callback function. The buffer will be free'd
 * at the completion of the send, and the cbfunc will be called
 * when the corresponding reply is received */
#define PMIX_PTL_SEND_RECV(r, p, b, c, d)                  \
    do {                                                   \
        pmix_ptl_sr_t *ms;                                 \
        pmix_peer_t *pr = (pmix_peer_t *) (p);             \
        if ((p)->finalized) {                              \
            (r) = PMIX_ERR_UNREACH;                        \
        } else {                                           \
            ms = PMIX_NEW(pmix_ptl_sr_t);                  \
            PMIX_RETAIN(pr);                               \
            ms->peer = pr;                                 \
            ms->bfr = (b);                                 \
            ms->cbfunc = (c);                              \
            ms->cbdata = (d);                              \
            PMIX_THREADSHIFT(ms, pmix_ptl_base_send_recv); \
            (r) = PMIX_SUCCESS;                            \
        }                                                  \
    } while (0)

/* (ONE-WAY) send a message to the peer. The buffer will be free'd
 * at the completion of the send */
#define PMIX_PTL_SEND_ONEWAY(r, p, b, t)             \
    do {                                             \
        pmix_ptl_queue_t *q;                         \
        pmix_peer_t *pr = (pmix_peer_t *) (p);       \
        if ((p)->finalized) {                        \
            (r) = PMIX_ERR_UNREACH;                  \
        } else {                                     \
            q = PMIX_NEW(pmix_ptl_queue_t);          \
            PMIX_RETAIN(pr);                         \
            q->peer = pr;                            \
            q->buf = (b);                            \
            q->tag = (t);                            \
            PMIX_THREADSHIFT(q, pmix_ptl_base_send); \
            (r) = PMIX_SUCCESS;                      \
        }                                            \
    } while (0)

#define PMIX_PTL_RECV(r, c, t)                                               \
    do {                                                                     \
        pmix_ptl_posted_recv_t *req;                                         \
        req = PMIX_NEW(pmix_ptl_posted_recv_t);                              \
        if (NULL == req) {                                                   \
            (r) = PMIX_ERR_NOMEM;                                            \
        } else {                                                             \
            req->tag = (t);                                                  \
            req->cbfunc = (c);                                               \
            pmix_event_assign(&(req->ev), pmix_globals.evbase, -1, EV_WRITE, \
                              pmix_ptl_base_post_recv, req);                 \
            pmix_event_active(&(req->ev), EV_WRITE, 1);                      \
            (r) = PMIX_SUCCESS;                                              \
        }                                                                    \
    } while (0)

#define PMIX_PTL_CANCEL(r, t)                                                \
    do {                                                                     \
        pmix_ptl_posted_recv_t *req;                                         \
        req = PMIX_NEW(pmix_ptl_posted_recv_t);                              \
        if (NULL == req) {                                                   \
            (r) = PMIX_ERR_NOMEM;                                            \
        } else {                                                             \
            req->tag = (t);                                                  \
            pmix_event_assign(&(req->ev), pmix_globals.evbase, -1, EV_WRITE, \
                              pmix_ptl_base_cancel_recv, req);               \
            pmix_event_active(&(req->ev), EV_WRITE, 1);                      \
            (r) = PMIX_SUCCESS;                                              \
        }                                                                    \
    } while (0)

/* expose functions used by the macros */
PMIX_EXPORT extern void pmix_ptl_base_send(int sd, short args, void *cbdata);
PMIX_EXPORT extern void pmix_ptl_base_send_recv(int sd, short args, void *cbdata);
PMIX_EXPORT extern void pmix_ptl_base_register_recv(int sd, short args, void *cbdata);
PMIX_EXPORT extern void pmix_ptl_base_cancel_recv(int sd, short args, void *cbdata);

/****    COMPONENT STRUCTURE DEFINITION    ****/

/*
 * the standard component data structure
 */
struct pmix_ptl_base_component_t {
    pmix_mca_base_component_t base;
    int priority;
    char *uri;
};
typedef struct pmix_ptl_base_component_t pmix_ptl_base_component_t;

/* export the PTL module struct */
PMIX_EXPORT extern pmix_ptl_module_t pmix_ptl;

/*
 * Macro for use in components that are of type ptl
 */
#define PMIX_PTL_BASE_VERSION_2_0_0 PMIX_MCA_BASE_VERSION_1_0_0("ptl", 2, 0, 0)

END_C_DECLS

#endif /* PMIX_PTL_H */

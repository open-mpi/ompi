/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif

#include "src/event/event-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/prtebacktrace/prtebacktrace.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"

#include "src/rml/rml.h"

void prte_rml_recv_buffer_nb(pmix_proc_t *peer,
                             prte_rml_tag_t tag,
                             bool persistent,
                             prte_rml_buffer_callback_fn_t cbfunc,
                             void *cbdata)
{
    prte_rml_recv_request_t *req;

    pmix_output_verbose(10, prte_rml_base.rml_output,
                        "%s rml_recv_buffer_nb for peer %s tag %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(peer), tag);

    /* push the request into the event base so we can add
     * the receive to our list of posted recvs */
    req = PMIX_NEW(prte_rml_recv_request_t);
    PMIX_XFER_PROCID(&req->post->peer, peer);
    req->post->tag = tag;
    req->post->persistent = persistent;
    req->post->cbfunc = cbfunc;
    req->post->cbdata = cbdata;
    PRTE_PMIX_THREADSHIFT(req, prte_event_base, prte_rml_base_post_recv);
}
void prte_rml_recv_cancel(pmix_proc_t *peer, prte_rml_tag_t tag)
{
    prte_rml_recv_request_t *req;

    pmix_output_verbose(10, prte_rml_base.rml_output,
                        "%s rml_recv_cancel for peer %s tag %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(peer), tag);

    PMIX_ACQUIRE_OBJECT(prte_event_base_active);
    if (!prte_event_base_active) {
        /* no event will be processed any more, so simply return. */
        return;
    }

    /* push the request into the event base so we can remove
     * the receive from our list of posted recvs */
    req = PMIX_NEW(prte_rml_recv_request_t);
    req->cancel = true;
    PMIX_XFER_PROCID(&req->post->peer, peer);
    req->post->tag = tag;
    PRTE_PMIX_THREADSHIFT(req, prte_event_base, prte_rml_base_post_recv);
}

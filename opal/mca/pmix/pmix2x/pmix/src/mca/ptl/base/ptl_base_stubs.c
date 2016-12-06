/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/include/pmix_globals.h"

#include "src/mca/ptl/base/base.h"

pmix_status_t pmix_ptl_stub_set_notification_cbfunc(pmix_ptl_cbfunc_t cbfunc)
{
    pmix_ptl_posted_recv_t *req;

    /* post a persistent recv for the special 0 tag so the client can recv
     * error notifications from the server */
    req = PMIX_NEW(pmix_ptl_posted_recv_t);
    if (NULL == req) {
        return PMIX_ERR_NOMEM;
    }
    req->tag = 0;
    req->cbfunc = cbfunc;
    pmix_output_verbose(5, pmix_globals.debug_output,
                        "posting notification recv on tag %d", req->tag);
    /* add it to the list of recvs - we cannot have unexpected messages
     * in this subsystem as the server never sends us something that
     * we didn't previously request */
    pmix_list_prepend(&pmix_ptl_globals.posted_recvs, &req->super);
    return PMIX_SUCCESS;
}

char* pmix_ptl_stub_get_available_modules(void)
{
    pmix_ptl_base_active_t *active;
    char **tmp=NULL, *reply=NULL;

    if (!pmix_ptl_globals.initialized) {
        return NULL;
    }

    PMIX_LIST_FOREACH(active, &pmix_ptl_globals.actives, pmix_ptl_base_active_t) {
        pmix_argv_append_nosize(&tmp, active->component->base.pmix_mca_component_name);
    }
    if (NULL != tmp) {
        reply = pmix_argv_join(tmp, ',');
        pmix_argv_free(tmp);
    }
    return reply;
}

pmix_status_t pmix_ptl_stub_send_recv(struct pmix_peer_t *peer,
                                      pmix_buffer_t *bfr,
                                      pmix_ptl_cbfunc_t cbfunc,
                                      void *cbdata)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;

    return pr->compat.ptl->send_recv(peer, bfr, cbfunc, cbdata);
}

pmix_status_t pmix_ptl_stub_send_oneway(struct pmix_peer_t *peer,
                                        pmix_buffer_t *bfr,
                                        pmix_ptl_tag_t tag)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;
    return pr->compat.ptl->send(peer, bfr, tag);
}

pmix_status_t pmix_ptl_stub_connect_to_peer(struct pmix_peer_t *peer,
                                            pmix_info_t info[], size_t ninfo)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;
    pmix_ptl_base_active_t *active;

    PMIX_LIST_FOREACH(active, &pmix_ptl_globals.actives, pmix_ptl_base_active_t) {
        if (NULL != active->module->connect_to_peer) {
            if (PMIX_SUCCESS == active->module->connect_to_peer(peer, info, ninfo)) {
                pr->compat.ptl = active->module;
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_UNREACH;
}

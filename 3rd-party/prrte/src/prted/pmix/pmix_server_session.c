/*
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/pmix/pmix-internal.h"
#include "src/prted/pmix/pmix_server_internal.h"
#include "src/rml/rml.h"

static void localrelease(void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t*)cbdata;
    pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
    PMIX_RELEASE(req);
}

static void infocbfunc(pmix_status_t status,
                       pmix_info_t *info, size_t ninfo,
                       void *cbdata,
                       pmix_release_cbfunc_t rel, void *relcbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t*)cbdata;

    if (NULL != req->infocbfunc) {
        req->infocbfunc(status, info, ninfo, req->cbdata, localrelease, req);
        if (NULL != rel) {
            rel(relcbdata);
        }
        return;
    }
    /* need to cleanup ourselves */
    if (NULL != rel) {
        rel(relcbdata);
    }
    pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
    PMIX_RELEASE(req);
}

static void pass_request(int sd, short args, void *cbdata)
{
    prte_pmix_server_op_caddy_t *cd = (prte_pmix_server_op_caddy_t*)cbdata;
    pmix_server_req_t *req;
    pmix_data_buffer_t *buf;
    uint8_t command;
    pmix_status_t rc;

    /* create a request tracker for this operation */
    req = PMIX_NEW(pmix_server_req_t);
    if (0 < cd->allocdir) {
        pmix_asprintf(&req->operation, "ALLOCATE: %u", cd->allocdir);
        command = 0;
    } else {
        pmix_asprintf(&req->operation, "SESSIONCTRL: %u", cd->sessionID);
        command = 1;
    }
    req->infocbfunc = cd->infocbfunc;
    req->cbdata = cd->cbdata;
    /* add this request to our local request tracker array */
    req->local_index = pmix_pointer_array_add(&prte_pmix_server_globals.local_reqs, req);

    /* if we are the DVM master, then handle this ourselves */
    if (PRTE_PROC_IS_MASTER) {
        if (!prte_pmix_server_globals.scheduler_connected) {
            /* the scheduler has not attached to us - there is
             * nothing we can do */
            rc = PMIX_ERR_NOT_SUPPORTED;
            goto callback;
        }

        /* if we have not yet set the scheduler as our server, do so */
        if (!prte_pmix_server_globals.scheduler_set_as_server) {
            rc = PMIx_tool_set_server(&prte_pmix_server_globals.scheduler, NULL, 0);
            if (PMIX_SUCCESS != rc) {
                goto callback;
            }
            prte_pmix_server_globals.scheduler_set_as_server = true;
        }

        if (0 == command) {
            rc = PMIx_Allocation_request_nb(cd->allocdir, cd->info, cd->ninfo,
                                            infocbfunc, req);
        } else {
#if PMIX_NUMERIC_VERSION < 0x00050000
        rc = PMIX_ERR_NOT_SUPPORTED;
#else
        rc = PMIx_Session_control(cd->sessionID, cd->info, cd->ninfo,
                                  infocbfunc, req);
#endif
        }
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto callback;
        }
        return;
    }

    PMIX_DATA_BUFFER_CREATE(buf);

    /* construct a request message for the command */
    rc = PMIx_Data_pack(NULL, buf, &command, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
        goto callback;
    }

    /* pack the local requestor ID */
    rc = PMIx_Data_pack(NULL, buf, &req->local_index, 1, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
        goto callback;
    }

    /* pack the requestor */
    rc = PMIx_Data_pack(NULL, buf, &cd->proc, 1, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
        goto callback;
    }

    if (0 == command) {
        /* pack the allocation directive */
        rc = PMIx_Data_pack(NULL, buf, &cd->allocdir, 1, PMIX_ALLOC_DIRECTIVE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(buf);
            pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
            goto callback;
        }
    } else {
        /* pack the sessionID */
        rc = PMIx_Data_pack(NULL, buf, &cd->sessionID, 1, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(buf);
            pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
            goto callback;
        }
    }

    /* pack the number of info */
    rc = PMIx_Data_pack(NULL, buf, &cd->ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(buf);
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
        goto callback;
    }
    if (0 < cd->ninfo) {
        /* pack the info */
        rc = PMIx_Data_pack(NULL, buf, cd->info, cd->ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(buf);
            pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
            goto callback;
        }
    }

    /* send this request to the DVM controller - might be us */
    PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, buf, PRTE_RML_TAG_SCHED);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
        PMIX_DATA_BUFFER_RELEASE(buf);
        goto callback;
    }
    PMIX_RELEASE(cd);
    return;

callback:
    PMIX_RELEASE(cd);
    /* this section gets executed solely upon an error */
    if (NULL != req->infocbfunc) {
        req->infocbfunc(rc, req->info, req->ninfo, req->cbdata, localrelease, req);
        return;
    }
    PMIX_RELEASE(req);
}

pmix_status_t pmix_server_alloc_fn(const pmix_proc_t *client,
                                   pmix_alloc_directive_t directive,
                                   const pmix_info_t data[], size_t ndata,
                                   pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    prte_pmix_server_op_caddy_t *cd;


    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "%s allocate upcalled on behalf of proc %s:%u with %" PRIsize_t " infos",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), client->nspace, client->rank, ndata);

    cd = PMIX_NEW(prte_pmix_server_op_caddy_t);
    PMIX_LOAD_PROCID(&cd->proc, client->nspace, client->rank);
    cd->allocdir = directive;
    cd->info = (pmix_info_t *) data;
    cd->ninfo = ndata;
    cd->infocbfunc = cbfunc;
    cd->cbdata = cbdata;
    prte_event_set(prte_event_base, &cd->ev, -1, PRTE_EV_WRITE, pass_request, cd);
    PMIX_POST_OBJECT(cd);
    prte_event_active(&cd->ev, PRTE_EV_WRITE, 1);
    return PRTE_SUCCESS;
}

#if PMIX_NUMERIC_VERSION >= 0x00050000

pmix_status_t pmix_server_session_ctrl_fn(const pmix_proc_t *requestor,
                                          uint32_t sessionID,
                                          const pmix_info_t directives[], size_t ndirs,
                                          pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    prte_pmix_server_op_caddy_t *cd;


    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "%s session ctrl upcalled on behalf of proc %s:%u with %" PRIsize_t " directives",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), requestor->nspace, requestor->rank, ndirs);

    cd = PMIX_NEW(prte_pmix_server_op_caddy_t);
    PMIX_LOAD_PROCID(&cd->proc, requestor->nspace, requestor->rank);
    cd->sessionID = sessionID;
    cd->info = (pmix_info_t *) directives;
    cd->ninfo = ndirs;
    cd->infocbfunc = cbfunc;
    cd->cbdata = cbdata;
    prte_event_set(prte_event_base, &cd->ev, -1, PRTE_EV_WRITE, pass_request, cd);
    PMIX_POST_OBJECT(cd);
    prte_event_active(&cd->ev, PRTE_EV_WRITE, 1);
    return PRTE_SUCCESS;
}

#endif

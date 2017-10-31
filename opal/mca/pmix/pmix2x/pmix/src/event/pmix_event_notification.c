/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <src/include/pmix_config.h>

#include <pmix.h>
#include <pmix_common.h>
#include <pmix_server.h>
#include <pmix_rename.h>

#include "src/threads/threads.h"
#include "src/util/error.h"
#include "src/util/output.h"

#include "src/mca/bfrops/bfrops.h"
#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

static pmix_status_t notify_server_of_event(pmix_status_t status,
                                            const pmix_proc_t *source,
                                            pmix_data_range_t range,
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata);

static bool check_range(pmix_range_trkr_t *range, const pmix_proc_t *proc);

/* if we are a client, we call this function to notify the server of
 * an event. If we are a server, our host RM will call this function
 * to notify us of an event */
PMIX_EXPORT pmix_status_t PMIx_Notify_event(pmix_status_t status,
                                            const pmix_proc_t *source,
                                            pmix_data_range_t range,
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!PMIX_PROC_IS_SERVER(pmix_globals.mypeer) && !pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);


    if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
        rc = pmix_server_notify_client_of_event(status, source, range,
                                                info, ninfo,
                                                cbfunc, cbdata);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix_server_notify_event source = %s:%d event_status = %d, rc= %d",
                            (NULL == source) ? "UNKNOWN" : source->nspace,
                            (NULL == source) ? PMIX_RANK_WILDCARD : source->rank, status, rc);
    } else {
        rc = notify_server_of_event(status, source, range,
                                    info, ninfo,
                                    cbfunc, cbdata);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix_client_notify_event source = %s:%d event_status =%d, rc=%d",
                            (NULL == source) ? pmix_globals.myid.nspace : source->nspace,
                            (NULL == source) ? pmix_globals.myid.rank : source->rank, status, rc);
    }
    return rc;
}

static void notify_event_cbfunc(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
                                pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t rc, ret;
    int32_t cnt = 1;
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    /* unpack the status */
    PMIX_BFROPS_UNPACK(rc, pr, buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    }
    /* do the cback */
    if (NULL != cb->cbfunc.opfn) {
        cb->cbfunc.opfn(ret, cb->cbdata);
    }
    PMIX_RELEASE(cb);
}

/* as a client, we pass the notification to our server */
static pmix_status_t notify_server_of_event(pmix_status_t status,
                                            const pmix_proc_t *source,
                                            pmix_data_range_t range,
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    pmix_buffer_t *msg = NULL;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;
    pmix_cb_t *cb;
    pmix_event_chain_t *chain;
    size_t n;
    pmix_notify_caddy_t *cd, *rbout;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "client: notifying server %s:%d of status %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        PMIx_Error_string(status));

    if (PMIX_RANGE_PROC_LOCAL != range) {
        /* create the msg object */
        msg = PMIX_NEW(pmix_buffer_t);
        if (NULL == msg) {
            return PMIX_ERR_NOMEM;
        }
        /* pack the command */
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        /* pack the status */
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &status, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        /* no need to pack the source as it is us */

        /* pack the range */
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &range, 1, PMIX_DATA_RANGE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        /* pack the info */
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ninfo, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        if (0 < ninfo) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, info, ninfo, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }

    /* setup for our own local callbacks */
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->status = status;
    (void)strncpy(chain->source.nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
    chain->source.rank = pmix_globals.myid.rank;
    /* we always leave space for a callback object and
     * the evhandler name. */
    chain->ninfo = ninfo + 2;
    PMIX_INFO_CREATE(chain->info, chain->ninfo);

    if (0 < ninfo) {
        /* need to copy the info */
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&chain->info[n], &info[n]);
        }
    }
    /* add the evhandler name tag - we
     * will fill it in as each handler is called */
    PMIX_INFO_LOAD(&chain->info[chain->ninfo-2], PMIX_EVENT_HDLR_NAME, NULL, PMIX_STRING);
    /* now add the callback object tag */
    PMIX_INFO_LOAD(&chain->info[chain->ninfo-1], PMIX_EVENT_RETURN_OBJECT, NULL, PMIX_POINTER);

    /* we need to cache this event so we can pass it into
     * ourselves should someone later register for it */
    cd = PMIX_NEW(pmix_notify_caddy_t);
    cd->status = status;
    if (NULL == source) {
        (void)strncpy(cd->source.nspace, "UNDEF", PMIX_MAX_NSLEN);
        cd->source.rank = PMIX_RANK_UNDEF;
    } else {
        (void)strncpy(cd->source.nspace, source->nspace, PMIX_MAX_NSLEN);
        cd->source.rank = source->rank;
    }
    cd->range = range;
    if (0 < chain->ninfo) {
        cd->ninfo = chain->ninfo;
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
       /* need to copy the info */
        for (n=0; n < cd->ninfo; n++) {
            PMIX_INFO_XFER(&cd->info[n], &chain->info[n]);
            if (0 == strncmp(cd->info[n].key, PMIX_EVENT_NON_DEFAULT, PMIX_MAX_KEYLEN)) {
                cd->nondefault = true;
            } else if (0 == strncmp(cd->info[n].key, PMIX_EVENT_CUSTOM_RANGE, PMIX_MAX_KEYLEN)) {
                /* provides an array of pmix_proc_t identifying the procs
                 * that are to receive this notification, or a single pmix_proc_t  */
                if (PMIX_DATA_ARRAY == cd->info[n].value.type &&
                    NULL != cd->info[n].value.data.darray &&
                    NULL != cd->info[n].value.data.darray->array) {
                    cd->ntargets = cd->info[n].value.data.darray->size;
                    PMIX_PROC_CREATE(cd->targets, cd->ntargets);
                    memcpy(cd->targets, cd->info[n].value.data.darray->array, cd->ntargets * sizeof(pmix_proc_t));
                } else if (PMIX_PROC == cd->info[n].value.type) {
                    cd->ntargets = 1;
                    PMIX_PROC_CREATE(cd->targets, cd->ntargets);
                    memcpy(cd->targets, cd->info[n].value.data.proc, sizeof(pmix_proc_t));
                } else {
                    /* this is an error */
                    PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                    return PMIX_ERR_BAD_PARAM;
                }
            }
        }
    }

    /* add to our cache */
    rbout = pmix_ring_buffer_push(&pmix_globals.notifications, cd);
    /* if an older event was bumped, release it */
    if (NULL != rbout) {
        PMIX_RELEASE(rbout);
    }

    if (PMIX_RANGE_PROC_LOCAL != range && NULL != msg) {
        /* create a callback object as we need to pass it to the
         * recv routine so we know which callback to use when
         * the server acks/nacks the register events request. The
         * server will _not_ send this notification back to us,
         * so we handle it locally */
        cb = PMIX_NEW(pmix_cb_t);
        cb->cbfunc.opfn = cbfunc;
        cb->cbdata = cbdata;
        /* send to the server */
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "client: notifying server %s:%d - sending",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank);
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                           msg, notify_event_cbfunc, cb);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cb);
            goto cleanup;
        }
    } else {
        cbfunc(PMIX_SUCCESS, cbdata);
    }

    /* now notify any matching registered callbacks we have */
    pmix_invoke_local_event_hdlr(chain);

    return PMIX_SUCCESS;

  cleanup:
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "client: notifying server - unable to send");
    PMIX_RELEASE(msg);
    /* we were unable to send anything, so we just return the error */
    return rc;
}


static void progress_local_event_hdlr(pmix_status_t status,
                                      pmix_info_t *results, size_t nresults,
                                      pmix_op_cbfunc_t cbfunc, void *thiscbdata,
                                      void *notification_cbdata)
{
    /* this may be in the host's thread, so we need to threadshift it
     * before accessing our internal data */

    pmix_event_chain_t *chain = (pmix_event_chain_t*)notification_cbdata;
    size_t n, nsave, cnt;
    pmix_info_t *newinfo;
    pmix_list_item_t *item;
    pmix_event_hdlr_t *nxt;

    /* aggregate the results per RFC0018 - first search the
     * prior chained results to see if any keys have been NULL'd
     * as this indicates that info struct should be removed */
    nsave = 0;
    for (n=0; n < chain->nresults; n++) {
        if (0 < strlen(chain->results[n].key)) {
            ++nsave;
        }
    }
    /* we have to at least record the status returned by each
     * stage of the event handler chain, so we have to reallocate
     * the array to make space */

    /* add in any new results plus space for the returned status */
    nsave += nresults + 1;
    /* create the new space */
    PMIX_INFO_CREATE(newinfo, nsave);
    /* transfer over the prior data */
    cnt = 0;
    for (n=0; n < chain->nresults; n++) {
        if (0 < strlen(chain->results[n].key)) {
            PMIX_INFO_XFER(&newinfo[cnt], &chain->results[n]);
            ++cnt;
        }
    }

    /* save this handler's returned status */
    if (NULL != chain->evhdlr->name) {
        (void)strncpy(newinfo[cnt].key, chain->evhdlr->name, PMIX_MAX_KEYLEN);
    } else {
        (void)strncpy(newinfo[cnt].key, "UNKNOWN", PMIX_MAX_KEYLEN);
    }
    newinfo[cnt].value.type = PMIX_STATUS;
    newinfo[cnt].value.data.status = status;
    ++cnt;
    /* transfer across the new results */
    for (n=0; n < nresults; n++) {
        PMIX_INFO_XFER(&newinfo[cnt], &results[n]);
        ++cnt;
    }
    /* release the prior results */
    if (0 < chain->nresults) {
        PMIX_INFO_FREE(chain->results, chain->nresults);
    }
    /* pass along the new ones */
    chain->results = newinfo;
    chain->nresults = cnt;

    /* if the caller indicates that the chain is completed,
     * or we completed the "last" event */
    if (PMIX_EVENT_ACTION_COMPLETE == status || chain->endchain) {
        goto complete;
    }
    item = NULL;

    /* see if we need to continue, starting with the single code events */
    if (1 == chain->evhdlr->ncodes) {
        /* the last handler was for a single code - see if there are
         * any others that match this event */
        item = &chain->evhdlr->super;
        while (pmix_list_get_end(&pmix_globals.events.single_events) != (item = pmix_list_get_next(item))) {
            nxt = (pmix_event_hdlr_t*)item;
            if (nxt->codes[0] == chain->status &&
                check_range(&nxt->rng, &chain->source)) {
                chain->evhdlr = nxt;
                /* update the handler name in case they want to reference it */
                for (n=0; n < chain->ninfo; n++) {
                    if (0 == strncmp(chain->info[n].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
                        if (NULL != chain->info[n].value.data.string) {
                            free(chain->info[n].value.data.string);
                        }
                        if (NULL != chain->evhdlr->name) {
                            chain->info[n].value.data.string = strdup(chain->evhdlr->name);
                        }
                        break;
                    }
                }
                /* update the evhdlr cbobject */
                for (n=0; n < chain->ninfo; n++) {
                    if (0 == strncmp(chain->info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                        if (NULL != chain->evhdlr->name) {
                            chain->info[n].value.data.ptr = chain->evhdlr->cbobject;
                        }
                        break;
                    }
                }
                nxt->evhdlr(nxt->index,
                            chain->status, &chain->source,
                            chain->info, chain->ninfo,
                            chain->results, chain->nresults,
                            progress_local_event_hdlr, (void*)chain);
                return;
            }
        }
        /* if we get here, then there are no more single code
         * events that match */
        item = pmix_list_get_begin(&pmix_globals.events.multi_events);
    }

    /* see if we need to continue with the multi code events */
    if (NULL != chain->evhdlr->codes || NULL != item) {
        /* the last handler was for a multi-code event, or we exhausted
         * all the single code events */
        if (NULL == item) {
            /* if the last handler was multi-code, then start from that point */
            item = &chain->evhdlr->super;
        }
        while (pmix_list_get_end(&pmix_globals.events.multi_events) != (item = pmix_list_get_next(item))) {
            nxt = (pmix_event_hdlr_t*)item;
            if (!check_range(&nxt->rng, &chain->source)) {
                continue;
            }
            for (n=0; n < nxt->ncodes; n++) {
                /* if this event handler provided a range, check to see if
                 * the source fits within it */
                if (nxt->codes[n] == chain->status) {
                    chain->evhdlr = nxt;
                    /* update the handler name in case they want to reference it */
                    for (n=0; n < chain->ninfo; n++) {
                        if (0 == strncmp(chain->info[n].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
                            if (NULL != chain->info[n].value.data.string) {
                                free(chain->info[n].value.data.string);
                            }
                            if (NULL != chain->evhdlr->name) {
                                chain->info[n].value.data.string = strdup(chain->evhdlr->name);
                            }
                            break;
                        }
                    }
                    /* update the evhdlr cbobject */
                    for (n=0; n < chain->ninfo; n++) {
                        if (0 == strncmp(chain->info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                            if (NULL != chain->evhdlr->name) {
                                chain->info[n].value.data.ptr = chain->evhdlr->cbobject;
                            }
                            break;
                        }
                    }
                    nxt->evhdlr(nxt->index,
                                chain->status, &chain->source,
                                chain->info, chain->ninfo,
                                chain->results, chain->nresults,
                                progress_local_event_hdlr, (void*)chain);
                    return;
                }
            }
        }
        /* if we get here, then there are no more multi-mode
         * events that match */
        item = pmix_list_get_begin(&pmix_globals.events.default_events);
    }

    /* if they didn't want it to go to a default handler, then ignore them */
    if (!chain->nondefault) {
        if (NULL == item) {
            item = &chain->evhdlr->super;
        }
        if (pmix_list_get_end(&pmix_globals.events.default_events) != (item = pmix_list_get_next(item))) {
            nxt = (pmix_event_hdlr_t*)item;
            /* if this event handler provided a range, check to see if
             * the source fits within it */
            if (check_range(&nxt->rng, &chain->source)) {
                chain->evhdlr = nxt;
                /* update the handler name in case they want to reference it */
                for (n=0; n < chain->ninfo; n++) {
                    if (0 == strncmp(chain->info[n].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
                        if (NULL != chain->info[n].value.data.string) {
                            free(chain->info[n].value.data.string);
                        }
                        if (NULL != chain->evhdlr->name) {
                            chain->info[n].value.data.string = strdup(chain->evhdlr->name);
                        }
                        break;
                    }
                }
                /* update the evhdlr cbobject */
                for (n=0; n < chain->ninfo; n++) {
                    if (0 == strncmp(chain->info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                        if (NULL != chain->evhdlr->name) {
                            chain->info[n].value.data.ptr = chain->evhdlr->cbobject;
                        }
                        break;
                    }
                }
                nxt->evhdlr(nxt->index,
                            chain->status, &chain->source,
                            chain->info, chain->ninfo,
                            chain->results, chain->nresults,
                            progress_local_event_hdlr, (void*)chain);
                return;
            }
        }
    }

    /* if we registered a "last" handler, and it fits the given range
     * and code, then invoke it now */
    if (NULL != pmix_globals.events.last &&
        check_range(&pmix_globals.events.last->rng, &chain->source)) {
        chain->endchain = true;  // ensure we don't do this again
        if (1 == pmix_globals.events.last->ncodes &&
            pmix_globals.events.last->codes[0] == chain->status) {
            chain->evhdlr = pmix_globals.events.last;
            /* update the handler name in case they want to reference it */
            for (n=0; n < chain->ninfo; n++) {
                if (0 == strncmp(chain->info[n].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
                    if (NULL != chain->info[n].value.data.string) {
                        free(chain->info[n].value.data.string);
                    }
                    if (NULL != chain->evhdlr->name) {
                        chain->info[n].value.data.string = strdup(chain->evhdlr->name);
                    }
                    break;
                }
            }
            /* update the evhdlr cbobject */
            for (n=0; n < chain->ninfo; n++) {
                if (0 == strncmp(chain->info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                    if (NULL != chain->evhdlr->name) {
                        chain->info[n].value.data.ptr = chain->evhdlr->cbobject;
                    }
                    break;
                }
            }
            chain->evhdlr->evhdlr(chain->evhdlr->index,
                                  chain->status, &chain->source,
                                  chain->info, chain->ninfo,
                                  chain->results, chain->nresults,
                                  progress_local_event_hdlr, (void*)chain);
            return;
        } else if (NULL != pmix_globals.events.last->codes) {
            /* need to check if this code is included in the array */
            for (n=0; n < pmix_globals.events.last->ncodes; n++) {
                if (pmix_globals.events.last->codes[n] == chain->status) {
                    chain->evhdlr = pmix_globals.events.last;
                    /* update the handler name in case they want to reference it */
                    for (n=0; n < chain->ninfo; n++) {
                        if (0 == strncmp(chain->info[n].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
                            if (NULL != chain->info[n].value.data.string) {
                                free(chain->info[n].value.data.string);
                            }
                            if (NULL != chain->evhdlr->name) {
                                chain->info[n].value.data.string = strdup(chain->evhdlr->name);
                            }
                            break;
                        }
                    }
                    /* update the evhdlr cbobject */
                    for (n=0; n < chain->ninfo; n++) {
                        if (0 == strncmp(chain->info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                            if (NULL != chain->evhdlr->name) {
                                chain->info[n].value.data.ptr = chain->evhdlr->cbobject;
                            }
                            break;
                        }
                    }
                    chain->evhdlr->evhdlr(chain->evhdlr->index,
                                          chain->status, &chain->source,
                                          chain->info, chain->ninfo,
                                          chain->results, chain->nresults,
                                          progress_local_event_hdlr, (void*)chain);
                    return;
                }
            }
        } else {
            /* gets run for all codes */
            chain->evhdlr = pmix_globals.events.last;
            /* update the handler name in case they want to reference it */
            for (n=0; n < chain->ninfo; n++) {
                if (0 == strncmp(chain->info[n].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
                    if (NULL != chain->info[n].value.data.string) {
                        free(chain->info[n].value.data.string);
                    }
                    if (NULL != chain->evhdlr->name) {
                        chain->info[n].value.data.string = strdup(chain->evhdlr->name);
                    }
                    break;
                }
            }
            /* update the evhdlr cbobject */
            for (n=0; n < chain->ninfo; n++) {
                if (0 == strncmp(chain->info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                    if (NULL != chain->evhdlr->name) {
                        chain->info[n].value.data.ptr = chain->evhdlr->cbobject;
                    }
                    break;
                }
            }
            chain->evhdlr->evhdlr(chain->evhdlr->index,
                                  chain->status, &chain->source,
                                  chain->info, chain->ninfo,
                                  chain->results, chain->nresults,
                                  progress_local_event_hdlr, (void*)chain);
            return;
        }
    }

  complete:
    /* we still have to call their final callback */
    if (NULL != chain->final_cbfunc) {
        chain->final_cbfunc(PMIX_SUCCESS, chain->final_cbdata);
        return;
    }
    /* maintain acctng */
    PMIX_RELEASE(chain);
    /* let the caller know that we are done with their callback */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, thiscbdata);
    }
}

/* given notification of an event, cycle thru our list of
 * registered callbacks and invoke the matching ones. Note
 * that we will invoke the callbacks in order from single
 * to multi-event to default, keeping a log of any returned
 * info and passing it down to the next invoked event handler.
 * Thus, each handler is given the opportunity to see what
 * prior handlers did, and decide if anything further needs
 * to be done.
 */
void pmix_invoke_local_event_hdlr(pmix_event_chain_t *chain)
{
    /* We need to parse thru each registered handler and determine
     * which one(s) to call for the specific error */
    size_t i;
    pmix_event_hdlr_t *evhdlr;
    pmix_status_t rc = PMIX_SUCCESS;
    bool found;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s:%d invoke_local_event_hdlr for status %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        PMIx_Error_string(chain->status));

    /* sanity check */
    if (NULL == chain->info) {
        /* should never happen as the return object must
         * at least be there, even if it is NULL */
        rc = PMIX_ERR_BAD_PARAM;
        goto complete;
    }

    /* check for directives */
    for (i=0; i < chain->ninfo; i++) {
        if (0 == strncmp(chain->info[i].key, PMIX_EVENT_NON_DEFAULT, PMIX_MAX_KEYLEN)) {
            chain->nondefault = true;
        }
    }

    /* if we registered a "first" handler, and it fits the given range,
     * then invoke it first */
    if (NULL != pmix_globals.events.first) {
        if (1 == pmix_globals.events.first->ncodes &&
            pmix_globals.events.first->codes[0] == chain->status &&
            check_range(&pmix_globals.events.first->rng, &chain->source)) {
            /* invoke the handler */
            chain->evhdlr = pmix_globals.events.first;
            goto invk;
        } else if (NULL != pmix_globals.events.first->codes) {
            /* need to check if this code is included in the array */
            found = false;
            for (i=0; i < pmix_globals.events.first->ncodes; i++) {
                if (pmix_globals.events.first->codes[i] == chain->status) {
                    found = true;
                    break;
                }
            }
            /* if this event handler provided a range, check to see if
             * the source fits within it */
            if (found && check_range(&pmix_globals.events.first->rng, &chain->source)) {
                /* invoke the handler */
                chain->evhdlr = pmix_globals.events.first;
                goto invk;
            }
        } else {
            /* take all codes for a default handler */
            if (check_range(&pmix_globals.events.first->rng, &chain->source)) {
                /* invoke the handler */
                chain->evhdlr = pmix_globals.events.first;
                goto invk;
            }
        }
        /* get here if there is no match, so fall thru */
    }

    /* cycle thru the single-event registrations first */
    PMIX_LIST_FOREACH(evhdlr, &pmix_globals.events.single_events, pmix_event_hdlr_t) {
        if (evhdlr->codes[0] == chain->status) {
            if (check_range(&evhdlr->rng, &chain->source)) {
                /* invoke the handler */
                chain->evhdlr = evhdlr;
                goto invk;
            }
        }
    }

    /* if we didn't find any match in the single-event registrations,
     * then cycle thru the multi-event registrations next */
    PMIX_LIST_FOREACH(evhdlr, &pmix_globals.events.multi_events, pmix_event_hdlr_t) {
        for (i=0; i < evhdlr->ncodes; i++) {
            if (evhdlr->codes[i] == chain->status) {
                if (check_range(&evhdlr->rng, &chain->source)) {
                    /* invoke the handler */
                    chain->evhdlr = evhdlr;
                    goto invk;
                }
            }
        }
    }

    /* if they didn't want it to go to a default handler, then ignore them */
    if (!chain->nondefault) {
        /* pass it to any default handlers */
        PMIX_LIST_FOREACH(evhdlr, &pmix_globals.events.default_events, pmix_event_hdlr_t) {
            if (check_range(&evhdlr->rng, &chain->source)) {
                /* invoke the handler */
                chain->evhdlr = evhdlr;
                goto invk;
            }
        }
    }

    /* if we registered a "last" handler, and it fits the given range
     * and code, then invoke it now */
    if (NULL != pmix_globals.events.last &&
        check_range(&pmix_globals.events.last->rng, &chain->source)) {
        chain->endchain = true;  // ensure we don't do this again
        if (1 == pmix_globals.events.last->ncodes &&
            pmix_globals.events.last->codes[0] == chain->status) {
            chain->evhdlr = pmix_globals.events.last;
            goto invk;
        } else if (NULL != pmix_globals.events.last->codes) {
            /* need to check if this code is included in the array */
            for (i=0; i < pmix_globals.events.last->ncodes; i++) {
                if (pmix_globals.events.last->codes[i] == chain->status) {
                    chain->evhdlr = pmix_globals.events.last;
                    goto invk;
                }
            }
        } else {
            /* gets run for all codes */
            chain->evhdlr = pmix_globals.events.last;
            goto invk;
        }
    }

    /* if we got here, then nothing was found */

  complete:
    /* we still have to call their final callback */
    if (NULL != chain->final_cbfunc) {
        chain->final_cbfunc(rc, chain->final_cbdata);
    }
    return;


  invk:
    /* update the handler name in case they want to reference it */
    for (i=0; i < chain->ninfo; i++) {
        if (0 == strncmp(chain->info[i].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
            if (NULL != chain->info[i].value.data.string) {
                free(chain->info[i].value.data.string);
            }
            if (NULL != chain->evhdlr->name) {
                chain->info[i].value.data.string = strdup(chain->evhdlr->name);
            }
            break;
        }
    }
    /* update the evhdlr cbobject */
    for (i=0; i < chain->ninfo; i++) {
        if (0 == strncmp(chain->info[i].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
            if (NULL != chain->evhdlr->name) {
                chain->info[i].value.data.ptr = chain->evhdlr->cbobject;
            }
            break;
        }
    }
    /* invoke the handler */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "[%s:%d] INVOKING EVHDLR %s", __FILE__, __LINE__,
                        (NULL == chain->evhdlr->name) ?
                        "NULL" : chain->evhdlr->name);
    chain->evhdlr->evhdlr(chain->evhdlr->index,
                          chain->status, &chain->source,
                          chain->info, chain->ninfo,
                          NULL, 0,
                          progress_local_event_hdlr, (void*)chain);
    return;
}

static void local_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t*)cbdata;

    if (NULL != cd->cbfunc) {
        cd->cbfunc(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

static void _notify_client_event(int sd, short args, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t*)cbdata;
    pmix_notify_caddy_t *rbout;
    pmix_regevents_info_t *reginfoptr;
    pmix_peer_events_info_t *pr;
    pmix_event_chain_t *chain;
    size_t n;
    bool matched, holdcd;
    pmix_buffer_t *bfr;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;
    pmix_status_t rc;
    pmix_list_t trk;
    pmix_namelist_t *nm;

    /* need to acquire the object from its originating thread */
    PMIX_ACQUIRE_OBJECT(cd);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_server: _notify_client_event notifying clients of event %s range %s type %s",
                        PMIx_Error_string(cd->status),
                        PMIx_Data_range_string(cd->range),
                        cd->nondefault ? "NONDEFAULT" : "OPEN");

    /* check for caching instructions */
    holdcd = true;
    if (0 < cd->ninfo) {
        /* check for caching instructions */
        for (n=0; n < cd->ninfo; n++) {
            if (0 == strncmp(cd->info[n].key, PMIX_EVENT_DO_NOT_CACHE, PMIX_MAX_KEYLEN)) {
                if (PMIX_INFO_TRUE(&cd->info[n])) {
                    holdcd = false;
                }
                break;
            }
        }
    }

    if (holdcd) {
        /* we cannot know if everyone who wants this notice has had a chance
         * to register for it - the notice may be coming too early. So cache
         * the message until all local procs have received it, or it ages to
         * the point where it gets pushed out by more recent events */
        PMIX_RETAIN(cd);
        rbout = pmix_ring_buffer_push(&pmix_globals.notifications, cd);

        /* if an older event was bumped, release it */
        if (NULL != rbout) {
            PMIX_RELEASE(rbout);
        }
    }

    holdcd = false;
    if (PMIX_RANGE_PROC_LOCAL != cd->range) {
        PMIX_CONSTRUCT(&trk, pmix_list_t);
        /* cycle across our registered events and send the message to
         * any client who registered for it */
        PMIX_LIST_FOREACH(reginfoptr, &pmix_server_globals.events, pmix_regevents_info_t) {
            if ((PMIX_MAX_ERR_CONSTANT == reginfoptr->code && !cd->nondefault) ||
                cd->status == reginfoptr->code) {
                PMIX_LIST_FOREACH(pr, &reginfoptr->peers, pmix_peer_events_info_t) {
                    /* if this client was the source of the event, then
                     * don't send it back as they will have processed it
                     * when they generated it */
                    if (0 == strncmp(cd->source.nspace, pr->peer->info->pname.nspace, PMIX_MAX_NSLEN) &&
                        cd->source.rank == pr->peer->info->pname.rank) {
                        continue;
                    }
                    /* if we have already notified this client, then don't do it again */
                    matched = false;
                    PMIX_LIST_FOREACH(nm, &trk, pmix_namelist_t) {
                        if (nm->pname == &pr->peer->info->pname) {
                            matched = true;
                            break;
                        }
                    }
                    if (matched) {
                        continue;
                    }
                    /* if we were given specific targets, check if this is one */
                    if (NULL != cd->targets) {
                        matched = false;
                        for (n=0; n < cd->ntargets; n++) {
                            if (0 != strncmp(pr->peer->info->pname.nspace, cd->targets[n].nspace, PMIX_MAX_NSLEN)) {
                                continue;
                            }
                            if (PMIX_RANK_WILDCARD == cd->targets[n].rank ||
                                pr->peer->info->pname.rank == cd->targets[n].rank) {
                                matched = true;
                                break;
                            }
                        }
                        if (!matched) {
                            /* do not notify this one */
                            continue;
                        }
                    }
                    pmix_output_verbose(2, pmix_globals.debug_output,
                                        "pmix_server: notifying client %s:%u on status %s",
                                        pr->peer->info->pname.nspace, pr->peer->info->pname.rank,
                                        PMIx_Error_string(cd->status));

                    /* record that we notified this client */
                    nm = PMIX_NEW(pmix_namelist_t);
                    nm->pname = &pr->peer->info->pname;
                    pmix_list_append(&trk, &nm->super);

                    bfr = PMIX_NEW(pmix_buffer_t);
                    if (NULL == bfr) {
                        continue;
                    }
                    /* pack the command */
                    PMIX_BFROPS_PACK(rc, pr->peer, bfr, &cmd, 1, PMIX_COMMAND);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(bfr);
                        continue;
                    }

                    /* pack the status */
                    PMIX_BFROPS_PACK(rc, pr->peer, bfr, &cd->status, 1, PMIX_STATUS);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(bfr);
                        continue;
                    }

                    /* pack the source */
                    PMIX_BFROPS_PACK(rc, pr->peer, bfr, &cd->source, 1, PMIX_PROC);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(bfr);
                        continue;
                    }
                    /* pack any info */
                    PMIX_BFROPS_PACK(rc, pr->peer, bfr, &cd->ninfo, 1, PMIX_SIZE);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(bfr);
                        continue;
                    }

                    if (0 < cd->ninfo) {
                        PMIX_BFROPS_PACK(rc, pr->peer, bfr, cd->info, cd->ninfo, PMIX_INFO);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_ERROR_LOG(rc);
                            PMIX_RELEASE(bfr);
                            continue;
                        }
                    }
                    PMIX_SERVER_QUEUE_REPLY(pr->peer, 0, bfr);
                }
            }
        }
        PMIX_LIST_DESTRUCT(&trk);
        if (PMIX_RANGE_LOCAL != cd->range &&
            0 == strncmp(cd->source.nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN) &&
            cd->source.rank == pmix_globals.myid.rank) {
            /* if we are the source, then we need to post this upwards as
             * well so the host RM can broadcast it as necessary - we rely
             * on the host RM to _not_ deliver this back to us! */
            if (NULL != pmix_host_server.notify_event) {
                /* mark that we sent it upstairs so we don't release
                 * the caddy until we return from the host RM */
                holdcd = true;
                pmix_host_server.notify_event(cd->status, &cd->source, cd->range,
                                              cd->info, cd->ninfo, local_cbfunc, cd);
            }

        }
    }

    /* we may also have registered for events, so be sure to check this
     * against our registrations */
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->status = cd->status;
    (void)strncpy(chain->source.nspace, cd->source.nspace, PMIX_MAX_NSLEN);
    chain->source.rank = cd->source.rank;
    /* we always leave space for a callback object and
     * the evhandler name. */
    chain->ninfo = cd->ninfo + 2;
    PMIX_INFO_CREATE(chain->info, chain->ninfo);
    if (0 < cd->ninfo) {
        /* need to copy the info */
        for (n=0; n < cd->ninfo; n++) {
            PMIX_INFO_XFER(&chain->info[n], &cd->info[n]);
        }
    }
    /* put the evhandler name tag in the next-to-last element - we
     * will fill it in as each handler is called */
    PMIX_INFO_LOAD(&chain->info[chain->ninfo-2], PMIX_EVENT_HDLR_NAME, NULL, PMIX_STRING);
    /* now put the callback object tag in the last element */
    PMIX_INFO_LOAD(&chain->info[chain->ninfo-1], PMIX_EVENT_RETURN_OBJECT, NULL, PMIX_POINTER);
    /* process it */
    pmix_invoke_local_event_hdlr(chain);

    if (!holdcd) {
        /* notify the caller */
        if (NULL != cd->cbfunc) {
            cd->cbfunc(PMIX_SUCCESS, cd->cbdata);
        }
        PMIX_RELEASE(cd);
    }
}


/* as a server, we must do two things:
 *
 * (a) notify all clients that have registered for this event
 *
 * (b) callback any of our own functions that have registered
 *     for this event
 */
pmix_status_t pmix_server_notify_client_of_event(pmix_status_t status,
                                                 const pmix_proc_t *source,
                                                 pmix_data_range_t range,
                                                 pmix_info_t info[], size_t ninfo,
                                                 pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_notify_caddy_t *cd;
    size_t n;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_server: notify client of event %s",
                        PMIx_Error_string(status));

    cd = PMIX_NEW(pmix_notify_caddy_t);
    cd->status = status;
    if (NULL == source) {
        (void)strncpy(cd->source.nspace, "UNDEF", PMIX_MAX_NSLEN);
        cd->source.rank = PMIX_RANK_UNDEF;
    } else {
        (void)strncpy(cd->source.nspace, source->nspace, PMIX_MAX_NSLEN);
        cd->source.rank = source->rank;
    }
    cd->range = range;
    /* have to copy the info to preserve it for future when cached */
    if (0 < ninfo) {
        cd->ninfo = ninfo;
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        /* need to copy the info */
        for (n=0; n < cd->ninfo; n++) {
            PMIX_INFO_XFER(&cd->info[n], &info[n]);
        }
    }

    /* check for directives */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_EVENT_NON_DEFAULT, PMIX_MAX_KEYLEN)) {
                cd->nondefault = true;
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_CUSTOM_RANGE, PMIX_MAX_KEYLEN)) {
                /* provides an array of pmix_proc_t identifying the procs
                 * that are to receive this notification, or a single pmix_proc_t  */
                if (PMIX_DATA_ARRAY == info[n].value.type &&
                    NULL != info[n].value.data.darray &&
                    NULL != info[n].value.data.darray->array) {
                    cd->ntargets = info[n].value.data.darray->size;
                    PMIX_PROC_CREATE(cd->targets, cd->ntargets);
                    memcpy(cd->targets, info[n].value.data.darray->array, cd->ntargets * sizeof(pmix_proc_t));
                } else if (PMIX_PROC == info[n].value.type) {
                    cd->ntargets = 1;
                    PMIX_PROC_CREATE(cd->targets, cd->ntargets);
                    memcpy(cd->targets, info[n].value.data.proc, sizeof(pmix_proc_t));
                } else {
                    /* this is an error */
                    PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                    return PMIX_ERR_BAD_PARAM;
                }
            }
        }
    }
    /*
     * If the range is PMIX_RANGE_NAMESPACE, then they should not have set a
     * PMIX_EVENT_CUSTOM_RANGE info object or at least we should ignore it
     */
    if (PMIX_RANGE_NAMESPACE == cd->range) {
        if (cd->targets) {
            PMIX_PROC_FREE(cd->targets, cd->ntargets);
        }
        PMIX_PROC_CREATE(cd->targets, 1);
        cd->ntargets = 1;
        cd->targets[0].rank = PMIX_RANK_WILDCARD;
        if (NULL == source) {
            strncpy(cd->targets[0].nspace, "UNDEF", PMIX_MAX_NSLEN);
        } else {
            strncpy(cd->targets[0].nspace, source->nspace, PMIX_MAX_NSLEN);
        }
    }

    /* track the eventual callback info */
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_server_notify_event status =%d, source = %s:%d, ninfo =%lu",
                         status, cd->source.nspace, cd->source.rank, ninfo);

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _notify_client_event);
    return PMIX_SUCCESS;
}

static bool check_range(pmix_range_trkr_t *rng,
                        const pmix_proc_t *proc)
{
    size_t n;

    if (PMIX_RANGE_UNDEF == rng->range ||
        PMIX_RANGE_GLOBAL == rng->range ||
        PMIX_RANGE_SESSION == rng->range ||
        PMIX_RANGE_LOCAL == rng->range) { // assume RM took care of session & local for now
        return true;
    }
    if (PMIX_RANGE_NAMESPACE == rng->range) {
        if (0 == strncmp(pmix_globals.myid.nspace, proc->nspace, PMIX_MAX_NSLEN)) {
            return true;
        }
        return false;
    }
    if (PMIX_RANGE_PROC_LOCAL == rng->range) {
        if (0 == strncmp(pmix_globals.myid.nspace, proc->nspace, PMIX_MAX_NSLEN) &&
            pmix_globals.myid.rank == proc->rank) {
            return true;
        }
        return false;
    }
    if (PMIX_RANGE_CUSTOM == rng->range) {
        if (NULL != rng->procs) {
            /* see if this proc was included */
            for (n=0; n < rng->nprocs; n++) {
                if (0 != strncmp(rng->procs[n].nspace, proc->nspace, PMIX_MAX_NSLEN)) {
                    continue;
                }
                if (PMIX_RANK_WILDCARD == rng->procs[n].rank ||
                    rng->procs[n].rank == proc->rank) {
                    return true;
                }
            }
            /* if we get here, then this proc isn't in range */
            return false;
        } else {
            /* if they didn't give us a list, then assume
             * everyone included */
            return true;
        }
    }

    /* if it is anything else, then reject it */
    return false;
}

void pmix_event_timeout_cb(int fd, short flags, void *arg)
{
    pmix_event_chain_t *ch = (pmix_event_chain_t*)arg;

    /* need to acquire the object from its originating thread */
    PMIX_ACQUIRE_OBJECT(ch);

    ch->timer_active = false;

    /* remove it from the list */
    pmix_list_remove_item(&pmix_globals.cached_events, &ch->super);

    /* process this event thru the regular channels */
    if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
        pmix_server_notify_client_of_event(ch->status, &ch->source,
                                           ch->range, ch->info, ch->ninfo,
                                           ch->final_cbfunc, ch->final_cbdata);
    } else {
        pmix_invoke_local_event_hdlr(ch);
    }
}

/****    CLASS INSTANTIATIONS    ****/

static void sevcon(pmix_event_hdlr_t *p)
{
    p->name = NULL;
    p->index = UINT_MAX;
    p->precedence = PMIX_EVENT_ORDER_NONE;
    p->locator = NULL;
    p->rng.range = PMIX_RANGE_UNDEF;
    p->rng.procs = NULL;
    p->rng.nprocs = 0;
    p->evhdlr = NULL;
    p->cbobject = NULL;
    p->codes = NULL;
    p->ncodes = 0;
}
static void sevdes(pmix_event_hdlr_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->locator) {
        free(p->locator);
    }
    if (NULL != p->rng.procs) {
        free(p->rng.procs);
    }
    if (NULL != p->codes) {
        free(p->codes);
    }
}
PMIX_CLASS_INSTANCE(pmix_event_hdlr_t,
                    pmix_list_item_t,
                    sevcon, sevdes);

static void accon(pmix_active_code_t *p)
{
    p->nregs = 0;
}
PMIX_CLASS_INSTANCE(pmix_active_code_t,
                    pmix_list_item_t,
                    accon, NULL);

static void evcon(pmix_events_t *p)
{
    p->nhdlrs = 0;
    p->first = NULL;
    p->last = NULL;
    PMIX_CONSTRUCT(&p->actives, pmix_list_t);
    PMIX_CONSTRUCT(&p->single_events, pmix_list_t);
    PMIX_CONSTRUCT(&p->multi_events, pmix_list_t);
    PMIX_CONSTRUCT(&p->default_events, pmix_list_t);
}
static void evdes(pmix_events_t *p)
{
    if (NULL != p->first) {
        PMIX_RELEASE(p->first);
    }
    if (NULL != p->last) {
        PMIX_RELEASE(p->last);
    }
    PMIX_LIST_DESTRUCT(&p->actives);
    PMIX_LIST_DESTRUCT(&p->single_events);
    PMIX_LIST_DESTRUCT(&p->multi_events);
    PMIX_LIST_DESTRUCT(&p->default_events);
}
PMIX_CLASS_INSTANCE(pmix_events_t,
                    pmix_object_t,
                    evcon, evdes);

static void chcon(pmix_event_chain_t *p)
{
    p->timer_active = false;
    memset(p->source.nspace, 0, PMIX_MAX_NSLEN+1);
    p->source.rank = PMIX_RANK_UNDEF;
    p->nondefault = false;
    p->endchain = false;
    p->range = PMIX_RANGE_UNDEF;
    p->info = NULL;
    p->ninfo = 0;
    p->results = NULL;
    p->nresults = 0;
    p->evhdlr = NULL;
    p->final_cbfunc = NULL;
    p->final_cbdata = NULL;
}
static void chdes(pmix_event_chain_t *p)
{
    if (p->timer_active) {
        pmix_event_del(&p->ev);
    }
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
    if (NULL != p->results) {
        PMIX_INFO_FREE(p->results, p->nresults);
    }
}
PMIX_CLASS_INSTANCE(pmix_event_chain_t,
                    pmix_list_item_t,
                    chcon, chdes);

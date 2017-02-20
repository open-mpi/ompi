/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#include "src/util/error.h"
#include "src/util/output.h"

#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

static pmix_status_t notify_server_of_event(pmix_status_t status,
                                            const pmix_proc_t *source,
                                            pmix_data_range_t range,
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata);

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

    if (PMIX_PROC_SERVER == pmix_globals.proc_type) {
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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ret, &cnt, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    }
    /* do the cback */
    if (NULL != cb->op_cbfunc) {
        cb->op_cbfunc(ret, cb->cbdata);
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
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;
    pmix_cb_t *cb;
    pmix_event_chain_t *chain;
    size_t n;


    pmix_output_verbose(2, pmix_globals.debug_output,
                        "client: notifying server %s:%d of status %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        PMIx_Error_string(status));

    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }
    /* create the msg object */
    msg = PMIX_NEW(pmix_buffer_t);

    /* pack the command */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* pack the status */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* no need to pack the source as it is us */

    /* pack the range */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &range, 1, PMIX_DATA_RANGE))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* pack the info */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    if (0 < ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* setup for our own local callbacks */
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->status = status;
    (void)strncpy(chain->source.nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
    chain->source.rank = pmix_globals.myid.rank;
    /* we always leave space for a callback object */
    chain->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(chain->info, chain->ninfo);

    if (0 < ninfo) {
        /* need to copy the info */
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&chain->info[n], &info[n]);
        }
    }
    /* now put the callback object tag in the last element */
    PMIX_INFO_LOAD(&chain->info[ninfo], PMIX_EVENT_RETURN_OBJECT, NULL, PMIX_POINTER);

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the server acks/nacks the register events request*/
    cb = PMIX_NEW(pmix_cb_t);
    cb->op_cbfunc = cbfunc;
    cb->cbdata = cbdata;
    /* send to the server */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "client: notifying server %s:%d - sending",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank);
    rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, msg, notify_event_cbfunc, cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cb);
        goto cleanup;
    }

    /* now notify any matching registered callbacks we have */
    pmix_invoke_local_event_hdlr(chain);
    PMIX_RELEASE(chain); // maintain accounting

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
    pmix_event_chain_t *chain = (pmix_event_chain_t*)notification_cbdata;
    size_t n, nsave;
    pmix_info_t *newinfo;
    pmix_list_item_t *nxt;
    pmix_single_event_t *sing;
    pmix_multi_event_t *multi;
    pmix_default_event_t *def;

    /* if the caller indicates that the chain is completed, then stop here */
    if (PMIX_EVENT_ACTION_COMPLETE == status) {
        goto complete;
    }

    /* save the current number of results */
    nsave = chain->nresults;
    /* create the new space */
    PMIX_INFO_CREATE(newinfo, chain->nresults + nresults + 1);
    /* transfer over the prior data */
    for (n=0; n < chain->nresults; n++) {
        PMIX_INFO_XFER(&newinfo[n], &chain->results[n]);
    }
    /* save this handler's response */
    if (NULL != chain->sing) {
        if (NULL != chain->sing->name) {
            (void)strncpy(newinfo[nsave].key, chain->sing->name, PMIX_MAX_KEYLEN);
        }
    } else if (NULL != chain->multi) {
        if (NULL != chain->multi->name) {
            (void)strncpy(newinfo[nsave].key, chain->multi->name, PMIX_MAX_KEYLEN);
        }
    } else if (NULL != chain->def) {
        if (NULL != chain->def->name) {
            (void)strncpy(newinfo[nsave].key, chain->def->name, PMIX_MAX_KEYLEN);
        }
    } else {
        (void)strncpy(newinfo[nsave].key, "UNKNOWN", PMIX_MAX_KEYLEN);
    }
    newinfo[nsave].value.type = PMIX_STATUS;
    newinfo[nsave].value.data.status = status;
    /* transfer across the new results */
    for (n=0; n < nresults; n++) {
        PMIX_INFO_XFER(&newinfo[n+nsave+1], &results[n]);
    }
    /* release the prior results */
    if (0 < chain->nresults) {
        PMIX_INFO_FREE(chain->results, chain->nresults);
    }
    /* pass along the new ones */
    chain->results = newinfo;
    chain->nresults = nsave + nresults;

    /* see if we need to continue, starting with the single code events */
    if (NULL != chain->sing) {
        /* the last handler was for a single code - see if there are
         * any others that match this event */
        while (pmix_list_get_end(&pmix_globals.events.single_events) != (nxt = pmix_list_get_next(&chain->sing->super))) {
            sing = (pmix_single_event_t*)nxt;
            if (sing->code == chain->status) {
                chain->sing = sing;
                /* add any cbobject - the info struct for it is at the end */
                chain->info[chain->ninfo-1].value.data.ptr = sing->cbobject;
                sing->evhdlr(sing->index,
                             chain->status, &chain->source,
                             chain->info, chain->ninfo,
                             chain->results, chain->nresults,
                             progress_local_event_hdlr, (void*)chain);
                goto complete;
            }
        }
        /* if we get here, then there are no more single code
         * events that match */
        chain->sing = NULL;
        /* pickup the beginning of the multi-code event list */
        chain->multi = (pmix_multi_event_t*)pmix_list_get_begin(&pmix_globals.events.multi_events);
    }

    /* see if we need to continue with the multi code events */
    if (NULL != chain->multi) {
        while (pmix_list_get_end(&pmix_globals.events.multi_events) != (nxt = pmix_list_get_next(&chain->multi->super))) {
            multi = (pmix_multi_event_t*)nxt;
            for (n=0; n < multi->ncodes; n++) {
                if (multi->codes[n] == chain->status) {
                    /* found it - invoke the handler, pointing its
                     * callback function to our progression function */
                    chain->multi = multi;
                    /* add any cbobject - the info struct for it is at the end */
                    chain->info[chain->ninfo-1].value.data.ptr = multi->cbobject;
                    multi->evhdlr(multi->index,
                                  chain->status, &chain->source,
                                  chain->info, chain->ninfo,
                                  chain->results, chain->nresults,
                                  progress_local_event_hdlr, (void*)chain);
                    goto complete;
                }
            }
        }
        /* if we get here, then there are no more multi-mode
         * events that match */
        chain->multi = NULL;
        /* pickup the beginning of the default event list */
        chain->def = (pmix_default_event_t*)pmix_list_get_begin(&pmix_globals.events.default_events);
    }

    /* if they didn't want it to go to a default handler, then we are done */
    if (chain->nondefault) {
        goto complete;
    }

    if (NULL != chain->def) {
        if (pmix_list_get_end(&pmix_globals.events.default_events) != (nxt = pmix_list_get_next(&chain->def->super))) {
            def = (pmix_default_event_t*)nxt;
            chain->def = def;
            /* add any cbobject - the info struct for it is at the end */
            chain->info[chain->ninfo-1].value.data.ptr = def->cbobject;
            def->evhdlr(def->index,
                        chain->status, &chain->source,
                        chain->info, chain->ninfo,
                        chain->results, chain->nresults,
                        progress_local_event_hdlr, (void*)chain);
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
    pmix_single_event_t *sing;
    pmix_multi_event_t *multi;
    pmix_default_event_t *def;
    pmix_status_t rc = PMIX_SUCCESS;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s:%d invoke_local_event_hdlr",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank);

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

    /* cycle thru the single-event registrations first */
    PMIX_LIST_FOREACH(sing, &pmix_globals.events.single_events, pmix_single_event_t) {
        if (sing->code == chain->status) {
            /* found it - invoke the handler, pointing its
             * callback function to our progression function */
            chain->sing = sing;
            /* add any cbobject - the info struct for it is at the end */
            chain->info[chain->ninfo-1].value.data.ptr = sing->cbobject;
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "[%s:%d] CALLING SINGLE EVHDLR",
                                pmix_globals.myid.nspace, pmix_globals.myid.rank);
            sing->evhdlr(sing->index,
                         chain->status, &chain->source,
                         chain->info, chain->ninfo,
                         NULL, 0,
                         progress_local_event_hdlr, (void*)chain);
            return;
        }
    }

    /* if we didn't find any match in the single-event registrations,
     * then cycle thru the multi-event registrations next */
    PMIX_LIST_FOREACH(multi, &pmix_globals.events.multi_events, pmix_multi_event_t) {
        for (i=0; i < multi->ncodes; i++) {
            if (multi->codes[i] == chain->status) {
                /* found it - invoke the handler, pointing its
                 * callback function to our progression function */
                chain->multi = multi;
                /* add any cbobject - the info struct for it is at the end */
                chain->info[chain->ninfo-1].value.data.ptr = multi->cbobject;
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "[%s:%d] CALLING MULTI EVHDLR",
                                    pmix_globals.myid.nspace, pmix_globals.myid.rank);
                multi->evhdlr(multi->index,
                              chain->status, &chain->source,
                              chain->info, chain->ninfo,
                              NULL, 0,
                              progress_local_event_hdlr, (void*)chain);
                return;
            }
        }
    }

    /* if they didn't want it to go to a default handler, then we are done */
    if (chain->nondefault) {
        goto complete;
    }

    /* finally, pass it to any default handlers */
    PMIX_LIST_FOREACH(def, &pmix_globals.events.default_events, pmix_default_event_t) {
        chain->def = def;
        /* add any cbobject - the info struct for it is at the end */
        chain->info[chain->ninfo-1].value.data.ptr = def->cbobject;
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "[%s:%d] CALLING DEFAULT EVHDLR", __FILE__, __LINE__);
        def->evhdlr(def->index,
                    chain->status, &chain->source,
                    chain->info, chain->ninfo,
                    NULL, 0,
                    progress_local_event_hdlr, (void*)chain);
        return;
    }

  complete:
    /* we still have to call their final callback */
    if (NULL != chain->final_cbfunc) {
        chain->final_cbfunc(rc, chain->final_cbdata);
    }
    return;
}


static void _notify_client_event(int sd, short args, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t*)cbdata;
    pmix_notify_caddy_t *rbout;
    pmix_regevents_info_t *reginfoptr;
    pmix_peer_events_info_t *pr;
    size_t n;
    bool matched;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_server: _notify_error notifying clients of error %s",
                        PMIx_Error_string(cd->status));

    /* we cannot know if everyone who wants this notice has had a chance
     * to register for it - the notice may be coming too early. So cache
     * the message until all local procs have received it, or it ages to
     * the point where it gets pushed out by more recent events */
    PMIX_RETAIN(cd);
    rbout = pmix_ring_buffer_push(&pmix_server_globals.notifications, cd);

   /* if an older event was bumped, release it */
    if (NULL != rbout) {
        PMIX_RELEASE(rbout);
    }

    /* cycle across our registered events and send the message to
     * any client who registered for it */
    PMIX_LIST_FOREACH(reginfoptr, &pmix_server_globals.events, pmix_regevents_info_t) {
        if ((PMIX_MAX_ERR_CONSTANT == reginfoptr->code && !cd->nondefault) ||
            cd->status == reginfoptr->code) {
            PMIX_LIST_FOREACH(pr, &reginfoptr->peers, pmix_peer_events_info_t) {
                /* if this client was the source of the event, then
                 * don't send it back */
                if (0 == strncmp(cd->source.nspace, pr->peer->info->nptr->nspace, PMIX_MAX_NSLEN) &&
                    cd->source.rank == pr->peer->info->rank) {
                    continue;
                }
                /* if we were given specific targets, check if this is one */
                if (NULL != cd->targets) {
                    matched = false;
                    for (n=0; n < cd->ntargets; n++) {
                        if (0 != strncmp(pr->peer->info->nptr->nspace, cd->targets[n].nspace, PMIX_MAX_NSLEN)) {
                            continue;
                        }
                        if (PMIX_RANK_WILDCARD == cd->targets[n].rank ||
                            pr->peer->info->rank == cd->targets[n].rank) {
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
                                    "pmix_server: notifying client %s:%d",
                                    pr->peer->info->nptr->nspace, pr->peer->info->rank);
                PMIX_RETAIN(cd->buf);
                PMIX_SERVER_QUEUE_REPLY(pr->peer, 0, cd->buf);
            }
        }
    }

    /* notify the caller */
    if (NULL != cd->cbfunc) {
        cd->cbfunc(PMIX_SUCCESS, cd->cbdata);
    }
   PMIX_RELEASE(cd);
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
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;
    pmix_status_t rc;
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

    /* pack the command */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }

    /* pack the status */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, &status, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }

    /* pack the source */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, source, 1, PMIX_PROC))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }

    /* pack any info */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }

    if (0 < ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(cd->buf, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
            return rc;
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

static void sevcon(pmix_single_event_t *p)
{
    p->name = NULL;
    p->evhdlr = NULL;
    p->cbobject = NULL;
}
static void sevdes(pmix_single_event_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
}
PMIX_CLASS_INSTANCE(pmix_single_event_t,
                    pmix_list_item_t,
                    sevcon, sevdes);

static void mevcon(pmix_multi_event_t *p)
{
    p->name = NULL;
    p->codes = NULL;
    p->ncodes = 0;
    p->evhdlr = NULL;
    p->cbobject = NULL;
}
static void mevdes(pmix_multi_event_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->codes) {
        free(p->codes);
    }
}
PMIX_CLASS_INSTANCE(pmix_multi_event_t,
                    pmix_list_item_t,
                    mevcon, mevdes);

static void devcon(pmix_default_event_t *p)
{
    p->name = NULL;
    p->evhdlr = NULL;
    p->cbobject = NULL;
}
static void devdes(pmix_default_event_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
}
PMIX_CLASS_INSTANCE(pmix_default_event_t,
                    pmix_list_item_t,
                    devcon, devdes);

PMIX_CLASS_INSTANCE(pmix_active_code_t,
                    pmix_list_item_t,
                    NULL, NULL);

static void evcon(pmix_events_t *p)
{
    p->nhdlrs = 0;
    PMIX_CONSTRUCT(&p->actives, pmix_list_t);
    PMIX_CONSTRUCT(&p->single_events, pmix_list_t);
    PMIX_CONSTRUCT(&p->multi_events, pmix_list_t);
    PMIX_CONSTRUCT(&p->default_events, pmix_list_t);
}
static void evdes(pmix_events_t *p)
{
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
    memset(p->source.nspace, 0, PMIX_MAX_NSLEN+1);
    p->source.rank = PMIX_RANK_UNDEF;
    p->nondefault = false;
    p->range = PMIX_RANGE_UNDEF;
    p->info = NULL;
    p->ninfo = 0;
    p->results = NULL;
    p->nresults = 0;
    p->sing = NULL;
    p->multi = NULL;
    p->def = NULL;
    p->final_cbfunc = NULL;
    p->final_cbdata = NULL;
}
static void chdes(pmix_event_chain_t *p)
{
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
    if (NULL != p->results) {
        PMIX_INFO_FREE(p->results, p->nresults);
    }
}
PMIX_CLASS_INSTANCE(pmix_event_chain_t,
                    pmix_object_t,
                    chcon, chdes);

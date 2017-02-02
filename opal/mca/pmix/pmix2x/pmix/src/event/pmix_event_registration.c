/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
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

 typedef struct {
    pmix_object_t super;
    size_t index;
    pmix_list_t *list;
    pmix_list_item_t *item;
    pmix_shift_caddy_t *cd;
    pmix_status_t *codes;
    size_t ncodes;
    pmix_info_t *info;
    size_t ninfo;
} pmix_rshift_caddy_t;
static void rscon(pmix_rshift_caddy_t *p)
{
    p->list = NULL;
    p->item = NULL;
    p->cd = NULL;
    p->codes = NULL;
    p->ncodes = 0;
    p->info = NULL;
    p->ninfo = 0;
}
static void rsdes(pmix_rshift_caddy_t *p)
{
    if (NULL != p->cd) {
        PMIX_RELEASE(p->cd);
    }
    if (NULL != p->codes) {
        free(p->codes);
    }
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
}
PMIX_CLASS_INSTANCE(pmix_rshift_caddy_t,
                    pmix_object_t,
                    rscon, rsdes);


static void regevents_cbfunc(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                             pmix_buffer_t *buf, void *cbdata)
{
    pmix_rshift_caddy_t *rb = (pmix_rshift_caddy_t*)cbdata;
    pmix_status_t rc, ret;
    int cnt;
    size_t index = rb->index;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: regevents callback recvd");

    /* unpack the status code */
    cnt = 1;
    if ((PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ret, &cnt, PMIX_STATUS))) ||
        (PMIX_SUCCESS != ret)) {
        PMIX_ERROR_LOG(rc);
        /* remove the err handler and call the error handler reg completion callback fn.*/
        if (NULL != rb->list && NULL != rb->item) {
            pmix_list_remove_item(rb->list, rb->item);
            PMIX_RELEASE(rb->item);
        }
        ret = PMIX_ERR_SERVER_FAILED_REQUEST;
        index = UINT_MAX;
    }

    /* call the callback */
    if (NULL != rb->cd && NULL != rb->cd->cbfunc.evregcbfn) {
        rb->cd->cbfunc.evregcbfn(ret, index, rb->cd->cbdata);
    }
    PMIX_RELEASE(rb);
}

static void reg_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_rshift_caddy_t *rb = (pmix_rshift_caddy_t*)cbdata;
    pmix_status_t rc = status;
    size_t index = rb->index;

    if (PMIX_SUCCESS != status) {
        /* if we failed to register, then remove this event */
        if (NULL != rb->list && NULL != rb->item) {
            pmix_list_remove_item(rb->list, rb->item);
            PMIX_RELEASE(rb->item);
            rc = PMIX_ERR_SERVER_FAILED_REQUEST;
            index = UINT_MAX;
        }
    }

    if (NULL != rb->cd && NULL != rb->cd->cbfunc.evregcbfn) {
        /* pass back our local index */
        rb->cd->cbfunc.evregcbfn(rc, index, rb->cd->cbdata);
    }

    PMIX_RELEASE(rb);
}

static pmix_status_t _send_to_server(pmix_rshift_caddy_t *rcd)
{
    pmix_status_t rc;
    pmix_buffer_t *msg;
    pmix_cmd_t cmd=PMIX_REGEVENTS_CMD;

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack the number of codes */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &rcd->cd->ncodes, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack any provided codes - may be NULL */
    if (NULL != rcd->cd->codes && 0 < rcd->cd->ncodes) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, rcd->cd->codes, rcd->cd->ncodes, PMIX_STATUS))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    }

    /* pack the number of info */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &rcd->ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack any provided info - may be NULL */
    if (NULL != rcd->info && 0 < rcd->ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, rcd->info, rcd->ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    }
    rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, msg, regevents_cbfunc, rcd);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
    }

    return rc;
}

static pmix_status_t _add_hdlr(pmix_list_t *list, pmix_list_item_t *item,
                               size_t index, bool prepend, pmix_list_t *xfer,
                               pmix_shift_caddy_t *cd)
{
    pmix_rshift_caddy_t *cd2;
    pmix_info_caddy_t *ixfer;
    size_t n;
    bool registered, need_register = false;
    pmix_active_code_t *active;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: _add_hdlr");

    if (prepend) {
        pmix_list_prepend(list, item);
    } else {
        pmix_list_append(list, item);
    }

    /* check to see if we have an active registration on these codes */
    if (NULL == cd->codes) {
        registered = false;
        PMIX_LIST_FOREACH(active, &pmix_globals.events.actives, pmix_active_code_t) {
            if (PMIX_MAX_ERR_CONSTANT == active->code) {
                /* we have registered a default */
                registered = true;
                break;
            }
        }
        if (!registered) {
            active = PMIX_NEW(pmix_active_code_t);
            active->code = PMIX_MAX_ERR_CONSTANT;
            pmix_list_append(&pmix_globals.events.actives, &active->super);
            /* ensure we register it */
            need_register = true;
        }
    } else {
        for (n=0; n < cd->ncodes; n++) {
            registered = false;
            PMIX_LIST_FOREACH(active, &pmix_globals.events.actives, pmix_active_code_t) {
                if (active->code == cd->codes[n]) {
                    registered = true;
                    break;
                }
            }
            if (!registered) {
                active = PMIX_NEW(pmix_active_code_t);
                active->code = cd->codes[n];
                pmix_list_append(&pmix_globals.events.actives, &active->super);
                /* ensure we register it */
                need_register = true;
            }
        }
    }

    /* prep next step */
    cd2 = PMIX_NEW(pmix_rshift_caddy_t);
    cd2->index = index;
    cd2->list = list;
    cd2->item = item;
    PMIX_RETAIN(cd);
    cd2->cd = cd;
    cd2->ninfo = pmix_list_get_size(xfer);
    if (0 < cd2->ninfo) {
        PMIX_INFO_CREATE(cd2->info, cd2->ninfo);
        n=0;
        PMIX_LIST_FOREACH(ixfer, xfer, pmix_info_caddy_t) {
            (void)strncpy(cd2->info[n].key, ixfer->info[n].key, PMIX_MAX_KEYLEN);
            pmix_value_load(&cd2->info[n].value, &ixfer->info[n].value.data, ixfer->info[n].value.type);
            ++n;
        }
    }

    /* if we are a client, and we haven't already registered a handler of this
     * type with our server, or if we have directives, then we need to notify
     * the server */
    if (PMIX_PROC_SERVER != pmix_globals.proc_type &&
       (need_register || 0 < pmix_list_get_size(xfer))) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: _add_hdlr sending to server");
        /* send the directives to the server - we will ack this
         * registration upon return from there */
        if (PMIX_SUCCESS != (rc = _send_to_server(cd2))) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix: add_hdlr - pack send_to_server failed status=%d", rc);
            PMIX_RELEASE(cd2);
            pmix_list_remove_item(list, item);
            PMIX_RELEASE(item);
            return rc;
        }
        return PMIX_ERR_WOULD_BLOCK;
    }

    /* if we are a server and are registering for events, then we only contact
     * our host if we want environmental events */

    if (PMIX_PROC_SERVER == pmix_globals.proc_type && cd->enviro &&
        NULL != pmix_host_server.register_events) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix: _add_hdlr registering with server");
            if (PMIX_SUCCESS != (rc = pmix_host_server.register_events(cd->codes, cd->ncodes,
                                                                       cd2->info, cd2->ninfo,
                                                                       reg_cbfunc, cd2))) {
                PMIX_RELEASE(cd2);
                pmix_list_remove_item(list, item);
                PMIX_RELEASE(item);
                return rc;
            }
            return PMIX_ERR_WOULD_BLOCK;
        }

    return PMIX_SUCCESS;
}

static void reg_event_hdlr(int sd, short args, void *cbdata)
{
    size_t index = 0, n;
    pmix_status_t rc;
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_single_event_t *sing;
    pmix_multi_event_t *multi;
    pmix_default_event_t *def;
    bool prepend = false;
    char *name = NULL;
    pmix_list_t xfer;
    pmix_info_caddy_t *ixfer;
    void *cbobject = NULL;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: register event_hdlr with %d infos", (int)cd->ninfo);

    PMIX_CONSTRUCT(&xfer, pmix_list_t);

    /* if directives were included */
    if (NULL != cd->info) {
        for (n=0; n < cd->ninfo; n++) {
            if (0 == strcmp(cd->info[n].key, PMIX_EVENT_ORDER_PREPEND)) {
                /* flag if they asked to prepend this event
                 * on the precedence order */
                prepend = true;
            } else if (0 == strcmp(cd->info[n].key, PMIX_EVENT_HDLR_NAME)) {
                name = cd->info[n].value.data.string;
            } else if (0 == strcmp(cd->info[n].key, PMIX_EVENT_ENVIRO_LEVEL)) {
                cd->enviro = cd->info[n].value.data.flag;
            } else if (0 == strcmp(cd->info[n].key, PMIX_EVENT_RETURN_OBJECT)) {
                cbobject = cd->info[n].value.data.ptr;
            } else {
                ixfer = PMIX_NEW(pmix_info_caddy_t);
                ixfer->info = &cd->info[n];
                pmix_list_append(&xfer, &ixfer->super);
            }
        }
    }

    /* if the code array is NULL, then this is a default event
     * registration request */
    if (NULL == cd->codes) {
        def = PMIX_NEW(pmix_default_event_t);
        if (NULL != name) {
            def->name = strdup(name);
        }
        index = pmix_globals.events.nhdlrs;
        ++pmix_globals.events.nhdlrs;
        def->index = index;
        def->evhdlr = cd->evhdlr;
        def->cbobject = cbobject;
        rc = _add_hdlr(&pmix_globals.events.default_events, &def->super,
                       index, prepend, &xfer, cd);
        PMIX_LIST_DESTRUCT(&xfer);
        if (PMIX_SUCCESS != rc &&
            PMIX_ERR_WOULD_BLOCK != rc) {
            /* unable to register */
            --pmix_globals.events.nhdlrs;
            rc = PMIX_ERR_EVENT_REGISTRATION;
            index = UINT_MAX;
            goto ack;
        }
        if (PMIX_ERR_WOULD_BLOCK == rc) {
            /* the callback will provide our response */
            PMIX_RELEASE(cd);
            return;
        }
        goto ack;
    }

    /* if there is only one code, then this is a single event registration */
    if (1 == cd->ncodes) {
        sing = PMIX_NEW(pmix_single_event_t);
        if (NULL != name) {
            sing->name = strdup(name);
        }
        sing->code = cd->codes[0];
        index = pmix_globals.events.nhdlrs;
        sing->index = index;
        sing->evhdlr = cd->evhdlr;
        ++pmix_globals.events.nhdlrs;
        sing->cbobject = cbobject;
        rc = _add_hdlr(&pmix_globals.events.single_events, &sing->super,
                       index, prepend, &xfer, cd);
        PMIX_LIST_DESTRUCT(&xfer);
        if (PMIX_SUCCESS != rc &&
            PMIX_ERR_WOULD_BLOCK != rc) {
                /* unable to register */
            --pmix_globals.events.nhdlrs;
            rc = PMIX_ERR_EVENT_REGISTRATION;
            index = UINT_MAX;
            goto ack;
        }
        if (PMIX_ERR_WOULD_BLOCK == rc) {
            /* the callback will provide our response */
            PMIX_RELEASE(cd);
            return;
        }
        goto ack;
    }

    /* must be a multi-code registration */
    multi = PMIX_NEW(pmix_multi_event_t);
    if (NULL != name) {
        multi->name = strdup(name);
    }
    multi->codes = (pmix_status_t*)malloc(cd->ncodes * sizeof(pmix_status_t));
    multi->ncodes = cd->ncodes;
    memcpy(multi->codes, cd->codes, cd->ncodes * sizeof(pmix_status_t));
    index = pmix_globals.events.nhdlrs;
    multi->index = index;
    multi->evhdlr = cd->evhdlr;
    ++pmix_globals.events.nhdlrs;
    multi->cbobject = cbobject;
    rc = _add_hdlr(&pmix_globals.events.multi_events, &multi->super,
                   index, prepend, &xfer, cd);
    PMIX_LIST_DESTRUCT(&xfer);
    if (PMIX_SUCCESS != rc &&
        PMIX_ERR_WOULD_BLOCK != rc) {
            /* unable to register */
        --pmix_globals.events.nhdlrs;
        rc = PMIX_ERR_EVENT_REGISTRATION;
        index = UINT_MAX;
        goto ack;
    }
    if (PMIX_ERR_WOULD_BLOCK == rc) {
        /* the callback will provide our response */
        PMIX_RELEASE(cd);
        return;
    }

  ack:
    /* acknowledge the registration so the caller can release
     * their data */
    cd->cbfunc.evregcbfn(rc, index, cd->cbdata);

    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_Register_event_handler(pmix_status_t codes[], size_t ncodes,
                                             pmix_info_t info[], size_t ninfo,
                                             pmix_notification_fn_t event_hdlr,
                                             pmix_evhdlr_reg_cbfunc_t cbfunc,
                                             void *cbdata)
{
    pmix_shift_caddy_t *cd;

    /* need to thread shift this request so we can access
     * our global data to register this *local* event handler */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->codes = codes;
    cd->ncodes = ncodes;
    cd->info = info;
    cd->ninfo = ninfo;
    cd->evhdlr = event_hdlr;
    cd->cbfunc.errregcbfn = cbfunc;
    cd->cbdata = cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_register_event_hdlr shifting to progress thread");

    PMIX_THREADSHIFT(cd, reg_event_hdlr);
}

static void dereg_event_hdlr(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_buffer_t *msg = NULL;
    pmix_single_event_t *sing, *s2;
    pmix_multi_event_t *multi, *m2;
    pmix_default_event_t *def;
    pmix_cmd_t cmd = PMIX_DEREGEVENTS_CMD;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_status_t wildcard = PMIX_MAX_ERR_CONSTANT;
    size_t n;
    bool found, foundcode;

    /* if I am not the server, then I need to notify the server
     * to remove my registration */
    if (PMIX_PROC_SERVER != pmix_globals.proc_type) {
        msg = PMIX_NEW(pmix_buffer_t);
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
            PMIX_RELEASE(msg);
            goto cleanup;
        }
    }

    /* the registration can be in any of three places, so check them all */
    PMIX_LIST_FOREACH(def, &pmix_globals.events.default_events, pmix_default_event_t) {
        if (def->index == cd->ref) {
            /* found it */
            pmix_list_remove_item(&pmix_globals.events.default_events, &def->super);
            if (NULL != msg) {
                /* if there are no more default handlers registered, tell
                 * the server to dereg the default handler */
                if (0 == pmix_list_get_size(&pmix_globals.events.default_events)) {
                    n = 1;
                    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &n, 1, PMIX_SIZE))) {
                        PMIX_RELEASE(msg);
                        goto cleanup;
                    }
                    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &wildcard, 1, PMIX_STATUS))) {
                        PMIX_RELEASE(msg);
                        goto cleanup;
                    }
                }
            }
            PMIX_RELEASE(def);
            goto report;
        }
    }
    PMIX_LIST_FOREACH(sing, &pmix_globals.events.single_events, pmix_single_event_t) {
        if (sing->index == cd->ref) {
            /* found it */
            pmix_list_remove_item(&pmix_globals.events.single_events, &sing->super);
            if (NULL != msg) {
                /* if there are no more handlers registered for this code, tell
                 * the server to dereg the handler for this code */
                found = false;
                PMIX_LIST_FOREACH(s2, &pmix_globals.events.single_events, pmix_single_event_t) {
                    if (s2->code == sing->code) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    n = 1;
                    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &n, 1, PMIX_SIZE))) {
                        PMIX_RELEASE(msg);
                        PMIX_RELEASE(sing);
                        goto cleanup;
                    }
                    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &sing->code, 1, PMIX_STATUS))) {
                        PMIX_RELEASE(msg);
                        PMIX_RELEASE(sing);
                        goto cleanup;
                    }
                }
            }
            PMIX_RELEASE(sing);
            goto report;
        }
    }
    PMIX_LIST_FOREACH(multi, &pmix_globals.events.multi_events, pmix_multi_event_t) {
        if (multi->index == cd->ref) {
            /* found it */
            pmix_list_remove_item(&pmix_globals.events.multi_events, &multi->super);
            if (NULL != msg) {
                /* if there are no more handlers registered for this code, tell
                 * the server to dereg the handler for this code */
                found = false;
                PMIX_LIST_FOREACH(m2, &pmix_globals.events.multi_events, pmix_multi_event_t) {
                    if (m2->ncodes != multi->ncodes) {
                        continue;
                    }
                    foundcode = true;
                    for (n=0; n < multi->ncodes; n++) {
                        if (m2->codes[n] != multi->codes[n]) {
                            foundcode = false;
                            break;
                        }
                    }
                    if (foundcode) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    n = multi->ncodes;
                    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &n, 1, PMIX_SIZE))) {
                        PMIX_RELEASE(msg);
                        PMIX_RELEASE(multi);
                        goto cleanup;
                    }
                    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &multi->codes, n, PMIX_STATUS))) {
                        PMIX_RELEASE(msg);
                        PMIX_RELEASE(multi);
                        goto cleanup;
                    }
                }
            }
            PMIX_RELEASE(multi);
            goto report;
        }
    }
    /* if we get here, then the registration could not be found */
    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(PMIX_ERR_NOT_FOUND, cd->cbdata);
    }
    PMIX_RELEASE(cd);
    return;

  report:
    if (NULL != msg) {
        /* send to the server */
        rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, msg, NULL, NULL);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

  cleanup:
    /* must release the caller */
    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(rc, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_Deregister_event_handler(size_t event_hdlr_ref,
                                               pmix_op_cbfunc_t cbfunc,
                                               void *cbdata)
{
    pmix_shift_caddy_t *cd;

    /* need to thread shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;
    cd->ref = event_hdlr_ref;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix_deregister_event_hdlr shifting to progress thread");
    PMIX_THREADSHIFT(cd, dereg_event_hdlr);
}

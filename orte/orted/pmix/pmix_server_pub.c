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
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_data_server.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"

#include "pmix_server_internal.h"

static void execute(int sd, short args, void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t*)cbdata;
    int rc;
    opal_buffer_t *xfer;

    /* add this request to our tracker hotel */
    if (OPAL_SUCCESS != (rc = opal_hotel_checkin(&orte_pmix_server_globals.reqs, req, &req->room_num))) {
        ORTE_ERROR_LOG(rc);
        goto callback;
    }

    /* setup the xfer */
    xfer = OBJ_NEW(opal_buffer_t);
    /* pack the room number */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(xfer, &req->room_num, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(xfer);
        goto callback;
    }
    opal_dss.copy_payload(xfer, &req->msg);

    /* send the request to the target */
    rc = orte_rml.send_buffer_nb(&req->target, xfer,
                                 ORTE_RML_TAG_DATA_SERVER,
                                 orte_rml_send_callback, NULL);
    if (ORTE_SUCCESS == rc) {
        return;
    }

  callback:
    /* execute the callback to avoid having the client hang */
    if (NULL != req->opcbfunc) {
        req->opcbfunc(rc, req->cbdata);
    } else if (NULL != req->lkcbfunc) {
        req->lkcbfunc(rc, NULL, req->cbdata);
    }
    opal_hotel_checkout(&orte_pmix_server_globals.reqs, req->room_num);
    OBJ_RELEASE(req);
}

int pmix_server_publish_fn(opal_process_name_t *proc,
                           opal_list_t *info,
                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int rc;
    uint8_t cmd = ORTE_PMIX_PUBLISH_CMD;
    opal_value_t *iptr;
    opal_pmix_data_range_t range = OPAL_PMIX_SESSION;
    opal_pmix_persistence_t persist = OPAL_PMIX_PERSIST_APP;
    bool rset, pset;

    /* create the caddy */
    req = OBJ_NEW(pmix_server_req_t);
    req->opcbfunc = cbfunc;
    req->cbdata = cbdata;

    /* load the command */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &cmd, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* pack the name of the publisher */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, proc, 1, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* no help for it - need to search for range/persistence */
    rset = false;
    pset = false;
    OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
        if (0 == strcmp(iptr->key, OPAL_PMIX_RANGE)) {
            range = iptr->data.integer;
            if (pset) {
                break;
            }
            rset = true;
        } else if (0 == strcmp(iptr->key, OPAL_PMIX_PERSISTENCE)) {
            persist = iptr->data.integer;
            if (rset) {
                break;
            }
            pset = true;
        }
    }

    /* pack the range */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &range, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* if the range is SESSION, then set the target to the global server */
    if (OPAL_PMIX_SESSION == range) {
        req->target = orte_pmix_server_globals.server;
    } else {
        req->target = *ORTE_PROC_MY_HNP;
    }

    /* pack the persistence */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &persist, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* if we have items, pack those too - ignore persistence
     * and range values */
    OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
        if (0 == strcmp(iptr->key, OPAL_PMIX_RANGE) ||
            0 == strcmp(iptr->key, OPAL_PMIX_PERSISTENCE)) {
            continue;
        }
        opal_output_verbose(5, orte_pmix_server_globals.output,
                            "%s publishing data %s of type %d from source %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), iptr->key, iptr->type,
                            ORTE_NAME_PRINT(proc));
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &iptr, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(req);
            return rc;
        }
    }

    /* thread-shift so we can store the tracker */
    opal_event_set(orte_event_base, &(req->ev),
                   -1, OPAL_EV_WRITE, execute, req);
    opal_event_set_priority(&(req->ev), ORTE_MSG_PRI);
    opal_event_active(&(req->ev), OPAL_EV_WRITE, 1);

    return OPAL_SUCCESS;

}

int pmix_server_lookup_fn(opal_process_name_t *proc, char **keys,
                          opal_list_t *info,
                          opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int rc;
    uint8_t cmd = ORTE_PMIX_LOOKUP_CMD;
    int32_t nkeys, i;
    opal_value_t *iptr;
    opal_pmix_data_range_t range = OPAL_PMIX_SESSION;

    /* the list of info objects are directives for us - they include
     * things like timeout constraints, so there is no reason to
     * forward them to the server */

    /* create the caddy */
    req = OBJ_NEW(pmix_server_req_t);
    req->lkcbfunc = cbfunc;
    req->cbdata = cbdata;

    /* load the command */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &cmd, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* no help for it - need to search for range */
    OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
        if (0 == strcmp(iptr->key, OPAL_PMIX_RANGE)) {
            range = iptr->data.integer;
            break;
        }
    }

    /* pack the range */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &range, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* if the range is SESSION, then set the target to the global server */
    if (OPAL_PMIX_SESSION == range) {
        req->target = orte_pmix_server_globals.server;
    } else {
        req->target = *ORTE_PROC_MY_HNP;
    }

    /* pack the number of keys */
    nkeys = opal_argv_count(keys);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &nkeys, 1, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* pack the keys too */
    for (i=0; i < nkeys; i++) {
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &keys[i], 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(req);
            return rc;
        }
    }

    /* if we have items, pack those too - ignore range value */
    OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
        if (0 == strcmp(iptr->key, OPAL_PMIX_RANGE)) {
            continue;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &iptr, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(req);
            return rc;
        }
    }

    /* thread-shift so we can store the tracker */
    opal_event_set(orte_event_base, &(req->ev),
                   -1, OPAL_EV_WRITE, execute, req);
    opal_event_set_priority(&(req->ev), ORTE_MSG_PRI);
    opal_event_active(&(req->ev), OPAL_EV_WRITE, 1);

    return OPAL_SUCCESS;
}

int pmix_server_unpublish_fn(opal_process_name_t *proc, char **keys,
                             opal_list_t *info,
                             opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int rc;
    uint8_t cmd = ORTE_PMIX_UNPUBLISH_CMD;
    uint32_t nkeys, n;
    opal_value_t *iptr;
    opal_pmix_data_range_t range = OPAL_PMIX_SESSION;

    /* create the caddy */
    req = OBJ_NEW(pmix_server_req_t);
    req->opcbfunc = cbfunc;
    req->cbdata = cbdata;

    /* load the command */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &cmd, 1, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* pack the name of the publisher */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, proc, 1, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* no help for it - need to search for range */
    OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
        if (0 == strcmp(iptr->key, OPAL_PMIX_RANGE)) {
            range = iptr->data.integer;
            break;
        }
    }

    /* pack the range */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &range, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* if the range is SESSION, then set the target to the global server */
    if (OPAL_PMIX_SESSION == range) {
        req->target = orte_pmix_server_globals.server;
    } else {
        req->target = *ORTE_PROC_MY_HNP;
    }

    /* pack the number of keys */
    nkeys = opal_argv_count(keys);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &nkeys, 1, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* pack the keys too */
    for (n=0; n < nkeys; n++) {
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &keys[n], 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(req);
            return rc;
        }
    }

    /* if we have items, pack those too - ignore range value */
    OPAL_LIST_FOREACH(iptr, info, opal_value_t) {
        if (0 == strcmp(iptr->key, OPAL_PMIX_RANGE)) {
            continue;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &iptr, 1, OPAL_VALUE))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(req);
            return rc;
        }
    }

    /* thread-shift so we can store the tracker */
    opal_event_set(orte_event_base, &(req->ev),
                   -1, OPAL_EV_WRITE, execute, req);
    opal_event_set_priority(&(req->ev), ORTE_MSG_PRI);
    opal_event_active(&(req->ev), OPAL_EV_WRITE, 1);

    return OPAL_SUCCESS;
}

void pmix_server_keyval_client(int status, orte_process_name_t* sender,
                               opal_buffer_t *buffer,
                               orte_rml_tag_t tg, void *cbdata)
{
    int rc, ret, room_num = -1;
    int32_t cnt;
    pmix_server_req_t *req=NULL;
    opal_list_t info;
    opal_value_t *iptr;
    opal_pmix_pdata_t *pdata;
    opal_process_name_t source;

    opal_output_verbose(1, orte_pmix_server_globals.output,
                        "%s recvd lookup data return",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    OBJ_CONSTRUCT(&info, opal_list_t);
    /* unpack the room number of the request tracker */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &room_num, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the status */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto release;
    }

    opal_output_verbose(5, orte_pmix_server_globals.output,
                        "%s recvd lookup returned status %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ret);

    if (ORTE_SUCCESS == ret) {
        /* see if any data was included - not an error if the answer is no */
        cnt = 1;
        while (OPAL_SUCCESS == opal_dss.unpack(buffer, &source, &cnt, OPAL_NAME)) {
            pdata = OBJ_NEW(opal_pmix_pdata_t);
            pdata->proc = source;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &iptr, &cnt, OPAL_VALUE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(pdata);
                continue;
            }
            opal_output_verbose(5, orte_pmix_server_globals.output,
                                "%s recvd lookup returned data %s of type %d from source %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), iptr->key, iptr->type,
                                ORTE_NAME_PRINT(&source));
            if (OPAL_SUCCESS != (rc = opal_value_xfer(&pdata->value, iptr))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(pdata);
                OBJ_RELEASE(iptr);
                continue;
            }
            OBJ_RELEASE(iptr);
            opal_list_append(&info, &pdata->super);
        }
    }

  release:
    if (0 <= room_num) {
        /* retrieve the tracker */
        opal_hotel_checkout_and_return_occupant(&orte_pmix_server_globals.reqs, room_num, (void**)&req);
    }

    if (NULL != req) {
        /* pass down the response */
        if (NULL != req->opcbfunc) {
            req->opcbfunc(ret, req->cbdata);
        } else if (NULL != req->lkcbfunc) {
            req->lkcbfunc(ret, &info, req->cbdata);
        } else {
            /* should not happen */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
        }

        /* cleanup */
        OPAL_LIST_DESTRUCT(&info);
        OBJ_RELEASE(req);
    }
}


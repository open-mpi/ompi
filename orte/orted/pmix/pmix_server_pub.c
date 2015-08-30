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

#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"

#include "pmix_server_internal.h"

#define ORTE_PMIX_PUBLISH_CMD    0x01
#define ORTE_PMIX_LOOKUP_CMD     0x02
#define ORTE_PMIX_UNPUBLISH_CMD  0x03


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
                           opal_pmix_data_range_t range,
                           opal_pmix_persistence_t persist,
                           opal_list_t *info,
                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int rc;
    uint8_t cmd = ORTE_PMIX_PUBLISH_CMD;
    int32_t ninfo;
    opal_pmix_info_t *iptr;

    /* create the caddy */
    req = OBJ_NEW(pmix_server_req_t);

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

    /* pack the number of info items */
    ninfo = opal_list_get_size(info);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &ninfo, 1, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* if we have items, pack those too */
    OPAL_LIST_FOREACH(iptr, info, opal_pmix_info_t) {
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &iptr->key, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(req);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &iptr->value, 1, OPAL_VALUE))) {
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

int pmix_server_lookup_fn(opal_process_name_t *proc,
                          opal_pmix_data_range_t range,
                          opal_list_t *info, char **keys,
                          opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int rc;
    uint8_t cmd = ORTE_PMIX_LOOKUP_CMD;
    int32_t nkeys;
    opal_pmix_info_t *iptr;

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

    /* pack the name of the requestor */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, proc, 1, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
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

    /* pack the number of info objects */
    nkeys = opal_list_get_size(info);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &nkeys, 1, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }
    /* pack the objects */
    if (0 < nkeys) {
        OPAL_LIST_FOREACH(iptr, info, opal_pmix_info_t) {
            if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &iptr->key, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(req);
                return rc;
            }
            if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &iptr->value, 1, OPAL_VALUE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(req);
                return rc;
            }
        }
    }

    /* pack the number of keys */
    nkeys = opal_argv_count(keys);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, &nkeys, 1, OPAL_UINT32))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* pack the keys too */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, keys, nkeys, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* thread-shift so we can store the tracker */
    opal_event_set(orte_event_base, &(req->ev),
                   -1, OPAL_EV_WRITE, execute, req);
    opal_event_set_priority(&(req->ev), ORTE_MSG_PRI);
    opal_event_active(&(req->ev), OPAL_EV_WRITE, 1);

    return OPAL_SUCCESS;
}

int pmix_server_unpublish_fn(opal_process_name_t *proc,
                             opal_pmix_data_range_t range, char **keys,
                             opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int rc;
    uint8_t cmd = ORTE_PMIX_UNPUBLISH_CMD;
    uint32_t nkeys;

    /* create the caddy */
    req = OBJ_NEW(pmix_server_req_t);

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
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&req->msg, keys, nkeys, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(req);
        return rc;
    }

    /* thread-shift so we can store the tracker */
    opal_event_set(orte_event_base, &(req->ev),
                   -1, OPAL_EV_WRITE, execute, req);
    opal_event_set_priority(&(req->ev), ORTE_MSG_PRI);
    opal_event_active(&(req->ev), OPAL_EV_WRITE, 1);

    return OPAL_SUCCESS;
}

void pmix_server_keyval_srvr(int status, orte_process_name_t* sender,
                             opal_buffer_t *buffer,
                             orte_rml_tag_t tg, void *cbdata)
{
    uint8_t cmd;
    int range, room_num, cnt, rc;
    opal_buffer_t *answer;
    opal_process_name_t proc;

    /* setup the answer */
    answer = OBJ_NEW(opal_buffer_t);

    /* unpack the remote room number */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &room_num, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return;
    }
    /* save it for the return */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &room_num, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return;
    }

    /* unpack the cmd */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &cmd, &cnt, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        goto release;
    }

    /* unpack the name of the publisher/requestor */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &proc, &cnt, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto release;
    }

    /* unpack the range */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &range, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto release;
    }

    switch(cmd) {
        case ORTE_PMIX_PUBLISH_CMD:
            /* unpack the publisher */
            /* unpack the range */
            /* unpack the persistence */
            /* unpack the key */
            /* unpack the value */
        case ORTE_PMIX_LOOKUP_CMD:
        case ORTE_PMIX_UNPUBLISH_CMD:
            break;
        default:
            break;
    }

  release:
    /* pack the error code */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(answer, &rc, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* send back the reply */
    rc = orte_rml.send_buffer_nb(sender, answer,
                                 ORTE_RML_TAG_DATA_CLIENT,
                                 orte_rml_send_callback, NULL);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
    }
}

void pmix_server_keyval_client(int status, orte_process_name_t* sender,
                               opal_buffer_t *buffer,
                               orte_rml_tag_t tg, void *cbdata)
{
    int rc, ret, room_num;
    int32_t cnt, ninfo, n;
    pmix_server_req_t *req;
    opal_list_t *info = NULL;
    opal_pmix_info_t *iptr;

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

    /* see if any data was included - not an error if the answer is no */
    cnt = 1;
    rc = opal_dss.unpack(buffer, &ninfo, &cnt, OPAL_INT32);
    if (ORTE_SUCCESS == rc && 0 < ninfo) {
        info = OBJ_NEW(opal_list_t);
        for (n=0; n < ninfo; n++) {
            iptr = OBJ_NEW(opal_pmix_info_t);
            opal_list_append(info, &iptr->super);
            rc = opal_dss.unpack(buffer, &iptr->key, &cnt, OPAL_STRING);
            if (OPAL_SUCCESS != rc) {
                ret = rc;
                OPAL_LIST_RELEASE(info);
                info = NULL;
            }
            rc = opal_dss.unpack(buffer, &iptr->value, &cnt, OPAL_VALUE);
            if (OPAL_SUCCESS != rc) {
                ret = rc;
                OPAL_LIST_RELEASE(info);
                info = NULL;
            }
        }
    }

    /* retrieve the tracker */
    opal_hotel_checkout_and_return_occupant(&orte_pmix_server_globals.reqs, room_num, (void**)&req);

  release:
    if (NULL != req) {
        /* pass down the response */
        if (NULL != req->opcbfunc) {
            req->opcbfunc(ret, req->cbdata);
        } else {
            req->lkcbfunc(ret, info, req->cbdata);
        }

        /* cleanup */
        OBJ_RELEASE(req);
    }
}


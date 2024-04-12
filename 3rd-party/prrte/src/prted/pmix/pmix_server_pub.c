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
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/rml/rml_contact.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_data_server.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"

#include "src/prted/pmix/pmix_server_internal.h"

static int init_server(void)
{
    char *server;
    pmix_value_t val;
    char input[1024], *filename;
    FILE *fp;
    int rc;
    pmix_status_t ret;

    /* only do this once */
    prte_pmix_server_globals.pubsub_init = true;

    /* if the universal server wasn't specified, then we use
     * our own HNP for that purpose */
    if (NULL == prte_data_server_uri) {
        prte_pmix_server_globals.server = *PRTE_PROC_MY_HNP;
    } else {
        if (0 == strncmp(prte_data_server_uri, "file", strlen("file")) ||
            0 == strncmp(prte_data_server_uri, "FILE", strlen("FILE"))) {
            /* it is a file - get the filename */
            filename = strchr(prte_data_server_uri, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                pmix_show_help("help-prun.txt", "prun:ompi-server-filename-bad", true,
                               prte_tool_basename, prte_data_server_uri);
                return PRTE_ERR_BAD_PARAM;
            }
            ++filename; /* space past the : */

            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                pmix_show_help("help-prun.txt", "prun:ompi-server-filename-missing", true,
                               prte_tool_basename, prte_data_server_uri);
                return PRTE_ERR_BAD_PARAM;
            }

            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                pmix_show_help("help-prun.txt", "prun:ompi-server-filename-access", true,
                               prte_tool_basename, prte_data_server_uri);
                return PRTE_ERR_BAD_PARAM;
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                pmix_show_help("help-prun.txt", "prun:ompi-server-file-bad", true,
                               prte_tool_basename, prte_data_server_uri, prte_tool_basename);
                return PRTE_ERR_BAD_PARAM;
            }
            fclose(fp);
            input[strlen(input) - 1] = '\0'; /* remove newline */
            server = strdup(input);
        } else {
            server = strdup(prte_data_server_uri);
        }
        /* parse the URI to get the server's name */
        rc = prte_rml_parse_uris(server, &prte_pmix_server_globals.server, NULL);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            free(server);
            return rc;
        }
        /* setup our route to the server */
        PMIX_VALUE_LOAD(&val, server, PMIX_STRING);
        ret = PMIx_Store_internal(&prte_pmix_server_globals.server, PMIX_PROC_URI, &val);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_VALUE_DESTRUCT(&val);
            return rc;
        }
        PMIX_VALUE_DESTRUCT(&val);

        /* check if we are to wait for the server to start - resolves
         * a race condition that can occur when the server is run
         * as a background job - e.g., in scripts
         */
        if (prte_pmix_server_globals.wait_for_server) {
            /* ping the server */
            struct timespec timeout = {prte_pmix_server_globals.timeout, 0};
            /* just hang loose */
            nanosleep(&timeout, NULL);
        }
    }

    return PRTE_SUCCESS;
}

static void execute(int sd, short args, void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t *) cbdata;
    int rc;
    pmix_data_buffer_t *xfer;
    pmix_proc_t *target;
    bool stored = false;
    PRTE_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(req);

    if (!prte_pmix_server_globals.pubsub_init) {
        /* we need to initialize our connection to the server */
        if (PRTE_SUCCESS != (rc = init_server())) {
            pmix_show_help("help-prted.txt", "noserver", true,
                           (NULL == prte_data_server_uri) ? "NULL" : prte_data_server_uri);
            goto callback;
        }
    }

    /* add this request to our tracker array */
    req->local_index = pmix_pointer_array_add(&prte_pmix_server_globals.local_reqs, req);
    stored = true;

    /* setup the xfer */
    PMIX_DATA_BUFFER_CREATE(xfer);

    /* pack the room number */
    rc = PMIx_Data_pack(NULL, xfer, &req->local_index, 1, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(xfer);
        goto callback;
    }
    rc = PMIx_Data_copy_payload(xfer, &req->msg);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(xfer);
        goto callback;
    }

    /* if the range is SESSION, then set the target to the global server */
    if (PMIX_RANGE_SESSION == req->range) {
        pmix_output_verbose(1, prte_pmix_server_globals.output,
                            "%s orted:pmix:server range SESSION",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        target = &prte_pmix_server_globals.server;
    } else if (PMIX_RANGE_LOCAL == req->range) {
        /* if the range is local, send it to myself */
        pmix_output_verbose(1, prte_pmix_server_globals.output, "%s orted:pmix:server range LOCAL",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        target = PRTE_PROC_MY_NAME;
    } else {
        pmix_output_verbose(1, prte_pmix_server_globals.output, "%s orted:pmix:server range GLOBAL",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        target = PRTE_PROC_MY_HNP;
    }

    /* send the request to the target */
    PRTE_RML_SEND(rc, target->rank, xfer, PRTE_RML_TAG_DATA_SERVER);
    if (PRTE_SUCCESS == rc) {
        return;
    }
    PRTE_ERROR_LOG(rc);
    rc = prte_pmix_convert_rc(rc);

callback:
    /* execute the callback to avoid having the client hang */
    if (NULL != req->opcbfunc) {
        req->opcbfunc(rc, req->cbdata);
    } else if (NULL != req->lkcbfunc) {
        req->lkcbfunc(rc, NULL, 0, req->cbdata);
    }
    if (stored) {
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, req->local_index, NULL);
    }
    PMIX_RELEASE(req);
}

pmix_status_t pmix_server_publish_fn(const pmix_proc_t *proc, const pmix_info_t info[],
                                     size_t ninfo, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    pmix_status_t rc;
    int ret;
    uint8_t cmd = PRTE_PMIX_PUBLISH_CMD;
    size_t n;

    pmix_output_verbose(1, prte_pmix_server_globals.output, "%s orted:pmix:server PUBLISH",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* create the caddy */
    req = PMIX_NEW(pmix_server_req_t);
    pmix_asprintf(&req->operation, "PUBLISH: %s:%d", __FILE__, __LINE__);
    req->opcbfunc = cbfunc;
    req->cbdata = cbdata;

    /* load the command */
    ret = PMIx_Data_pack(NULL, &req->msg, &cmd, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_RELEASE(req);
        return PMIX_ERR_PACK_FAILURE;
    }

    /* no help for it - need to search for range/persistence */
    for (n = 0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_RANGE, PMIX_MAX_KEYLEN)) {
            req->range = info[n].value.data.range;
        } else if (0 == strncmp(info[n].key, PMIX_TIMEOUT, PMIX_MAX_KEYLEN)) {
            req->timeout = info[n].value.data.integer;
        }
    }

    /* pack the name of the publisher */
    if (PMIX_SUCCESS
        != (rc = PMIx_Data_pack(NULL, &req->msg, (pmix_proc_t *) proc, 1, PMIX_PROC))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }

    /* pack the number of infos */
    if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &req->msg, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }

    /* pack the infos */
    if (PMIX_SUCCESS
        != (rc = PMIx_Data_pack(NULL, &req->msg, (pmix_info_t *) info, ninfo, PMIX_INFO))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }

    /* thread-shift so we can store the tracker */
    prte_event_set(prte_event_base, &(req->ev), -1, PRTE_EV_WRITE, execute, req);
    PMIX_POST_OBJECT(req);
    prte_event_active(&(req->ev), PRTE_EV_WRITE, 1);

    return PRTE_SUCCESS;
}

pmix_status_t pmix_server_lookup_fn(const pmix_proc_t *proc, char **keys, const pmix_info_t info[],
                                    size_t ninfo, pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int ret;
    uint8_t cmd = PRTE_PMIX_LOOKUP_CMD;
    size_t m, n;
    pmix_status_t rc;

    if (NULL == keys || 0 == PMIX_ARGV_COUNT_COMPAT(keys)) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* create the caddy */
    req = PMIX_NEW(pmix_server_req_t);
    pmix_asprintf(&req->operation, "LOOKUP: %s:%d", __FILE__, __LINE__);
    req->lkcbfunc = cbfunc;
    req->cbdata = cbdata;

    /* load the command */
    if (PRTE_SUCCESS != (ret = PMIx_Data_pack(NULL, &req->msg, &cmd, 1, PMIX_UINT8))) {
        PRTE_ERROR_LOG(ret);
        PMIX_RELEASE(req);
        return PMIX_ERR_PACK_FAILURE;
    }

    /* no help for it - need to search for range and timeout */
    for (n = 0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_RANGE, PMIX_MAX_KEYLEN)) {
            req->range = info[n].value.data.range;
        } else if (0 == strncmp(info[n].key, PMIX_TIMEOUT, PMIX_MAX_KEYLEN)) {
            req->timeout = info[n].value.data.integer;
        }
    }

    /* pack the name of the requestor */
    if (PMIX_SUCCESS
        != (rc = PMIx_Data_pack(NULL, &req->msg, (pmix_proc_t *) proc, 1, PMIX_PROC))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }

    /* pack the number of keys */
    n = PMIX_ARGV_COUNT_COMPAT(keys);
    if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &req->msg, &n, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }
    /* pack the keys */
    for (m = 0; NULL != keys[m]; m++) {
        if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &req->msg, &keys[m], 1, PMIX_STRING))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(req);
            return rc;
        }
    }

    /* pack the number of infos */
    if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &req->msg, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }

    if (0 < ninfo) {
        /* pack the infos */
        if (PMIX_SUCCESS
            != (rc = PMIx_Data_pack(NULL, &req->msg, (pmix_info_t *) info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(req);
            return rc;
        }
    }

    /* thread-shift so we can store the tracker */
    prte_event_set(prte_event_base, &(req->ev), -1, PRTE_EV_WRITE, execute, req);
    PMIX_POST_OBJECT(req);
    prte_event_active(&(req->ev), PRTE_EV_WRITE, 1);

    return PRTE_SUCCESS;
}

pmix_status_t pmix_server_unpublish_fn(const pmix_proc_t *proc, char **keys,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_server_req_t *req;
    int ret;
    uint8_t cmd = PRTE_PMIX_UNPUBLISH_CMD;
    size_t m, n;
    pmix_status_t rc;

    /* create the caddy */
    req = PMIX_NEW(pmix_server_req_t);
    pmix_asprintf(&req->operation, "UNPUBLISH: %s:%d", __FILE__, __LINE__);
    req->opcbfunc = cbfunc;
    req->cbdata = cbdata;

    /* load the command */
    if (PRTE_SUCCESS != (ret = PMIx_Data_pack(NULL, &req->msg, &cmd, 1, PMIX_UINT8))) {
        PRTE_ERROR_LOG(ret);
        PMIX_RELEASE(req);
        return PMIX_ERR_PACK_FAILURE;
    }

    /* no help for it - need to search for range and timeout */
    for (n = 0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_RANGE, PMIX_MAX_KEYLEN)) {
            req->range = info[n].value.data.range;
        } else if (0 == strncmp(info[n].key, PMIX_TIMEOUT, PMIX_MAX_KEYLEN)) {
            req->timeout = info[n].value.data.integer;
        }
    }

    /* pack the name of the requestor */
    if (PMIX_SUCCESS
        != (rc = PMIx_Data_pack(NULL, &req->msg, (pmix_proc_t *) proc, 1, PMIX_PROC))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }

    /* pack the number of keys */
    n = PMIX_ARGV_COUNT_COMPAT(keys);
    if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &req->msg, &n, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }
    /* pack the keys */
    for (m = 0; m < n; m++) {
        if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &req->msg, &keys[m], 1, PMIX_STRING))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(req);
            return rc;
        }
    }

    /* pack the number of infos */
    if (PMIX_SUCCESS != (rc = PMIx_Data_pack(NULL, &req->msg, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }

    if (0 < ninfo) {
        /* pack the infos */
        if (PMIX_SUCCESS
            != (rc = PMIx_Data_pack(NULL, &req->msg, (pmix_info_t *) info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(req);
            return rc;
        }
    }

    /* thread-shift so we can store the tracker */
    prte_event_set(prte_event_base, &(req->ev), -1, PRTE_EV_WRITE, execute, req);
    PMIX_POST_OBJECT(req);
    prte_event_active(&(req->ev), PRTE_EV_WRITE, 1);

    return PRTE_SUCCESS;
}

void pmix_server_keyval_client(int status, pmix_proc_t *sender,
                               pmix_data_buffer_t *buffer,
                               prte_rml_tag_t tg, void *cbdata)
{
    uint8_t command;
    int rc, room_num = -1;
    int32_t cnt;
    pmix_server_req_t *req = NULL;
    pmix_byte_object_t bo;
    pmix_data_buffer_t pbkt;
    pmix_status_t ret = PMIX_SUCCESS, rt = PMIX_SUCCESS;
    pmix_info_t info;
    pmix_pdata_t *pdata = NULL;
    size_t n, npdata = 0;
    PRTE_HIDE_UNUSED_PARAMS(sender, tg, cbdata);

    pmix_output_verbose(1, prte_pmix_server_globals.output,
                        "%s recvd lookup data return",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* unpack the room number of the request tracker */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &room_num, &cnt, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        ret = PMIX_ERR_UNPACK_FAILURE;
        goto release;
    }

    /* unpack the command */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &command, &cnt, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the return status */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &status, &cnt, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        ret = PMIX_ERR_UNPACK_FAILURE;
        goto release;
    }

    if (PRTE_ERR_NOT_FOUND == status) {
        ret = PMIX_ERR_NOT_FOUND;
        goto release;
    } else if (PRTE_ERR_PARTIAL_SUCCESS == status) {
        rt = PMIX_QUERY_PARTIAL_SUCCESS;
    } else {
        ret = PMIX_SUCCESS;
    }
    if (PRTE_PMIX_UNPUBLISH_CMD == command) {
        /* nothing else will be included */
        goto release;
    }

    /* unpack the byte object payload */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &bo, &cnt, PMIX_BYTE_OBJECT);
    /* there may not be anything returned here - e.g., a publish
     * command will not return any data if no matching pending
     * requests were found */
    if (PMIX_SUCCESS != rc) {
        if (PMIX_SUCCESS == ret) {
            ret = rt;
        }
        goto release;
    }

    /* load it into a pmix data buffer for processing */
    PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);
    rc = PMIx_Data_load(&pbkt, &bo);
    bo.bytes = NULL;
    PMIX_BYTE_OBJECT_DESTRUCT(&bo);

    /* unpack the number of data items */
    cnt = 1;
    if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, &pbkt, &npdata, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(ret);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
        goto release;
    }

    if (0 < npdata) {
        PMIX_PDATA_CREATE(pdata, npdata);
        for (n = 0; n < npdata; n++) {
            PMIX_INFO_CONSTRUCT(&info);
            cnt = 1;
            if (PMIX_SUCCESS
                != (ret = PMIx_Data_unpack(NULL, &pbkt, &pdata[n].proc, &cnt, PMIX_PROC))) {
                PMIX_ERROR_LOG(ret);
                PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                goto release;
            }
            cnt = 1;
            if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, &pbkt, &info, &cnt, PMIX_INFO))) {
                PMIX_ERROR_LOG(ret);
                PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                goto release;
            }
            PMIX_LOAD_KEY(pdata[n].key, info.key);
            PMIX_VALUE_XFER_DIRECT(ret, &pdata[n].value, &info.value);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                goto release;
            }
            PMIX_INFO_DESTRUCT(&info);
        }
    }
    if (PMIX_SUCCESS == ret) {
        ret = rt;
    }

release:
    if (0 <= room_num) {
        req = (pmix_server_req_t*)pmix_pointer_array_get_item(&prte_pmix_server_globals.local_reqs, room_num);
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, room_num, NULL);
    }

    if (NULL != req) {
        /* pass down the response */
        if (NULL != req->opcbfunc) {
            req->opcbfunc(ret, req->cbdata);
        } else if (NULL != req->lkcbfunc) {
            req->lkcbfunc(ret, pdata, npdata, req->cbdata);
        } else {
            /* should not happen */
            PRTE_ERROR_LOG(PRTE_ERR_NOT_SUPPORTED);
        }

        /* cleanup */
        PMIX_RELEASE(req);
    }
    if (NULL != pdata) {
        PMIX_PDATA_FREE(pdata, npdata);
    }
}

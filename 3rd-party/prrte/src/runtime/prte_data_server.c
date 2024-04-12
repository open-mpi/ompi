/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2012-2016 Los Alamos National Security, LLC.
 *                         All rights reserved
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
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
#include "types.h"

#include <string.h>

#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif

#include "src/class/pmix_pointer_array.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/util/name_fns.h"

#include "src/runtime/prte_data_server.h"

/* define an object to hold data */
typedef struct {
    /* base object */
    pmix_object_t super;
    /* index of this object in the storage array */
    int32_t index;
    /* process that owns this data - only the
     * owner can remove it
     */
    pmix_proc_t owner;
    /* uid of the owner - helps control
     * access rights */
    uint32_t uid;
    /* characteristics */
    pmix_data_range_t range;
    pmix_persistence_t persistence;
    /* and the values themselves */
    pmix_info_t *info;
    size_t ninfo;
    /* the value itself */
} prte_data_object_t;

static void construct(prte_data_object_t *ptr)
{
    ptr->index = -1;
    PMIX_PROC_CONSTRUCT(&ptr->owner);
    ptr->uid = UINT32_MAX;
    ptr->range = PMIX_RANGE_SESSION;
    ptr->persistence = PMIX_PERSIST_SESSION;
    ptr->info = NULL;
    ptr->ninfo = 0;
}

static void destruct(prte_data_object_t *ptr)
{
    if (NULL != ptr->info) {
        PMIX_INFO_FREE(ptr->info, ptr->ninfo);
    }
}

static PMIX_CLASS_INSTANCE(prte_data_object_t, pmix_object_t, construct, destruct);

/* define a request object for delayed answers */
typedef struct {
    pmix_list_item_t super;
    pmix_proc_t proxy;
    pmix_proc_t requestor;
    int room_number;
    uint32_t uid;
    pmix_data_range_t range;
    char **keys;
    pmix_list_t answers;
} prte_data_req_t;
static void rqcon(prte_data_req_t *p)
{
    p->keys = NULL;
    PMIX_CONSTRUCT(&p->answers, pmix_list_t);
}
static void rqdes(prte_data_req_t *p)
{
    PMIX_ARGV_FREE_COMPAT(p->keys);
    PMIX_LIST_DESTRUCT(&p->answers);
}
static PMIX_CLASS_INSTANCE(prte_data_req_t, pmix_list_item_t, rqcon, rqdes);

/* local globals */
static pmix_pointer_array_t prte_data_server_store;
static pmix_list_t pending;
static bool initialized = false;
static int prte_data_server_output = -1;
static int prte_data_server_verbosity = -1;

int prte_data_server_init(void)
{
    int rc;

    if (initialized) {
        return PRTE_SUCCESS;
    }
    initialized = true;

    /* register a verbosity */
    prte_data_server_verbosity = -1;
    (void) pmix_mca_base_var_register("prte", "prte", "data", "server_verbose",
                                      "Debug verbosity for PRTE data server",
                                      PMIX_MCA_BASE_VAR_TYPE_INT,
                                      &prte_data_server_verbosity);
    if (0 <= prte_data_server_verbosity) {
        prte_data_server_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(prte_data_server_output, prte_data_server_verbosity);
    }

    PMIX_CONSTRUCT(&prte_data_server_store, pmix_pointer_array_t);
    if (PRTE_SUCCESS != (rc = pmix_pointer_array_init(&prte_data_server_store, 1, INT_MAX, 1))) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    PMIX_CONSTRUCT(&pending, pmix_list_t);

    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DATA_SERVER,
                  PRTE_RML_PERSISTENT, prte_data_server, NULL);

    return PRTE_SUCCESS;
}

void prte_data_server_finalize(void)
{
    int32_t i;
    prte_data_object_t *data;

    if (!initialized) {
        return;
    }
    initialized = false;

    for (i = 0; i < prte_data_server_store.size; i++) {
        if (NULL
            != (data = (prte_data_object_t *) pmix_pointer_array_get_item(&prte_data_server_store,
                                                                          i))) {
            PMIX_RELEASE(data);
        }
    }
    PMIX_DESTRUCT(&prte_data_server_store);
    PMIX_LIST_DESTRUCT(&pending);
}

void prte_data_server(int status, pmix_proc_t *sender,
                      pmix_data_buffer_t *buffer,
                      prte_rml_tag_t tag, void *cbdata)
{
    uint8_t command;
    int32_t count;
    prte_data_object_t *data;
    pmix_data_buffer_t *answer, *reply;
    int rc, k;
    uint32_t ninfo, i;
    char **keys = NULL, *str;
    bool wait = false;
    int room_number;
    uint32_t uid = UINT32_MAX;
    pmix_data_range_t range;
    prte_data_req_t *req, *rqnext;
    pmix_data_buffer_t pbkt;
    pmix_byte_object_t pbo;
    pmix_status_t ret;
    pmix_proc_t requestor;
    prte_ds_info_t *rinfo;
    size_t n, nanswers;
    pmix_info_t *info;
    pmix_list_t answers;
    void *ilist;
    pmix_data_array_t darray;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    pmix_output_verbose(1, prte_data_server_output, "%s data server got message from %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender));

    /* unpack the room number of the caller's request */
    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &room_number, &count, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* unpack the command */
    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &command, &count, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    PMIX_DATA_BUFFER_CREATE(answer);
    /* pack the room number as this must lead any response */
    rc = PMIx_Data_pack(NULL, answer, &room_number, 1, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return;
    }
    /* and the command */
    rc = PMIx_Data_pack(NULL, answer, &command, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return;
    }

    switch (command) {
    case PRTE_PMIX_PUBLISH_CMD:
        data = PMIX_NEW(prte_data_object_t);

        /* unpack the publisher */
        count = 1;
        if (PMIX_SUCCESS
            != (ret = PMIx_Data_unpack(NULL, buffer, &data->owner, &count, PMIX_PROC))) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(data);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }

        pmix_output_verbose(1, prte_data_server_output,
                            "%s data server: publishing data from %s:%d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), data->owner.nspace,
                            data->owner.rank);

        /* unpack the number of infos and directives they sent */
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &ninfo, &count, PMIX_SIZE))) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(data);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }

        /* if it isn't at least one, then that's an error */
        if (1 > ninfo) {
            ret = PMIX_ERR_BAD_PARAM;
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(data);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }

        /* create the space */
        PMIX_INFO_CREATE(info, ninfo);

        /* unpack into it */
        count = ninfo;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, info, &count, PMIX_INFO))) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(data);
            PMIX_INFO_FREE(info, ninfo);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }

        PMIX_INFO_LIST_START(ilist);
        /* check for directives */
        for (n = 0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_RANGE)) {
                data->range = info[n].value.data.range;
            } else if (0 == strcmp(info[n].key, PMIX_PERSISTENCE)) {
                data->persistence = info[n].value.data.persist;
            } else if (0 == strcmp(info[n].key, PMIX_USERID)) {
                data->uid = info[n].value.data.uint32;
            } else {
                /* add it to the list */
                PMIX_INFO_LIST_XFER(ret, ilist, &info[n]);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_RELEASE(data);
                    rc = PRTE_ERR_UNPACK_FAILURE;
                    PMIX_INFO_LIST_RELEASE(ilist);
                    PMIX_INFO_FREE(info, ninfo);
                    goto SEND_ERROR;
                }
            }
        }
        PMIX_INFO_FREE(info, ninfo); // done with the array
        PMIX_INFO_LIST_CONVERT(ret, ilist, &darray);
        data->info = (pmix_info_t *) darray.array;
        data->ninfo = darray.size;
        PMIX_INFO_LIST_RELEASE(ilist);

        /* store this object */
        data->index = pmix_pointer_array_add(&prte_data_server_store, data);

        pmix_output_verbose(1, prte_data_server_output,
                            "%s data server: checking for pending requests",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

        /* check for pending requests that match this data */
        reply = NULL;
        PMIX_LIST_FOREACH_SAFE(req, rqnext, &pending, prte_data_req_t)
        {
            if (req->uid != data->uid) {
                continue;
            }
            /* if the published range is constrained to namespace, then only
             * consider this data if the publisher is
             * in the same namespace as the requestor */
            if (PMIX_RANGE_NAMESPACE == data->range) {
                if (0 != strncmp(req->requestor.nspace, data->owner.nspace, PMIX_MAX_NSLEN)) {
                    continue;
                }
            }
            for (i = 0; NULL != req->keys[i]; i++) {
                /* cycle thru the data keys for matches */
                for (n = 0; n < data->ninfo; n++) {
                    pmix_output_verbose(10, prte_data_server_output, "%s\tCHECKING %s TO %s",
                                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), data->info[n].key,
                                        req->keys[i]);
                    if (0 == strncmp(data->info[n].key, req->keys[i], PMIX_MAX_KEYLEN)) {
                        pmix_output_verbose(10, prte_data_server_output,
                                            "%s data server: packaging return",
                                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
                        /* track this response */
                        pmix_output_verbose(
                            10, prte_data_server_output,
                            "%s data server: adding %s data %s from %s:%d to response",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), data->info[n].key,
                            PMIx_Data_type_string(data->info[n].value.type), data->owner.nspace,
                            data->owner.rank);
                        rinfo = PMIX_NEW(prte_ds_info_t);
                        memcpy(&rinfo->source, &data->owner, sizeof(pmix_proc_t));
                        rinfo->info = &data->info[n];
                        pmix_list_append(&req->answers, &rinfo->super);
                        break; // a key can only occur once
                    }
                }
            }
            if (0 < (n = pmix_list_get_size(&req->answers))) {
                /* send it back to the requestor */
                pmix_output_verbose(1, prte_data_server_output,
                                    "%s data server: returning data to %s:%d",
                                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), req->requestor.nspace,
                                    req->requestor.rank);

                PMIX_DATA_BUFFER_CREATE(reply);
                /* start with their room number */
                rc = PMIx_Data_pack(NULL, reply, &req->room_number, 1, PMIX_INT);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_RELEASE(reply);
                    goto SEND_ERROR;
                }
                /* we are responding to a lookup cmd */
                command = PRTE_PMIX_LOOKUP_CMD;
                rc = PMIx_Data_pack(NULL, reply, &command, 1, PMIX_UINT8);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_RELEASE(reply);
                    goto SEND_ERROR;
                }
                /* if we found all of the requested keys, then indicate so */
                if (n == (size_t) PMIX_ARGV_COUNT_COMPAT(req->keys)) {
                    i = PRTE_SUCCESS;
                } else {
                    i = (uint32_t) PRTE_ERR_PARTIAL_SUCCESS;
                }
                /* return the status */
                rc = PMIx_Data_pack(NULL, reply, &i, 1, PMIX_INT);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_RELEASE(reply);
                    goto SEND_ERROR;
                }

                /* pack the rest into a pmix_data_buffer_t */
                PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);

                /* pack the number of returned info's */
                if (PMIX_SUCCESS != (ret = PMIx_Data_pack(NULL, &pbkt, &n, 1, PMIX_SIZE))) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                    rc = PRTE_ERR_PACK_FAILURE;
                    PMIX_DATA_BUFFER_RELEASE(reply);
                    goto SEND_ERROR;
                }
                /* loop thru and pack the individual responses - this is somewhat less
                 * efficient than packing an info array, but avoids another malloc
                 * operation just to assemble all the return values into a contiguous
                 * array */
                while (NULL != (rinfo = (prte_ds_info_t *) pmix_list_remove_first(&req->answers))) {
                    /* pack the data owner */
                    if (PMIX_SUCCESS
                        != (ret = PMIx_Data_pack(NULL, &pbkt, &rinfo->source, 1, PMIX_PROC))) {
                        PMIX_ERROR_LOG(ret);
                        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                        rc = PRTE_ERR_PACK_FAILURE;
                        PMIX_DATA_BUFFER_RELEASE(reply);
                        goto SEND_ERROR;
                    }
                    /* pack the data */
                    if (PMIX_SUCCESS
                        != (ret = PMIx_Data_pack(NULL, &pbkt, rinfo->info, 1, PMIX_INFO))) {
                        PMIX_ERROR_LOG(ret);
                        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                        rc = PRTE_ERR_PACK_FAILURE;
                        PMIX_DATA_BUFFER_RELEASE(reply);
                        goto SEND_ERROR;
                    }
                }
                PMIX_LIST_DESTRUCT(&req->answers);
                PMIX_CONSTRUCT(&req->answers, pmix_list_t);

                /* unload the pmix buffer */
                rc = PMIx_Data_unload(&pbkt, &pbo);

                /* pack it into our reply */
                rc = PMIx_Data_pack(NULL, reply, &pbo, 1, PMIX_BYTE_OBJECT);
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_RELEASE(reply);
                    goto SEND_ERROR;
                }
                PRTE_RML_SEND(rc, req->proxy.rank, reply, PRTE_RML_TAG_DATA_CLIENT);
                if (PRTE_SUCCESS != rc) {
                    PRTE_ERROR_LOG(rc);
                    PMIX_DATA_BUFFER_RELEASE(reply);
                }
            }
        }

        /* tell the user it was wonderful... */
        rc = PRTE_SUCCESS;
        rc = PMIx_Data_pack(NULL, answer, &rc, 1, PMIX_INT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        goto SEND_ANSWER;

    case PRTE_PMIX_LOOKUP_CMD:
        pmix_output_verbose(1, prte_data_server_output, "%s data server: lookup data from %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender));

        /* unpack the requestor */
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &requestor, &count, PMIX_PROC))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }

        /* unpack the number of keys */
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &ninfo, &count, PMIX_SIZE))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }
        if (0 == ninfo) {
            /* they forgot to send us the keys?? */
            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
            rc = PRTE_ERR_BAD_PARAM;
            goto SEND_ERROR;
        }

        /* unpack the keys */
        for (n = 0; n < ninfo; n++) {
            count = 1;
            if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &str, &count, PMIX_STRING))) {
                PMIX_ERROR_LOG(ret);
                rc = PRTE_ERR_UNPACK_FAILURE;
                PMIX_ARGV_FREE_COMPAT(keys);
                goto SEND_ERROR;
            }
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&keys, str);
            free(str);
        }

        /* unpack the number of directives, if any */
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &ninfo, &count, PMIX_SIZE))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }
        if (0 < ninfo) {
            PMIX_INFO_CREATE(info, ninfo);
            count = ninfo;
            if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, info, &count, PMIX_INFO))) {
                PMIX_ERROR_LOG(ret);
                PMIX_INFO_FREE(info, ninfo);
                rc = PRTE_ERR_UNPACK_FAILURE;
                goto SEND_ERROR;
            }
            /* scan the directives for things we care about */
            for (n = 0; n < ninfo; n++) {
                if (0 == strncmp(info[n].key, PMIX_USERID, PMIX_MAX_KEYLEN)) {
                    uid = info[n].value.data.uint32;
                } else if (0 == strncmp(info[n].key, PMIX_WAIT, PMIX_MAX_KEYLEN)) {
                    /* flag that we wait until the data is present */
                    wait = true;
                } else if (0 == strcmp(info[n].key, PMIX_RANGE)) {
                    range = info[n].value.data.range;
                }
            }
            /* ignore anything else for now */
            PMIX_INFO_FREE(info, ninfo);
        }

        /* cycle across the provided keys */
        PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);
        PMIX_CONSTRUCT(&answers, pmix_list_t);

        for (i = 0; NULL != keys[i]; i++) {
            pmix_output_verbose(10, prte_data_server_output, "%s data server: looking for %s",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), keys[i]);
            /* cycle across the stored data, looking for a match */
            for (k = 0; k < prte_data_server_store.size; k++) {
                data = (prte_data_object_t *) pmix_pointer_array_get_item(&prte_data_server_store,
                                                                          k);
                if (NULL == data) {
                    continue;
                }
                /* for security reasons, can only access data posted by the same user id */
                if (uid != data->uid) {
                    pmix_output_verbose(10, prte_data_server_output, "%s\tMISMATCH UID %u %u",
                                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (unsigned) uid,
                                        (unsigned) data->uid);
                    continue;
                }
                /* if the published range is constrained to namespace, then only
                 * consider this data if the publisher is
                 * in the same namespace as the requestor */
                if (PMIX_RANGE_NAMESPACE == data->range) {
                    if (0 != strncmp(requestor.nspace, data->owner.nspace, PMIX_MAX_NSLEN)) {
                        pmix_output_verbose(10, prte_data_server_output,
                                            "%s\tMISMATCH NSPACES %s %s",
                                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), requestor.nspace,
                                            data->owner.nspace);
                        continue;
                    }
                }
                /* see if we have this key */
                for (n = 0; n < data->ninfo; n++) {
                    pmix_output_verbose(10, prte_data_server_output, "%s COMPARING %s %s",
                                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), keys[i],
                                        data->info[n].key);
                    if (PMIX_CHECK_KEY(&data->info[n], keys[i])) {
                        rinfo = PMIX_NEW(prte_ds_info_t);
                        memcpy(&rinfo->source, &data->owner, sizeof(pmix_proc_t));
                        rinfo->info = &data->info[n];
                        rinfo->persistence = data->persistence;
                        pmix_list_append(&answers, &rinfo->super);
                        pmix_output_verbose(1, prte_data_server_output,
                                            "%s data server: adding %s to data from %s",
                                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), data->info[n].key,
                                            PRTE_NAME_PRINT(&data->owner));
                    }
                }
            } // loop over stored data
        }     // loop over keys

        if (0 < (nanswers = pmix_list_get_size(&answers))) {
            /* pack the number of data items found */
            if (PMIX_SUCCESS != (ret = PMIx_Data_pack(NULL, &pbkt, &nanswers, 1, PMIX_SIZE))) {
                PMIX_ERROR_LOG(ret);
                rc = PRTE_ERR_PACK_FAILURE;
                PMIX_LIST_DESTRUCT(&answers);
                PMIX_ARGV_FREE_COMPAT(keys);
                goto SEND_ERROR;
            }
            /* loop thru and pack the individual responses - this is somewhat less
             * efficient than packing an info array, but avoids another malloc
             * operation just to assemble all the return values into a contiguous
             * array */
            PMIX_LIST_FOREACH(rinfo, &answers, prte_ds_info_t)
            {
                /* pack the data owner */
                if (PMIX_SUCCESS
                    != (ret = PMIx_Data_pack(NULL, &pbkt, &rinfo->source, 1, PMIX_PROC))) {
                    PMIX_ERROR_LOG(ret);
                    rc = PRTE_ERR_PACK_FAILURE;
                    PMIX_LIST_DESTRUCT(&answers);
                    PMIX_ARGV_FREE_COMPAT(keys);
                    goto SEND_ERROR;
                }
                if (PMIX_SUCCESS
                    != (ret = PMIx_Data_pack(NULL, &pbkt, rinfo->info, 1, PMIX_INFO))) {
                    PMIX_ERROR_LOG(ret);
                    rc = PRTE_ERR_PACK_FAILURE;
                    PMIX_LIST_DESTRUCT(&answers);
                    PMIX_ARGV_FREE_COMPAT(keys);
                    goto SEND_ERROR;
                }
                if (PMIX_PERSIST_FIRST_READ == rinfo->persistence) {
                    pmix_output_verbose(1, prte_data_server_output,
                                        "%s REMOVING DATA FROM %s FOR KEY %s",
                                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                        PRTE_NAME_PRINT(&rinfo->source), rinfo->info->key);
                    memset(rinfo->info->key, 0, PMIX_MAX_KEYLEN + 1);
                }
            }
        }
        PMIX_LIST_DESTRUCT(&answers);

        if (nanswers == (size_t) PMIX_ARGV_COUNT_COMPAT(keys)) {
            rc = PRTE_SUCCESS;
        } else {
            pmix_output_verbose(1, prte_data_server_output,
                                "%s data server:lookup: at least some data not found %d vs %d",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (int) nanswers,
                                (int) PMIX_ARGV_COUNT_COMPAT(keys));

            /* if we were told to wait for the data, then queue this up
             * for later processing */
            if (wait) {
                pmix_output_verbose(1, prte_data_server_output,
                                    "%s data server:lookup: pushing request to wait",
                                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
                PMIX_DATA_BUFFER_RELEASE(answer);
                req = PMIX_NEW(prte_data_req_t);
                req->room_number = room_number;
                req->proxy = *sender;
                memcpy(&req->requestor, &requestor, sizeof(pmix_proc_t));
                req->uid = uid;
                req->range = range;
                req->keys = keys;
                pmix_list_append(&pending, &req->super);
                /* drop the partial response we have - we'll build it when everything
                 * becomes available */
                PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                return;
            }
            if (0 == nanswers) {
                /* nothing was found - indicate that situation */
                rc = PRTE_ERR_NOT_FOUND;
                PMIX_ARGV_FREE_COMPAT(keys);
                PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
                goto SEND_ERROR;
            } else {
                rc = PRTE_ERR_PARTIAL_SUCCESS;
            }
        }
        PMIX_ARGV_FREE_COMPAT(keys);
        pmix_output_verbose(1, prte_data_server_output, "%s data server:lookup: data found",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        /* pack the status */
        rc = PMIx_Data_pack(NULL, answer, &rc, 1, PMIX_INT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(answer);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            return;
        }
        /* unload the packed values */
        rc = PMIx_Data_unload(&pbkt, &pbo);

        /* pack it into our reply */
        rc = PMIx_Data_pack(NULL, answer, &pbo, 1, PMIX_BYTE_OBJECT);
        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(answer);
            goto SEND_ERROR;
        }
        goto SEND_ANSWER;

    case PRTE_PMIX_UNPUBLISH_CMD:
        /* unpack the requestor */
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &requestor, &count, PMIX_PROC))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }

        pmix_output_verbose(1, prte_data_server_output, "%s data server: unpublish data from %s:%d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), requestor.nspace, requestor.rank);

        /* unpack the number of keys */
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &ninfo, &count, PMIX_SIZE))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }
        if (0 == ninfo) {
            /* they forgot to send us the keys?? */
            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
            rc = PRTE_ERR_BAD_PARAM;
            goto SEND_ERROR;
        }

        /* unpack the keys */
        for (n = 0; n < ninfo; n++) {
            count = 1;
            if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &str, &count, PMIX_STRING))) {
                PMIX_ERROR_LOG(ret);
                rc = PRTE_ERR_UNPACK_FAILURE;
                PMIX_ARGV_FREE_COMPAT(keys);
                goto SEND_ERROR;
            }
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&keys, str);
            free(str);
        }

        /* unpack the number of directives, if any */
        range = PMIX_RANGE_SESSION; // default
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &ninfo, &count, PMIX_SIZE))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }
        if (0 < ninfo) {
            PMIX_INFO_CREATE(info, ninfo);
            count = ninfo;
            if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, info, &count, PMIX_INFO))) {
                PMIX_ERROR_LOG(ret);
                PMIX_INFO_FREE(info, ninfo);
                rc = PRTE_ERR_UNPACK_FAILURE;
                goto SEND_ERROR;
            }
            /* scan the directives for things we care about */
            for (n = 0; n < ninfo; n++) {
                if (0 == strncmp(info[n].key, PMIX_USERID, PMIX_MAX_KEYLEN)) {
                    uid = info[n].value.data.uint32;
                } else if (0 == strncmp(info[n].key, PMIX_RANGE, PMIX_MAX_KEYLEN)) {
                    range = info[n].value.data.range;
                }
            }
            /* ignore anything else for now */
            PMIX_INFO_FREE(info, ninfo);
        }

        /* cycle across the provided keys */
        for (i = 0; NULL != keys[i]; i++) {
            /* cycle across the stored data, looking for a match */
            for (k = 0; k < prte_data_server_store.size; k++) {
                data = (prte_data_object_t *) pmix_pointer_array_get_item(&prte_data_server_store,
                                                                          k);
                if (NULL == data) {
                    continue;
                }
                /* can only access data posted by the same user id */
                if (uid != data->uid) {
                    continue;
                }
                /* can only access data posted by the same process */
                if (0 != strncmp(requestor.nspace, data->owner.nspace, PMIX_MAX_NSLEN)
                    || requestor.rank != data->owner.rank) {
                    continue;
                }
                /* can only access data posted for the same range */
                if (range != data->range) {
                    continue;
                }
                /* see if we have this key */
                nanswers = 0;
                for (n = 0; n < data->ninfo; n++) {
                    if (0 == strlen(data->info[n].key)) {
                        ++nanswers;
                        continue;
                    }
                    if (0 == strncmp(data->info[n].key, keys[i], PMIX_MAX_KEYLEN)) {
                        /* found it -  delete the object from the data store */
                        memset(data->info[n].key, 0, PMIX_MAX_KEYLEN + 1);
                        ++nanswers;
                    }
                }
                /* if all the data has been removed, then remove the object */
                if (nanswers == data->ninfo) {
                    pmix_pointer_array_set_item(&prte_data_server_store, k, NULL);
                    PMIX_RELEASE(data);
                }
            }
        }
        PMIX_ARGV_FREE_COMPAT(keys);

        /* tell the sender this succeeded */
        ret = PRTE_SUCCESS;
        rc = PMIx_Data_pack(NULL, answer, &ret, 1, PMIX_INT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        goto SEND_ANSWER;

    case PRTE_PMIX_PURGE_PROC_CMD:
        /* unpack the proc whose data is to be purged - session
         * data is purged by providing a requestor whose rank
         * is wildcard */
        count = 1;
        if (PMIX_SUCCESS != (ret = PMIx_Data_unpack(NULL, buffer, &requestor, &count, PMIX_PROC))) {
            PMIX_ERROR_LOG(ret);
            rc = PRTE_ERR_UNPACK_FAILURE;
            goto SEND_ERROR;
        }

        pmix_output_verbose(1, prte_data_server_output, "%s data server: purge data from %s:%d",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), requestor.nspace, requestor.rank);

        /* cycle across the stored data, looking for a match */
        for (k = 0; k < prte_data_server_store.size; k++) {
            data = (prte_data_object_t *) pmix_pointer_array_get_item(&prte_data_server_store, k);
            if (NULL == data) {
                continue;
            }
            /* check if data posted by the specified process */
            if (0 != strncmp(requestor.nspace, data->owner.nspace, PMIX_MAX_NSLEN)
                || (PMIX_RANK_WILDCARD != requestor.rank && requestor.rank != data->owner.rank)) {
                continue;
            }
            /* check persistence - if it is intended to persist beyond the
             * proc itself, then we only delete it if rank=wildcard*/
            if ((data->persistence == PMIX_PERSIST_APP || data->persistence == PMIX_PERSIST_SESSION)
                && PMIX_RANK_WILDCARD != requestor.rank) {
                continue;
            }
            /* remove the object */
            pmix_pointer_array_set_item(&prte_data_server_store, k, NULL);
            PMIX_RELEASE(data);
        }
        /* no response is required */
        PMIX_RELEASE(answer);
        return;

    default:
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        rc = PRTE_ERR_BAD_PARAM;
        break;
    }

SEND_ERROR:
    pmix_output_verbose(1, prte_data_server_output, "%s data server: sending error %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_ERROR_NAME(rc));
    /* pack the error code */
    rc = PMIx_Data_pack(NULL, answer, &rc, 1, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }

SEND_ANSWER:
    PRTE_RML_SEND(rc, sender->rank, answer, PRTE_RML_TAG_DATA_CLIENT);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(answer);
    }
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress_threads.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix_native.h"

static int native_init(void);
static int native_fini(void);
static bool native_initialized(void);
static int native_abort(int flag, const char msg[]);
static int native_fence(opal_process_name_t *procs, size_t nprocs);
static int native_fence_nb(opal_process_name_t *procs, size_t nprocs,
                           opal_pmix_cbfunc_t cbfunc, void *cbdata);
static int native_put(opal_pmix_scope_t scope,
                      opal_value_t *kv);
static int native_get(const opal_identifier_t *id,
                      const char *key,
                      opal_value_t **kv);
static void native_get_nb(const opal_identifier_t *id,
                          const char *key,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata);
static int native_publish(const char service_name[],
                          opal_list_t *info,
                          const char port[]);
static int native_lookup(const char service_name[],
                         opal_list_t *info,
                         char port[], int portLen);
static int native_unpublish(const char service_name[], 
                            opal_list_t *info);
static bool native_get_attr(const char *attr, opal_value_t **kv);
static int native_get_attr_nb(const char *attr,
                              opal_pmix_cbfunc_t cbfunc,
                              void *cbdata);
static int native_spawn(int count, const char * cmds[],
                        int argcs[], const char ** argvs[],
                        const int maxprocs[],
                        opal_list_t *info_keyval_vector,
                        opal_list_t *preput_keyval_vector,
                        char jobId[], int jobIdSize,
                        int errors[]);
static int native_job_connect(const char jobId[]);
static int native_job_disconnect(const char jobId[]);

const opal_pmix_base_module_t opal_pmix_native_module = {
    native_init,
    native_fini,
    native_initialized,
    native_abort,
    native_fence,
    native_fence_nb,
    native_put,
    native_get,
    native_get_nb,
    native_publish,
    native_lookup,
    native_unpublish,
    native_get_attr,
    native_get_attr_nb,
    native_spawn,
    native_job_connect,
    native_job_disconnect,
    opal_pmix_base_register_handler,
    opal_pmix_base_deregister_handler
};

// local variables
static int init_cntr = 0;
static struct {
    uint32_t jid;
    uint32_t vid;
} native_pname;
static char *local_uri = NULL;

/* callback for wait completion */
static void wait_cbfunc(opal_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    int status=OPAL_SUCCESS;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native recv callback activated with %d bytes",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        (NULL == buf) ? -1 : (int)buf->bytes_used);

    if (NULL != buf) {
        /* transfer the data to the cb */
        opal_dss.copy_payload(&cb->data, buf);
    }
    if (NULL != cb->cbfunc) {
        cb->cbfunc(status, NULL, cb->cbdata);
    }
    cb->active = false;
}

static int native_init(void)
{
    char **uri, *srv;

    ++init_cntr;
    if (1 < init_cntr) {
        return OPAL_SUCCESS;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native init called",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* if we don't have a path to the daemon rendezvous point,
     * then we need to return an error UNLESS we have been directed
     * to allow init prior to having an identified server. This is
     * needed for singletons as they will start without a server
     * to support them, but may have one assigned at a later time */
    if (NULL == mca_pmix_native_component.uri) {
        if (!opal_pmix_base_allow_delayed_server) {
            /* not ready yet, so decrement our init_cntr so we can come thru
             * here again */
            --init_cntr;
            /* let the caller know that the server isn't available yet */
            return OPAL_ERR_SERVER_NOT_AVAIL;
        }
        if (NULL == (srv = getenv("PMIX_SERVER_URI"))) {
            /* error out - should have been here, but isn't */
            return OPAL_ERROR;
        }
        mca_pmix_native_component.uri = strdup(srv);
        mca_pmix_native_component.id = OPAL_PROC_MY_NAME;
    }

    /* if we have it, setup the path to the daemon rendezvous point */
    if (NULL != mca_pmix_native_component.uri) {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s pmix:native constructing component fields with server %s",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            mca_pmix_native_component.uri);

        memset(&mca_pmix_native_component.address, 0, sizeof(struct sockaddr_un));
        mca_pmix_native_component.address.sun_family = AF_UNIX;
        uri = opal_argv_split(mca_pmix_native_component.uri, ':');
        if (2 != opal_argv_count(uri)) {
            return OPAL_ERROR;
        }
        /* if the rendezvous file doesn't exist, that's an error */
        if (0 != access(uri[1], R_OK)) {
            return OPAL_ERR_NOT_FOUND;
        }
        mca_pmix_native_component.server = strtoull(uri[0], NULL, 10);
        snprintf(mca_pmix_native_component.address.sun_path,
                 sizeof(mca_pmix_native_component.address.sun_path)-1,
                 "%s", uri[1]);
        opal_argv_free(uri);

        /* create an event base and progress thread for us */
        if (NULL == (mca_pmix_native_component.evbase = opal_start_progress_thread("pmix_native", true))) {
            return OPAL_ERROR;
        }
    }

    /* we will connect on first send */

    return OPAL_SUCCESS;
}

static int native_fini(void)
{
    opal_buffer_t *msg;
    pmix_cb_t *cb;
    pmix_cmd_t cmd = PMIX_FINALIZE_CMD;
    int rc;

    if (1 != init_cntr) {
        --init_cntr;
       return OPAL_SUCCESS;
    }
    init_cntr = 0;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native finalize called",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    if (NULL == mca_pmix_native_component.uri) {
        /* nothing was setup, so return */
        return OPAL_SUCCESS;
    }

    if (PMIX_USOCK_CONNECTED == mca_pmix_native_component.state) {
        /* setup a cmd message to notify the PMIx
         * server that we are normally terminating */
        msg = OBJ_NEW(opal_buffer_t);
        /* pack the cmd */
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }

        /* create a callback object as we need to pass it to the
         * recv routine so we know which callback to use when
         * the return message is recvd */
        cb = OBJ_NEW(pmix_cb_t);
        cb->active = true;

        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s pmix:native sending finalize sync to server",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

        /* push the message into our event base to send to the server */
        PMIX_ACTIVATE_SEND_RECV(msg, wait_cbfunc, cb);

        /* wait for the ack to return */
        PMIX_WAIT_FOR_COMPLETION(cb->active);
        OBJ_RELEASE(cb);
    }

    if (NULL != mca_pmix_native_component.evbase) {
        opal_stop_progress_thread("pmix_native", true);
        mca_pmix_native_component.evbase = NULL;
    }

    if (0 <= mca_pmix_native_component.sd) {
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
    }

    return OPAL_SUCCESS;
}

static bool native_initialized(void)
{
    if (0 < init_cntr) {
        return true;
    }
    return false;
}

static int native_abort(int flag, const char msg[])
{
    opal_buffer_t *bfr;
    pmix_cmd_t cmd = PMIX_ABORT_CMD;
    int rc;
    pmix_cb_t *cb;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native abort called",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    if (NULL == mca_pmix_native_component.uri) {
        /* no server available, so just return */
        return OPAL_SUCCESS;
    }

    /* create a buffer to hold the message */
    bfr = OBJ_NEW(opal_buffer_t);
    /* pack the cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(bfr, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(bfr);
        return rc;
    }
    /* pack the status flag */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(bfr, &flag, 1, OPAL_INT))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(bfr);
        return rc;
    }
    /* pack the string message - a NULL is okay */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(bfr, &msg, 1, OPAL_STRING))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(bfr);
        return rc;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = OBJ_NEW(pmix_cb_t);
    cb->active = true;

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(bfr, wait_cbfunc, cb);

    /* wait for the release */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    OBJ_RELEASE(cb);
    return OPAL_SUCCESS;
}

static int native_spawn(int count, const char * cmds[],
                        int argcs[], const char ** argvs[],
                        const int maxprocs[],
                        opal_list_t *info_keyval_vector,
                        opal_list_t *preput_keyval_vector,
                        char jobId[], int jobIdSize,
                        int errors[])
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int native_put(opal_pmix_scope_t scope,
                      opal_value_t *kv)
{
    int rc;

    /* pack the cache that matches the scope */
    if (PMIX_LOCAL == scope) {
        if (NULL == mca_pmix_native_component.cache_local) {
            mca_pmix_native_component.cache_local = OBJ_NEW(opal_buffer_t);
        }
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s pmix:native put local data for key %s",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(mca_pmix_native_component.cache_local, &kv, 1, OPAL_VALUE))) {
            OPAL_ERROR_LOG(rc);
        }
    } else if (PMIX_REMOTE == scope) {
        if (NULL == mca_pmix_native_component.cache_remote) {
            mca_pmix_native_component.cache_remote = OBJ_NEW(opal_buffer_t);
        }
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s pmix:native put remote data for key %s",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(mca_pmix_native_component.cache_remote, &kv, 1, OPAL_VALUE))) {
            OPAL_ERROR_LOG(rc);
        }
    } else {
        /* must be global */
        if (NULL == mca_pmix_native_component.cache_global) {
            mca_pmix_native_component.cache_global = OBJ_NEW(opal_buffer_t);
        }
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s pmix:native put global data for key %s",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kv->key);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(mca_pmix_native_component.cache_global, &kv, 1, OPAL_VALUE))) {
            OPAL_ERROR_LOG(rc);
        }
    }

    /* if this is our uri, save it as we need to send it to our server
     * as a special, separate item */
    if (0 == strcmp(OPAL_DSTORE_URI, kv->key)) {
        local_uri = strdup(kv->data.string);
    }

    /* have to save a copy locally as some of our components will
     * look for it */
    (void)opal_dstore.store(opal_dstore_internal, &OPAL_PROC_MY_NAME, kv);
    return rc;
}


static int native_fence(opal_process_name_t *procs, size_t nprocs)
{
    opal_buffer_t *msg, *bptr;
    pmix_cmd_t cmd = PMIX_FENCE_CMD;
    pmix_cb_t *cb;
    int rc, ret;
    opal_pmix_scope_t scope;
    int32_t cnt;
    opal_value_t *kp;
    opal_identifier_t id;
    size_t i;
    uint32_t np;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native executing fence on %u procs",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), (unsigned int)nprocs);

    if (NULL == mca_pmix_native_component.uri) {
        /* no server available, so just return */
        return OPAL_SUCCESS;
    }

    msg = OBJ_NEW(opal_buffer_t);
    /* pack the fence cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    /* pack the number of procs */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &nprocs, 1, OPAL_SIZE))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    if (0 < nprocs) {
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, procs, nprocs, OPAL_UINT64))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
    }
    /* provide our URI */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &local_uri, 1, OPAL_STRING))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    /* only do it once */
    if (NULL != local_uri) {
        free(local_uri);
        local_uri = NULL;
    }

    /* if we haven't already done it, ensure we have committed our values */
    if (NULL != mca_pmix_native_component.cache_local) {
        scope = PMIX_LOCAL;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &scope, 1, PMIX_SCOPE_T))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &mca_pmix_native_component.cache_local, 1, OPAL_BUFFER))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        OBJ_RELEASE(mca_pmix_native_component.cache_local);
    }
    if (NULL != mca_pmix_native_component.cache_remote) {
        scope = PMIX_REMOTE;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &scope, 1, PMIX_SCOPE_T))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &mca_pmix_native_component.cache_remote, 1, OPAL_BUFFER))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        OBJ_RELEASE(mca_pmix_native_component.cache_remote);
    }
    if (NULL != mca_pmix_native_component.cache_global) {
        scope = PMIX_GLOBAL;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &scope, 1, PMIX_SCOPE_T))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &mca_pmix_native_component.cache_global, 1, OPAL_BUFFER))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        OBJ_RELEASE(mca_pmix_native_component.cache_global);
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = OBJ_NEW(pmix_cb_t);
    cb->active = true;

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(msg, wait_cbfunc, cb);

    /* wait for the fence to complete */
    PMIX_WAIT_FOR_COMPLETION(cb->active);

    /* get the number of contributors */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&cb->data, &np, &cnt, OPAL_UINT64))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    /* if data was returned, unpack and store it */
    for (i=0; i < np; i++) {
        /* get the buffer that contains the data for the next proc */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&cb->data, &msg, &cnt, OPAL_BUFFER))) {
            if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
                break;
            }
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        /* extract the id of the contributor from the blob */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(msg, &id, &cnt, OPAL_UINT64))) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
        /* extract all blobs from this proc, starting with the scope */
        cnt = 1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(msg, &scope, &cnt, PMIX_SCOPE_T))) {
            /* extract the blob for this scope */
            cnt = 1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(msg, &bptr, &cnt, OPAL_BUFFER))) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
            /* now unpack and store the values - everything goes into our internal store */
            cnt = 1;
            while (OPAL_SUCCESS == (rc = opal_dss.unpack(bptr, &kp, &cnt, OPAL_VALUE))) {
                if (OPAL_SUCCESS != (ret = opal_dstore.store(opal_dstore_internal, &id, kp))) {
                    OPAL_ERROR_LOG(ret);
                }
                OBJ_RELEASE(kp);
                cnt = 1;
            }
            OBJ_RELEASE(bptr);
            cnt = 1;
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            OPAL_ERROR_LOG(rc);
        }
        OBJ_RELEASE(msg);
    }
    if (OPAL_SUCCESS != rc && OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        OPAL_ERROR_LOG(rc);
    } else {
        rc = OPAL_SUCCESS;
    }

    OBJ_RELEASE(cb);

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native fence released",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    return OPAL_SUCCESS;
}

static void fencenb_cbfunc(opal_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    opal_buffer_t *msg, *bptr;
    int rc, ret;
    opal_pmix_scope_t scope;
    int32_t cnt;
    opal_value_t *kp;
    opal_identifier_t id;
    size_t i;
    uint32_t np;

    /* get the number of contributors */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &np, &cnt, OPAL_UINT64))) {
        OPAL_ERROR_LOG(rc);
        return;
    }

    /* if data was returned, unpack and store it */
    for (i=0; i < np; i++) {
        /* get the buffer that contains the data for the next proc */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(buf, &msg, &cnt, OPAL_BUFFER))) {
            if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
                break;
            }
            OPAL_ERROR_LOG(rc);
            return;
        }
        /* extract the id of the contributor from the blob */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(msg, &id, &cnt, OPAL_UINT64))) {
            OPAL_ERROR_LOG(rc);
            return;
        }
        /* extract all blobs from this proc, starting with the scope */
        cnt = 1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(msg, &scope, &cnt, PMIX_SCOPE_T))) {
            /* extract the blob for this scope */
            cnt = 1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(msg, &bptr, &cnt, OPAL_BUFFER))) {
                OPAL_ERROR_LOG(rc);
                return;
            }
            /* now unpack and store the values - everything goes into our internal store */
            cnt = 1;
            while (OPAL_SUCCESS == (rc = opal_dss.unpack(bptr, &kp, &cnt, OPAL_VALUE))) {
                if (OPAL_SUCCESS != (ret = opal_dstore.store(opal_dstore_internal, &id, kp))) {
                    OPAL_ERROR_LOG(ret);
                }
                OBJ_RELEASE(kp);
                cnt = 1;
            }
            OBJ_RELEASE(bptr);
            cnt = 1;
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            OPAL_ERROR_LOG(rc);
        }
        OBJ_RELEASE(msg);
    }
    if (OPAL_SUCCESS != rc && OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        OPAL_ERROR_LOG(rc);
    }

    /* if a callback was provided, execute it */
    if (NULL != cb && NULL != cb->cbfunc) {
        cb->cbfunc(rc, NULL, cb->cbdata);
    }
    OBJ_RELEASE(cb);
}

static int native_fence_nb(opal_process_name_t *procs, size_t nprocs,
                           opal_pmix_cbfunc_t cbfunc, void *cbdata)
{
    opal_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FENCENB_CMD;
    int rc;
    pmix_cb_t *cb;
    opal_pmix_scope_t scope;

    if (NULL == mca_pmix_native_component.uri) {
        /* no server available, so just execute the callback */
        if (NULL != cbfunc) {
            cbfunc(OPAL_SUCCESS, NULL, cbdata);
        }
        return OPAL_SUCCESS;
    }

    msg = OBJ_NEW(opal_buffer_t);
    /* pack the fence cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    /* pack the number of procs */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &nprocs, 1, OPAL_SIZE))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    if (0 < nprocs) {
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, procs, nprocs, OPAL_UINT64))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
    }
    /* provide our URI */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &local_uri, 1, OPAL_STRING))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    /* only do it once */
    if (NULL != local_uri) {
        free(local_uri);
        local_uri = NULL;
    }

    /* if we haven't already done it, ensure we have committed our values */
    if (NULL != mca_pmix_native_component.cache_local) {
        scope = PMIX_LOCAL;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &scope, 1, PMIX_SCOPE_T))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &mca_pmix_native_component.cache_local, 1, OPAL_BUFFER))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        OBJ_RELEASE(mca_pmix_native_component.cache_local);
    }
    if (NULL != mca_pmix_native_component.cache_remote) {
        scope = PMIX_REMOTE;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &scope, 1, PMIX_SCOPE_T))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &mca_pmix_native_component.cache_remote, 1, OPAL_BUFFER))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        OBJ_RELEASE(mca_pmix_native_component.cache_remote);
    }
    if (NULL != mca_pmix_native_component.cache_global) {
        scope = PMIX_GLOBAL;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &scope, 1, PMIX_SCOPE_T))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &mca_pmix_native_component.cache_global, 1, OPAL_BUFFER))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(msg);
            return rc;
        }
        OBJ_RELEASE(mca_pmix_native_component.cache_global);
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = OBJ_NEW(pmix_cb_t);
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(msg, fencenb_cbfunc, cb);

    return OPAL_SUCCESS;
}

static int native_get(const opal_identifier_t *id,
                      const char *key,
                      opal_value_t **kv)
{
    opal_buffer_t *msg, *bptr;
    pmix_cmd_t cmd = PMIX_GET_CMD;
    pmix_cb_t *cb;
    int rc, ret;
    int32_t cnt;
    opal_list_t vals;
    opal_value_t *kp;
    bool found;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native getting value for proc %s key %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        OPAL_NAME_PRINT(*id), key);

    /* first see if we already have the info in our dstore */
    OBJ_CONSTRUCT(&vals, opal_list_t);
    if (OPAL_SUCCESS == opal_dstore.fetch(opal_dstore_internal, id,
                                          key, &vals)) {
        *kv = (opal_value_t*)opal_list_remove_first(&vals);
        OPAL_LIST_DESTRUCT(&vals);
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s pmix:native value retrieved from dstore",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        return OPAL_SUCCESS;
    }

    if (NULL == mca_pmix_native_component.uri) {
        /* no server available, so just return */
        return OPAL_ERR_NOT_FOUND;
    }

    /* nope - see if we can get it */
    msg = OBJ_NEW(opal_buffer_t);
    /* pack the get cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    /* pack the request information - we'll get the entire blob
     * for this proc, so we don't need to pass the key */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, id, 1, OPAL_UINT64))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = OBJ_NEW(pmix_cb_t);
    cb->active = true;

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(msg, wait_cbfunc, cb);

    /* wait for the data to return */
    PMIX_WAIT_FOR_COMPLETION(cb->active);

    /* we have received the entire data blob for this process - unpack
     * and cache all values, keeping the one we requested to return
     * to the caller */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&cb->data, &ret, &cnt, OPAL_INT))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(cb);
        return rc;
    }
    found = false;
    cnt = 1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&cb->data, &bptr, &cnt, OPAL_BUFFER))) {
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(bptr, &kp, &cnt, OPAL_VALUE))) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s pmix:native retrieved %s (%s) from server for proc %s",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kp->key,
                                (OPAL_STRING == kp->type) ? kp->data.string : "NS",
                                OPAL_NAME_PRINT(*id));
            if (OPAL_SUCCESS != (ret = opal_dstore.store(opal_dstore_internal, id, kp))) {
                OPAL_ERROR_LOG(ret);
            }
            if (0 == strcmp(key, kp->key)) {
                *kv = kp;
                found = true;
            } else {
                OBJ_RELEASE(kp);
            }
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            OPAL_ERROR_LOG(rc);
        }
        OBJ_RELEASE(bptr);
        cnt = 1;
    }
    if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        OPAL_ERROR_LOG(rc);
    } else {
        rc = OPAL_SUCCESS;
    }
    OBJ_RELEASE(cb);

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native get completed",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
    if (found) {
        return OPAL_SUCCESS;
    }
    *kv = NULL;
    if (OPAL_SUCCESS == rc) {
        rc = ret;
    }
    return rc;
}

static void native_get_nb(const opal_identifier_t *id,
                          const char *key,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata)
{
    return;
}

static int native_publish(const char service_name[],
                          opal_list_t *info,
                          const char port[])
{
    return OPAL_SUCCESS;
}

static int native_lookup(const char service_name[],
                         opal_list_t *info,
                         char port[], int portLen)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int native_unpublish(const char service_name[], 
                            opal_list_t *info)
{
    return OPAL_SUCCESS;;
}

static bool native_get_attr(const char *attr, opal_value_t **kv)
{
    opal_buffer_t *msg, *bptr;
    opal_list_t vals;
    opal_value_t *kp, *lclpeers=NULL, kvn;
    pmix_cmd_t cmd = PMIX_GETATTR_CMD;
    char **ranks;
    int rc, ret;
    int32_t cnt;
    bool found=false;
    opal_hwloc_locality_t locality;
    pmix_cb_t *cb;
    uint32_t i, myrank;
    opal_identifier_t id;
    char *cpuset;
    opal_buffer_t buf;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s pmix:native get_attr called",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* try to retrieve the requested value from the dstore */
    OBJ_CONSTRUCT(&vals, opal_list_t);
    if (OPAL_SUCCESS == opal_dstore.fetch(opal_dstore_internal, &OPAL_PROC_MY_NAME, attr, &vals)) {
        *kv = (opal_value_t*)opal_list_remove_first(&vals);
        OPAL_LIST_DESTRUCT(&vals);
        return true;
    }

    if (NULL == mca_pmix_native_component.uri) {
        /* no server available, so just return */
        return OPAL_ERR_NOT_FOUND;
    }

    /* if the value isn't yet available, then we should try to retrieve
     * all the available attributes and store them for future use */
    msg = OBJ_NEW(opal_buffer_t);
    /* pack the cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return false;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = OBJ_NEW(pmix_cb_t);
    cb->active = true;

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(msg, wait_cbfunc, cb);

    /* wait for the data to return */
    PMIX_WAIT_FOR_COMPLETION(cb->active);

    /* we have received the entire data blob for this process - unpack
     * and cache all values, keeping the one we requested to return
     * to the caller */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&cb->data, &ret, &cnt, OPAL_INT))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(cb);
        return false;
    }
    if (OPAL_SUCCESS == ret) {
        /* unpack the buffer containing the values */
        cnt = 1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&cb->data, &bptr, &cnt, OPAL_BUFFER))) {
            OPAL_ERROR_LOG(rc);
            OBJ_RELEASE(cb);
            return false;
        }
        cnt = 1;
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(bptr, &kp, &cnt, OPAL_VALUE))) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s unpacked attr %s",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), kp->key);
            /* if this is the local topology, we need to save it in a special way */
#if OPAL_HAVE_HWLOC
            {
                hwloc_topology_t topo;
                if (0 == strcmp(PMIX_LOCAL_TOPO, kp->key)) {
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s saving topology",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                    /* transfer the byte object for unpacking */
                    OBJ_CONSTRUCT(&buf, opal_buffer_t);
                    opal_dss.load(&buf, kp->data.bo.bytes, kp->data.bo.size);
                    kp->data.bo.bytes = NULL;  // protect the data region
                    kp->data.bo.size = 0;
                    OBJ_RELEASE(kp);
                    /* extract the topology */
                    cnt=1;
                    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &topo, &cnt, OPAL_HWLOC_TOPO))) {
                        OPAL_ERROR_LOG(rc);
                        OBJ_DESTRUCT(&buf);
                        continue;
                    }
                    OBJ_DESTRUCT(&buf);
                    if (NULL == opal_hwloc_topology) {
                        opal_hwloc_topology = topo;
                    } else {
                        hwloc_topology_destroy(topo);
                    }
                    cnt = 1;
                    continue;
                }
            }
#endif
            /* if this is the local cpuset blob, then unpack and store its contents */
            if (0 == strcmp(PMIX_LOCAL_CPUSETS, kp->key)) {
                opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                    "%s received local cpusets",
                                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                /* transfer the byte object for unpacking */
                OBJ_CONSTRUCT(&buf, opal_buffer_t);
                opal_dss.load(&buf, kp->data.bo.bytes, kp->data.bo.size);
                kp->data.bo.bytes = NULL;  // protect the data region
                kp->data.bo.size = 0;
                OBJ_RELEASE(kp);
                cnt=1;
                while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &id, &cnt, OPAL_UINT64))) {
                    cnt=1;
                    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &cpuset, &cnt, OPAL_STRING))) {
                        OPAL_ERROR_LOG(rc);
                        OBJ_DESTRUCT(&buf);
                        cnt = 1;
                        continue;
                    }
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s saving cpuset %s for local peer %s",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                        (NULL == cpuset) ? "NULL" : cpuset,
                                        OPAL_NAME_PRINT(id));
                    OBJ_CONSTRUCT(&kvn, opal_value_t);
                    kvn.key = strdup(OPAL_DSTORE_CPUSET);
                    kvn.type = OPAL_STRING;
                    kvn.data.string = cpuset;
                    if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal, &id, &kvn))) {
                        OPAL_ERROR_LOG(rc);
                        OBJ_DESTRUCT(&kvn);
                        cnt = 1;
                        continue;
                    }
                    OBJ_DESTRUCT(&kvn);
                }
                OBJ_DESTRUCT(&buf);
                if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
                    OPAL_ERROR_LOG(rc);
                    return false;
                }
                cnt=1;
                continue;
            }
            if (OPAL_SUCCESS != (rc = opal_dstore.store(opal_dstore_internal, &OPAL_PROC_MY_NAME, kp))) {
                OPAL_ERROR_LOG(rc);
                OBJ_RELEASE(kp);
                cnt = 1;
                continue;
            }
            /* save the list of local peers */
            if (0 == strcmp(PMIX_LOCAL_PEERS, kp->key)) {
                OBJ_RETAIN(kp);
                lclpeers = kp;
                opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                    "%s saving local peers %s",
                                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), lclpeers->data.string);
            } else if (0 == strcmp(PMIX_JOBID, kp->key)) {
                native_pname.jid = kp->data.uint32;
            } else if (0 == strcmp(PMIX_RANK, kp->key)) {
                native_pname.vid = kp->data.uint32;
            }
            if (0 == strcmp(attr, kp->key)) {
                OBJ_RETAIN(kp);
                *kv = kp;
                found = true;
            }
            OBJ_RELEASE(kp);
            cnt = 1;
        }
        OBJ_RELEASE(bptr);
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            OPAL_ERROR_LOG(rc);
            return false;
        }
    } else {
        OPAL_ERROR_LOG(ret);
        OBJ_RELEASE(cb);
        return false;
    }
    OBJ_RELEASE(cb);
    opal_proc_set_name((opal_process_name_t*)&native_pname);

    /* if the list of local peers wasn't included, then we are done */
    if (NULL == lclpeers) {
        opal_output_verbose(0, opal_pmix_base_framework.framework_output,
                            "%s no local peers reported",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        return found;
    }

    /* baseline all the procs as nonlocal */
    myrank = native_pname.vid;
    /* we only need to set locality for each local rank as "not found"
     * equates to "non local" */
    ranks = opal_argv_split(lclpeers->data.string, ',');
    for (i=0; NULL != ranks[i]; i++) {
        if (myrank == i) {
            continue;
        }
        native_pname.vid = strtoul(ranks[i], NULL, 10);
#if OPAL_HAVE_HWLOC
        OBJ_CONSTRUCT(&vals, opal_list_t);
        if (OPAL_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_internal, (opal_identifier_t*)&native_pname,
                                                    OPAL_DSTORE_CPUSET, &vals))) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s cpuset for local proc %s not found",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                OPAL_NAME_PRINT(*(opal_identifier_t*)&native_pname));
            OPAL_LIST_DESTRUCT(&vals);
            /* even though the cpuset wasn't found, we at least know it is
             * on the same node with us */
            locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
        } else {
            kp = (opal_value_t*)opal_list_get_first(&vals);
            if (NULL == kp->data.string) {
                /* if we share a node, but we don't know anything more, then
                 * mark us as on the node as this is all we know
                 */
                locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
            } else {
                /* determine relative location on our node */
                locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                 opal_process_info.cpuset,
                                                                 kp->data.string);
            }
            OPAL_LIST_DESTRUCT(&vals);
        }
#else
        /* all we know is we share a node */
        locality = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
#endif
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_base_framework.framework_output,
                             "%s pmix:native proc %s locality %s",
                             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                             OPAL_NAME_PRINT(*(opal_identifier_t*)&native_pname),
                             opal_hwloc_base_print_locality(locality)));
    
        OBJ_CONSTRUCT(&kvn, opal_value_t);
        kvn.key = strdup(OPAL_DSTORE_LOCALITY);
        kvn.type = OPAL_UINT16;
        kvn.data.uint16 = locality;
        (void)opal_dstore.store(opal_dstore_internal, (opal_identifier_t*)&native_pname, &kvn);
        OBJ_DESTRUCT(&kvn);
    }
    opal_argv_free(ranks);

    return found;
}

static int native_get_attr_nb(const char *attr,
                              opal_pmix_cbfunc_t cbfunc,
                              void *cbdata)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int native_job_connect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int native_job_disconnect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

/***   INSTANTIATE INTERNAL CLASSES   ***/
static void scon(pmix_usock_send_t *p)
{
    p->hdr.type = 0;
    p->hdr.tag = UINT32_MAX;
    p->hdr.nbytes = 0;
    p->data = NULL;
    p->hdr_sent = false;
    p->sdptr = NULL;
    p->sdbytes = 0;
}
OBJ_CLASS_INSTANCE(pmix_usock_send_t,
                   opal_list_item_t,
                   scon, NULL);

static void rcon(pmix_usock_recv_t *p)
{
    p->hdr.type = 0;
    p->hdr.tag = UINT32_MAX;
    p->hdr.nbytes = 0;
    p->data = NULL;
    p->hdr_recvd = false;
    p->rdptr = NULL;
    p->rdbytes = 0;
}
OBJ_CLASS_INSTANCE(pmix_usock_recv_t,
                   opal_list_item_t,
                   rcon, NULL);

static void prcon(pmix_usock_posted_recv_t *p)
{
    p->tag = UINT32_MAX;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(pmix_usock_posted_recv_t,
                   opal_list_item_t,
                   prcon, NULL);

static void cbcon(pmix_cb_t *p)
{
    p->active = false;
    OBJ_CONSTRUCT(&p->data, opal_buffer_t);
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
static void cbdes(pmix_cb_t *p)
{
    OBJ_DESTRUCT(&p->data);
}
OBJ_CLASS_INSTANCE(pmix_cb_t,
                   opal_object_t,
                   cbcon, cbdes);


static void srcon(pmix_usock_sr_t *p)
{
    p->bfr = NULL;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(pmix_usock_sr_t,
                   opal_object_t,
                   srcon, NULL);

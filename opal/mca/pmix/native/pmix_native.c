/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/types.h"

#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include <string.h>

#include "pmix_native.h"

static int native_init(void);
static int native_fini(void);
static bool native_initialized(void);
static int native_abort(int flag, const char msg[]);
static int native_get_jobid(char jobId[], int jobIdSize);
static int native_get_rank(int *rank);
static int native_get_size(opal_pmix_scope_t scope, int *size);
static int native_get_appnum(int *appnum);
static int native_fence(void);
static int native_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata);
static int native_put(opal_identifier_t *id,
                      const char key[],
                      opal_pmix_scope_t scope,
                      opal_value_t *kv);
static int native_get(opal_identifier_t *id,
                      const char *key,
                      opal_value_t *kv);
static void native_get_nb(opal_identifier_t *id,
                          const char *key,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata);
static int native_publish(opal_identifier_t *id,
                          const char service_name[],
                          opal_list_t *info,
                          const char port[]);
static int native_lookup(opal_identifier_t *id,
                         const char service_name[],
                         opal_list_t *info,
                         char port[], int portLen);
static int native_unpublish(opal_identifier_t *id,
                            const char service_name[], 
                            opal_list_t *info);
static int native_local_info(int vpid, int **ranks_ret,
                             int *procs_ret, char **error);
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
    native_get_jobid,
    native_get_rank,
    native_get_size,
    native_get_appnum,
    native_fence,
    native_fence_nb,
    native_put,
    native_get,
    native_get_nb,
    native_publish,
    native_lookup,
    native_unpublish,
    native_local_info,
    native_spawn,
    native_job_connect,
    native_job_disconnect
};

// local variables
static bool init_cntr = 0;

static char* pmix_error(int pmix_err);
#define OPAL_PMI_ERROR(pmi_err, pmi_func)                       \
    do {                                                        \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                   \
                    pmi_func, __FILE__, __LINE__, __func__,     \
                    pmix_error(pmi_err));                        \
    } while(0);
/* callback for wait completion */
static wait_cbfunc(int status, opal_value_t *kv, void *cbdata)
{
    pmix_cback_t *cb = (pmix_cback_t*)cbdata;
    cb->active = false;
    if (NULL != cb->cbfunc) {
        cb->cbfunc(status, kv, cb->cbdata);
    }
}

static int native_init(void)
{
    char *t;

    /* if we are intitialized, then we are good */
    if (0 < init_cntr) {
        return OPAL_SUCCESS;
    }
    ++init_cntr;

    /* get the PMIX server contact info - we know
     * it is here because the component checked for it */
    if (NULL == (t = getenv("PMIX_SERVER_URI"))) {
        return OPAL_ERROR;
    }
    mca_pmix_native_component.uri = strdup(t);

    /* construct the server fields */
    mca_pmix_native_component.cache = NULL;
    mca_pmix_native_component.sd = -1;
    mca_pmix_native_component.state = PMIX_USOCK_UNCONNECTED;
    OBJ_CONSTRUCT(&mca_pmix_native_component.send_queue, opal_list_t);
    mca_pmix_native_component.send_msg = NULL;
    mca_pmix_native_component.recv_msg = NULL;
    mca_pmix_native_component.send_ev_active = false;
    mca_pmix_native_component.recv_ev_active = false;
    mca_pmix_native_component.timer_ev_active = false;

    /* create an event base and progress thread for us */
    if (NULL == (mca_pmix_native_component.evbase = opal_start_progress_thread("pmix_native", true))) {
        free(uri);
        return OPAL_ERROR;
    }

    /* we will connect on first send */

    return OPAL_SUCCESS;
}

static int native_fini(void) {
    if (0 == init_cntr) {
        return OPAL_SUCCESS;
    }
    --init_cntr;

    if (0 == init_cntr) {
        /* finalize things */
    }

    if (NULL != mca_pmix_native_component.evbase) {
        opal_stop_progress_thread("pmix_native");
        mca_pmix_native_component.evbase = NULL;
    }

    if (NULL != mca_pmix_native_component.uri) {
        free(mca_pmix_native_component.uri);
    }
    if (0 <= mca_pmix_native_component.sd) {
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
    }
    OPAL_LIST_DESTRUCT(&mca_pmix_native_component.send_queue);

    OBJ_DESTRUCT(&mca_pmix_native_component.server);

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
    opal_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_ABORT_CMD;

    /* create a buffer to hold the message */
    msg = OBJ_NEW(opal_buffer_t);
    /* pack the cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    /* pack the status flag */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &flag, 1, OPAL_INT))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }
    /* pack the string message - a NULL is okay */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &msg, 1, OPAL_STRING))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }

    /* push it into our event base to send to the server, marking
     * that we don't need/expect a return message for this send */
    PMIX_ACTIVATE_POST_SEND(msg, NULL, NULL);

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

static int native_get_jobid(char jobId[], int jobIdSize)
{
    return OPAL_SUCCESS;
}

static int native_get_rank(int *rank)
{
    return OPAL_SUCCESS;
}

static int native_get_size(opal_pmix_scope_t scope, int *size)
{
    return OPAL_SUCCESS;
}

static int native_put(opal_identifier_t *id,
                  const char key[],
                  opal_pmix_scope_t scope,
                  opal_value_t *kv)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}


static int native_fence(void)
{
    opal_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FENCE_CMD;
    pmix_cback_t *cb;

    msg = OBJ_NEW(opal_buffer_t);
    /* pack the fence cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }

    /* if we haven't already done it, ensure we have committed our values */
    if (NULL != mca_pmix_native_component.cache) {
        opal_dss.copy_payload(msg, mca_pmix_native_component.cache);
        OBJ_RELEASE(mca_pmix_native_component.cache);
    }

    /* create a callback object as we need to pass it to the
     * send routine so we know which callback to use when
     * the return message is recvd */
    cb = OBJ_NEW(pmix_cback_t);

    /* push it into our event base to send to the server, asking
     * that our wait_cbfunc be called upon completion so we know
     * that the "release" was recvd */
    PMIX_ACTIVATE_POST_SEND(msg, cb, wait_cbfunc);

    /* wait for the fence to complete */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    OBJ_RELEASE(cb);

    return OPAL_SUCCESS;
}

static int native_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata)
{
    opal_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FENCE_CMD;
    pmix_cback_t *cb;

    msg = OBJ_NEW(opal_buffer_t);
    /* pack the fence cmd */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(msg, &cmd, 1, PMIX_CMD_T))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(msg);
        return rc;
    }

    /* if we haven't already done it, ensure we commit our values
     * as part of the fence message to reduce message traffic */
    if (NULL != mca_pmix_native_component.cache) {
        opal_dss.copy_payload(msg, mca_pmix_native_component.cache);
        OBJ_RELEASE(mca_pmix_native_component.cache);
    }

    /* create a callback object as we need to pass it to the
     * send routine so we know which callback to use when
     * the return message is recvd */
    cb = OBJ_NEW(pmix_cback_t);
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;

    /* push it into our event base to send to the server - the
     * provided function will be called upon receipt of the
     * "release" message */
    PMIX_ACTIVATE_POST_SEND(msg, cb, wait_cbfunc);

    return OPAL_SUCCESS;
}

static int native_get(opal_identifier_t *id,
                      const char *key,
                      opal_value_t *kv)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static void native_get_nb(opal_identifier_t *id,
                          const char *key,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata)
{
    return;
}

#if 0
static int native_get_node_attr(const char name[],
                                char value[],
                                int valuelen,
                                int *found,
                                int waitfor)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int native_get_node_attr_array(const char name[],
                                      int array[],
                                      int arraylen,
                                      int *outlen,
                                      int *found)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int native_put_node_attr(const char name[], const char value[])
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int native_get_job_attr(const char name[],
                               char value[],
                               int valuelen,
                               int *found)
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int native_get_job_attr_array(const char name[],
                                     int array[],
                                     int arraylen,
                                     int *outlen,
                                     int *found)
{
    return OPAL_ERR_NOT_SUPPORTED;
}
#endif

static int native_publish(opal_identifier_t *id,
                          const char service_name[],
                          opal_list_t *info,
                          const char port[])
{
    return OPAL_SUCCESS;
}

static int native_lookup(opal_identifier_t *id,
                         const char service_name[],
                         opal_list_t *info,
                         char port[], int portLen)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int native_unpublish(opal_identifier_t *id,
                            const char service_name[], 
                            opal_list_t *info)
{
    return OPAL_SUCCESS;;
}

static int native_local_info(int vpid, int **ranks_ret,
                             int *procs_ret, char **error)
{
#if 0
    int *ranks;
    int procs = -1;
    int rc;

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&procs))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_size");
        *error = "mca_common_pmix_local_info: could not get PMI clique size";
        return OPAL_ERROR;
    }
    /* now get the specific ranks */
    ranks = (int*)calloc(procs, sizeof(int));
    if (NULL == ranks) {
        *error = "mca_common_pmix_local_info: could not get memory for local ranks";
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(ranks, procs))) {
        OPAL_PMI_ERROR(rc, "PMI_Get_clique_ranks");
        *error = "mca_common_pmix_local_info: could not get clique ranks";
        return OPAL_ERROR;
    }

    *ranks_ret = ranks;
    *procs_ret = procs;
#endif
    return OPAL_SUCCESS;
}

static int native_job_connect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int native_job_disconnect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int native_get_appnum(int *appnum)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static char* pmix_error(int pmix_err)
{
#if 0
    char * err_msg;

    switch(pmix_err) {
        case PMI_FAIL: err_msg = "Operation failed"; break;
        case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid keyvalp argument"; break;
        case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
#if defined(PMI_ERR_INVALID_KVS)
	/* pmix.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
#endif
    return NULL;
}

/***   INSTANTIATE INTERNAL CLASSES   ***/
static void mcon(pmix_usock_msg_t *p)
{
    p->data = NULL;
    p->crnt_ptr = NULL;
    p->crnt_bytes = 0;
    p->total_bytes = 0;
}
OBJ_CLASS_INSTANCE(pmix_usock_msg_t,
                   opal_object_t,
                   mcon, NULL);

static void cbcon(pmix_cb_t *p)
{
    p->active = false;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(pmix_cb_t,
                   opal_object_t,
                   cbcon, NULL);

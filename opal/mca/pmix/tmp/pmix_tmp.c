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

#include <string.h>

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress_threads.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix_tmp.h"

static int tmp_init(void);
static int tmp_fini(void);
static bool tmp_initialized(void);
static int tmp_abort(int flag, const char msg[]);
static int tmp_get_jobid(char jobId[], int jobIdSize);
static int tmp_get_rank(int *rank);
static int tmp_get_size(opal_pmix_scope_t scope, int *size);
static int tmp_get_appnum(int *appnum);
static int tmp_fence(void);
static int tmp_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata);
static int tmp_put(opal_pmix_scope_t scope,
                      opal_value_t *kv);
static int tmp_get(opal_identifier_t *id,
                      const char *key,
                      opal_value_t *kv);
static void tmp_get_nb(opal_identifier_t *id,
                          const char *key,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata);
static int tmp_publish(const char service_name[],
                          opal_list_t *info,
                          const char port[]);
static int tmp_lookup(const char service_name[],
                         opal_list_t *info,
                         char port[], int portLen);
static int tmp_unpublish(const char service_name[], 
                            opal_list_t *info);
static int tmp_local_info(int vpid, int **ranks_ret,
                             int *procs_ret, char **error);
static int tmp_spawn(int count, const char * cmds[],
                        int argcs[], const char ** argvs[],
                        const int maxprocs[],
                        opal_list_t *info_keyval_vector,
                        opal_list_t *preput_keyval_vector,
                        char jobId[], int jobIdSize,
                        int errors[]);
static int tmp_job_connect(const char jobId[]);
static int tmp_job_disconnect(const char jobId[]);

const opal_pmix_base_module_t opal_pmix_tmp_module = {
    tmp_init,
    tmp_fini,
    tmp_initialized,
    tmp_abort,
    tmp_get_jobid,
    tmp_get_rank,
    tmp_get_size,
    tmp_get_appnum,
    tmp_fence,
    tmp_fence_nb,
    tmp_put,
    tmp_get,
    tmp_get_nb,
    tmp_publish,
    tmp_lookup,
    tmp_unpublish,
    tmp_local_info,
    tmp_spawn,
    tmp_job_connect,
    tmp_job_disconnect
};

static int init_cntr = 0;

static int tmp_init(void)
{
    /* if we are intitialized, then we are good */
    if (0 < init_cntr) {
        return OPAL_SUCCESS;
    }
    ++init_cntr;

    return OPAL_SUCCESS;
}

static int tmp_fini(void) {
    if (0 == init_cntr) {
        return OPAL_SUCCESS;
    }
    --init_cntr;

    if (0 == init_cntr) {
        /* finalize things */
    }

   return OPAL_SUCCESS;
}

static bool tmp_initialized(void)
{
    if (0 < init_cntr) {
        return true;
    }
    return false;
}

static int tmp_abort(int flag, const char msg[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_spawn(int count, const char * cmds[],
                        int argcs[], const char ** argvs[],
                        const int maxprocs[],
                        opal_list_t *info_keyval_vector,
                        opal_list_t *preput_keyval_vector,
                        char jobId[], int jobIdSize,
                        int errors[])
{
    return OPAL_ERR_NOT_SUPPORTED;
}

static int tmp_get_jobid(char jobId[], int jobIdSize)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_get_rank(int *rank)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_get_size(opal_pmix_scope_t scope, int *size)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_put(opal_pmix_scope_t scope,
                      opal_value_t *kv)
{
    /* just store the value in the datastore */
    return opal_dstore.store(opal_dstore_internal, OPAL_MY_ID, kv);
}


static int tmp_fence(void)
{
   return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_fence_nb(opal_pmix_cbfunc_t cbfunc, void *cbdata)
{
   return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_get(opal_identifier_t *id,
                      const char *key,
                      opal_value_t *kv)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static void tmp_get_nb(opal_identifier_t *id,
                          const char *key,
                          opal_pmix_cbfunc_t cbfunc,
                          void *cbdata)
{
    return;
}

static int tmp_publish(const char service_name[],
                          opal_list_t *info,
                          const char port[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_lookup(const char service_name[],
                         opal_list_t *info,
                         char port[], int portLen)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_unpublish(const char service_name[], 
                            opal_list_t *info)
{
    return OPAL_ERR_NOT_IMPLEMENTED;;
}

static int tmp_local_info(int vpid, int **ranks_ret,
                             int *procs_ret, char **error)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_job_connect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_job_disconnect(const char jobId[])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int tmp_get_appnum(int *appnum)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

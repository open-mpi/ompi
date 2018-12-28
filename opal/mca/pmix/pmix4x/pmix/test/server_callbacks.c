/*
 * Copyright (c) 2015-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <pthread.h>
#include <stdio.h>
#include "server_callbacks.h"
#include "src/util/argv.h"
#include "test_server.h"

extern bool spawn_wait;

pmix_server_module_t mymodule = {
    .client_connected = connected,
    .client_finalized = finalized,
    .abort = abort_fn,
    .fence_nb = fencenb_fn,
    .direct_modex = dmodex_fn,
    .publish = publish_fn,
    .lookup = lookup_fn,
    .unpublish = unpublish_fn,
    .spawn = spawn_fn,
    .connect = connect_fn,
    .disconnect = disconnect_fn,
    .register_events = regevents_fn,
    .deregister_events = deregevents_fn
};

typedef struct {
    pmix_list_item_t super;
    pmix_info_t data;
    char *namespace_published;
    int rank_published;
} pmix_test_info_t;

static void tcon(pmix_test_info_t *p)
{
    PMIX_INFO_CONSTRUCT(&p->data);
}

static void tdes(pmix_test_info_t *p)
{
    PMIX_INFO_DESTRUCT(&p->data);
}

PMIX_CLASS_INSTANCE(pmix_test_info_t,
                          pmix_list_item_t,
                          tcon, tdes);

pmix_list_t *pmix_test_published_list = NULL;

static int finalized_count = 0;

pmix_status_t connected(const pmix_proc_t *proc, void *server_object,
                        pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

pmix_status_t finalized(const pmix_proc_t *proc, void *server_object,
              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    cli_info_t *cli = NULL;
    int i;
    for (i = 0; i < cli_info_cnt; i++) {
        if((proc->rank == cli_info[i].rank) &&
                (0 == strcmp(proc->nspace, cli_info[i].ns))){
            cli = &cli_info[i];
            break;
        }
    }
    if (NULL == cli) {
        TEST_ERROR(("cannot found rank %d", proc->rank));
        return PMIX_SUCCESS;
    }
    if( CLI_TERM <= cli->state ){
        TEST_ERROR(("double termination of rank %d", proc->rank));
        return PMIX_SUCCESS;
    }
    TEST_VERBOSE(("Rank %s:%d terminated", proc->nspace, proc->rank));
    cli_finalize(cli);
    finalized_count++;
    if (finalized_count == cli_info_cnt) {
        if (NULL != pmix_test_published_list) {
            PMIX_LIST_RELEASE(pmix_test_published_list);
        }
    }
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

pmix_status_t abort_fn(const pmix_proc_t *proc, void *server_object,
             int status, const char msg[],
             pmix_proc_t procs[], size_t nprocs,
             pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    TEST_VERBOSE(("Abort is called with status = %d, msg = %s",
                  status, msg));
    test_abort = true;
    return PMIX_SUCCESS;
}

pmix_status_t fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
               const pmix_info_t info[], size_t ninfo,
               char *data, size_t ndata,
               pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE(("Getting data for %s:%d",
                  procs[0].nspace, procs[0].rank));

    if ((pmix_list_get_size(server_list) == 1) && (my_server_id == 0)) {
        if (NULL != cbfunc) {
            cbfunc(PMIX_SUCCESS, data, ndata, cbdata, NULL, NULL);
        }
        return PMIX_SUCCESS;
    }
    return server_fence_contrib(data, ndata, cbfunc, cbdata);
}

pmix_status_t dmodex_fn(const pmix_proc_t *proc,
              const pmix_info_t info[], size_t ninfo,
              pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE(("Getting data for %s:%d", proc->nspace, proc->rank));

    /* return not_found fot single server mode */
    if ((pmix_list_get_size(server_list) == 1) && (my_server_id == 0)) {
        return PMIX_ERR_NOT_FOUND;
    }
    // TODO: add support tracker for dmodex requests
    return server_dmdx_get(proc->nspace, proc->rank, cbfunc, cbdata);
}

pmix_status_t publish_fn(const pmix_proc_t *proc,
               const pmix_info_t info[], size_t ninfo,
               pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    size_t i;
    int found;
    pmix_test_info_t *new_info, *old_info;
    if (NULL == pmix_test_published_list) {
        pmix_test_published_list = PMIX_NEW(pmix_list_t);
    }
    for (i = 0; i < ninfo; i++) {
        found = 0;
        PMIX_LIST_FOREACH(old_info, pmix_test_published_list, pmix_test_info_t) {
            if (!strcmp(old_info->data.key, info[i].key)) {
                found = 1;
                break;
            }
        }
        if (!found) {
            new_info = PMIX_NEW(pmix_test_info_t);
            strncpy(new_info->data.key, info[i].key, strlen(info[i].key)+1);
            pmix_value_xfer(&new_info->data.value, (pmix_value_t*)&info[i].value);
            new_info->namespace_published = strdup(proc->nspace);
            new_info->rank_published = proc->rank;
            pmix_list_append(pmix_test_published_list, &new_info->super);
        }
    }
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

pmix_status_t lookup_fn(const pmix_proc_t *proc, char **keys,
              const pmix_info_t info[], size_t ninfo,
              pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    size_t i, ndata, ret;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_pdata_t *pdata;
    pmix_test_info_t *tinfo;
    if (NULL == pmix_test_published_list) {
        return PMIX_ERR_NOT_FOUND;
    }
    ndata = pmix_argv_count(keys);
    PMIX_PDATA_CREATE(pdata, ndata);
    ret = 0;
    for (i = 0; i < ndata; i++) {
        PMIX_LIST_FOREACH(tinfo, pmix_test_published_list, pmix_test_info_t) {
            if (0 == strcmp(tinfo->data.key, keys[i])) {
                (void)strncpy(pdata[i].proc.nspace, tinfo->namespace_published, PMIX_MAX_NSLEN);
                pdata[i].proc.rank = tinfo->rank_published;
                memset(pdata[i].key, 0, PMIX_MAX_KEYLEN+1);
                (void)strncpy(pdata[i].key, keys[i], PMIX_MAX_KEYLEN);
                pmix_value_xfer(&pdata[i].value, &tinfo->data.value);
                ret++;
                break;
            }
        }
    }
    if (ret != ndata) {
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, pdata, ndata, cbdata);
    }
error:
    PMIX_PDATA_FREE(pdata, ndata);
    return rc;
}

pmix_status_t unpublish_fn(const pmix_proc_t *proc, char **keys,
                 const pmix_info_t info[], size_t ninfo,
                 pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    size_t i;
    pmix_test_info_t *iptr, *next;
    if (NULL == pmix_test_published_list) {
        return PMIX_ERR_NOT_FOUND;
    }
    PMIX_LIST_FOREACH_SAFE(iptr, next, pmix_test_published_list, pmix_test_info_t) {
        if (1) {  // if data posted by this process
            if (NULL == keys) {
                pmix_list_remove_item(pmix_test_published_list, &iptr->super);
                PMIX_RELEASE(iptr);
            } else {
                ninfo = pmix_argv_count(keys);
                for (i = 0; i < ninfo; i++) {
                    if (!strcmp(iptr->data.key, keys[i])) {
                        pmix_list_remove_item(pmix_test_published_list, &iptr->super);
                        PMIX_RELEASE(iptr);
                        break;
                    }
                }
            }
        }
    }
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

typedef struct {
    pmix_status_t status;
    pmix_spawn_cbfunc_t cbfunc;
    void *cbdata;
} release_cbdata;


static void * _release_cb(void *arg)
{
    release_cbdata *cb = (release_cbdata*)arg;
    if (NULL != cb->cbfunc) {
        cb->cbfunc(cb->status, "foobar", cb->cbdata);
    }
    free(cb);
    spawn_wait = false;
    pthread_exit(NULL);
}

static void release_cb(pmix_status_t status, void *cbdata)
{
    pthread_t thread;

    if (0 > pthread_create(&thread, NULL, _release_cb, cbdata)) {
        spawn_wait = false;
        return;
    }
    pthread_detach(thread);
}

pmix_status_t spawn_fn(const pmix_proc_t *proc,
             const pmix_info_t job_info[], size_t ninfo,
             const pmix_app_t apps[], size_t napps,
             pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    release_cbdata *cb = malloc(sizeof(release_cbdata));

    cb->status = PMIX_SUCCESS;
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;

    spawn_wait = true;
    PMIx_server_register_nspace("foobar", napps, NULL, 0, release_cb, (void*)cb);
    return PMIX_SUCCESS;
}
static int numconnect = 0;

pmix_status_t connect_fn(const pmix_proc_t procs[], size_t nprocs,
                         const pmix_info_t info[], size_t ninfo,
                         pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    numconnect++;
    return PMIX_SUCCESS;
}

pmix_status_t disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                            const pmix_info_t info[], size_t ninfo,
                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

pmix_status_t regevents_fn (pmix_status_t *codes, size_t ncodes,
                           const pmix_info_t info[], size_t ninfo,
                           pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE ((" pmix host server regevents_fn called "));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

pmix_status_t deregevents_fn (pmix_status_t *codes, size_t ncodes,
                             pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE ((" pmix host server deregevents_fn called "));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

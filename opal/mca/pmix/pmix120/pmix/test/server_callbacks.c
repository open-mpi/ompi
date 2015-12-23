/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "server_callbacks.h"
#include "src/util/argv.h"

pmix_server_module_t mymodule = {
    connected,
    finalized,
    abort_fn,
    fencenb_fn,
    dmodex_fn,
    publish_fn,
    lookup_fn,
    unpublish_fn,
    spawn_fn,
    connect_fn,
    disconnect_fn,
    regevents_fn,
    deregevents_fn,
    NULL
};

typedef struct {
    pmix_list_item_t super;
    pmix_modex_data_t data;
} pmix_test_data_t;

static void pcon(pmix_test_data_t *p)
{
    p->data.blob = NULL;
    p->data.size = 0;
}

static void pdes(pmix_test_data_t *p)
{
    if (NULL != p->data.blob) {
        free(p->data.blob);
    }
}

PMIX_CLASS_INSTANCE(pmix_test_data_t,
                    pmix_list_item_t,
                    pcon, pdes);

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

int connected(const pmix_proc_t *proc, void *server_object)
{
    return PMIX_SUCCESS;
}

int finalized(const pmix_proc_t *proc, void *server_object,
              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    if( CLI_TERM <= cli_info[proc->rank].state ){
        TEST_ERROR(("double termination of rank %d", proc->rank));
        return PMIX_SUCCESS;
    }
    TEST_VERBOSE(("Rank %d terminated", proc->rank));
    cli_finalize(&cli_info[proc->rank]);
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

int abort_fn(const pmix_proc_t *proc, void *server_object,
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

int fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
               const pmix_info_t info[], size_t ninfo,
               char *data, size_t ndata,
               pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE(("Getting data for %s:%d",
                  procs[0].nspace, procs[0].rank));

    /* In a perfect world, we should wait until
     * the test servers from all involved procs
     * respond. We don't have multi-server capability
     * yet, so we'll just respond right away and
     * return what we were given */

    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, data, ndata, cbdata, NULL, NULL);
    }
    return PMIX_SUCCESS;
}

int dmodex_fn(const pmix_proc_t *proc,
              const pmix_info_t info[], size_t ninfo,
              pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE(("Getting data for %s:%d", proc->nspace, proc->rank));

    /* In a perfect world, we should call another server
     * to get the data for one of its clients. We don't
     * have multi-server capability yet, so we'll just
     * respond right away */

    if (NULL != cbfunc) {
        cbfunc(PMIX_ERR_NOT_FOUND, NULL, 0, cbdata, NULL, NULL);
    }
    return PMIX_SUCCESS;
}

int publish_fn(const pmix_proc_t *proc,
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

int lookup_fn(const pmix_proc_t *proc, char **keys,
              const pmix_info_t info[], size_t ninfo,
              pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    size_t i, ndata, ret;
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
                (void)strncpy(pdata[i].key, keys[i], strlen(keys[i])+1);
                pmix_value_xfer(&pdata[i].value, &tinfo->data.value);
                ret++;
                break;
            }
        }
    }
    if (NULL != cbfunc) {
        cbfunc((ret == ndata) ? PMIX_SUCCESS : PMIX_ERR_NOT_FOUND, pdata, ndata, cbdata);
    }
    PMIX_PDATA_FREE(pdata, ndata);
    return PMIX_SUCCESS;
}

int unpublish_fn(const pmix_proc_t *proc, char **keys,
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

static void release_cb(pmix_status_t status, void *cbdata)
{
    release_cbdata *cb = (release_cbdata*)cbdata;
    if (NULL != cb->cbfunc) {
        cb->cbfunc(cb->status, "foobar", cb->cbdata);
    }
    free(cb);
}

int spawn_fn(const pmix_proc_t *proc,
             const pmix_info_t job_info[], size_t ninfo,
             const pmix_app_t apps[], size_t napps,
             pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    release_cbdata *cb = malloc(sizeof(release_cbdata));
    cb->status = PMIX_SUCCESS;
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;
    PMIx_server_register_nspace("foobar", napps, NULL, 0, release_cb, (void*)cb);
    return PMIX_SUCCESS;
}

int connect_fn(const pmix_proc_t procs[], size_t nprocs,
               const pmix_info_t info[], size_t ninfo,
               pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    if (NULL != cbfunc) {
        /* return PMIX_EXISTS here just to ensure we get the correct status on the client */
        cbfunc(PMIX_EXISTS, cbdata);
    }
   return PMIX_SUCCESS;
}

int disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                  const pmix_info_t info[], size_t ninfo,
                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

int regevents_fn (const pmix_info_t info[], size_t ninfo,
                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE ((" pmix host server regevents_fn called "));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

int deregevents_fn (const pmix_info_t info[], size_t ninfo,
                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    TEST_VERBOSE ((" pmix host server deregevents_fn called "));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }
    return PMIX_SUCCESS;
}

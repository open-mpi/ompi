/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
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

#include "opal/util/proc.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix1.h"
#include "opal/mca/pmix/pmix1xx/pmix/include/pmix.h"
#include "opal/mca/pmix/pmix1xx/pmix/src/buffer_ops/buffer_ops.h"

static pmix_proc_t myproc;
static char *dbgvalue=NULL;

static int convert_scope(pmix_scope_t *scope,
                         opal_pmix_scope_t sc);
static int convert_persistence(pmix_persistence_t *p,
                               opal_pmix_persistence_t persist);
static int convert_data_range(pmix_data_range_t *sc,
                              opal_pmix_data_range_t scope);



int pmix1_client_init(void)
{
    opal_process_name_t pname;
    pmix_status_t rc;
    int dbg;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client init");

    if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
        asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
        putenv(dbgvalue);
    }
    rc = PMIx_Init(&myproc);
    if (PMIX_SUCCESS == rc) {
        /* store our jobid and rank */
        opal_convert_string_to_jobid(&pname.jobid, myproc.nspace);
        pname.vpid = myproc.rank;
        opal_proc_set_name(&pname);
    }
    return pmix1_convert_rc(rc);
}

int pmix1_client_finalize(void)
{
    pmix_status_t rc;

    rc = PMIx_Finalize();
    return pmix1_convert_rc(rc);
}

int pmix1_initialized(void)
{
    pmix_status_t rc;

    rc = PMIx_Initialized();
    return pmix1_convert_rc(rc);
}

int pmix1_abort(int flag, const char *msg,
                  opal_list_t *procs)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            (void)strncpy(parray[n].nspace, opal_convert_jobid_to_string(ptr->name.jobid), PMIX_MAX_NSLEN);
            parray[n].rank = ptr->name.vpid;
            ++n;
        }
    }

    /* call the library abort */
    rc = PMIx_Abort(flag, msg, parray, cnt);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);

    return pmix1_convert_rc(rc);
}

int pmix1_commit(void)
{
    pmix_status_t rc;

    rc = PMIx_Commit();
    return pmix1_convert_rc(rc);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;

    if (NULL != op->opcbfunc) {
        op->opcbfunc(pmix1_convert_rc(status), op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix1_fence(opal_list_t *procs, int collect_data)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix_info_t info, *iptr;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            (void)strncpy(parray[n].nspace, opal_convert_jobid_to_string(ptr->name.jobid), PMIX_MAX_NSLEN);
            parray[n].rank = ptr->name.vpid;
            ++n;
        }
    }
    if (collect_data) {
        PMIX_INFO_CONSTRUCT(&info);
        (void)strncpy(info.key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
        info.value.type = PMIX_BOOL;
        info.value.data.flag = true;
        iptr = &info;
        n = 1;
    } else {
        iptr = NULL;
        n = 0;
    }

    /* call the library function */
    rc = PMIx_Fence(parray, cnt, iptr, n);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);
    if (NULL != iptr) {
        PMIX_INFO_DESTRUCT(&info);
    }

    return pmix1_convert_rc(rc);

}

int pmix1_fencenb(opal_list_t *procs, int collect_data,
                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix1_opcaddy_t *op;
    pmix_info_t info, *iptr;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            (void)strncpy(parray[n].nspace, opal_convert_jobid_to_string(ptr->name.jobid), PMIX_MAX_NSLEN);
            parray[n].rank = ptr->name.vpid;
            ++n;
        }
    }

    if (collect_data) {
        PMIX_INFO_CONSTRUCT(&info);
        (void)strncpy(info.key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
        iptr = &info;
        n = 1;
    } else {
        iptr = NULL;
        n = 0;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->procs = parray;
    op->nprocs = cnt;

    /* call the library function */
    rc = PMIx_Fence_nb(parray, cnt, iptr, n, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }

    return pmix1_convert_rc(rc);

}

int pmix1_put(opal_pmix_scope_t scope,
                opal_value_t *val)
{
    pmix_scope_t pscope;
    pmix_value_t kv;
    pmix_status_t rc;
    int irc;

    /* convert the scope */
    if (OPAL_SUCCESS != (irc = convert_scope(&pscope, scope))) {
        return irc;
    }

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix1_value_load(&kv, val);

    rc = PMIx_Put(pscope, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);
    return pmix1_convert_rc(rc);
}

int pmix1_get(const opal_process_name_t *proc,
                const char *key, opal_value_t **val)
{
    int ret;
    pmix_value_t *kv;
    pmix_status_t rc;
    pmix_proc_t p, *pptr;

    /* prep default response */
    *val = NULL;
    if (NULL != proc) {
        /* convert the process jobid */
        (void)strncpy(p.nspace, opal_convert_jobid_to_string(proc->jobid), PMIX_MAX_NSLEN);
        p.rank = proc->vpid;
        pptr = &p;
    } else {
        /* if they are asking for our jobid, then return our nspace */
        if (0 == strcmp(key, OPAL_PMIX_JOBID)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_STRING;
            (*val)->data.string = strdup(myproc.nspace);
            return OPAL_SUCCESS;
        } else if (0 == strcmp(key, OPAL_PMIX_RANK)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_INT;
            (*val)->data.integer = myproc.rank;
            return OPAL_SUCCESS;
        }
        pptr = NULL;
    }

    /* pass the request down */
    rc = PMIx_Get(pptr, key, NULL, 0, &kv);
    if (PMIX_SUCCESS == rc) {
        if (NULL == kv) {
            ret = OPAL_SUCCESS;
        } else {
            *val = OBJ_NEW(opal_value_t);
            ret = pmix1_value_unload(*val, kv);
            PMIX_VALUE_FREE(kv, 1);
        }
    } else {
        ret = pmix1_convert_rc(rc);
    }
    return ret;
}

static void val_cbfunc(pmix_status_t status,
                       pmix_value_t *kv, void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;
    int rc;
    opal_value_t val, *v=NULL;

    rc = pmix1_convert_opalrc(status);
    if (PMIX_SUCCESS == status && NULL != kv) {
        rc = pmix1_value_unload(&val, kv);
        v = &val;
    }

    if (NULL != op->valcbfunc) {
        op->valcbfunc(rc, v, op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix1_getnb(const opal_process_name_t *proc,
                  const char *key,
                  opal_pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    pmix1_opcaddy_t *op;
    pmix_status_t rc;
    char *tmp;

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->valcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != proc) {
        /* convert the process jobid */
        tmp = strdup(opal_convert_jobid_to_string(proc->jobid));
        (void)strncpy(op->p.nspace, tmp, PMIX_MAX_NSLEN);
        free(tmp);
        op->p.rank = proc->vpid;
    } else {
        (void)strncpy(op->p.nspace, myproc.nspace, PMIX_MAX_NSLEN);
        op->p.rank = PMIX_RANK_WILDCARD;
    }


    /* call the library function */
    rc = PMIx_Get_nb(&op->p, key, NULL, 0, val_cbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }

    return pmix1_convert_rc(rc);
}

int pmix1_publish(opal_pmix_data_range_t scope,
                    opal_pmix_persistence_t persist,
                    opal_list_t *info)
{
    pmix_data_range_t rng;
    pmix_persistence_t pst;
    int rc;
    pmix_info_t *pinfo;
    pmix_status_t ret;
    opal_pmix_info_t *iptr;
    size_t sz, n;

    rc = convert_data_range(&rng, scope);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    rc = convert_persistence(&pst, scope);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    sz = opal_list_get_size(info);
    if (0 < sz) {
        PMIX_INFO_CREATE(pinfo, sz);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_pmix_info_t) {
            (void)strncpy(pinfo[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, &iptr->value);
            ++n;
        }
    }

    ret = PMIx_Publish(rng, pst, pinfo, sz);

    return pmix1_convert_rc(ret);
}

int pmix1_publishnb(opal_pmix_data_range_t scope,
                      opal_pmix_persistence_t persist,
                      opal_list_t *info,
                      opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_data_range_t rng;
    pmix_persistence_t pst;
    int rc;
    pmix_status_t ret;
    opal_pmix_info_t *iptr;
    size_t n;
    pmix1_opcaddy_t *op;

    rc = convert_data_range(&rng, scope);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
    rc = convert_persistence(&pst, persist);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    op->sz = opal_list_get_size(info);
    if (0 < op->sz) {
        PMIX_INFO_CREATE(op->info, op->sz);
        n=0;
        OPAL_LIST_FOREACH(iptr, info, opal_pmix_info_t) {
            (void)strncpy(op->info[n].key, iptr->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&op->info[n].value, &iptr->value);
            ++n;
        }
    }

    ret = PMIx_Publish_nb(rng, pst, op->info, op->sz, opcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_lookup(opal_pmix_data_range_t scope,
                   opal_list_t *data)
{
    pmix_data_range_t rng;
    pmix_pdata_t *pdata;
    size_t sz, n;
    int rc;
    pmix_status_t ret;
    opal_pmix_pdata_t *d;

    rc = convert_data_range(&rng, scope);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
    sz = opal_list_get_size(data);

    PMIX_PDATA_CREATE(pdata, sz);
    n=0;
    OPAL_LIST_FOREACH(d, data, opal_pmix_pdata_t) {
        (void)strncpy(pdata[n++].key, d->key, PMIX_MAX_KEYLEN);
    }

    ret = PMIx_Lookup(rng, NULL, 0, pdata, sz);

    /* transfer the data back */
    n=0;
    OPAL_LIST_FOREACH(d, data, opal_pmix_pdata_t) {
        rc = opal_convert_string_to_jobid(&d->proc.jobid, pdata[n].proc.nspace);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            PMIX_PDATA_FREE(pdata, sz);
            return OPAL_ERR_BAD_PARAM;
        }
        d->proc.vpid = pdata[n].proc.rank;
        rc = pmix1_value_unload(&d->value, &pdata[n].value);
        if (OPAL_SUCCESS != rc) {
            OPAL_ERROR_LOG(rc);
            PMIX_PDATA_FREE(pdata, sz);
            return OPAL_ERR_BAD_PARAM;
        }
    }

    return pmix1_convert_rc(ret);
}

static void lk_cbfunc(pmix_status_t status,
                      pmix_pdata_t data[], size_t ndata,
                      void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;
    opal_pmix_pdata_t *d;
    opal_list_t results;
    int rc;
    size_t n;

    if (NULL == op->lkcbfunc) {
        OBJ_RELEASE(op);
        return;
    }

    rc = pmix1_convert_rc(status);
    OBJ_CONSTRUCT(&results, opal_list_t);
    for (n=0; n < ndata; n++) {
        d = OBJ_NEW(opal_pmix_pdata_t);
        opal_list_append(&results, &d->super);
        rc = opal_convert_string_to_jobid(&d->proc.jobid, data[n].proc.nspace);
        if (OPAL_SUCCESS != rc) {
            rc = OPAL_ERR_BAD_PARAM;
            OPAL_ERROR_LOG(rc);
            goto release;
        }
        d->proc.vpid = data[n].proc.rank;
        d->key = strdup(data[n].key);
        rc = pmix1_value_unload(&d->value, &data[n].value);
        if (OPAL_SUCCESS != rc) {
            rc = OPAL_ERR_BAD_PARAM;
            OPAL_ERROR_LOG(rc);
            goto release;
        }
    }

  release:
    /* execute the callback */
    op->lkcbfunc(rc, &results, op->cbdata);

    OPAL_LIST_DESTRUCT(&results);
    OBJ_RELEASE(op);
}

int pmix1_lookupnb(opal_pmix_data_range_t scope, int wait, char **keys,
                     opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_data_range_t rng;
    int rc;
    pmix_status_t ret;
    pmix1_opcaddy_t *op;

    rc = convert_data_range(&rng, scope);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->lkcbfunc = cbfunc;
    op->cbdata = cbdata;

    ret = PMIx_Lookup_nb(rng, keys, NULL, 0, lk_cbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_unpublish(opal_pmix_data_range_t scope, char **keys)
{
    int rc;
    pmix_status_t ret;
    pmix_data_range_t rng;

    rc = convert_data_range(&rng, scope);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    ret = PMIx_Unpublish(rng, keys);

    return pmix1_convert_rc(ret);
}

int pmix1_unpublishnb(opal_pmix_data_range_t scope, char **keys,
                        opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix_status_t ret;
    pmix_data_range_t rng;
    pmix1_opcaddy_t *op;

    rc = convert_data_range(&rng, scope);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    ret = PMIx_Unpublish_nb(rng, keys, opcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_spawn(opal_list_t *job_info, opal_list_t *apps, opal_jobid_t *jobid)
{
    pmix_status_t ret;
    pmix_info_t *pinfo = NULL;
    pmix_app_t *papps;
    size_t napps, n, m, ninfo = 0;
    char nspace[PMIX_MAX_NSLEN+1];
    opal_pmix_info_t *info;
    opal_pmix_app_t *app;

    if (NULL != job_info && 0 < (ninfo = opal_list_get_size(job_info))) {
        PMIX_INFO_CREATE(pinfo, ninfo);
        n=0;
        OPAL_LIST_FOREACH(info, job_info, opal_pmix_info_t) {
            (void)strncpy(pinfo[n].key, info->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&pinfo[n].value, &info->value);
            ++n;
        }
    }

    napps = opal_list_get_size(apps);
    PMIX_APP_CREATE(papps, napps);
    n=0;
    OPAL_LIST_FOREACH(app, apps, opal_pmix_app_t) {
        papps[n].cmd = strdup(app->cmd);
        papps[n].argc = app->argc;
        papps[n].argv = opal_argv_copy(app->argv);
        papps[n].env = opal_argv_copy(app->env);
        papps[n].maxprocs = app->maxprocs;
        if (0 < (papps[n].ninfo = opal_list_get_size(&app->info))) {
            PMIX_INFO_CREATE(papps[n].info, papps[n].ninfo);
            m=0;
            OPAL_LIST_FOREACH(info, &app->info, opal_pmix_info_t) {
                (void)strncpy(papps[n].info[m].key, info->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&papps[n].info[m].value, &info->value);
                ++m;
            }
        }
        ++n;
    }

    ret = PMIx_Spawn(pinfo, ninfo, papps, napps, nspace);
    if (PMIX_SUCCESS == ret) {
        (void)opal_convert_string_to_jobid(jobid, nspace);
    }
    PMIX_APP_FREE(papps, napps);

    return pmix1_convert_rc(ret);
}

static void spcbfunc(pmix_status_t status,
                     char *nspace, void *cbdata)
{
    pmix1_opcaddy_t *op = (pmix1_opcaddy_t*)cbdata;
    int rc;
    opal_jobid_t jobid=OPAL_JOBID_INVALID;

    rc = pmix1_convert_rc(status);
    if (PMIX_SUCCESS == status) {
        rc = opal_convert_string_to_jobid(&jobid, nspace);
    }

    op->spcbfunc(rc, jobid, op->cbdata);
    OBJ_RELEASE(op);
}

int pmix1_spawnnb(opal_list_t *job_info, opal_list_t *apps,
                    opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t ret;
    pmix1_opcaddy_t *op;
    size_t n, m;
    opal_pmix_info_t *info;
    opal_pmix_app_t *app;

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->spcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != job_info && 0 < (op->ninfo = opal_list_get_size(job_info))) {
        PMIX_INFO_CREATE(op->info, op->ninfo);
        n=0;
        OPAL_LIST_FOREACH(info, job_info, opal_pmix_info_t) {
            (void)strncpy(op->info[n].key, info->key, PMIX_MAX_KEYLEN);
            pmix1_value_load(&op->info[n].value, &info->value);
            ++n;
        }
    }

    op->sz = opal_list_get_size(apps);
    PMIX_APP_CREATE(op->apps, op->sz);
    n=0;
    OPAL_LIST_FOREACH(app, apps, opal_pmix_app_t) {
        op->apps[n].cmd = strdup(app->cmd);
        op->apps[n].argc = app->argc;
        op->apps[n].argv = opal_argv_copy(app->argv);
        op->apps[n].env = opal_argv_copy(app->env);
        op->apps[n].maxprocs = app->maxprocs;
        if (0 < (op->apps[n].ninfo = opal_list_get_size(&app->info))) {
            PMIX_INFO_CREATE(op->apps[n].info, op->apps[n].ninfo);
            m=0;
            OPAL_LIST_FOREACH(info, &app->info, opal_pmix_info_t) {
                (void)strncpy(op->apps[n].info[m].key, info->key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&op->apps[n].info[m].value, &info->value);
                ++m;
            }
        }
        ++n;
    }

    ret = PMIx_Spawn_nb(op->info, op->ninfo, op->apps, op->sz, spcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_connect(opal_list_t *procs)
{
    pmix_status_t ret;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    char *strname;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(parray, cnt);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        strname = opal_convert_jobid_to_string(ptr->name.jobid);
        (void)strncpy(parray[n].nspace, strname, PMIX_MAX_NSLEN);
        free(strname);
        parray[n].rank = ptr->name.vpid;
        ++n;
    }

    ret = PMIx_Connect(parray, cnt, NULL, 0);
    PMIX_PROC_FREE(parray, cnt);

    return pmix1_convert_rc(ret);
}

int pmix1_connectnb(opal_list_t *procs,
                      opal_pmix_op_cbfunc_t cbfunc,
                      void *cbdata)
{
    pmix_status_t ret;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix1_opcaddy_t *op;
    char *strname;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->nprocs = cnt;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        strname = opal_convert_jobid_to_string(ptr->name.jobid);
        (void)strncpy(op->procs[n].nspace, strname, PMIX_MAX_NSLEN);
        free(strname);
        op->procs[n].rank = ptr->name.vpid;
        ++n;
    }

    ret = PMIx_Connect_nb(op->procs, op->nprocs, NULL, 0, opcbfunc, op);

    return pmix1_convert_rc(ret);
}

int pmix1_disconnect(opal_list_t *procs)
{
    pmix_status_t ret;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(parray, cnt);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        (void)strncpy(parray[n].nspace, opal_convert_jobid_to_string(ptr->name.jobid), PMIX_MAX_NSLEN);
        parray[n].rank = ptr->name.vpid;
        ++n;
    }

    ret = PMIx_Disconnect(parray, cnt, NULL, 0);
    PMIX_PROC_FREE(parray, cnt);

    return pmix1_convert_rc(ret);
}

int pmix1_disconnectnb(opal_list_t *procs,
                         opal_pmix_op_cbfunc_t cbfunc,
                         void *cbdata)
{
    pmix_status_t ret;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix1_opcaddy_t *op;
    char *strname;

    /* protect against bozo error */
    if (NULL == procs || 0 == (cnt = opal_list_get_size(procs))) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->nprocs = cnt;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    PMIX_PROC_CREATE(op->procs, op->nprocs);
    n=0;
    OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
        strname = opal_convert_jobid_to_string(ptr->name.jobid);
        (void)strncpy(op->procs[n].nspace, strname, PMIX_MAX_NSLEN);
        free(strname);
        op->procs[n].rank = ptr->name.vpid;
        ++n;
    }

    ret = PMIx_Disconnect_nb(op->procs, op->nprocs, NULL, 0, opcbfunc, op);

    return pmix1_convert_rc(ret);
}


int pmix1_resolve_peers(const char *nodename, opal_jobid_t jobid,
                          opal_list_t *procs)
{
    char *nspace;
    pmix_proc_t *array=NULL;
    size_t nprocs, n;
    opal_namelist_t *nm;
    int rc;
    pmix_status_t ret;

    if (OPAL_JOBID_WILDCARD == jobid) {
        nspace = NULL;
    } else {
        nspace = opal_convert_jobid_to_string(jobid);
    }

    ret = PMIx_Resolve_peers(nodename, nspace, &array, &nprocs);
    rc = pmix1_convert_rc(ret);

    if (NULL != array && 0 < nprocs) {
        for (n=0; n < nprocs; n++) {
            nm = OBJ_NEW(opal_namelist_t);
            opal_list_append(procs, &nm->super);
            rc = opal_convert_string_to_jobid(&nm->name.jobid, array[n].nspace);
            if (OPAL_SUCCESS != rc) {
                if (NULL != nspace) {
                    free(nspace);
                }
                PMIX_PROC_FREE(array, nprocs);
                return rc;
            }
            nm->name.vpid = array[n].rank;
        }
    }
    PMIX_PROC_FREE(array, nprocs);

    return rc;
}

int pmix1_resolve_nodes(opal_jobid_t jobid, char **nodelist)
{
    pmix_status_t ret;
    char *nspace;

    if (OPAL_JOBID_WILDCARD == jobid) {
        nspace = NULL;
    } else {
        nspace = opal_convert_jobid_to_string(jobid);
    }

    ret = PMIx_Resolve_nodes(nspace, nodelist);
    if (NULL != nspace) {
        free(nspace);
    }

    return pmix1_convert_rc(ret);;
}

/***  UTILITY FUNCTIONS  ***/
static int convert_scope(pmix_scope_t *sc,
                         opal_pmix_scope_t scope)
{
    int rc = PMIX_SUCCESS;

    switch (scope) {
    case OPAL_PMIX_SCOPE_UNDEF:
        *sc = PMIX_SCOPE_UNDEF;
        break;
    case OPAL_PMIX_LOCAL:
        *sc = PMIX_LOCAL;
        break;
    case OPAL_PMIX_REMOTE:
        *sc = PMIX_REMOTE;
        break;
    case OPAL_PMIX_GLOBAL:
        *sc = PMIX_GLOBAL;
        break;
    default:
        *sc = PMIX_SCOPE_UNDEF;
        rc = OPAL_ERR_BAD_PARAM;
        break;
    }
    return rc;
}

static int convert_persistence(pmix_persistence_t *p,
                               opal_pmix_persistence_t persist)
{
    int rc = OPAL_SUCCESS;

    switch (persist) {
    case OPAL_PMIX_PERSIST_INDEF:
        *p = PMIX_PERSIST_INDEF;
        break;
    case OPAL_PMIX_PERSIST_PROC:
        *p = PMIX_PERSIST_PROC;
        break;
    case OPAL_PMIX_PERSIST_APP:
        *p = PMIX_PERSIST_APP;
        break;
    case OPAL_PMIX_PERSIST_SESSION:
        *p = PMIX_PERSIST_SESSION;
        break;
    default:
        *p = PMIX_PERSIST_PROC;
        rc = OPAL_ERR_BAD_PARAM;
    }
    return rc;
}

static int convert_data_range(pmix_data_range_t *sc,
                              opal_pmix_data_range_t scope)
{
    int rc = OPAL_SUCCESS;

    switch (scope) {
    case OPAL_PMIX_DATA_RANGE_UNDEF:
        *sc = PMIX_DATA_RANGE_UNDEF;
        break;
    case OPAL_PMIX_NAMESPACE:
        *sc = PMIX_NAMESPACE;
        break;
    case OPAL_PMIX_SESSION:
        *sc = PMIX_SESSION;
        break;
    default:
        *sc = PMIX_DATA_RANGE_UNDEF;
        rc = OPAL_ERR_BAD_PARAM;
    }
    return rc;
}

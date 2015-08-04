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
#include "pmix1xx.h"
#include "opal/mca/pmix/pmix1xx/pmix/include/pmix.h"
#include "opal/mca/pmix/pmix1xx/pmix/src/buffer_ops/buffer_ops.h"

static char mynspace[PMIX_MAX_NSLEN];
static int myrank;
static char *dbgvalue=NULL;

int pmix1xx_client_init(void)
{
    pmix_status_t rc;
    int dbg;

    opal_output_verbose(1, opal_pmix_base_framework.framework_output,
                        "PMIx_client init");

    if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
        asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
        putenv(dbgvalue);
    }
    rc = PMIx_Init(mynspace, &myrank);
    if (PMIX_SUCCESS == rc) {
        /* store our jobid and rank */
        opal_convert_string_to_jobid(&OPAL_PROC_MY_NAME.jobid, mynspace);
        OPAL_PROC_MY_NAME.vpid = myrank;
    }
    return pmix1xx_convert_rc(rc);
}

int pmix1xx_client_finalize(void)
{
    pmix_status_t rc;
    
    rc = PMIx_Finalize();
    return pmix1xx_convert_rc(rc);
}

int pmix1xx_initialized(void)
{
    pmix_status_t rc;
    
    rc = PMIx_Initialized();
    return pmix1xx_convert_rc(rc);
}

int pmix1xx_abort(int flag, const char *msg,
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
            parray[n].rank - ptr->name.vpid;
            ++n;
        }
    }
    
    /* call the library abort */
    rc = PMIx_Abort(flag, msg, parray, cnt);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);
    
    return pmix1xx_convert_rc(rc);
}

int pmix1xx_commit(void)
{
    pmix_status_t rc;
    
    rc = PMIx_Commit();
    return pmix1xx_convert_rc(rc);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix1xx_opcaddy_t *op = (pmix1xx_opcaddy_t*)cbdata;

    if (NULL != op->opcbfunc) {
        op->opcbfunc(pmix1xx_convert_rc(status), op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix1xx_fence(opal_list_t *procs, int collect_data)
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
            parray[n].rank - ptr->name.vpid;
            ++n;
        }
    }

    /* call the library function */
    rc = PMIx_Fence(parray, cnt, collect_data);

    /* release the array */
    PMIX_PROC_FREE(parray, cnt);
    
    return pmix1xx_convert_rc(rc);

}

int pmix1xx_fencenb(opal_list_t *procs, int collect_data,
                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    pmix_proc_t *parray=NULL;
    size_t n, cnt=0;
    opal_namelist_t *ptr;
    pmix1xx_opcaddy_t *op;

    /* convert the list of procs to an array
     * of pmix_proc_t */
    if (NULL != procs && 0 < (cnt = opal_list_get_size(procs))) {
        PMIX_PROC_CREATE(parray, cnt);
        n=0;
        OPAL_LIST_FOREACH(ptr, procs, opal_namelist_t) {
            (void)strncpy(parray[n].nspace, opal_convert_jobid_to_string(ptr->name.jobid), PMIX_MAX_NSLEN);
            parray[n].rank - ptr->name.vpid;
            ++n;
        }
    }

    /* create the caddy */
    op = OBJ_NEW(pmix1xx_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    op->procs = parray;
    op->nprocs = cnt;

    /* call the library function */
    rc = PMIx_Fence_nb(parray, cnt, collect_data, opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    
    return pmix1xx_convert_rc(rc);

}

int pmix1xx_put(opal_pmix_scope_t scope,
                opal_value_t *val)
{
    pmix_scope_t pscope;
    pmix_value_t kv;
    pmix_status_t rc;

    /* convert the scope */
    if (PMIX_SUCCESS != (rc = pmix1xx_convert_opalscope(&pscope, scope))) {
        return rc;
    }

    PMIX_VALUE_CONSTRUCT(&kv);
    pmix1xx_value_load(&kv, val);

    rc = PMIx_Put(pscope, val->key, &kv);
    PMIX_VALUE_DESTRUCT(&kv);
    return pmix1xx_convert_rc(rc);
}

int pmix1xx_get(const opal_process_name_t *proc,
                const char *key, opal_value_t **val)
{
    char nspace[PMIX_MAX_NSLEN], *ns;
    int rank, ret;
    pmix_value_t *kv;
    pmix_status_t rc;

    /* prep default response */
    *val = NULL;
    if (NULL != proc) {
        /* convert the process jobid */
        (void)strncpy(nspace, opal_convert_jobid_to_string(proc->jobid), PMIX_MAX_NSLEN);
        ns = nspace;
        rank = proc->vpid;
    } else {
        /* if they are asking for our jobid, then return our nspace */
        if (0 == strcmp(key, OPAL_PMIX_JOBID)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_STRING;
            (*val)->data.string = strdup(mynspace);
            return OPAL_SUCCESS;
        } else if (0 == strcmp(key, OPAL_PMIX_RANK)) {
            (*val) = OBJ_NEW(opal_value_t);
            (*val)->type = OPAL_INT;
            (*val)->data.integer = myrank;
            return OPAL_SUCCESS;
        }
        ns = NULL;
        rank = PMIX_RANK_WILDCARD;
    }

    /* pass the request down */
    rc = PMIx_Get(ns, rank, key, &kv);
    if (PMIX_SUCCESS == rc) {
        if (NULL == kv) {
            ret = OPAL_SUCCESS;
        } else {
            *val = OBJ_NEW(opal_value_t);
            ret = pmix1xx_value_unload(*val, kv);
            PMIX_VALUE_FREE(kv, 1);
        }
    } else {
        ret = pmix1xx_convert_rc(rc);
    }
    return ret;
}

static void val_cbfunc(pmix_status_t status,
                       pmix_value_t *kv, void *cbdata)
{
    pmix1xx_opcaddy_t *op = (pmix1xx_opcaddy_t*)cbdata;
    int rc;
    opal_value_t val, *v=NULL;

    rc = pmix1xx_convert_opalrc(status);
    if (PMIX_SUCCESS == status && NULL != kv) {
        rc = pmix1xx_value_unload(&val, kv);
        v = &val;
    }

  complete:
    if (NULL != op->valcbfunc) {
        op->valcbfunc(rc, v, op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix1xx_getnb(const opal_process_name_t *proc,
                  const char *key,
                  opal_pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    pmix1xx_opcaddy_t *op;
    pmix_status_t rc;
    
    /* create the caddy */
    op = OBJ_NEW(pmix1xx_opcaddy_t);
    op->valcbfunc = cbfunc;
    op->cbdata = cbdata;

    if (NULL != proc) {
        /* convert the process jobid */
        op->nspace = strdup(opal_convert_jobid_to_string(proc->jobid));
        op->rank = proc->vpid;
    }


    /* call the library function */
    rc = PMIx_Get_nb(op->nspace, op->rank, key, val_cbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    
    return pmix1xx_convert_rc(rc);
}

int pmix1xx_publish(opal_pmix_data_range_t scope,
                    opal_pmix_persistence_t persist,
                    opal_list_t *info)
{
}

int pmix1xx_publishnb(opal_pmix_data_range_t scope,
                      opal_pmix_persistence_t persist,
                      opal_list_t *info,
                      opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
}

int pmix1xx_lookup(opal_pmix_data_range_t scope,
                   opal_list_t *data)
{
}

int pmix1xx_lookupnb(opal_pmix_data_range_t scope, int wait, const char **keys,
                     opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
}

int pmix1xx_unpublish(opal_pmix_data_range_t scope, const char **keys)
{
}

int pmix1xx_unpublishnb(opal_pmix_data_range_t scope, const char **keys,
                        opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
}

int pmix1xx_spawn(opal_list_t *apps, opal_jobid_t *jobid)
{
}

int pmix1xx_spawnnb(opal_list_t *apps,
                    opal_pmix_spawn_cbfunc_t cbfunc,
                    void *cbdata)
{
}

int pmix1xx_connect(opal_list_t *procs)
{
}

int pmix1xx_connectnb(opal_list_t *procs,
                      opal_pmix_op_cbfunc_t cbfunc,
                      void *cbdata)
{
}

int pmix1xx_disconnect(opal_list_t *procs)
{
}

int pmix1xx_disconnectnb(opal_list_t *procs,
                         opal_pmix_op_cbfunc_t cbfunc,
                         void *cbdata)
{
}

int pmix1xx_resolve_peers(const char *nodename, opal_jobid_t jobid,
                          opal_list_t *procs)
{
}

int pmix1xx_resolve_nodes(opal_jobid_t jobid, char **nodelist)
{
}



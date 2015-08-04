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

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress_threads.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/mca/pmix/base/base.h"
#include "pmix1xx.h"

#include "pmix.h"
#include "pmix_server.h"

static pmix_status_t server_finalized_fn(const char nspace[], int rank, void* server_object,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_abort_fn(const char nspace[], int rank, void *server_object,
                                     int status, const char msg[],
                                     pmix_proc_t procs[], size_t nprocs,
                                     pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                                       char *data, size_t ndata,
                                       pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_dmodex_req_fn(const char nspace[], int rank,
                                          pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_publish_fn(const char nspace[], int rank,
                                       pmix_data_range_t scope, pmix_persistence_t persist,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_lookup_fn(const char nspace[], int rank,
                                      pmix_data_range_t scope, int wait, char **keys,
                                      pmix_lookup_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_unpublish_fn(const char nspace[], int rank,
                                         pmix_data_range_t scope, char **keys,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_spawn_fn(const pmix_app_t apps[], size_t napps,
                                     pmix_spawn_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_listener_fn(int listening_sd,
                                        pmix_connection_cbfunc_t cbfunc);

static pmix_server_module_t mymodule = {
    server_finalized_fn,
    server_abort_fn,
    server_fencenb_fn,
    server_dmodex_req_fn,
    server_publish_fn,
    server_lookup_fn,
    server_unpublish_fn,
    server_spawn_fn,
    server_connect_fn,
    server_disconnect_fn,
    server_listener_fn
};

static opal_pmix_server_module_t *host_module = NULL;
static char *dbgvalue=NULL;


/****  SOUTHBOUND INTERFACES  ****/
int pmix1xx_server_init(opal_pmix_server_module_t *module)
{
    pmix_status_t rc;
    struct sockaddr_un addr;
    int dbg;

    if (0 < (dbg = opal_output_get_verbosity(opal_pmix_base_framework.framework_output))) {
        asprintf(&dbgvalue, "PMIX_DEBUG=%d", dbg);
        putenv(dbgvalue);
    }

    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, true))) {
        return pmix1xx_convert_rc(rc);
    }
    /* fetch our rendezvous address in case we need it */
    if (PMIX_SUCCESS != (rc = PMIx_get_rendezvous_address(&addr, &mca_pmix_pmix1xx_component.uri))) {
        return pmix1xx_convert_rc(rc);
    }
    /* record the host module */
    host_module = module;

    return OPAL_SUCCESS;    
}

int pmix1xx_server_finalize(void)
{
    pmix_status_t rc;

    if (NULL != mca_pmix_pmix1xx_component.uri) {
        free(mca_pmix_pmix1xx_component.uri);
    }
    rc = PMIx_server_finalize();
    return pmix1xx_convert_rc(rc);
}

const char* pmix1xx_server_get_addr(void)
{
    return mca_pmix_pmix1xx_component.uri;
}

int pmix1xx_server_gen_regex(const char *input, char **regex)
{
    pmix_status_t rc;

    rc = PMIx_generate_regex(input, regex);
    return pmix1xx_convert_rc(rc);
}


int pmix1xx_server_gen_ppn(const char *input, char **ppn)
{
    pmix_status_t rc;

    rc = PMIx_generate_ppn(input, ppn);
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

int pmix1xx_server_register_nspace(opal_jobid_t jobid,
                                   int nlocalprocs,
                                   opal_list_t *info,
                                   opal_pmix_op_cbfunc_t cbfunc,
                                   void *cbdata)
{
    opal_value_t *kv, *k2;
    pmix_info_t *pinfo, *pmap;
    size_t sz, szmap, m, n;
    char nspace[PMIX_MAX_NSLEN];
    pmix_status_t rc;
    pmix1xx_opcaddy_t *op;
    opal_list_t *pmapinfo;

    /* convert the jobid */
    (void)snprintf(nspace, PMIX_MAX_NSLEN, opal_convert_jobid_to_string(jobid));

    /* convert the list to an array of pmix_info_t */
    if (NULL != info) {
        sz = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            if (0 == strcmp(kv->key, OPAL_PMIX_PROC_DATA)) {
                pinfo[n].value.type = PMIX_INFO_ARRAY;
                /* the value contains a list of values - convert
                 * that list to another array */
                pmapinfo = (opal_list_t*)kv->data.ptr;
                szmap = opal_list_get_size(pmapinfo);
                PMIX_INFO_CREATE(pmap, szmap);
                pinfo[n].value.data.array.array = (struct pmix_info_t*)pmap;
                pinfo[n].value.data.array.size = szmap;
                m = 0;
                OPAL_LIST_FOREACH(k2, pmapinfo, opal_value_t) {
                    (void)strncpy(pmap[m].key, k2->key, PMIX_MAX_KEYLEN);
                    pmix1xx_value_load(&pmap[m].value, k2);
                    ++m;
                }
            } else {
                pmix1xx_value_load(&pinfo[n].value, kv);
            }
            ++n;
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }

    /* setup the caddy */
    op = OBJ_NEW(pmix1xx_opcaddy_t);
    op->info = pinfo;
    op->sz = sz;
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    rc = PMIx_server_register_nspace(nspace, nlocalprocs, pinfo, sz,
                                     opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix1xx_convert_rc(rc);
}


int pmix1xx_server_register_client(const opal_process_name_t *proc,
                                   uid_t uid, gid_t gid,
                                   void *server_object,
                                   opal_pmix_op_cbfunc_t cbfunc,
                                   void *cbdata)
{
    pmix_status_t rc;
    pmix1xx_opcaddy_t *op;
    char nspace[PMIX_MAX_NSLEN];
    char *jc;

    /* convert the jobid */
    jc = opal_convert_jobid_to_string(proc->jobid);
    (void)snprintf(nspace, PMIX_MAX_NSLEN, jc);

    op = OBJ_NEW(pmix1xx_opcaddy_t);
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;
    rc = PMIx_server_register_client(nspace, (int)proc->vpid,
                                     uid, gid, server_object,
                                     opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix1xx_convert_rc(rc);
}


int pmix1xx_server_setup_fork(const opal_process_name_t *proc, char ***env)
{
    pmix_status_t rc;
    char nspace[PMIX_MAX_NSLEN];
    char *jc;

    /* convert the jobid */
    jc = opal_convert_jobid_to_string(proc->jobid);
    (void)snprintf(nspace, PMIX_MAX_NSLEN, jc);
   
    rc = PMIx_server_setup_fork(nspace, (int)proc->vpid, env);
    return pmix1xx_convert_rc(rc);
}

static void dmdx_response(pmix_status_t status, char *data,
                          size_t sz, void *cbdata)
{
    int rc;
    pmix1xx_opcaddy_t *op = (pmix1xx_opcaddy_t*)cbdata;

    rc = pmix1xx_convert_rc(status);
    if (NULL != op->mdxcbfunc) {
        op->mdxcbfunc(rc, data, sz, op->cbdata);
    }
    OBJ_RELEASE(op);
}

int pmix1xx_server_dmodex(const opal_process_name_t *proc,
                          opal_pmix_modex_cbfunc_t cbfunc,
                          void *cbdata)
{
    pmix1xx_opcaddy_t *op;
    pmix_status_t rc;
    char nspace[PMIX_MAX_NSLEN];

    /* convert the jobid */
    (void)snprintf(nspace, PMIX_MAX_NSLEN, opal_convert_jobid_to_string(proc->jobid));

    /* setup the caddy */
    op = OBJ_NEW(pmix1xx_opcaddy_t);
    op->mdxcbfunc = cbfunc;
    op->cbdata = cbdata;

    /* find the internally-cached data for this proc */
    rc = PMIx_server_dmodex_request(nspace, (int)proc->vpid,
                                    dmdx_response, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix1xx_convert_rc(rc);
}

int pmix1xx_server_notify_error(int status,
                                opal_list_t *procs,
                                opal_list_t *error_procs,
                                opal_list_t *info,
                                opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    opal_value_t *kv;
    pmix_info_t *pinfo;
    size_t sz, psz, esz, n;
    pmix_proc_t *ps, *eps;
    pmix_status_t rc;
    pmix1xx_opcaddy_t *op;
    opal_namelist_t *nm;

    /* convert the list of procs */
    if (NULL != procs) {
        psz = opal_list_get_size(procs);
        PMIX_PROC_CREATE(ps, psz);
        n = 0;
        OPAL_LIST_FOREACH(nm, procs, opal_namelist_t) {
            (void)snprintf(ps[n].nspace, PMIX_MAX_NSLEN, opal_convert_jobid_to_string(nm->name.jobid));
            ps[n].rank = (int)nm->name.vpid;
            ++n;
        }
    } else {
        psz = 0;
        ps = NULL;
    }
    if (NULL != error_procs) {
        esz = opal_list_get_size(error_procs);
        PMIX_PROC_CREATE(eps, esz);
        n = 0;
        OPAL_LIST_FOREACH(nm, error_procs, opal_namelist_t) {
            (void)snprintf(eps[n].nspace, PMIX_MAX_NSLEN, opal_convert_jobid_to_string(nm->name.jobid));
            eps[n].rank = (int)nm->name.vpid;
            ++n;
        }
    } else {
        esz = 0;
        eps = NULL;
    }

    /* convert the list to an array of pmix_info_t */
    if (NULL != info) {
        sz = opal_list_get_size(info);
        PMIX_INFO_CREATE(pinfo, sz);
        n = 0;
        OPAL_LIST_FOREACH(kv, info, opal_value_t) {
            (void)strncpy(pinfo[n].key, kv->key, PMIX_MAX_KEYLEN);
            pmix1xx_value_load(&pinfo[n].value, kv);
        }
    } else {
        sz = 0;
        pinfo = NULL;
    }

    /* setup the caddy */
    op = OBJ_NEW(pmix1xx_opcaddy_t);
    op->procs = ps;
    op->nprocs = psz;
    op->error_procs = eps;
    op->nerror_procs = esz;
    op->info = pinfo;
    op->sz = sz;
    op->opcbfunc = cbfunc;
    op->cbdata = cbdata;

    rc = pmix1xx_convert_opalrc(status);
    rc = PMIx_server_notify_error(rc, ps, psz, eps, esz,
                                  pinfo, sz, NULL, NULL,
                                  opcbfunc, op);
    if (PMIX_SUCCESS != rc) {
        OBJ_RELEASE(op);
    }
    return pmix1xx_convert_rc(rc);
}



/****  NORTHBOUND INTERFACES  ****/
static void opal_opcbfunc(int status, void *cbdata)
{
    pmix1xx_opalcaddy_t *opalcaddy = (pmix1xx_opalcaddy_t*)cbdata;

    if (NULL != opalcaddy->opcbfunc) {
        opalcaddy->opcbfunc(pmix1xx_convert_opalrc(status), opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_finalized_fn(const char nspace[], int rank, void* server_object,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;

    if (NULL == host_module || NULL == host_module->finalized) {
        return PMIX_SUCCESS;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, nspace))) {
        return pmix1xx_convert_opalrc(rc);
    }
    proc.vpid = rank;

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* pass it up */
    rc = host_module->finalized(&proc, server_object, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix1xx_convert_opalrc(rc);
}

static pmix_status_t server_abort_fn(const char nspace[], int rank, void *server_object,
                                     int status, const char msg[],
                                     pmix_proc_t procs[], size_t nprocs,
                                     pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    size_t n;
    opal_namelist_t *nm;
    opal_process_name_t proc;
    int rc;
    pmix1xx_opalcaddy_t *opalcaddy;

    if (NULL == host_module || NULL == host_module->abort) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, nspace))) {
        OBJ_RELEASE(opalcaddy);
        return pmix1xx_convert_opalrc(rc);
    }
    proc.vpid = rank;

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1xx_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* pass it up */
    rc = host_module->abort(&proc, server_object, status, msg,
                            &opalcaddy->procs, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix1xx_convert_opalrc(rc);
}

static void opmdx_response(int status, const char *data,
                           size_t sz, void *cbdata)
{
    pmix_status_t rc;
    pmix1xx_opalcaddy_t *opalcaddy = (pmix1xx_opalcaddy_t*)cbdata;

    rc = pmix1xx_convert_rc(status);
    if (NULL != opalcaddy->mdxcbfunc) {
        opalcaddy->mdxcbfunc(rc, data, sz, opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                                       char *data, size_t ndata,
                                       pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix1xx_opalcaddy_t *opalcaddy;
    size_t n;
    opal_namelist_t *nm;
    int rc;

    if (NULL == host_module || NULL == host_module->fence_nb) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->mdxcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1xx_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* pass it up */
    rc = host_module->fence_nb(&opalcaddy->procs, data, ndata,
                               opmdx_response, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix1xx_convert_opalrc(rc);
}

static pmix_status_t server_dmodex_req_fn(const char nspace[], int rank,
                                          pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;

    if (NULL == host_module || NULL == host_module->direct_modex) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, nspace))) {
        return pmix1xx_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = rank;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->mdxcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* pass it up */
    rc = host_module->direct_modex(&proc, opmdx_response, opalcaddy);
    if (OPAL_SUCCESS != rc && OPAL_ERR_IN_PROCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    if (OPAL_ERR_IN_PROCESS == rc) {
        rc = OPAL_SUCCESS;
    }
    return pmix1xx_convert_opalrc(rc);
}

static pmix_status_t server_publish_fn(const char nspace[], int rank,
                                       pmix_data_range_t scope, pmix_persistence_t persist,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    size_t n;
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_pmix_data_range_t oscp;
    opal_pmix_persistence_t opers;
    opal_pmix_info_t *oinfo;

    if (NULL == host_module || NULL == host_module->publish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

   /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, nspace))) {
        return pmix1xx_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = rank;
    }

    /* convert the data range */
    if (OPAL_SUCCESS != (rc = pmix1xx_convert_data_range(&oscp, scope))) {
        return pmix1xx_convert_opalrc(rc);
    }

    /* convert the persistence */
    if (OPAL_SUCCESS != (rc = pmix1xx_convert_persistence(&opers, persist))) {
        return pmix1xx_convert_opalrc(rc);
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the info array */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_pmix_info_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1xx_value_unload(&oinfo->value, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1xx_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->publish(&proc, oscp, opers, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1xx_convert_opalrc(rc);
}

static void opal_lkupcbfunc(int status,
                            opal_list_t *data,
                            void *cbdata)
{
    pmix1xx_opalcaddy_t *opalcaddy = (pmix1xx_opalcaddy_t*)cbdata;
    pmix_status_t rc;
    pmix_pdata_t *d=NULL;
    size_t nd=0, n;
    opal_pmix_pdata_t *p;

    if (NULL != opalcaddy->lkupcbfunc) {
        rc = pmix1xx_convert_opalrc(status);
        /* convert any returned data */
        if (NULL != data) {
            nd = opal_list_get_size(data);
            PMIX_PDATA_CREATE(d, nd);
            n=0;
            OPAL_LIST_FOREACH(p, data, opal_pmix_pdata_t) {
                /* convert the jobid */
                (void)snprintf(d[n].proc.nspace, PMIX_MAX_NSLEN, opal_convert_jobid_to_string(p->proc.jobid));
                d[n].proc.rank = p->proc.vpid;
                (void)strncpy(d[n].key, p->key, PMIX_MAX_KEYLEN);
                pmix1xx_value_load(&d[n].value, &p->value);
            }
        }
        opalcaddy->lkupcbfunc(rc, d, nd, opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_lookup_fn(const char nspace[], int rank,
                                      pmix_data_range_t scope, int wait, char **keys,
                                      pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_pmix_data_range_t oscp;
    opal_process_name_t proc;

    if (NULL == host_module || NULL == host_module->lookup) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, nspace))) {
        return pmix1xx_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = rank;
    }

    /* convert the scope */
    if (OPAL_SUCCESS != (rc = pmix1xx_convert_data_range(&oscp, scope))) {
        return pmix1xx_convert_opalrc(rc);
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->lkupcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* pass it up */
    rc = host_module->lookup(&proc, oscp, wait, keys, opal_lkupcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1xx_convert_opalrc(rc);
}


static pmix_status_t server_unpublish_fn(const char nspace[], int rank,
                                         pmix_data_range_t scope, char **keys,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_pmix_data_range_t oscp;

    if (NULL == host_module || NULL == host_module->unpublish) {
        return PMIX_SUCCESS;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, nspace))) {
        return pmix1xx_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = rank;
    }

    /* convert the data range */
    if (OPAL_SUCCESS != (rc = pmix1xx_convert_data_range(&oscp, scope))) {
        return pmix1xx_convert_opalrc(rc);
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* pass it up */
    rc = host_module->unpublish(&proc, oscp, keys, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1xx_convert_opalrc(rc);
}

static void opal_spncbfunc(int status, opal_jobid_t jobid, void *cbdata)
{
    pmix1xx_opalcaddy_t *opalcaddy = (pmix1xx_opalcaddy_t*)cbdata;
    pmix_status_t rc;
    char nspace[PMIX_MAX_NSLEN];

    if (NULL != opalcaddy->spwncbfunc) {
        rc = pmix1xx_convert_opalrc(status);
        /* convert the jobid */
        (void)snprintf(nspace, PMIX_MAX_NSLEN, opal_convert_jobid_to_string(jobid));
        opalcaddy->spwncbfunc(rc, nspace, cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_spawn_fn(const pmix_app_t apps[], size_t napps,
                                     pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_pmix_app_t *app;
    opal_pmix_info_t *oinfo;
    size_t k, n;
    int rc;

    if (NULL == host_module || NULL == host_module->spawn) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->spwncbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the apps */
    for (n=0; n < napps; n++) {
        app = OBJ_NEW(opal_pmix_app_t);
        opal_list_append(&opalcaddy->apps, &app->super);
        if (NULL != apps[n].cmd) {
            app->cmd = strdup(apps[n].cmd);
        }
        app->argc = apps[n].argc;
        if (NULL != apps[n].argv) {
            app->argv = opal_argv_copy(apps[n].argv);
        }
        if (NULL != apps[n].env) {
            app->env = opal_argv_copy(apps[n].env);
        }
        app->maxprocs = apps[n].maxprocs;
        for (k=0; k < apps[n].ninfo; k++) {
            oinfo = OBJ_NEW(opal_pmix_info_t);
            opal_list_append(&app->info, &oinfo->super);
            oinfo->key = strdup(apps[n].info[k].key);
            if (OPAL_SUCCESS != (rc = pmix1xx_value_unload(&oinfo->value, &apps[n].info[k].value))) {
                OBJ_RELEASE(opalcaddy);
                return pmix1xx_convert_opalrc(rc);
            }
        }
    }

    /* pass it up */
    rc = host_module->spawn(&opalcaddy->apps, opal_spncbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1xx_convert_opalrc(rc);
}


static pmix_status_t server_connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_namelist_t *nm;
    size_t n;

    if (NULL == host_module || NULL == host_module->connect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1xx_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* pass it up */
    rc = host_module->connect(&opalcaddy->procs, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1xx_convert_opalrc(rc);
}


static pmix_status_t server_disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1xx_opalcaddy_t *opalcaddy;
    opal_namelist_t *nm;
    size_t n;

    if (NULL == host_module || NULL == host_module->disconnect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1xx_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1xx_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* pass it up */
    rc = host_module->disconnect(&opalcaddy->procs, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1xx_convert_opalrc(rc);
}

static pmix_status_t server_listener_fn(int listening_sd,
                                        pmix_connection_cbfunc_t cbfunc)
{
    int rc;

    if (NULL == host_module || NULL == host_module->listener) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    rc = host_module->listener(listening_sd, cbfunc);
    return pmix1xx_convert_opalrc(rc);
}



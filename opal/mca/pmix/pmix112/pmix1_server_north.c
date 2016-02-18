/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
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
#include "pmix1.h"

#include "pmix.h"
#include "pmix_server.h"

/****    N.O.R.T.H.B.O.U.N.D   I.N.T.E.R.F.A.C.E.S     ****/

/* These are the interfaces used by the embedded PMIx server
 * to call up into ORTE for service requests */

static pmix_status_t server_client_connected_fn(const pmix_proc_t *proc, void* server_object);
static pmix_status_t server_client_finalized_fn(const pmix_proc_t *proc, void* server_object,
                                                pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_abort_fn(const pmix_proc_t *proc, void *server_object,
                                     int status, const char msg[],
                                     pmix_proc_t procs[], size_t nprocs,
                                     pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                                       const pmix_info_t info[], size_t ninfo,
                                       char *data, size_t ndata,
                                       pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_dmodex_req_fn(const pmix_proc_t *proc,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_publish_fn(const pmix_proc_t *proc,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_lookup_fn(const pmix_proc_t *proc,  char **keys,
                                      const pmix_info_t info[], size_t ninfo,
                                      pmix_lookup_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_unpublish_fn(const pmix_proc_t *proc, char **keys,
                                         const pmix_info_t info[], size_t ninfo,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_spawn_fn(const pmix_proc_t *proc,
                                     const pmix_info_t job_info[], size_t ninfo,
                                     const pmix_app_t apps[], size_t napps,
                                     pmix_spawn_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_register_events(const pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_deregister_events(const pmix_info_t info[], size_t ninfo,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t server_listener_fn(int listening_sd,
                                        pmix_connection_cbfunc_t cbfunc);

pmix_server_module_t pmix112_module = {
    server_client_connected_fn,
    server_client_finalized_fn,
    server_abort_fn,
    server_fencenb_fn,
    server_dmodex_req_fn,
    server_publish_fn,
    server_lookup_fn,
    server_unpublish_fn,
    server_spawn_fn,
    server_connect_fn,
    server_disconnect_fn,
    server_register_events,
    server_deregister_events,
    server_listener_fn
};

opal_pmix_server_module_t *pmix112_host_module = NULL;


static void opal_opcbfunc(int status, void *cbdata)
{
    pmix1_opalcaddy_t *opalcaddy = (pmix1_opalcaddy_t*)cbdata;

    if (NULL != opalcaddy->opcbfunc) {
        opalcaddy->opcbfunc(pmix1_convert_opalrc(status), opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_client_connected_fn(const pmix_proc_t *p, void *server_object)
{
    int rc;
    opal_process_name_t proc;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->client_connected) {
        return PMIX_SUCCESS;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    proc.vpid = p->rank;

    /* pass it up */
    rc = pmix112_host_module->client_connected(&proc, server_object);
    return pmix1_convert_opalrc(rc);
}

static pmix_status_t server_client_finalized_fn(const pmix_proc_t *p, void* server_object,
                                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->client_finalized) {
        return PMIX_SUCCESS;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    proc.vpid = p->rank;

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* pass it up */
    rc = pmix112_host_module->client_finalized(&proc, server_object, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix1_convert_opalrc(rc);
}

static pmix_status_t server_abort_fn(const pmix_proc_t *p, void *server_object,
                                     int status, const char msg[],
                                     pmix_proc_t procs[], size_t nprocs,
                                     pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    size_t n;
    opal_namelist_t *nm;
    opal_process_name_t proc;
    int rc;
    pmix1_opalcaddy_t *opalcaddy;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->abort) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    proc.vpid = p->rank;

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* pass it up */
    rc = pmix112_host_module->abort(&proc, server_object, status, msg,
                            &opalcaddy->procs, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix1_convert_opalrc(rc);
}

static void _data_release(void *cbdata)
{
    pmix1_opalcaddy_t *opalcaddy = (pmix1_opalcaddy_t*)cbdata;

    if (NULL != opalcaddy->odmdxfunc) {
        opalcaddy->odmdxfunc(opalcaddy->ocbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static void opmdx_response(int status, const char *data, size_t sz, void *cbdata,
                           opal_pmix_release_cbfunc_t relcbfunc, void *relcbdata)
{
    pmix_status_t rc;
    pmix1_opalcaddy_t *opalcaddy = (pmix1_opalcaddy_t*)cbdata;

    rc = pmix1_convert_rc(status);
    if (NULL != opalcaddy->mdxcbfunc) {
        opalcaddy->odmdxfunc = relcbfunc;
        opalcaddy->ocbdata = relcbdata;
        opalcaddy->mdxcbfunc(rc, data, sz, opalcaddy->cbdata,
                             _data_release, opalcaddy);
    } else {
        OBJ_RELEASE(opalcaddy);
    }
}

static pmix_status_t server_fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                                       const pmix_info_t info[], size_t ninfo,
                                       char *data, size_t ndata,
                                       pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix1_opalcaddy_t *opalcaddy;
    size_t n;
    opal_namelist_t *nm;
    opal_value_t *iptr;
    int rc;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->fence_nb) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->mdxcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->fence_nb(&opalcaddy->procs, &opalcaddy->info,
                               data, ndata, opmdx_response, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix1_convert_opalrc(rc);
}

static pmix_status_t server_dmodex_req_fn(const pmix_proc_t *p,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *iptr;
    size_t n;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->direct_modex) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == p->rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = p->rank;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->mdxcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->direct_modex(&proc, &opalcaddy->info, opmdx_response, opalcaddy);
    if (OPAL_SUCCESS != rc && OPAL_ERR_IN_PROCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    if (OPAL_ERR_IN_PROCESS == rc) {
        rc = OPAL_SUCCESS;
    }
    return pmix1_convert_opalrc(rc);
}

static pmix_status_t server_publish_fn(const pmix_proc_t *p,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    size_t n;
    pmix1_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *oinfo;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->publish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

   /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == p->rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = p->rank;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the info array */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->publish(&proc, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1_convert_opalrc(rc);
}

static void opal_lkupcbfunc(int status,
                            opal_list_t *data,
                            void *cbdata)
{
    pmix1_opalcaddy_t *opalcaddy = (pmix1_opalcaddy_t*)cbdata;
    pmix_status_t rc;
    pmix_pdata_t *d=NULL;
    size_t nd=0, n;
    opal_pmix_pdata_t *p;

    if (NULL != opalcaddy->lkupcbfunc) {
        rc = pmix1_convert_opalrc(status);
        /* convert any returned data */
        if (NULL != data) {
            nd = opal_list_get_size(data);
            PMIX_PDATA_CREATE(d, nd);
            n=0;
            OPAL_LIST_FOREACH(p, data, opal_pmix_pdata_t) {
                /* convert the jobid */
                (void)opal_snprintf_jobid(d[n].proc.nspace, PMIX_MAX_NSLEN, p->proc.jobid);
                d[n].proc.rank = p->proc.vpid;
                (void)strncpy(d[n].key, p->value.key, PMIX_MAX_KEYLEN);
                pmix1_value_load(&d[n].value, &p->value);
            }
        }
        opalcaddy->lkupcbfunc(rc, d, nd, opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_lookup_fn(const pmix_proc_t *p, char **keys,
                                      const pmix_info_t info[], size_t ninfo,
                                      pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *iptr;
    size_t n;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->lookup) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == p->rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = p->rank;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->lkupcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->lookup(&proc, keys, &opalcaddy->info, opal_lkupcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1_convert_opalrc(rc);
}


static pmix_status_t server_unpublish_fn(const pmix_proc_t *p, char **keys,
                                         const pmix_info_t info[], size_t ninfo,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *iptr;
    size_t n;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->unpublish) {
        return PMIX_SUCCESS;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == p->rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = p->rank;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->unpublish(&proc, keys, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1_convert_opalrc(rc);
}

static void opal_spncbfunc(int status, opal_jobid_t jobid, void *cbdata)
{
    pmix1_opalcaddy_t *opalcaddy = (pmix1_opalcaddy_t*)cbdata;
    pmix_status_t rc;
    char nspace[PMIX_MAX_NSLEN];

    if (NULL != opalcaddy->spwncbfunc) {
        rc = pmix1_convert_opalrc(status);
        /* convert the jobid */
        (void)opal_snprintf_jobid(nspace, PMIX_MAX_NSLEN, jobid);
        opalcaddy->spwncbfunc(rc, nspace, opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_spawn_fn(const pmix_proc_t *p,
                                     const pmix_info_t job_info[], size_t ninfo,
                                     const pmix_app_t apps[], size_t napps,
                                     pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    pmix1_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_pmix_app_t *app;
    opal_value_t *oinfo;
    size_t k, n;
    int rc;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->spawn) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix1_convert_opalrc(rc);
    }
    if (PMIX_RANK_WILDCARD == p->rank) {
        proc.vpid = OPAL_VPID_WILDCARD;
    } else {
        proc.vpid = p->rank;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->spwncbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the job info */
    for (k=0; k < ninfo; k++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(job_info[k].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(oinfo, &job_info[k].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

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
            oinfo = OBJ_NEW(opal_value_t);
            opal_list_append(&app->info, &oinfo->super);
            oinfo->key = strdup(apps[n].info[k].key);
            if (OPAL_SUCCESS != (rc = pmix1_value_unload(oinfo, &apps[n].info[k].value))) {
                OBJ_RELEASE(opalcaddy);
                return pmix1_convert_opalrc(rc);
            }
        }
    }

    /* pass it up */
    rc = pmix112_host_module->spawn(&proc, &opalcaddy->info, &opalcaddy->apps, opal_spncbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1_convert_opalrc(rc);
}


static pmix_status_t server_connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1_opalcaddy_t *opalcaddy;
    opal_namelist_t *nm;
    size_t n;
    opal_value_t *oinfo;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->connect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->connect(&opalcaddy->procs, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1_convert_opalrc(rc);
}


static pmix_status_t server_disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix1_opalcaddy_t *opalcaddy;
    opal_namelist_t *nm;
    size_t n;
    opal_value_t *oinfo;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->disconnect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            nm->name.vpid = OPAL_VPID_WILDCARD;
        } else {
            nm->name.vpid = procs[n].rank;
        }
    }

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->disconnect(&opalcaddy->procs, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1_convert_opalrc(rc);
}

static pmix_status_t server_register_events(const pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix1_opalcaddy_t *opalcaddy;
    size_t n;
    opal_value_t *oinfo;
    int rc;

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix1_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix1_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix1_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = pmix112_host_module->register_events(&opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix1_convert_opalrc(rc);
}

static pmix_status_t server_deregister_events(const pmix_info_t info[], size_t ninfo,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_ERR_NOT_IMPLEMENTED;
}
static pmix_status_t server_listener_fn(int listening_sd,
                                        pmix_connection_cbfunc_t cbfunc)
{
    int rc;

    if (NULL == pmix112_host_module || NULL == pmix112_host_module->listener) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    rc = pmix112_host_module->listener(listening_sd, cbfunc);
    return pmix1_convert_opalrc(rc);
}

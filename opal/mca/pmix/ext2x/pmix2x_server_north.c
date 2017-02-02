/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
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
#include "pmix2x.h"

#include "pmix.h"
#include "pmix_server.h"

/****    N.O.R.T.H.B.O.U.N.D   I.N.T.E.R.F.A.C.E.S     ****/

/* These are the interfaces used by the embedded PMIx server
 * to call up into ORTE for service requests */

 static pmix_status_t server_client_connected_fn(const pmix_proc_t *proc, void* server_object,
                                                 pmix_op_cbfunc_t cbfunc, void *cbdata);
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
 static pmix_status_t server_register_events(pmix_status_t *codes, size_t ncodes,
                                             const pmix_info_t info[], size_t ninfo,
                                             pmix_op_cbfunc_t cbfunc, void *cbdata);
 static pmix_status_t server_deregister_events(pmix_status_t *codes, size_t ncodes,
                                               pmix_op_cbfunc_t cbfunc, void *cbdata);
 static pmix_status_t server_notify_event(pmix_status_t code,
                                          const pmix_proc_t *source,
                                          pmix_data_range_t range,
                                          pmix_info_t info[], size_t ninfo,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata);
 static pmix_status_t server_query(pmix_proc_t *proct,
                                   pmix_query_t *queryies, size_t nqueries,
                                   pmix_info_cbfunc_t cbfunc,
                                   void *cbdata);
 static void server_tool_connection(pmix_info_t *info, size_t ninfo,
                                    pmix_tool_connection_cbfunc_t cbfunc,
                                    void *cbdata);
static void server_log(const pmix_proc_t *client,
                       const pmix_info_t data[], size_t ndata,
                       const pmix_info_t directives[], size_t ndirs,
                       pmix_op_cbfunc_t cbfunc, void *cbdata);

  pmix_server_module_t mymodule = {
    .client_connected = server_client_connected_fn,
    .client_finalized = server_client_finalized_fn,
    .abort = server_abort_fn,
    .fence_nb = server_fencenb_fn,
    .direct_modex = server_dmodex_req_fn,
    .publish = server_publish_fn,
    .lookup = server_lookup_fn,
    .unpublish = server_unpublish_fn,
    .spawn = server_spawn_fn,
    .connect = server_connect_fn,
    .disconnect = server_disconnect_fn,
    .register_events = server_register_events,
    .deregister_events = server_deregister_events,
    .notify_event = server_notify_event,
    .query = server_query,
    .tool_connected = server_tool_connection,
    .log = server_log
};

opal_pmix_server_module_t *host_module = NULL;


static void opal_opcbfunc(int status, void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy = (pmix2x_opalcaddy_t*)cbdata;

    if (NULL != opalcaddy->opcbfunc) {
        opalcaddy->opcbfunc(pmix2x_convert_opalrc(status), opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_client_connected_fn(const pmix_proc_t *p, void *server_object,
                                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    opal_process_name_t proc;
    pmix2x_opalcaddy_t *opalcaddy;

    if (NULL == host_module || NULL == host_module->client_connected) {
        return PMIX_SUCCESS;
    }

    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* pass it up */
    rc = host_module->client_connected(&proc, server_object,
                                       opal_opcbfunc, opalcaddy);
    return pmix2x_convert_opalrc(rc);
}

static pmix_status_t server_client_finalized_fn(const pmix_proc_t *p, void* server_object,
                                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;

    if (NULL == host_module || NULL == host_module->client_finalized) {
        return PMIX_SUCCESS;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* pass it up */
    rc = host_module->client_finalized(&proc, server_object, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix2x_convert_opalrc(rc);
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
    pmix2x_opalcaddy_t *opalcaddy;

    if (NULL == host_module || NULL == host_module->abort) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
        nm->name.vpid = pmix2x_convert_rank(procs[n].rank);
    }

    /* pass it up */
    rc = host_module->abort(&proc, server_object, status, msg,
                            &opalcaddy->procs, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix2x_convert_opalrc(rc);
}

static void _data_release(void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy = (pmix2x_opalcaddy_t*)cbdata;

    if (NULL != opalcaddy->odmdxfunc) {
        opalcaddy->odmdxfunc(opalcaddy->ocbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static void opmdx_response(int status, const char *data, size_t sz, void *cbdata,
                           opal_pmix_release_cbfunc_t relcbfunc, void *relcbdata)
{
    pmix_status_t rc;
    pmix2x_opalcaddy_t *opalcaddy = (pmix2x_opalcaddy_t*)cbdata;

    rc = pmix2x_convert_rc(status);
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
    pmix2x_opalcaddy_t *opalcaddy;
    size_t n;
    opal_namelist_t *nm;
    opal_value_t *iptr;
    int rc;

    if (NULL == host_module || NULL == host_module->fence_nb) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->mdxcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
        nm->name.vpid = pmix2x_convert_rank(procs[n].rank);
    }

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->fence_nb(&opalcaddy->procs, &opalcaddy->info,
                               data, ndata, opmdx_response, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix2x_convert_opalrc(rc);
}

static pmix_status_t server_dmodex_req_fn(const pmix_proc_t *p,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *iptr;
    size_t n;

    if (NULL == host_module || NULL == host_module->direct_modex) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->mdxcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->direct_modex(&proc, &opalcaddy->info, opmdx_response, opalcaddy);
    if (OPAL_SUCCESS != rc && OPAL_ERR_IN_PROCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }
    if (OPAL_ERR_IN_PROCESS == rc) {
        rc = OPAL_SUCCESS;
    }
    return pmix2x_convert_opalrc(rc);
}

static pmix_status_t server_publish_fn(const pmix_proc_t *p,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    size_t n;
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *oinfo;

    if (NULL == host_module || NULL == host_module->publish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

   /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the info array */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->publish(&proc, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}

static void opal_lkupcbfunc(int status,
                            opal_list_t *data,
                            void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy = (pmix2x_opalcaddy_t*)cbdata;
    pmix_status_t rc;
    pmix_pdata_t *d=NULL;
    size_t nd=0, n;
    opal_pmix_pdata_t *p;

    if (NULL != opalcaddy->lkupcbfunc) {
        rc = pmix2x_convert_opalrc(status);
        /* convert any returned data */
        if (NULL != data) {
            nd = opal_list_get_size(data);
            PMIX_PDATA_CREATE(d, nd);
            n=0;
            OPAL_LIST_FOREACH(p, data, opal_pmix_pdata_t) {
                /* convert the jobid */
                (void)opal_snprintf_jobid(d[n].proc.nspace, PMIX_MAX_NSLEN, p->proc.jobid);
                d[n].proc.rank = pmix2x_convert_opalrank(p->proc.vpid);
                (void)strncpy(d[n].key, p->value.key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&d[n].value, &p->value);
            }
        }
        opalcaddy->lkupcbfunc(rc, d, nd, opalcaddy->cbdata);
        PMIX_PDATA_FREE(d, nd);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_lookup_fn(const pmix_proc_t *p, char **keys,
                                      const pmix_info_t info[], size_t ninfo,
                                      pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *iptr;
    size_t n;

    if (NULL == host_module || NULL == host_module->lookup) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->lkupcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->lookup(&proc, keys, &opalcaddy->info, opal_lkupcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}


static pmix_status_t server_unpublish_fn(const pmix_proc_t *p, char **keys,
                                         const pmix_info_t info[], size_t ninfo,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_value_t *iptr;
    size_t n;

    if (NULL == host_module || NULL == host_module->unpublish) {
        return PMIX_SUCCESS;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_info_t to the list of info */
    for (n=0; n < ninfo; n++) {
        iptr = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &iptr->super);
        iptr->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(iptr, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->unpublish(&proc, keys, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}

static void opal_spncbfunc(int status, opal_jobid_t jobid, void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy = (pmix2x_opalcaddy_t*)cbdata;
    pmix_status_t rc;
    char nspace[PMIX_MAX_NSLEN];

    if (NULL != opalcaddy->spwncbfunc) {
        rc = pmix2x_convert_opalrc(status);
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
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t proc;
    opal_pmix_app_t *app;
    opal_value_t *oinfo;
    size_t k, n;
    int rc;

    if (NULL == host_module || NULL == host_module->spawn) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* convert the nspace/rank to an opal_process_name_t */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&proc.jobid, p->nspace))) {
        return pmix2x_convert_opalrc(rc);
    }
    proc.vpid = pmix2x_convert_rank(p->rank);

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->spwncbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the job info */
    for (k=0; k < ninfo; k++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(job_info[k].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &job_info[k].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* convert the apps */
    for (n=0; n < napps; n++) {
        app = OBJ_NEW(opal_pmix_app_t);
        opal_list_append(&opalcaddy->apps, &app->super);
        if (NULL != apps[n].cmd) {
            app->cmd = strdup(apps[n].cmd);
        }
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
            if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &apps[n].info[k].value))) {
                OBJ_RELEASE(opalcaddy);
                return pmix2x_convert_opalrc(rc);
            }
        }
    }

    /* pass it up */
    rc = host_module->spawn(&proc, &opalcaddy->info, &opalcaddy->apps, opal_spncbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}


static pmix_status_t server_connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix2x_opalcaddy_t *opalcaddy;
    opal_namelist_t *nm;
    size_t n;
    opal_value_t *oinfo;

    if (NULL == host_module || NULL == host_module->connect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
        nm->name.vpid = pmix2x_convert_rank(procs[n].rank);
    }

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->connect(&opalcaddy->procs, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}


static pmix_status_t server_disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int rc;
    pmix2x_opalcaddy_t *opalcaddy;
    opal_namelist_t *nm;
    size_t n;
    opal_value_t *oinfo;

    if (NULL == host_module || NULL == host_module->disconnect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the array of pmix_proc_t to the list of procs */
    for (n=0; n < nprocs; n++) {
        nm = OBJ_NEW(opal_namelist_t);
        opal_list_append(&opalcaddy->procs, &nm->super);
        if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&nm->name.jobid, procs[n].nspace))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
        nm->name.vpid = pmix2x_convert_rank(procs[n].rank);
    }

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->disconnect(&opalcaddy->procs, &opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}

static pmix_status_t server_register_events(pmix_status_t *codes, size_t ncodes,
                                            const pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy;
    size_t n;
    opal_value_t *oinfo;
    int rc;

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* pass it up */
    rc = host_module->register_events(&opalcaddy->info, opal_opcbfunc, opalcaddy);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}

static pmix_status_t server_deregister_events(pmix_status_t *codes, size_t ncodes,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_ERR_NOT_SUPPORTED;
}

static pmix_status_t server_notify_event(pmix_status_t code,
                                         const pmix_proc_t *source,
                                         pmix_data_range_t range,
                                         pmix_info_t info[], size_t ninfo,
                                         pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t src;
    size_t n;
    opal_value_t *oinfo;
    int rc, status;

    if (NULL == host_module || NULL == host_module->notify_event) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the code */
    status = pmix2x_convert_rc(code);

    /* convert the source */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&src.jobid, source->nspace))) {
        OBJ_RELEASE(opalcaddy);
        return pmix2x_convert_opalrc(rc);
    }
    src.vpid = pmix2x_convert_rank(source->rank);

    /* ignore the range for now */

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            return pmix2x_convert_opalrc(rc);
        }
    }

    /* send it upstairs */
    if (OPAL_SUCCESS != (rc = host_module->notify_event(status, &src, &opalcaddy->info,
                                                        opal_opcbfunc, opalcaddy))) {
        OBJ_RELEASE(opalcaddy);
    }
    return pmix2x_convert_opalrc(rc);
}

static void _info_rel(void *cbdata)
{
    pmix2x_opcaddy_t *pcaddy = (pmix2x_opcaddy_t*)cbdata;

    OBJ_RELEASE(pcaddy);
}
static void info_cbfunc(int status,
                        opal_list_t *info,
                        void *cbdata,
                        opal_pmix_release_cbfunc_t release_fn,
                        void *release_cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy = (pmix2x_opalcaddy_t*)cbdata;
    pmix2x_opcaddy_t *pcaddy;
    opal_value_t *kv;
    size_t n;

    pcaddy = OBJ_NEW(pmix2x_opcaddy_t);

    /* convert the status */
    pcaddy->status = pmix2x_convert_opalrc(status);

    /* convert the list to a pmix_info_t array */
    if (NULL != info) {
        pcaddy->ninfo = opal_list_get_size(info);
        if (0 < pcaddy->ninfo) {
            PMIX_INFO_CREATE(pcaddy->info, pcaddy->ninfo);
            n = 0;
            OPAL_LIST_FOREACH(kv, info, opal_value_t) {
                (void)strncpy(pcaddy->info[n].key, kv->key, PMIX_MAX_KEYLEN);
                pmix2x_value_load(&pcaddy->info[n].value, kv);
            }
        }
    }
    /* we are done with the incoming data */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* provide the answer downward */
    if (NULL != opalcaddy->infocbfunc) {
        opalcaddy->infocbfunc(pcaddy->status, pcaddy->info, pcaddy->ninfo,
                              opalcaddy->cbdata, _info_rel, pcaddy);
    }
    OBJ_RELEASE(opalcaddy);
}

static pmix_status_t server_query(pmix_proc_t *proct,
                                  pmix_query_t *queries, size_t nqueries,
                                  pmix_info_cbfunc_t cbfunc,
                                  void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t requestor;
    int rc;
    size_t n, m;
    opal_pmix_query_t *q;
    opal_value_t *oinfo;

    if (NULL == host_module || NULL == host_module->query) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->infocbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the requestor */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&requestor.jobid, proct->nspace))) {
        OBJ_RELEASE(opalcaddy);
        return pmix2x_convert_opalrc(rc);
    }
    requestor.vpid = pmix2x_convert_rank(proct->rank);

    /* convert the queries */
    for (n=0; n < nqueries; n++) {
            q = OBJ_NEW(opal_pmix_query_t);
        /* we "borrow" the info field of the caddy as we and the
         * server function both agree on what will be there */
        opal_list_append(&opalcaddy->info, &q->super);
        q->keys = opal_argv_copy(queries[n].keys);
        for (m=0; m < queries[n].nqual; m++) {
            oinfo = OBJ_NEW(opal_value_t);
            opal_list_append(&q->qualifiers, &oinfo->super);
            oinfo->key = strdup(queries[n].qualifiers[m].key);
            if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &queries[n].qualifiers[m].value))) {
                OBJ_RELEASE(opalcaddy);
                return pmix2x_convert_opalrc(rc);
            }
        }
    }

    /* pass the call upwards */
    if (OPAL_SUCCESS != (rc = host_module->query(&requestor,
                                                 &opalcaddy->info,
                                                 info_cbfunc, opalcaddy))) {
        OBJ_RELEASE(opalcaddy);
    }

    return pmix2x_convert_opalrc(rc);
}

static void toolcbfunc(int status,
                       opal_process_name_t proc,
                       void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy = (pmix2x_opalcaddy_t*)cbdata;
    pmix_status_t rc;
    pmix_proc_t p;
    opal_pmix2x_jobid_trkr_t *job;

    /* convert the status */
    rc = pmix2x_convert_opalrc(status);

    memset(&p, 0, sizeof(pmix_proc_t));
    if (OPAL_SUCCESS == status) {
        /* convert the process name */
        (void)opal_snprintf_jobid(p.nspace, PMIX_MAX_NSLEN, proc.jobid);
        p.rank = pmix2x_convert_opalrank(proc.vpid);
        /* store this job in our list of known nspaces */
        job = OBJ_NEW(opal_pmix2x_jobid_trkr_t);
        (void)strncpy(job->nspace, p.nspace, PMIX_MAX_NSLEN);
        job->jobid = proc.jobid;
        opal_list_append(&mca_pmix_ext2x_component.jobids, &job->super);
    }

    /* pass it down */
    if (NULL != opalcaddy->toolcbfunc) {
        opalcaddy->toolcbfunc(rc, &p, opalcaddy->cbdata);
    }
    OBJ_RELEASE(opalcaddy);
}

static void server_tool_connection(pmix_info_t *info, size_t ninfo,
                                   pmix_tool_connection_cbfunc_t cbfunc,
                                   void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy;
    size_t n;
    opal_value_t *oinfo;
    int rc;
    pmix_status_t err;

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->toolcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the info */
    for (n=0; n < ninfo; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        opal_list_append(&opalcaddy->info, &oinfo->super);
        oinfo->key = strdup(info[n].key);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &info[n].value))) {
            OBJ_RELEASE(opalcaddy);
            err = pmix2x_convert_opalrc(rc);
            if (NULL != cbfunc) {
                cbfunc(err, NULL, cbdata);
            }
        }
    }

    /* pass it up */
    host_module->tool_connected(&opalcaddy->info, toolcbfunc, opalcaddy);
}

static void server_log(const pmix_proc_t *proct,
                       const pmix_info_t data[], size_t ndata,
                       const pmix_info_t directives[], size_t ndirs,
                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix2x_opalcaddy_t *opalcaddy;
    opal_process_name_t requestor;
    int rc;
    size_t n;
    opal_value_t *oinfo;
    pmix_status_t ret;

    if (NULL == host_module || NULL == host_module->log) {
        if (NULL != cbfunc) {
            cbfunc(PMIX_ERR_NOT_SUPPORTED, cbdata);
        }
        return;
    }

    /* setup the caddy */
    opalcaddy = OBJ_NEW(pmix2x_opalcaddy_t);
    opalcaddy->opcbfunc = cbfunc;
    opalcaddy->cbdata = cbdata;

    /* convert the requestor */
    if (OPAL_SUCCESS != (rc = opal_convert_string_to_jobid(&requestor.jobid, proct->nspace))) {
    OBJ_RELEASE(opalcaddy);
    ret = pmix2x_convert_opalrc(rc);
        if (NULL != cbfunc) {
            cbfunc(ret, cbdata);
        }
        return;
    }
    requestor.vpid = pmix2x_convert_rank(proct->rank);

    /* convert the data */
    for (n=0; n < ndata; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        /* we "borrow" the info field of the caddy as we and the
         * server function both agree on what will be there */
        opal_list_append(&opalcaddy->info, &oinfo->super);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &data[n].value))) {
            OBJ_RELEASE(opalcaddy);
            ret = pmix2x_convert_opalrc(rc);
            if (NULL != cbfunc) {
                cbfunc(ret, cbdata);
            }
            return;
        }
    }

    /* convert the directives */
    for (n=0; n < ndirs; n++) {
        oinfo = OBJ_NEW(opal_value_t);
        /* we "borrow" the apps field of the caddy as we and the
         * server function both agree on what will be there */
        opal_list_append(&opalcaddy->apps, &oinfo->super);
        if (OPAL_SUCCESS != (rc = pmix2x_value_unload(oinfo, &directives[n].value))) {
            OBJ_RELEASE(opalcaddy);
            ret = pmix2x_convert_opalrc(rc);
            if (NULL != cbfunc) {
                cbfunc(ret, cbdata);
            }
            return;
        }
    }

    /* pass the call upwards */
    host_module->log(&requestor,
                     &opalcaddy->info,
                     &opalcaddy->apps,
                     opal_opcbfunc, opalcaddy);
}

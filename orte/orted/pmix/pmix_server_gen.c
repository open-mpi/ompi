/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
 #include "orte/mca/plm/base/plm_private.h"

#include "pmix_server_internal.h"

static void _client_conn(int sd, short args, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_proc_t *p, *ptr;
    int i;

    if (NULL != cd->server_object) {
        /* we were passed back the orte_proc_t */
        p = (orte_proc_t*)cd->server_object;
    } else {
        /* find the named process */
        p = NULL;
        if (NULL == (jdata = orte_get_job_data_object(cd->proc->jobid))) {
            return;
        }
        for (i=0; i < jdata->procs->size; i++) {
            if (NULL == (ptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if (cd->proc->jobid != ptr->name.jobid) {
                continue;
            }
            if (cd->proc->vpid == ptr->name.vpid) {
                p = ptr;
                break;
            }
        }
    }
    if (NULL != p) {
        ORTE_FLAG_SET(p, ORTE_PROC_FLAG_REG);
        ORTE_ACTIVATE_PROC_STATE(&p->name, ORTE_PROC_STATE_REGISTERED);
    }
    if (NULL != cd->cbfunc) {
        cd->cbfunc(OPAL_SUCCESS, cd->cbdata);
    }
    OBJ_RELEASE(cd);
}

int pmix_server_client_connected_fn(opal_process_name_t *proc, void *server_object,
                                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    /* need to thread-shift this request as we are going
     * to access our global list of registered events */
    ORTE_PMIX_THREADSHIFT(proc, server_object, OPAL_SUCCESS, NULL,
                          NULL, _client_conn, cbfunc, cbdata);
    return ORTE_SUCCESS;
}

static void _client_finalized(int sd, short args, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_proc_t *p, *ptr;
    int i;

    if (NULL != cd->server_object) {
        /* we were passed back the orte_proc_t */
        p = (orte_proc_t*)cd->server_object;
    } else {
        /* find the named process */
        p = NULL;
        if (NULL == (jdata = orte_get_job_data_object(cd->proc->jobid))) {
            return;
        }
        for (i=0; i < jdata->procs->size; i++) {
            if (NULL == (ptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if (cd->proc->jobid != ptr->name.jobid) {
                continue;
            }
            if (cd->proc->vpid == ptr->name.vpid) {
                p = ptr;
                break;
            }
        }
    }
    if (NULL != p) {
        ORTE_FLAG_SET(p, ORTE_PROC_FLAG_HAS_DEREG);
        /* release the caller */
        if (NULL != cd->cbfunc) {
            cd->cbfunc(ORTE_SUCCESS, cd->cbdata);
        }
    }
    OBJ_RELEASE(cd);
}

int pmix_server_client_finalized_fn(opal_process_name_t *proc, void *server_object,
                                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    /* need to thread-shift this request as we are going
     * to access our global list of registered events */
    ORTE_PMIX_THREADSHIFT(proc, server_object, OPAL_SUCCESS, NULL,
                          NULL, _client_finalized, cbfunc, cbdata);
    return ORTE_SUCCESS;

}

static void _client_abort(int sd, short args, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_proc_t *p, *ptr;
    int i;

    if (NULL != cd->server_object) {
        p = (orte_proc_t*)cd->server_object;
    } else {
        /* find the named process */
        p = NULL;
        if (NULL == (jdata = orte_get_job_data_object(cd->proc->jobid))) {
            return;
        }
        for (i=0; i < jdata->procs->size; i++) {
            if (NULL == (ptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if (cd->proc->jobid != ptr->name.jobid) {
                continue;
            }
            if (cd->proc->vpid == ptr->name.vpid) {
                p = ptr;
                break;
            }
        }
    }
    if (NULL != p) {
        p->exit_code = cd->status;
        ORTE_ACTIVATE_PROC_STATE(&p->name, ORTE_PROC_STATE_CALLED_ABORT);
    }

    ORTE_UPDATE_EXIT_STATUS(cd->status);

    /* release the caller */
    if (NULL != cd->cbfunc) {
        cd->cbfunc(OPAL_SUCCESS, cd->cbdata);
    }
    OBJ_RELEASE(cd);
}

int pmix_server_abort_fn(opal_process_name_t *proc, void *server_object,
                         int status, const char msg[],
                         opal_list_t *procs_to_abort,
                         opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    /* need to thread-shift this request as we are going
     * to access our global list of registered events */
    ORTE_PMIX_THREADSHIFT(proc, server_object, status, msg,
                          procs_to_abort, _client_abort, cbfunc, cbdata);
    return ORTE_SUCCESS;
}

static void _register_events(int sd, short args, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;
    opal_value_t *info;

    /* the OPAL layer "owns" the list, but let's deconstruct it
     * here so we don't have to duplicate the data */
    while (NULL != (info = (opal_value_t*)opal_list_remove_first(cd->info))) {
        /* don't worry about duplication as the underlying host
         * server is already protecting us from it */
        opal_list_append(&orte_pmix_server_globals.notifications, &info->super);
    }

    if (NULL != cd->cbfunc) {
        cd->cbfunc(ORTE_SUCCESS, cd->cbdata);
    }
    OBJ_RELEASE(cd);
}

/* hook for the local PMIX server to pass event registrations
 * up to us - we will assume the responsibility for providing
 * notifications for registered events */
int pmix_server_register_events_fn(opal_list_t *info,
                                   opal_pmix_op_cbfunc_t cbfunc,
                                   void *cbdata)
{
    /* need to thread-shift this request as we are going
     * to access our global list of registered events */
    ORTE_PMIX_OPERATION(NULL, info, _register_events, cbfunc, cbdata);
    return ORTE_SUCCESS;
}

static void _deregister_events(int sd, short args, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;
    opal_value_t *info, *iptr, *nptr;

    /* the OPAL layer "owns" the list, but let's deconstruct it
     * here for consistency */
    while (NULL != (info = (opal_value_t*)opal_list_remove_first(cd->info))) {
        /* search for matching requests */
        OPAL_LIST_FOREACH_SAFE(iptr, nptr, &orte_pmix_server_globals.notifications, opal_value_t) {
            if (OPAL_EQUAL == opal_dss.compare(iptr, info, OPAL_VALUE)) {
                opal_list_remove_item(&orte_pmix_server_globals.notifications, &iptr->super);
                OBJ_RELEASE(iptr);
                break;
            }
        }
        OBJ_RELEASE(info);
    }

    if (NULL != cd->cbfunc) {
        cd->cbfunc(ORTE_SUCCESS, cd->cbdata);
    }
    OBJ_RELEASE(cd);
}
/* hook for the local PMIX server to pass event deregistrations
 * up to us */
int pmix_server_deregister_events_fn(opal_list_t *info,
                                     opal_pmix_op_cbfunc_t cbfunc,
                                     void *cbdata)
{
    /* need to thread-shift this request as we are going
     * to access our global list of registered events */
    ORTE_PMIX_OPERATION(NULL, info, _deregister_events, cbfunc, cbdata);
    return ORTE_SUCCESS;
}

static void _notify_release(int status, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;

    if (NULL != cd->info) {
        OPAL_LIST_RELEASE(cd->info);
    }
    OBJ_RELEASE(cd);
}

/* someone has sent us an event that we need to distribute
 * to our local clients */
void pmix_server_notify(int status, orte_process_name_t* sender,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tg, void *cbdata)
{
    int cnt, rc, ret, ninfo, n;
    opal_value_t *val;
    orte_pmix_server_op_caddy_t *cd;
    orte_process_name_t source;

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s Notification received",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* unpack the status */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the source */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &source, &cnt, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the infos that were provided */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ninfo, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* if any were provided, add them to the list */
    cd = OBJ_NEW(orte_pmix_server_op_caddy_t);

    if (0 < ninfo) {
        cd->info = OBJ_NEW(opal_list_t);
        for (n=0; n < ninfo; n++) {
            val = OBJ_NEW(opal_value_t);
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &val, &cnt, OPAL_VALUE))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(val);
                OPAL_LIST_RELEASE(cd->info);
                OBJ_RELEASE(cd);
                return;
            }
            opal_list_append(cd->info, &val->super);
        }
    }

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s NOTIFYING PMIX SERVER OF STATUS %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ret);
    if (OPAL_SUCCESS != (rc = opal_pmix.server_notify_event(ret, &source, cd->info, _notify_release, cd))) {
        ORTE_ERROR_LOG(rc);
        if (NULL != cd->info) {
            OPAL_LIST_RELEASE(cd->info);
        }
        OBJ_RELEASE(cd);
    }
}

static void _query(int sd, short args, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;
    opal_value_t *kv;
    orte_job_t *jdata;
    int rc;
    size_t nresults=0;
    uint32_t key;
    void *nptr;
    char **nspaces=NULL, nspace[512];

    /* see what they wanted */
    OPAL_LIST_FOREACH(kv, cd->info, opal_value_t) {
        if (0 == strcmp(kv->key, OPAL_PMIX_QUERY_NAMESPACES)) {
            /* get the current jobids */
            rc = opal_hash_table_get_first_key_uint32(orte_job_data, &key, (void **)&jdata, &nptr);
            while (OPAL_SUCCESS == rc) {
                if (ORTE_PROC_MY_NAME->jobid != jdata->jobid) {
                    memset(nspace, 0, 512);
                    (void)opal_snprintf_jobid(nspace, 512, jdata->jobid);
                    opal_argv_append_nosize(&nspaces, nspace);
                }
                rc = opal_hash_table_get_next_key_uint32(orte_job_data, &key, (void **)&jdata, nptr, &nptr);
            }
            /* join the results into a single comma-delimited string */
            kv->type = OPAL_STRING;
            if (NULL != nspaces) {
                kv->data.string = opal_argv_join(nspaces, ',');
            } else {
                kv->data.string = NULL;
            }
            ++nresults;
        }
    }
    if (0 == nresults) {
        rc = ORTE_ERR_NOT_FOUND;
    } else if (nresults < opal_list_get_size(cd->info)) {
        rc = ORTE_ERR_PARTIAL_SUCCESS;
    } else {
        rc = ORTE_SUCCESS;
    }
    cd->infocbfunc(rc, cd->info, cd->cbdata, NULL, NULL);
}

int pmix_server_query_fn(opal_process_name_t *requestor,
                         opal_list_t *info, opal_list_t *directives,
                         opal_pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd;

    if (NULL == info || NULL == cbfunc) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* need to threadshift this request */
    cd = OBJ_NEW(orte_pmix_server_op_caddy_t);
    cd->proc = requestor;
    cd->info = info;
    cd->infocbfunc = cbfunc;
    cd->cbdata = cbdata;

    opal_event_set(orte_event_base, &(cd->ev), -1,
                   OPAL_EV_WRITE, _query, cd);
    opal_event_set_priority(&(cd->ev), ORTE_MSG_PRI);
    opal_event_active(&(cd->ev), OPAL_EV_WRITE, 1);

    return ORTE_SUCCESS;
}

static void _toolconn(int sd, short args, void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd = (orte_pmix_server_op_caddy_t*)cbdata;
    orte_job_t jdata;
    orte_process_name_t tool;
    int rc;

   /* if we are the HNP, we can directly assign the jobid */
    if (ORTE_PROC_IS_HNP) {
        OBJ_CONSTRUCT(&jdata, orte_job_t);
        rc = orte_plm_base_create_jobid(&jdata);
        tool.jobid = jdata.jobid;
        tool.vpid = 0;
        if (NULL != cd->toolcbfunc) {
            cd->toolcbfunc(rc, tool, cd->cbdata);
        }
        OBJ_RELEASE(cd);
        return;
    }

    /* otherwise, we have to send the request to the HNP.
     * Eventually, when we switch to nspace instead of an
     * integer jobid, we'll just locally assign this value */
    if (NULL != cd->toolcbfunc) {
        cd->toolcbfunc(ORTE_ERR_NOT_SUPPORTED, tool, cd->cbdata);
    }
    OBJ_RELEASE(cd);
}
void pmix_tool_connected_fn(opal_list_t *info,
                            opal_pmix_tool_connection_cbfunc_t cbfunc,
                            void *cbdata)
{
    orte_pmix_server_op_caddy_t *cd;

    opal_output(0, "TOOL CONNECTION REQUEST RECVD");

    /* need to threadshift this request */
    cd = OBJ_NEW(orte_pmix_server_op_caddy_t);
    cd->info = info;
    cd->toolcbfunc = cbfunc;
    cd->cbdata = cbdata;

    opal_event_set(orte_event_base, &(cd->ev), -1,
                   OPAL_EV_WRITE, _toolconn, cd);
    opal_event_set_priority(&(cd->ev), ORTE_MSG_PRI);
    opal_event_active(&(cd->ev), OPAL_EV_WRITE, 1);

}

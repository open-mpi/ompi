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

#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"

#include "pmix_server_internal.h"

int pmix_server_client_connected_fn(opal_process_name_t *proc, void *server_object)
{
    ORTE_ACTIVATE_PROC_STATE(proc, ORTE_PROC_STATE_REGISTERED);
    return ORTE_SUCCESS;
}

int pmix_server_client_finalized_fn(opal_process_name_t *proc, void *server_object,
                                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    orte_job_t *jdata;
    orte_proc_t *p, *ptr;
    int i;

    if (NULL != cbdata) {
        /* we were passed back the orte_proc_t */
        p = (orte_proc_t*)cbdata;
    } else {
        /* find the named process */
        p = NULL;
        if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
            return ORTE_ERR_NOT_FOUND;
        }
        for (i=0; i < jdata->procs->size; i++) {
            if (NULL == (ptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            if (proc->jobid != ptr->name.jobid) {
                continue;
            }
            if (proc->vpid == ptr->name.vpid) {
                p = ptr;
                break;
            }
        }
    }
    if (NULL != p) {
        p->state = ORTE_PROC_STATE_TERMINATED;
            /* release the caller */
        if (NULL != cbfunc) {
            cbfunc(ORTE_SUCCESS, cbdata);
        }
        return ORTE_SUCCESS;
    }
    return ORTE_ERR_NOT_FOUND;
}

int pmix_server_abort_fn(opal_process_name_t *proc, void *server_object,
                         int status, const char msg[],
                         opal_list_t *procs_to_abort,
                         opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    orte_proc_t *p;

    if (NULL != server_object) {
        p = (orte_proc_t*)server_object;
        p->exit_code = status;
    }

    ORTE_UPDATE_EXIT_STATUS(status);
    ORTE_ACTIVATE_PROC_STATE(proc, ORTE_PROC_STATE_CALLED_ABORT);

    /* release the caller */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }
    return OPAL_SUCCESS;
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

    if (NULL != cd->procs) {
        OPAL_LIST_RELEASE(cd->procs);
    }
    if (NULL != cd->eprocs) {
        OPAL_LIST_RELEASE(cd->eprocs);
    }
    if (NULL != cd->info) {
        OPAL_LIST_RELEASE(cd->info);
    }
    OBJ_RELEASE(cd);
}
void pmix_server_notify(int status, orte_process_name_t* sender,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tg, void *cbdata)
{
    opal_list_t *procs = NULL, *eprocs = NULL, *info = NULL;
    int cnt, rc, ret, nprocs, n;
    opal_namelist_t *nm;
    opal_value_t *val;
    orte_pmix_server_op_caddy_t *cd;

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s Notification received",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* unpack the status */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the target procs that are to be notified */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nprocs, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* if any were provided, add them to the list */
    if (0 < nprocs) {
        procs = OBJ_NEW(opal_list_t);
        for (n=0; n < nprocs; n++) {
            nm = OBJ_NEW(opal_namelist_t);
            opal_list_append(procs, &nm->super);
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nm->name, &cnt, OPAL_NAME))) {
                ORTE_ERROR_LOG(rc);
                OPAL_LIST_RELEASE(procs);
                return;
            }
        }
    }

    /* unpack the procs that were impacted by the error */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nprocs, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        if (NULL != procs) {
            OPAL_LIST_RELEASE(procs);
        }
        return;
    }

    /* if any were provided, add them to the list */
    if (0 < nprocs) {
        eprocs = OBJ_NEW(opal_list_t);
        for (n=0; n < nprocs; n++) {
            nm = OBJ_NEW(opal_namelist_t);
            opal_list_append(eprocs, &nm->super);
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nm->name, &cnt, OPAL_NAME))) {
                ORTE_ERROR_LOG(rc);
                if (NULL != procs) {
                    OPAL_LIST_RELEASE(procs);
                }
                OPAL_LIST_RELEASE(eprocs);
                return;
            }
        }
    }

    /* unpack the infos that were provided */
    cnt = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nprocs, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        if (NULL != procs) {
            OPAL_LIST_RELEASE(procs);
        }
        return;
    }

    /* if any were provided, add them to the list */
    if (0 < nprocs) {
        info = OBJ_NEW(opal_list_t);
        for (n=0; n < nprocs; n++) {
            val = OBJ_NEW(opal_value_t);
            opal_list_append(info, &val->super);
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &val, &cnt, OPAL_VALUE))) {
                ORTE_ERROR_LOG(rc);
                if (NULL != procs) {
                    OPAL_LIST_RELEASE(procs);
                }
                if (NULL != eprocs) {
                    OPAL_LIST_RELEASE(eprocs);
                }
                OPAL_LIST_RELEASE(info);
                return;
            }
        }
    }

    cd = OBJ_NEW(orte_pmix_server_op_caddy_t);
    cd->procs = procs;
    cd->eprocs = eprocs;
    cd->info = info;

    if (OPAL_SUCCESS != (rc = opal_pmix.server_notify_error(ret, procs, eprocs, info, _notify_release, cd))) {
        ORTE_ERROR_LOG(rc);
        if (NULL != procs) {
            OPAL_LIST_RELEASE(procs);
        }
        if (NULL != eprocs) {
            OPAL_LIST_RELEASE(eprocs);
        }
        if (NULL != info) {
            OPAL_LIST_RELEASE(info);
        }
        OBJ_RELEASE(cd);
    }
}

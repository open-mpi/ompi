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
 * Copyright (c) 2014-2017 Mellanox Technologies, Inc.
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
        p = (orte_proc_t*)server_object;
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
        ORTE_FLAG_SET(p, ORTE_PROC_FLAG_HAS_DEREG);
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

    ORTE_ACTIVATE_PROC_STATE(proc, ORTE_PROC_STATE_CALLED_ABORT);

    /* release the caller */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }
    return OPAL_SUCCESS;
}

int pmix_server_register_events_fn(opal_list_t *info,
                                   opal_pmix_op_cbfunc_t cbfunc,
                                   void *cbdata)
{
    /* for now, just execute the cbfunc */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }
    return OPAL_SUCCESS;
}

int pmix_server_deregister_events_fn(opal_list_t *info,
                                     opal_pmix_op_cbfunc_t cbfunc,
                                     void *cbdata)
{
    /* for now, just execute the cbfunc */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }
    return OPAL_SUCCESS;
}

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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif /* HAVE_DIRENT_H */
#include <time.h>

#include <stdlib.h>
#include <stdarg.h>

#include "orte/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/argv.h"

#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/proc_info.h"

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_locks.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

/*
 * Object stuff
 */
void orte_errmgr_predicted_proc_construct(orte_errmgr_predicted_proc_t *item);
void orte_errmgr_predicted_proc_destruct( orte_errmgr_predicted_proc_t *item);

OBJ_CLASS_INSTANCE(orte_errmgr_predicted_proc_t,
                   opal_list_item_t,
                   orte_errmgr_predicted_proc_construct,
                   orte_errmgr_predicted_proc_destruct);

void orte_errmgr_predicted_proc_construct(orte_errmgr_predicted_proc_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;
}

void orte_errmgr_predicted_proc_destruct( orte_errmgr_predicted_proc_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;
}

void orte_errmgr_predicted_node_construct(orte_errmgr_predicted_node_t *item);
void orte_errmgr_predicted_node_destruct( orte_errmgr_predicted_node_t *item);

OBJ_CLASS_INSTANCE(orte_errmgr_predicted_node_t,
                   opal_list_item_t,
                   orte_errmgr_predicted_node_construct,
                   orte_errmgr_predicted_node_destruct);

void orte_errmgr_predicted_node_construct(orte_errmgr_predicted_node_t *item)
{
    item->node_name = NULL;
}

void orte_errmgr_predicted_node_destruct( orte_errmgr_predicted_node_t *item)
{
    if( NULL != item->node_name ) {
        free(item->node_name);
        item->node_name = NULL;
    }
}

void orte_errmgr_predicted_map_construct(orte_errmgr_predicted_map_t *item);
void orte_errmgr_predicted_map_destruct( orte_errmgr_predicted_map_t *item);

OBJ_CLASS_INSTANCE(orte_errmgr_predicted_map_t,
                   opal_list_item_t,
                   orte_errmgr_predicted_map_construct,
                   orte_errmgr_predicted_map_destruct);

void orte_errmgr_predicted_map_construct(orte_errmgr_predicted_map_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;

    item->node_name = NULL;

    item->map_proc_name.vpid  = ORTE_VPID_INVALID;
    item->map_proc_name.jobid = ORTE_JOBID_INVALID;

    item->map_node_name = NULL;
    item->off_current_node = false;
    item->pre_map_fixed_node = NULL;
}

void orte_errmgr_predicted_map_destruct( orte_errmgr_predicted_map_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;

    if( NULL != item->node_name ) {
        free(item->node_name);
        item->node_name = NULL;
    }

    item->map_proc_name.vpid  = ORTE_VPID_INVALID;
    item->map_proc_name.jobid = ORTE_JOBID_INVALID;

    if( NULL != item->map_node_name ) {
        free(item->map_node_name);
        item->map_node_name = NULL;
    }

    item->off_current_node = false;

    if( NULL != item->pre_map_fixed_node ) {
        free(item->pre_map_fixed_node);
        item->pre_map_fixed_node = NULL;
    }
}

/*
 * Public interfaces
 */
void orte_errmgr_base_log(int error_code, char *filename, int line)
{
    char *errstring = NULL;

    errstring = (char*)ORTE_ERROR_NAME(error_code);

    if (NULL == errstring) {
        /* if the error is silent, say nothing */
        return;
    }

    opal_output(0, "%s ORTE_ERROR_LOG: %s in file %s at line %d",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                errstring, filename, line);
}

void orte_errmgr_base_abort(int error_code, char *fmt, ...)
{
    va_list arglist;

    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, "%s", buffer );
        free( buffer );
    }
    va_end(arglist);

    /* if I am a daemon or the HNP... */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* whack my local procs */
        orte_odls.kill_local_procs(NULL);
        /* whack any session directories */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    }

    /* if a critical connection failed, or a sensor limit was exceeded, exit without dropping a core */
    if (ORTE_ERR_CONNECTION_FAILED == error_code ||
        ORTE_ERR_SENSOR_LIMIT_EXCEEDED == error_code) {
        orte_ess.abort(error_code, false);
    } else {
        orte_ess.abort(error_code, true);
    }

    /*
     * We must exit in orte_ess.abort; all implementations of orte_ess.abort
     * contain __opal_attribute_noreturn__
     */
    /* No way to reach here */
}

void orte_errmgr_base_register_migration_warning(struct timeval *tv)
{
    /* stub function - ignore */
    return;
}

int orte_errmgr_base_abort_peers(orte_process_name_t *procs,
                                 orte_std_cntr_t num_procs,
                                 int error_code)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_errmgr_base_register_error_callback(orte_errmgr_error_callback_fn_t *cbfunc,
                                             orte_errmgr_error_order_t order)
{
    orte_errmgr_cback_t *cb, *cbcur;

    /* check the order to see what to do */
    switch(order) {
    case ORTE_ERRMGR_CALLBACK_FIRST:
        /* only one can be so designated */
        if (NULL != (cb = (orte_errmgr_cback_t*)opal_list_get_first(&orte_errmgr_base.error_cbacks))) {
            if (ORTE_ERRMGR_CALLBACK_FIRST == cb->order) {
                return ORTE_ERR_NOT_SUPPORTED;
            }
        }
        cb = OBJ_NEW(orte_errmgr_cback_t);
        cb->order = order;
        cb->callback =cbfunc;
        opal_list_prepend(&orte_errmgr_base.error_cbacks, &cb->super);
        break;
    case ORTE_ERRMGR_CALLBACK_LAST:
        /* only one can be so designated */
        if (NULL != (cb = (orte_errmgr_cback_t*)opal_list_get_last(&orte_errmgr_base.error_cbacks))) {
            if (ORTE_ERRMGR_CALLBACK_LAST == cb->order) {
                return ORTE_ERR_NOT_SUPPORTED;
            }
        }
        cb = OBJ_NEW(orte_errmgr_cback_t);
        cb->order = order;
        cb->callback = cbfunc;
        opal_list_append(&orte_errmgr_base.error_cbacks, &cb->super);
        break;
    case ORTE_ERRMGR_CALLBACK_PREPEND:
        cb = OBJ_NEW(orte_errmgr_cback_t);
        cb->order = order;
        cb->callback =cbfunc;
        if (NULL != (cbcur = (orte_errmgr_cback_t*)opal_list_get_first(&orte_errmgr_base.error_cbacks)) &&
            ORTE_ERRMGR_CALLBACK_FIRST == cbcur->order) {
            opal_list_insert(&orte_errmgr_base.error_cbacks, &cb->super, 1);
        } else {
            opal_list_prepend(&orte_errmgr_base.error_cbacks, &cb->super);
        }
        break;
    case ORTE_ERRMGR_CALLBACK_APPEND:
        cb = OBJ_NEW(orte_errmgr_cback_t);
        cb->order = order;
        cb->callback =cbfunc;
        if (NULL != (cbcur = (orte_errmgr_cback_t*)opal_list_get_last(&orte_errmgr_base.error_cbacks)) &&
            ORTE_ERRMGR_CALLBACK_LAST == cbcur->order) {
            opal_list_insert_pos(&orte_errmgr_base.error_cbacks, &cbcur->super, &cb->super);
        } else {
            opal_list_append(&orte_errmgr_base.error_cbacks, &cb->super);
        }
        opal_list_append(&orte_errmgr_base.error_cbacks, &cb->super);
        break;
    }
    return ORTE_SUCCESS;
}

void orte_errmgr_base_execute_error_callbacks(opal_pointer_array_t *errors)
{
    orte_errmgr_cback_t *cb;
    char *errstring=NULL;
    orte_error_t *err;
    int errcode = ORTE_ERROR_DEFAULT_EXIT_CODE;

    /* if no callbacks have been provided, then we abort */
    if (0 == opal_list_get_size(&orte_errmgr_base.error_cbacks)) {
        /* take the first entry, if available */
        if (NULL != errors &&
            (NULL != (err = (orte_error_t*)opal_pointer_array_get_item(errors, 0)))) {
            errstring = (char*)ORTE_ERROR_NAME(err->errcode);
            errcode = err->errcode;
        }
        if (NULL == errstring) {
            /* if the error is silent, say nothing */
            orte_errmgr.abort(errcode, NULL);
        }
        orte_errmgr.abort(errcode, "Executing default error callback: %s", errstring);
    }

    /* cycle across the provided callbacks until we complete the list
     * or one reports that no further action is required
     */
    OPAL_LIST_FOREACH(cb, &orte_errmgr_base.error_cbacks, orte_errmgr_cback_t) {
        if (ORTE_SUCCESS == cb->callback(errors)) {
            break;
        }
    }
}

/********************
 * Utility functions
 ********************/

/********************
 * Local Functions
 ********************/

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"

#include <sys/types.h>
#include <unistd.h>

#include "include/constants.h"
#include "event/event.h"
#include "util/output.h"
#include "event/event.h"
#include "threads/mutex.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/iof/base/base.h"
#include "mca/rml/base/base.h"
#include "mca/errmgr/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "mca/rmgr/base/base.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/univ_info.h"
#include "util/session_dir.h"

#include "runtime/runtime.h"
#include "runtime/runtime_internal.h"
#include "runtime/orte_wait.h"


/**
 * Cleanup and restart a selected set of services.
 */

int orte_restart(orte_process_name_t *name, const char* uri)
{
    int rc, id;
    orte_process_name_t* old_name;
    orte_process_name_t* new_name;
    char *jobid_str, *procid_str;

    if (ORTE_SUCCESS != (rc = orte_ns.copy_process_name(&old_name, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.copy_process_name(&new_name, name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * Restart event library
     */

    if (ORTE_SUCCESS != (rc = ompi_event_restart())) {
        ORTE_ERROR_LOG(rc);
	return rc;
    }

    /*
     * Close selected components.
     */

    orte_iof_base.iof_flush = false;
    if (ORTE_SUCCESS != (rc = orte_iof_base_close())) {
        ORTE_ERROR_LOG(rc);
	return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_base_close())) {
        ORTE_ERROR_LOG(rc);
	return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns_base_close())) {
        ORTE_ERROR_LOG(rc);
	return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_rml_base_close())) {
        ORTE_ERROR_LOG(rc);
	return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_wait_finalize())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * setup new global state
     */
    /* close the proc_info structure so it can be reinitialized */
    if (ORTE_SUCCESS != (rc = orte_proc_info_finalize())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* set seed flag to false */
    id = mca_base_param_register_int("seed", NULL, NULL, NULL, (int)false);
    if (ORTE_SUCCESS != (rc = mca_base_param_set_int(id, (int)false))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* if NULL, set ns_replica to old_name and set the corresponding uri parameter */
    if (NULL == orte_process_info.ns_replica) {
        orte_process_info.ns_replica = old_name;
        if (NULL != orte_process_info.ns_replica_uri) {
            free(orte_process_info.ns_replica_uri);
            orte_process_info.ns_replica_uri = NULL;
        }
        id = mca_base_param_register_string("ns", "replica", "uri", NULL, NULL);
        if (ORTE_SUCCESS != (rc = mca_base_param_set_string(id, (char*)uri))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* if NULL, set gpr_replica to old_name and set the corresponding uri parameter */
    if (NULL == orte_process_info.gpr_replica) {
        orte_process_info.gpr_replica = old_name;
        if (NULL != orte_process_info.gpr_replica_uri) {
            free(orte_process_info.gpr_replica_uri);
            orte_process_info.gpr_replica_uri = NULL;
        }
        id = mca_base_param_register_string("gpr", "replica", "uri", NULL, NULL);
        if (ORTE_SUCCESS != (rc = mca_base_param_set_string(id, (char*)uri))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* call proc_info to reset the structure */
    if (ORTE_SUCCESS != (rc = orte_proc_info())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* ensure my_name is set to the new_name */
    if (NULL != orte_process_info.my_name) {
        free(orte_process_info.my_name);
    }
    orte_process_info.my_name = new_name;

    /* finalize the sys_info structure so it can be reinitialized */
    if (ORTE_SUCCESS != (rc = orte_sys_info_finalize())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* call the sys_info function to load structure with any new info */
    orte_system_info.init = false;
    if (ORTE_SUCCESS != (rc = orte_sys_info())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
   
    /* establish the session directory structure for this process */
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid_string(&jobid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_string(&procid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
 
    if (orte_debug_flag) {
        ompi_output(0, "[%d,%d,%d] setting up session dir with",
                    ORTE_NAME_ARGS(orte_process_info.my_name));
        if (NULL != orte_process_info.tmpdir_base) {
            ompi_output(0, "\ttmpdir %s", orte_process_info.tmpdir_base);
        }
        ompi_output(0, "\tuniverse %s", orte_universe_info.name);
        ompi_output(0, "\tuser %s", orte_system_info.user);
        ompi_output(0, "\thost %s", orte_system_info.nodename);
        ompi_output(0, "\tjobid %s", jobid_str);
        ompi_output(0, "\tprocid %s", procid_str);
    }
    if (ORTE_SUCCESS != (rc = orte_session_dir(true,
                                orte_process_info.tmpdir_base,
                                orte_system_info.user,
                                orte_system_info.nodename, NULL,
                                orte_universe_info.name,
                                jobid_str, procid_str))) {
        ORTE_ERROR_LOG(rc);
        if (jobid_str != NULL) free(jobid_str);
        if (procid_str != NULL) free(procid_str);
        return rc;
    }
    if (NULL != jobid_str) {
        free(jobid_str);
    }
    if (NULL != procid_str) {
        free(procid_str);
    }

    /*
     * Re-open components.
     */

    if (ORTE_SUCCESS != (rc = orte_wait_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_rml_base_open())) {
        ORTE_ERROR_LOG(rc);
            return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * Select new modules.
     */

    if (ORTE_SUCCESS != (rc = orte_rml_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * Set contact info for our parent
     */

    if (ORTE_SUCCESS != (rc = orte_rml.set_uri(uri))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * Re-init selected modules.
     */
    if (ORTE_SUCCESS != (rc = orte_rml.init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /*
     * Complete restart
     */
    if (ORTE_SUCCESS != (rc = orte_iof_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_iof_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


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

#include "runtime/runtime.h"
#include "runtime/runtime_internal.h"
#include "runtime/orte_wait.h"


/**
 * Cleanup and restart a selected set of services.
 */

int orte_restart(orte_process_name_t *name, const char* uri)
{
    int rc;

    orte_process_name_t* old_name;
    orte_process_name_t* new_name;
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
    orte_process_info.seed = false;
    if(NULL == orte_process_info.ns_replica)
        orte_process_info.ns_replica = old_name;
    if(NULL == orte_process_info.gpr_replica)
        orte_process_info.gpr_replica = old_name;
    orte_process_info.my_name = new_name;

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
     * Set contact info.
     */

    if (ORTE_SUCCESS != (rc = orte_rml.set_uri(uri))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (NULL != orte_process_info.ns_replica_uri) {
        if (ORTE_SUCCESS != (rc = orte_rml.set_uri(orte_process_info.ns_replica_uri))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    if (NULL != orte_process_info.gpr_replica_uri) {
        if (ORTE_SUCCESS != (rc = orte_rml.set_uri(orte_process_info.gpr_replica_uri))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
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


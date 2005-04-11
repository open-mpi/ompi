/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"

#include <sys/types.h>
#ifndef WIN32
#include <unistd.h>
#endif

#include "include/constants.h"
#include "event/event.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "dps/dps.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/rml/base/base.h"
#include "mca/errmgr/base/base.h"
#include "mca/iof/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/base.h"
#include "mca/soh/base/base.h"
#include "util/malloc.h"
#include "util/univ_info.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/sys_info.h"
#include "util/cmd_line.h"

#include "runtime/runtime.h"
#include "runtime/runtime_internal.h"
#include "runtime/orte_wait.h"

int orte_init_stage1(void)
{
    int ret;
    char *universe;
    char *jobid_str = NULL;
    char *procid_str = NULL;
    pid_t pid;

    /* Open up the output streams */
    if (!ompi_output_init()) {
        return OMPI_ERROR;
    }
                                                                                                                   
    /* 
     * If threads are supported - assume that we are using threads - and reset otherwise. 
     */
    ompi_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);
                                                                                                                   
    /* For malloc debugging */
    ompi_malloc_init();

    /* Ensure the universe_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_univ_info())) {
        return ret;
    }

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        return ret;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        return ret;
    }
    
    /*
     * Initialize the MCA framework 
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        return ret;
    }
 
    /*
     * Open the name services to ensure access to local functions 
     */
    if (OMPI_SUCCESS != (ret = orte_ns_base_open())) {
        return ret;
    }

    /* Open the error manager to activate error logging - needs local name services */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        return ret;
    }
    
    /*****   ERROR LOGGING NOW AVAILABLE *****/
    
    /* check for debug flag */
    if (0 > (ret =  mca_base_param_register_int("orte", "debug", NULL, NULL, 0))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = mca_base_param_lookup_int(ret, &orte_debug_flag))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initialize the event library 
    */
    if (OMPI_SUCCESS != (ret = ompi_event_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Intialize the general progress engine
     */
    if (OMPI_SUCCESS != (ret = ompi_progress_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Internal startup
     */
    if (OMPI_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initialize the data packing service.
     */
    if (ORTE_SUCCESS != (ret = orte_dps_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Registry
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initialize schema utilities
     */

    if (ORTE_SUCCESS != (ret = orte_schema_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* check for existing universe to join */
    if (ORTE_SUCCESS != (ret = orte_universe_exists())) {
        if (orte_debug_flag) {
                ompi_output(0, "orte_init: could not join existing universe");
        }
        if (ORTE_ERR_NOT_FOUND != ret) {
                /* if it exists but no contact could be established,
                 * define unique name based on current one.
                 * and start new universe with me as seed
                 */
                universe = strdup(orte_universe_info.name);
                free(orte_universe_info.name);
                orte_universe_info.name = NULL;
                pid = getpid();
                if (0 > asprintf(&orte_universe_info.name, "%s-%d", universe, pid)) {
                    ompi_output(0, "orte_init: failed to create unique universe name");
                    return ret;
                }
        }

        orte_process_info.seed = true;
        if (NULL != orte_process_info.ns_replica) {
                free(orte_process_info.ns_replica);
                orte_process_info.ns_replica = NULL;
        }
        if (NULL != orte_process_info.gpr_replica) {
                free(orte_process_info.gpr_replica);
                orte_process_info.gpr_replica = NULL;
        }
    }

    /*
     * Name Server 
     */
    if (OMPI_SUCCESS != (ret = orte_ns_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Registry 
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
 
    /*****    SET MY NAME    *****/
    if (ORTE_SUCCESS != (ret = orte_ns.set_my_name())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* setup my session directory */
    if (ORTE_SUCCESS != (ret = orte_ns.get_jobid_string(&jobid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = orte_ns.get_vpid_string(&procid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        return ret;
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
    if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                orte_process_info.tmpdir_base,
                                orte_system_info.user,
                                orte_system_info.nodename, NULL,
                                orte_universe_info.name,
                                jobid_str, procid_str))) {
        if (jobid_str != NULL) free(jobid_str);
        if (procid_str != NULL) free(procid_str);
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    if (NULL != jobid_str) {
        free(jobid_str);
    }
    if (NULL != procid_str) {
        free(procid_str);
    }

    /* set contact info for ns/gpr */
    if(NULL != orte_process_info.ns_replica_uri) {
        orte_rml.set_uri(orte_process_info.ns_replica_uri);
    }
    if(NULL != orte_process_info.gpr_replica_uri) {
        orte_rml.set_uri(orte_process_info.gpr_replica_uri);
    }

    /* open/load rmgr/soh */

    if (ORTE_SUCCESS != (ret = orte_rmgr_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if (ORTE_SUCCESS != (ret = orte_soh_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

     /* setup jobid-0 */
 
    if(orte_process_info.seed) {
         if (ORTE_SUCCESS != (ret = orte_rmgr_base_set_job_slots(0,1))) {
             ORTE_ERROR_LOG(ret);
             return ret;
         }
         if (ORTE_SUCCESS != (ret = orte_rmaps_base_set_vpid_range(0,0,1))) {
             ORTE_ERROR_LOG(ret);
             return ret;
         }
         if (ORTE_SUCCESS != (ret = orte_rmgr_base_proc_stage_gate_init(0))) {
             ORTE_ERROR_LOG(ret);
             return ret;
         }
    }

    return ORTE_SUCCESS;
}

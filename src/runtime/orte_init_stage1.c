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
#ifdef HAVE_UNISTD_H
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
#include "mca/schema/base/base.h"
#include "mca/soh/base/base.h"
#include "util/malloc.h"
#include "util/univ_info.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/sys_info.h"
#include "util/cmd_line.h"
#include "util/universe_setup_file_io.h"
#include "util/os_path.h"

#include "runtime/runtime.h"
#include "runtime/runtime_internal.h"
#include "runtime/orte_wait.h"

int orte_init_stage1(void)
{
    int ret;
    char *universe;
    char *jobid_str = NULL;
    char *procid_str = NULL;
    char *contact_path = NULL;
    pid_t pid;
    orte_universe_t univ;

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

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        return ret;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        return ret;
    }
    
    /* Ensure the universe_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_univ_info())) {
        return ret;
    }

    /*
     * Initialize the MCA framework 
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
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

    if (ORTE_SUCCESS != (ret = orte_schema_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* if we were NOT given registry and name service replicas (i.e., we
     * weren't told a universe contact point), check for some
     * existing universe to join */
    if (NULL == orte_process_info.ns_replica_uri || NULL == orte_process_info.gpr_replica_uri) {
        if (ORTE_SUCCESS == (ret = orte_universe_exists(&univ))) {
            /* copy universe info into our universe structure */
            orte_universe_info.name = univ.name;
            orte_universe_info.host = univ.host;
            orte_universe_info.uid = univ.uid;
            orte_universe_info.persistence = univ.persistence;
            orte_universe_info.scope = univ.scope;
            orte_universe_info.console = univ.console;
            orte_universe_info.seed_uri = univ.seed_uri;
            orte_universe_info.console_connected = univ.console_connected;
            orte_universe_info.scriptfile = univ.scriptfile;
            /* define the replica contact points */
            orte_process_info.ns_replica_uri = strdup(univ.seed_uri);
            orte_process_info.gpr_replica_uri = strdup(univ.seed_uri);
        } else {
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
                ompi_output(0, "Could not join a running, existing universe");
                ompi_output(0, "Establishing a new one named: %s",
                                orte_universe_info.name);
    
            }
            orte_process_info.seed = true;
            /* since we are seed, ensure that all replica info is NULL'd */
            if (NULL != orte_process_info.ns_replica_uri) {
                free(orte_process_info.ns_replica_uri);
                orte_process_info.ns_replica_uri = NULL;
            }
            if (NULL != orte_process_info.ns_replica) {
                    free(orte_process_info.ns_replica);
                    orte_process_info.ns_replica = NULL;
            }
    
            if (NULL != orte_process_info.gpr_replica_uri) {
                free(orte_process_info.gpr_replica_uri);
                orte_process_info.gpr_replica_uri = NULL;
            }
            if (NULL != orte_process_info.gpr_replica) {
                    free(orte_process_info.gpr_replica);
                    orte_process_info.gpr_replica = NULL;
            }
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
 
    /* set contact info for ns/gpr */
    if(NULL != orte_process_info.ns_replica_uri) {
        orte_rml.set_uri(orte_process_info.ns_replica_uri);
    }
    if(NULL != orte_process_info.gpr_replica_uri) {
        orte_rml.set_uri(orte_process_info.gpr_replica_uri);
    }

    /*****    SET MY NAME    *****/
    if (ORTE_SUCCESS != (ret = orte_ns.set_my_name())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /* if I'm the seed, set the seed uri to be me! */
    if (orte_process_info.seed) {
        orte_universe_info.seed_uri = orte_rml.get_uri();
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
        ompi_output(0, "[%lu,%lu,%lu] setting up session dir with",
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

    /* if i'm the seed, get my contact info and write my setup file for others to find */
    if (orte_process_info.seed) {
        if (NULL != orte_universe_info.seed_uri) {
            free(orte_universe_info.seed_uri);
            orte_universe_info.seed_uri = NULL;
        }
        if (NULL == (orte_universe_info.seed_uri = orte_rml.get_uri())) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        contact_path = orte_os_path(false, orte_process_info.universe_session_dir,
                    "universe-setup.txt", NULL);
        if (orte_debug_flag) {
            ompi_output(0, "[%lu,%lu,%lu] contact_file %s",
                        ORTE_NAME_ARGS(orte_process_info.my_name), contact_path);
        }

        if (OMPI_SUCCESS != (ret = orte_write_universe_setup_file(contact_path, &orte_universe_info))) {
            if (orte_debug_flag) {
                ompi_output(0, "[%lu,%lu,%lu] couldn't write setup file", ORTE_NAME_ARGS(orte_process_info.my_name));
            }
        } else if (orte_debug_flag) {
            ompi_output(0, "[%lu,%lu,%lu] wrote setup file", ORTE_NAME_ARGS(orte_process_info.my_name));
        }
        free(contact_path);
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

/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
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

#include "orte/orte_constants.h"

#include "opal/event/event.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/threads/mutex.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/os_path.h"
#include "opal/util/cmd_line.h"
#include "opal/util/malloc.h"

#include "orte/dss/dss.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rds/base/base.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/odls/base/base.h"

#include "orte/mca/rmaps/base/base.h"
#if OPAL_ENABLE_FT == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/base/base.h"
#include "orte/mca/schema/base/base.h"
#include "orte/mca/smr/base/base.h"
#include "orte/util/univ_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"
#include "orte/util/universe_setup_file_io.h"

/* these are to be cleaned up for 2.0 */
#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/rmgr/base/rmgr_private.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/runtime_internal.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/params.h"

#include "orte/runtime/orte_cr.h"

int orte_init_stage1(bool infrastructure)
{
    int ret;
    char *error = NULL;
    char *jobid_str = NULL;
    char *procid_str = NULL;
    char *contact_path = NULL;

    if (orte_initialized) {
        return ORTE_SUCCESS;
    }

    /* register handler for errnum -> string conversion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* Register all MCA Params */
    if (ORTE_SUCCESS != (ret = orte_register_params(infrastructure))) {
        error = "orte_register_params";
        goto error;
    }

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        error = "orte_sys_info";
        goto error;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        error = "orte_proc_info";
        goto error;
    }

    /* Ensure the universe_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_univ_info())) {
        error = "orte_univ_info";
        goto error;
    }

    /*
     * Initialize the data storage service.
     */
    if (ORTE_SUCCESS != (ret = orte_dss_open())) {
        error = "orte_dss_open";
        goto error;
    }

    /*
     * Open the name services to ensure access to local functions
     */
    if (ORTE_SUCCESS != (ret = orte_ns_base_open())) {
        error = "orte_ns_base_open";
        goto error;
    }

    /* Open the error manager to activate error logging - needs local name services */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        error = "orte_errmgr_base_open";
        goto error;
    }

    /*****   ERROR LOGGING NOW AVAILABLE *****/

    /*
     * Internal startup
     */
    if (ORTE_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_wait_init";
        goto error;
    }

    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_open";
        goto error;
    }

    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml_base_select";
        goto error;
    }

    /*
     * Registry
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_gpr_base_open";
        goto error;
    }

    /*
     * Initialize the daemon launch system so those types
     * are registered (needed by the sds to talk to its
     * local daemon)
     */
    if (ORTE_SUCCESS != (ret = orte_odls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_open";
        goto error;
    }
    
    /*
     * Initialize schema utilities
     */
    if (ORTE_SUCCESS != (ret = orte_schema_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_schema_base_open";
        goto error;
    }

    /*
     * Initialize and select the Startup Discovery Service. This must
     * be done here since some environments have different requirements
     * for detecting/connecting to a universe. Note that this does
     * *not* set our name - that will come later
     */
    if (ORTE_SUCCESS != (ret = orte_sds_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_sds_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_select";
        goto error;
    }
    
    /* Try to connect to the universe. If we don't find one and are a
     * singleton, this will startup a new HNP and define our name
     * within it - in which case, we will skip the name discovery
     * process since we already have one
     */
    if (ORTE_SUCCESS != (ret = orte_sds_base_contact_universe())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_contact_universe";
        goto error;
    }

    /*
     * Name Server
     */
    if (ORTE_SUCCESS != (ret = orte_ns_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns_base_select";
        goto error;
    }

    /*
     * Registry
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_gpr_base_select";
        goto error;
    }

    /* set contact info for ns/gpr */
    if(NULL != orte_process_info.ns_replica_uri) {
        orte_rml.set_uri(orte_process_info.ns_replica_uri);
    }
    if(NULL != orte_process_info.gpr_replica_uri) {
        orte_rml.set_uri(orte_process_info.gpr_replica_uri);
    }

    /* Set my name */
    if (ORTE_SUCCESS != (ret = orte_sds_base_set_name())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_sds_base_set_name";
        goto error;
    }

    /* all done with sds - clean up and call it a day */
    orte_sds_base_close();

    /*
     * Now that we know for certain if we are an HNP and/or a daemon,
     * setup the resource management frameworks. This includes
     * selecting the daemon launch framework - that framework "knows"
     * what to do if it isn't in a daemon.
     */
    if (ORTE_SUCCESS != (ret = orte_rds_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rds_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rds_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rds_base_select";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_ras_base_find_available())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ras_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmaps_base_find_available())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmaps_base_find_available";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_pls_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pls_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_pls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_pls_base_select";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_odls_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_odls_base_select";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmgr_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmgr_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_rmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rmgr_base_select";
        goto error;
    }
    
    /*
     * setup the state monitor
     */
    if (ORTE_SUCCESS != (ret = orte_smr_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_smr_base_open";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_smr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_smr_base_select";
        goto error;
    }
    
    /*
     * setup the errmgr -- open has been done way before
     */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_errmgr_base_select";
        goto error;
    }
    
    /* initialize the rml module so it can open its interfaces - this
     * is needed so that we can get a uri for ourselves if we are an
     * HNP.  Note that this function creates listeners to the HNP
     * (for non-HNP procs) - if we are an HNP, it sets up the listener
     * for incoming connections from daemons and application procs.
     */
    if (ORTE_SUCCESS != (ret = orte_rml.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.init";
        goto error;
    }

    /* if I'm the seed, set the seed uri to be me! */
    if (orte_process_info.seed) {
        if (NULL != orte_universe_info.seed_uri) {
            free(orte_universe_info.seed_uri);
        }
        orte_universe_info.seed_uri = orte_rml.get_uri();
        /* and make sure that the daemon flag is NOT set so that
         * components unique to non-HNP orteds can be selected
         */
        orte_process_info.daemon = false;
    }

    /* setup my session directory */
    if (ORTE_SUCCESS != (ret = orte_ns.get_jobid_string(&jobid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns.get_jobid_string";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_ns.get_vpid_string(&procid_str, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns.get_vpid_string";
        goto error;
    }
                
    if (orte_debug_flag) {
        opal_output(0, "%s setting up session dir with",
                    ORTE_NAME_PRINT(orte_process_info.my_name));
        if (NULL != orte_process_info.tmpdir_base) {
            opal_output(0, "\ttmpdir %s", orte_process_info.tmpdir_base);
        }
        opal_output(0, "\tuniverse %s", orte_universe_info.name);
        opal_output(0, "\tuser %s", orte_system_info.user);
        opal_output(0, "\thost %s", orte_system_info.nodename);
        opal_output(0, "\tjobid %s", jobid_str);
        opal_output(0, "\tprocid %s", procid_str);
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
        error = "orte_session_dir";
        goto error;
    }
    if (NULL != jobid_str) {
        free(jobid_str);
    }
    if (NULL != procid_str) {
        free(procid_str);
    }

    /* Once the session directory location has been established, set
       the opal_output default file location to be in the
       proc-specific session directory. */
    opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                     "output-", NULL, NULL);

    /* if i'm the seed, get my contact info and write my setup file for others to find */
    if (orte_process_info.seed) {
        contact_path = opal_os_path(false, orte_process_info.universe_session_dir,
                    "universe-setup.txt", NULL);
        if (orte_debug_flag) {
            opal_output(0, "%s contact_file %s",
                        ORTE_NAME_PRINT(orte_process_info.my_name), contact_path);
        }

        if (ORTE_SUCCESS != (ret = orte_write_universe_setup_file(contact_path, &orte_universe_info))) {
            if (orte_debug_flag) {
                opal_output(0, "%s couldn't write setup file", ORTE_NAME_PRINT(orte_process_info.my_name));
            }
        } else if (orte_debug_flag) {
            opal_output(0, "%s wrote setup file", ORTE_NAME_PRINT(orte_process_info.my_name));
        }
        free(contact_path);
    }

    /* 
     * Initialize the selected modules now that all components/name are available.
     */
    if (ORTE_SUCCESS != (ret = orte_ns.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns.init";
        goto error;
    }
    
    if (ORTE_SUCCESS != (ret = orte_gpr.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_gpr.init";
        goto error;
    }
    
    /*
     * setup I/O forwarding system
     */
    if (ORTE_SUCCESS != (ret = orte_iof_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_select";
        goto error;
    }
    
    if (orte_process_info.seed) {
        /* if we are an HNP, we have to setup an app_context for ourselves so
         * various frameworks can find their required info
         */
        orte_app_context_t *app;
        orte_gpr_value_t *value;
        
        app = OBJ_NEW(orte_app_context_t);
        app->app = strdup("orted");
        app->num_procs = 1;
        if (ORTE_SUCCESS != (ret = orte_rmgr.store_app_context(ORTE_PROC_MY_NAME->jobid, &app, 1))) {
            ORTE_ERROR_LOG(ret);
            error = "HNP store app context";
            goto error;
        }
        OBJ_RELEASE(app);
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_value(&value,
                                                        ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                                        "orte-job-0", 2,
                                                        0))) {
            ORTE_ERROR_LOG(ret);
            error = "HNP create value";
            goto error;
        }
        
        /* store the node name and the daemon's name */
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_NODE_NAME_KEY,
                                                         ORTE_STRING, orte_system_info.nodename))) {
            ORTE_ERROR_LOG(ret);
            error = "HNP create keyval";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_PROC_NAME_KEY,
                                                         ORTE_NAME, ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(ret);
            error = "HNP create keyval";
            goto error;
        }
        
        /* set the tokens */
        if (ORTE_SUCCESS != (ret = orte_schema.get_proc_tokens(&(value->tokens), &(value->num_tokens), ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(ret);
            error = "HNP get proc tokens";
            goto error;
        }
        
        /* insert values */
        if (ORTE_SUCCESS != (ret = orte_gpr.put(1, &value))) {
            ORTE_ERROR_LOG(ret);
            error = "HNP put values";
            goto error;
        }
        OBJ_RELEASE(value);
        
        /* and set our state to LAUNCHED */
        if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(ORTE_PROC_MY_NAME, ORTE_PROC_STATE_LAUNCHED, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "HNP could not set launched state";
            goto error;
        }
    }
    
    if (orte_process_info.singleton) {
        /* since all frameworks are now open and active, walk through
        * the spawn sequence to ensure that we collect info on all
        * available resources. Although we are a singleton and hence
        * don't need to be spawned, we may choose to dynamically spawn
        * additional processes. If we do that, then we need to know
        * about any resources that have been allocated to us - executing
        * the RDS and RAS frameworks is the only way to get that info.
        *
        * THIS ONLY SHOULD BE DONE FOR SINGLETONS - DO NOT DO IT
        * FOR ANY OTHER CASE
        */
        orte_app_context_t *app;
        orte_gpr_value_t *values[2];
        orte_std_cntr_t one=1, zero=0;
        orte_proc_state_t init=ORTE_PROC_STATE_INIT;
        orte_vpid_t lrank=0, vone=1;
        char *segment;

        app = OBJ_NEW(orte_app_context_t);
        app->app = strdup("singleton");
        app->num_procs = 1;
        
        if (ORTE_SUCCESS !=
            (ret = orte_rmgr_base_put_app_context(ORTE_PROC_MY_NAME->jobid, &app, 1))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not store app context";
            goto error;
        }
        OBJ_RELEASE(app);
        
        /* ensure we read the allocation, even if we are not sitting on a node that is within it */
        if (ORTE_SUCCESS != (ret = orte_ras.allocate_job(ORTE_PROC_MY_NAME->jobid, NULL))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not allocate job";
            goto error;
        }
        
        /* setup the launch system's stage gate counters and subscriptions */
        if (ORTE_SUCCESS != (ret = orte_rmgr_base_proc_stage_gate_init(ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not init triggers";
            goto error;
        }
        
        /* let's deal with the procs first - start by getting their job segment name */
        if (ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create job segment name";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_value(&(values[0]),
                                                        ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                                        segment, 7, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create gpr value";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[0]), ORTE_PROC_RANK_KEY, ORTE_VPID, &(ORTE_PROC_MY_NAME->vpid)))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[1]), ORTE_PROC_NAME_KEY, ORTE_NAME, ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[2]), ORTE_NODE_NAME_KEY, ORTE_STRING, orte_system_info.nodename))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[3]), ORTE_PROC_APP_CONTEXT_KEY, ORTE_STD_CNTR, &zero))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[4]), ORTE_PROC_STATE_KEY, ORTE_PROC_STATE, &init))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[5]), ORTE_PROC_LOCAL_RANK_KEY, ORTE_VPID, &lrank))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[6]), ORTE_NODE_NUM_PROCS_KEY, ORTE_STD_CNTR, &one))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_schema.get_proc_tokens(&(values[0]->tokens), &(values[0]->num_tokens), ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not get gpr tokens";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_gpr.create_value(&values[1],
                                                        ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                                        segment, 3, 1))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create gpr value";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[1]->keyvals[0]), ORTE_PROC_NUM_AT_INIT, ORTE_STD_CNTR, &one))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[1]->keyvals[1]), ORTE_JOB_VPID_START_KEY, ORTE_VPID, &(ORTE_PROC_MY_NAME->vpid)))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[1]->keyvals[2]), ORTE_JOB_VPID_RANGE_KEY, ORTE_VPID, &vone))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not create keyval";
            goto error;
        }
        values[1]->tokens[0] = strdup(ORTE_JOB_GLOBALS); /* counter is in the job's globals container */

        /* insert all values in one call */
        if (ORTE_SUCCESS != (ret = orte_gpr.put(2, values))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not put its launch data";
            goto error;
        }
        OBJ_RELEASE(values[0]);
        OBJ_RELEASE(values[1]);
        free(segment);

        /* wireup our io */
        if (ORTE_SUCCESS != (ret = orte_iof.iof_pull(ORTE_PROC_MY_NAME, ORTE_NS_CMP_JOBID, ORTE_IOF_STDOUT, 1))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not setup iof";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_iof.iof_pull(ORTE_PROC_MY_NAME, ORTE_NS_CMP_JOBID, ORTE_IOF_STDERR, 2))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not setup iof";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_iof.iof_push(ORTE_PROC_MY_NAME, ORTE_NS_CMP_JOBID, ORTE_IOF_STDIN, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not setup iof";
            goto error;
        }
        
        /* setup the errmgr, as required */
        if (ORTE_SUCCESS != (ret = orte_errmgr.register_job(ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not setup errmgr callbacks";
            goto error;
        }
        
       /* set our state to LAUNCHED */
        if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(ORTE_PROC_MY_NAME, ORTE_PROC_STATE_LAUNCHED, 0))) {
            ORTE_ERROR_LOG(ret);
            error = "singleton could not set launched state";
            goto error;
        }
    }
    
    /*
     * Setup the FileM
     */
    if (ORTE_SUCCESS != (ret = orte_filem_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_open";
        goto error;
    }

    if (ORTE_SUCCESS != (ret = orte_filem_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_filem_base_select";
        goto error;
    }

#if OPAL_ENABLE_FT == 1
    /*
     * Setup the SnapC
     */
    if (ORTE_SUCCESS != (ret = orte_snapc_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_open";
        goto error;
    }

    if (ORTE_SUCCESS != (ret = orte_snapc_base_select(orte_process_info.seed, !orte_process_info.daemon))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_snapc_base_select";
        goto error;
    }

    /* Need to figure out if we are an application or part of ORTE */
    if(infrastructure || 
       orte_process_info.seed || 
       orte_process_info.daemon) {
        /* ORTE doesn't need the OPAL CR stuff */
        opal_cr_set_enabled(false);
    }
    else {
        /* The application does however */
        opal_cr_set_enabled(true);
    }
#else
    opal_cr_set_enabled(false);
#endif

error:
    if (ret != ORTE_SUCCESS) {
        opal_show_help("help-orte-runtime",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }

    return ret;
}

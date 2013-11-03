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
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>

#include "opal/mca/event/event.h"
#include "opal/runtime/opal.h"

#include "orte/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/util/argv.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/plm/base/base.h"

#include "orte/mca/rmaps/base/base.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/db/db.h"
#endif
#include "orte/mca/filem/base/base.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/regex.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/runtime/orte_cr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/env/ess_env.h"

static int env_set_name(void);

static int rte_init(void);
static int rte_finalize(void);

#if OPAL_ENABLE_FT_CR == 1
static int rte_ft_event(int state);
#endif

orte_ess_base_module_t orte_ess_env_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
#if OPAL_ENABLE_FT_CR == 1
    rte_ft_event
#else
    NULL
#endif
};

static int rte_init(void)
{
    int ret;
    char *error = NULL;
    char **hosts = NULL;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* Start by getting a unique name from the enviro */
    env_set_name();

    /* if I am a daemon, complete my setup using the
     * default procedure
     */
    if (ORTE_PROC_IS_DAEMON) {
        if (NULL != orte_node_regex) {
            /* extract the nodes */
            if (ORTE_SUCCESS != (ret = orte_regex_extract_node_names(orte_node_regex, &hosts))) {
                error = "orte_regex_extract_node_names";
                goto error;
            }
        }
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup(hosts))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
        opal_argv_free(hosts);
        return ORTE_SUCCESS;
    }
    
    if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto error;
        }
        /* as a tool, I don't need a nidmap - so just return now */
        return ORTE_SUCCESS;
        
    }
    
    /* use the default procedure to finish my setup */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup(true))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }
    
    /* if data was provided, update the database */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(orte_process_info.sync_buf))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    
    /* setup process binding */
    if (ORTE_SUCCESS != (ret = orte_ess_base_proc_binding())) {
        error = "proc_binding";
        goto error;
    }

    /* if we are an ORTE app - and not an MPI app - then
     * we need to exchange our connection info here.
     * MPI_Init has its own modex, so we don't need to do
     * two of them. However, if we don't do a modex at all,
     * then processes have no way to communicate
     *
     * NOTE: only do this when the process originally launches.
     * Cannot do this on a restart as the rest of the processes
     * in the job won't be executing this step, so we would hang
     */
    if (ORTE_PROC_IS_NON_MPI && !orte_do_not_barrier) {
        orte_grpcomm_collective_t coll;
        OBJ_CONSTRUCT(&coll, orte_grpcomm_collective_t);
        coll.id = orte_process_info.peer_modex;
        coll.active = true;
        if (ORTE_SUCCESS != (ret = orte_grpcomm.modex(&coll))) {
            ORTE_ERROR_LOG(ret);
            error = "orte modex";
            goto error;
        }
        ORTE_WAIT_FOR_COMPLETION(coll.active);
        OBJ_DESTRUCT(&coll);
    }
    
    return ORTE_SUCCESS;

error:
    if (ORTE_ERR_SILENT != ret && !orte_report_silent_errors) {
        orte_show_help("help-orte-runtime.txt",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }

    return ret;
}

static int rte_finalize(void)
{
    int ret;

    /* if I am a daemon, finalize using the default procedure */
    if (ORTE_PROC_IS_DAEMON) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        return ret;
    } else if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        /* as a tool, I didn't create a nidmap - so just return now */
        return ret;
    }

    /* otherwise, I must be an application process
     * use the default procedure to finish
     */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
        ORTE_ERROR_LOG(ret);
    }

    /* deconstruct the nidmap and jobmap arrays */
    orte_util_nidmap_finalize();

    return ORTE_SUCCESS;
}

static int env_set_name(void)
{
    int rc;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    
    if (NULL == orte_ess_base_jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, orte_ess_base_jobid))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }

    if (NULL == orte_ess_base_vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, orte_ess_base_vpid))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }

    ORTE_PROC_MY_NAME->jobid = jobid;
    ORTE_PROC_MY_NAME->vpid = vpid;
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                         "ess:env set name to %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* get the non-name common environmental variables */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

#if OPAL_ENABLE_FT_CR == 1
static int rte_ft_event(int state)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_proc_type_t svtype;
    orte_grpcomm_collective_t coll;

    OBJ_CONSTRUCT(&coll, orte_grpcomm_collective_t);
    coll.id = orte_process_info.peer_init_barrier;

    /******** Checkpoint Prep ********/
    if(OPAL_CRS_CHECKPOINT == state) {
        /*
         * Notify SnapC
         */
        if( ORTE_SUCCESS != (ret = orte_snapc.ft_event(OPAL_CRS_CHECKPOINT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify Routed
         */
        if( ORTE_SUCCESS != (ret = orte_routed.ft_event(OPAL_CRS_CHECKPOINT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify RML -> OOB
         */
        if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_CHECKPOINT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    /******** Continue Recovery ********/
    else if (OPAL_CRS_CONTINUE == state ) {
        OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                             "ess:env ft_event(%2d) - %s is Continuing",
                             state, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        /*
         * Notify RML -> OOB
         */
        if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_CONTINUE))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify Routed
         */
        if( ORTE_SUCCESS != (ret = orte_routed.ft_event(OPAL_CRS_CONTINUE))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify SnapC
         */
        if( ORTE_SUCCESS != (ret = orte_snapc.ft_event(OPAL_CRS_CONTINUE))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if( orte_cr_continue_like_restart ) {
            /*
             * Barrier to make all processes have been successfully restarted before
             * we try to remove some restart only files.
             */
            if (ORTE_SUCCESS != (ret = orte_grpcomm.barrier(&coll))) {
                opal_output(0, "ess:env: ft_event(%2d): Failed in orte_grpcomm.barrier (%d)",
                            state, ret);
                exit_status = ret;
                goto cleanup;
            }
            coll.active = true;
            ORTE_WAIT_FOR_COMPLETION(coll.active);

            if( orte_cr_flush_restart_files ) {
                OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                                     "ess:env ft_event(%2d): %s "
                                     "Cleanup restart files...",
                                     state, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                opal_crs_base_cleanup_flush();
            }
        }
    }
    /******** Restart Recovery ********/
    else if (OPAL_CRS_RESTART == state ) {
        OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                             "ess:env ft_event(%2d) - %s is Restarting",
                             state, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        /*
         * This should follow the ess init() function
         */

        /*
         * Clear nidmap and jmap
         */
        orte_util_nidmap_finalize();

        /*
         * - Reset Contact information
         */
        if( ORTE_SUCCESS != (ret = env_set_name() ) ) {
            exit_status = ret;
        }

        /*
         * Notify RML -> OOB
         */
        if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_RESTART))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Restart the routed framework
         * JJH: Lie to the finalize function so it does not try to contact the daemon.
         */
        svtype = orte_process_info.proc_type;
        orte_process_info.proc_type = ORTE_PROC_TOOL;
        if (ORTE_SUCCESS != (ret = orte_routed.finalize()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        orte_process_info.proc_type = svtype;
        if (ORTE_SUCCESS != (ret = orte_routed.initialize()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Group Comm - Clean out stale data
         */
        orte_grpcomm.finalize();
        if (ORTE_SUCCESS != (ret = orte_grpcomm.init())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        if (ORTE_SUCCESS != (ret = orte_db.remove(NULL, NULL))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Restart the PLM - Does nothing at the moment, but included for completeness
         */
        if (ORTE_SUCCESS != (ret = orte_plm.finalize())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = orte_plm.init())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * RML - Enable communications
         */
        if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Notify Routed
         */
        if( ORTE_SUCCESS != (ret = orte_routed.ft_event(OPAL_CRS_RESTART))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /* if one was provided, build my nidmap */
        if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(orte_process_info.sync_buf))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Barrier to make all processes have been successfully restarted before
         * we try to remove some restart only files.
         */
        if (ORTE_SUCCESS != (ret = orte_grpcomm.barrier(&coll))) {
            opal_output(0, "ess:env ft_event(%2d): Failed in orte_grpcomm.barrier (%d)",
                        state, ret);
            exit_status = ret;
            goto cleanup;
        }
        coll.active = true;
	ORTE_WAIT_FOR_COMPLETION(coll.active);

        if( orte_cr_flush_restart_files ) {
            OPAL_OUTPUT_VERBOSE((1, orte_ess_base_framework.framework_output,
                                 "ess:env ft_event(%2d): %s "
                                 "Cleanup restart files...",
                                 state, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

            opal_crs_base_cleanup_flush();
        }

        /*
         * Session directory re-init
         */
        if (orte_create_session_dirs) {
            if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                                        orte_process_info.tmpdir_base,
                                                        orte_process_info.nodename,
                                                        NULL, /* Batch ID -- Not used */
                                                        ORTE_PROC_MY_NAME))) {
                exit_status = ret;
            }
            
            opal_output_set_output_file_info(orte_process_info.proc_session_dir,
                                             "output-", NULL, NULL);
        }

        /*
         * Notify SnapC
         */
        if( ORTE_SUCCESS != (ret = orte_snapc.ft_event(OPAL_CRS_RESTART))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Nothing */
    }
    else {
        /* Error state = Nothing */
    }

cleanup:
    OBJ_DESTRUCT(&coll);
    return exit_status;
}
#endif

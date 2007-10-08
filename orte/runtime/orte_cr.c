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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file 
 * 
 * ORTE Layer Checkpoint/Restart Runtime functions
 *
 */

#include "orte_config.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */

#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/event/event.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/runtime/opal_cr.h"

#include "orte/orte_constants.h"
#include "orte/runtime/orte_cr.h"
#include "orte/runtime/runtime.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/schema/base/base.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

/*************
 * Local functions
 *************/
static int orte_cr_coord_pre_ckpt(void);
static int orte_cr_coord_pre_restart(void);
static int orte_cr_coord_pre_continue(void);

static int orte_cr_coord_post_ckpt(void);
static int orte_cr_coord_post_restart(void);
static int orte_cr_coord_post_continue(void);

/*************
 * Local vars
 *************/
static opal_cr_coord_callback_fn_t  prev_coord_callback = NULL;

static int orte_cr_output = -1;

/*
 * CR Init
 */
int orte_cr_init(void) 
{
    int ret, exit_status = ORTE_SUCCESS;
    int val;

    /*
     * OPAL Frameworks
     */
    if (OPAL_SUCCESS != (ret = opal_cr_init() ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Register MCA Parameters
     */
    mca_base_param_reg_int_name("orte_cr", "verbose",
                                "Verbose output for the ORTE Checkpoint/Restart functionality",
                                false, false,
                                0,
                                &val);
    if(0 != val) {
        orte_cr_output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_cr_output, val);
    } else {
        orte_cr_output = opal_cr_output;
    }

    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: init: orte_cr_init()\n");

    /* Init ORTE Entry Point Function */
    if( ORTE_SUCCESS != (ret = orte_cr_entry_point_init()) ) {
        exit_status = ret;
        goto cleanup;
    }

    /* Register the ORTE interlevel coordination callback */
    opal_cr_reg_coord_callback(orte_cr_coord, &prev_coord_callback);
    
 cleanup:

    return exit_status;
}

/*
 * Finalize
 */
int orte_cr_finalize(void)
{
    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: finalize: orte_cr_finalize()");

    orte_cr_entry_point_finalize();

    /*
     * OPAL Frameworks...
     */
    opal_cr_finalize();

    return ORTE_SUCCESS;
}

/*
 * Interlayer coordination callback
 */
int orte_cr_coord(int state) 
{
    int ret, exit_status = ORTE_SUCCESS;

    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: coord: orte_cr_coord(%s)",
                        opal_crs_base_state_str((opal_crs_state_type_t)state));

    /*
     * Before calling the previous callback, we have the opportunity to 
     * take action given the state.
     */
    if(OPAL_CRS_CHECKPOINT == state) {
        /* Do Checkpoint Phase work */
        orte_cr_coord_pre_ckpt();
    }
    else if (OPAL_CRS_CONTINUE == state ) {
        /* Do Continue Phase work */
        orte_cr_coord_pre_continue();
    }
    else if (OPAL_CRS_RESTART == state ) {
#if 0 /* JJH Do we need this ? */
        if (ORTE_SUCCESS != (ret = orte_cr_init_quick_params())) {
            opal_output(orte_cr_output,
                        "ERROR: orte_cr: coord: Unable to init quick parameters\n");
        }
#endif
        /* Do Restart Phase work */
        orte_cr_coord_pre_restart();
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Do Continue Phase work in prep to terminate the application */
    }
    else {
        /* We must have been in an error state from the checkpoint
         * recreate everything, as in the Continue Phase
         */
    }

    /*
     * Call the previous callback, which should be OPAL
     */
    if(OPAL_SUCCESS != (ret = prev_coord_callback(state)) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    
    /*
     * After calling the previous callback, we have the opportunity to 
     * take action given the state to tidy up.
     */
    if(OPAL_CRS_CHECKPOINT == state) {
        /* Do Checkpoint Phase work */
        orte_cr_coord_post_ckpt();
    }
    else if (OPAL_CRS_CONTINUE == state ) {
        /* Do Continue Phase work */
        orte_cr_coord_post_continue();
    }
    else if (OPAL_CRS_RESTART == state ) {
        /* Do Restart Phase work */
        orte_cr_coord_post_restart();
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Do Continue Phase work in prep to terminate the application */
    }
    else {
        /* We must have been in an error state from the checkpoint
         * recreate everything, as in the Continue Phase
         */
    }

 cleanup:
    return exit_status;
}

/*************
 * Pre Lower Layer
 *************/
static int orte_cr_coord_pre_ckpt(void) {
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * All the checkpoint heavey lifting in here...
     */
    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: coord_pre_ckpt: orte_cr_coord_pre_ckpt()");

    /*
     * Notify IOF
     */
    if( ORTE_SUCCESS != (ret = orte_iof.ft_event(OPAL_CRS_CHECKPOINT))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Notify NS
     */
    if( ORTE_SUCCESS != (ret = orte_ns.ft_event(OPAL_CRS_CHECKPOINT))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Notify GPR
     */
    if( ORTE_SUCCESS != (ret = orte_gpr.ft_event(OPAL_CRS_CHECKPOINT))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Notify RML & OOB
     */
    if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_CHECKPOINT))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:

    return exit_status;
}

static int orte_cr_coord_pre_restart(void) {
    /*
     * Can not really do much until OPAL is up and running,
     * so defer action until the post_restart function.
     */
    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: coord_pre_restart: orte_cr_coord_pre_restart()");
    
    return ORTE_SUCCESS;
}
    
static int orte_cr_coord_pre_continue(void) {
    /*
     * Can not really do much until OPAL is up and running,
     * so defer action until the post_continue function.
     */
    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: coord_pre_continue: orte_cr_coord_pre_continue()");

    return ORTE_SUCCESS;
}

/*************
 * Post Lower Layer
 *************/
static int orte_cr_coord_post_ckpt(void) {
    /*
     * Now that OPAL is shutdown, we really can't do much
     * so assume pre_ckpt took care of everything.
     */
    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: coord_post_ckpt: orte_cr_coord_post_ckpt()");

    return ORTE_SUCCESS;
}

static int orte_cr_coord_post_restart(void) {
    int ret, exit_status = ORTE_SUCCESS;
    char * procid_str = NULL;
    char * jobid_str  = NULL;
    int id;

    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: coord_post_restart: orte_cr_coord_post_restart()");

    /* Need to invalidate these so we can grab the new values from the environment 
     * Don't call orte_proc_info_finalize() since we want to preserve some values
     * such as orte_process_info.gpr_replica
     */
    if (NULL != orte_process_info.ns_replica_uri) {
        free(orte_process_info.ns_replica_uri);
        orte_process_info.ns_replica_uri = NULL;
    }

    if (NULL != orte_process_info.gpr_replica_uri) {
        free(orte_process_info.gpr_replica_uri);
        orte_process_info.gpr_replica_uri = NULL;
    }

    if (NULL != orte_process_info.tmpdir_base) {
        free(orte_process_info.tmpdir_base);
        orte_process_info.tmpdir_base = NULL;
    }
    
    if (NULL != orte_process_info.top_session_dir) {
        free(orte_process_info.top_session_dir);
        orte_process_info.top_session_dir = NULL;
    }
 
    if (NULL != orte_process_info.universe_session_dir) {
        free(orte_process_info.universe_session_dir);
        orte_process_info.universe_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.job_session_dir) {
        free(orte_process_info.job_session_dir);
        orte_process_info.job_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.proc_session_dir) {
        free(orte_process_info.proc_session_dir);
        orte_process_info.proc_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.sock_stdin) {
        free(orte_process_info.sock_stdin);
        orte_process_info.sock_stdin = NULL;
    }
    
    if (NULL != orte_process_info.sock_stdout) {
        free(orte_process_info.sock_stdout);
        orte_process_info.sock_stdout = NULL;
    }
    
    if (NULL != orte_process_info.sock_stderr) {
        free(orte_process_info.sock_stderr);
        orte_process_info.sock_stderr = NULL;
    }

    if( NULL != orte_system_info.nodename ) {
        free(orte_system_info.nodename);
        orte_system_info.nodename = NULL;
    }

    /* We want these to be read out of the HNP contact info file ? */
    id = mca_base_param_find("gpr", "replica", "uri");
    mca_base_param_unset(id);
    id = mca_base_param_find("ns", "replica", "uri");
    mca_base_param_unset(id);
    
    /*
     * Refresh System information
     */ 
    orte_system_info.init = false;
    if( ORTE_SUCCESS != (ret = orte_sys_info()) ) {
        exit_status = ret;
    }

    if( ORTE_SUCCESS != (ret = orte_proc_info()) ) {
        exit_status = ret;
    }
    if (NULL != orte_process_info.my_name) {
        free(orte_process_info.my_name);
        orte_process_info.my_name = NULL;
    }

    /*
     * Re-attach to session directory
     */
    orte_universe_info.state = ORTE_UNIVERSE_STATE_PRE_INIT;
    if( ORTE_SUCCESS != (ret = orte_univ_info()) ) {
        exit_status = ret;
    }

    /*
     * Notify RML & OOB
     */
    if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_RESTART))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Startup Discovery Service:
     *  - Connect to the universe
     * Structure Elements Refreshed:
     *   orte_universe_info.name
     *   orte_universe_info.host
     *   orte_universe_info.uid
     *   orte_universe_info.persistence
     *   orte_universe_info.scope
     *   orte_universe_info.seed_uri
     *   orte_universe_info.console_connected
     *   orte_universe_info.scriptfile
     *
     *   orte_process_info.ns_replica_uri
     *   orte_process_info.gpr_replica_uri
     */
    if (ORTE_SUCCESS != (ret = orte_sds_base_open())) {
        exit_status = ret;
    }

    if (ORTE_SUCCESS != (ret = orte_sds_base_select())) {
        exit_status = ret;
    }

    if( ORTE_SUCCESS != (ret = orte_sds_base_contact_universe() ) ) {
        exit_status = ret;
    }

    /*
     * - Reset Contact information
     */
    if(NULL != orte_process_info.ns_replica_uri) {
        orte_rml.set_contact_info(orte_process_info.ns_replica_uri);
    }

    if(NULL != orte_process_info.gpr_replica_uri) {
        orte_rml.set_contact_info(orte_process_info.gpr_replica_uri);
    }

    if( ORTE_SUCCESS != (ret = orte_sds_base_set_name() ) ) {
        exit_status = ret;
    }
    orte_sds_base_close();

    /* Session directory stuff:
     *   orte_process_info.top_session_dir
     *   orte_process_info.universe_session_dir
     *   orte_process_info.job_session_dir
     *   orte_process_info.proc_session_dir
     */
    if (ORTE_SUCCESS != (ret = orte_ns.get_jobid_string(&jobid_str, orte_process_info.my_name))) {
        exit_status = ret;
    }

    if (ORTE_SUCCESS != (ret = orte_ns.get_vpid_string(&procid_str, orte_process_info.my_name))) {
        exit_status = ret;
    }

    if (ORTE_SUCCESS != (ret = orte_session_dir(true,
                                                orte_process_info.tmpdir_base,
                                                orte_system_info.user,
                                                orte_system_info.nodename,
                                                NULL, /* Batch ID -- Not used */
                                                orte_universe_info.name,
                                                jobid_str,
                                                procid_str))) {
        exit_status = ret;
    }

    /*
     * Re-enable communication through the RML
     */
    if (ORTE_SUCCESS != (ret = orte_rml.enable_comm())) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Notify NS
     */
    if( ORTE_SUCCESS != (ret = orte_ns.ft_event(OPAL_CRS_RESTART))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Notify GPR
     */
    if( ORTE_SUCCESS != (ret = orte_gpr.ft_event(OPAL_CRS_RESTART))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Notify IOF
     */
    if( ORTE_SUCCESS != (ret = orte_iof.ft_event(OPAL_CRS_RESTART))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Re-exchange the routes
     */
    if (ORTE_SUCCESS != (ret = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Send new PID to GPR
     * The checkpointer could have used a proxy program to boot us
     * so the pid that the orted got from fork() may not be the
     * PID of this application.
     * - Note: BLCR does this because it tries to preseve the PID
     *         of the program across checkpointes
     */
    if( ORTE_SUCCESS != (ret = orte_rmgr.set_process_info(orte_process_info.my_name, getpid(), orte_system_info.nodename) ) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if (NULL != jobid_str) {
        free(jobid_str);
        jobid_str = NULL;
    }

    if (NULL != procid_str) {
        free(procid_str);
        procid_str = NULL;
    }

    return exit_status;
}

static int orte_cr_coord_post_continue(void) {
    int ret, exit_status = ORTE_SUCCESS;

    opal_output_verbose(10, orte_cr_output,
                        "orte_cr: coord_post_continue: orte_cr_coord_post_continue()\n");

    /*
     * Notify RML & OOB
     */
    if( ORTE_SUCCESS != (ret = orte_rml.ft_event(OPAL_CRS_CONTINUE))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Notify NS
     */
    if( ORTE_SUCCESS != (ret = orte_gpr.ft_event(OPAL_CRS_CONTINUE))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Notify GPR
     */
    if( ORTE_SUCCESS != (ret = orte_gpr.ft_event(OPAL_CRS_CONTINUE))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Notify IOF
     */
    if( ORTE_SUCCESS != (ret = orte_iof.ft_event(OPAL_CRS_CONTINUE))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:

    return exit_status;
}

/*************************************************
 * ORTE Entry Point functionality
 *************************************************/
int orte_cr_entry_point_init(void)
{
#if 0
    /* JJH XXX
     * Make sure to finalize the OPAL Entry Point function if it is active.
     */
    opal_cr_entry_point_finalize();
#endif

    return ORTE_SUCCESS;
}

int orte_cr_entry_point_finalize(void)
{
    /* Nothing to do here... */
    return ORTE_SUCCESS;
}

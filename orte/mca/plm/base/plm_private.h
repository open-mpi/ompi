/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
/** @file:
 */

#ifndef MCA_PLM_PRIVATE_H
#define MCA_PLM_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/threads/condition.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/runtime/orte_globals.h"


BEGIN_C_DECLS

/* globals for use solely within PLM framework */
typedef struct {
    /** Verbose/debug output stream */
    int output;
    /* orted cmd comm lock */
    opal_mutex_t orted_cmd_lock;
    /* orted cmd cond */
    opal_condition_t orted_cmd_cond;
    /* next jobid */
    orte_jobid_t next_jobid;
} orte_plm_globals_t;
/**
 * Global instance of PLM framework data
 */
ORTE_DECLSPEC extern orte_plm_globals_t orte_plm_globals;


/**
 * Utility routine to set progress engine schedule
 */
ORTE_DECLSPEC int orte_plm_base_set_progress_sched(int sched);

/*
 * Launch support
 */
ORTE_DECLSPEC int orte_plm_base_setup_job(orte_job_t *jdata);
ORTE_DECLSPEC int orte_plm_base_launch_apps(orte_jobid_t job);
ORTE_DECLSPEC void orte_plm_base_launch_failed(orte_jobid_t job, pid_t pid, int status, orte_job_state_t state);

ORTE_DECLSPEC int orte_plm_base_daemon_callback(orte_std_cntr_t num_daemons);

ORTE_DECLSPEC void orte_plm_base_check_job_completed(orte_job_t *jdata);

ORTE_DECLSPEC int orte_plm_base_set_hnp_name(void);

ORTE_DECLSPEC int orte_plm_base_create_jobid(orte_jobid_t *jobid);

ORTE_DECLSPEC int orte_plm_base_setup_orted_cmd(int *argc, char ***argv);

/**
 * Heartbeat support
 */
ORTE_DECLSPEC void orte_plm_base_heartbeat(int fd, short event, void *data);
ORTE_DECLSPEC void orte_plm_base_start_heart(void);

/**
 * Utilities for plm components that use proxy daemons
 */
ORTE_DECLSPEC int orte_plm_base_orted_exit(orte_daemon_cmd_flag_t command);
ORTE_DECLSPEC int orte_plm_base_orted_kill_local_procs(orte_jobid_t job);
ORTE_DECLSPEC int orte_plm_base_orted_signal_local_procs(orte_jobid_t job, int32_t signal);

/*
 * communications utilities
 */
ORTE_DECLSPEC int orte_plm_base_comm_start(void);
ORTE_DECLSPEC int orte_plm_base_comm_stop(void);
ORTE_DECLSPEC void orte_plm_base_recv(int status, orte_process_name_t* sender,
                                      opal_buffer_t* buffer, orte_rml_tag_t tag,
                                      void* cbdata);

    
/**
 * Construct basic ORTE Daemon command line arguments
 */
ORTE_DECLSPEC int orte_plm_base_orted_append_basic_args(int *argc, char ***argv,
                                                        char *sds,
                                                        int *proc_vpid_index,
                                                        bool heartbeat);

/*
 * Proxy functions for use by daemons and application procs
 * needing dynamic operations
 */
ORTE_DECLSPEC int orte_plm_proxy_init(void);
ORTE_DECLSPEC int orte_plm_proxy_spawn(orte_job_t *jdata);
ORTE_DECLSPEC int orte_plm_proxy_finalize(void);

END_C_DECLS

#endif  /* MCA_PLS_PRIVATE_H */

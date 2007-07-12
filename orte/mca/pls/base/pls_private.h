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

#ifndef MCA_PLS_PRIVATE_H
#define MCA_PLS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/class/opal_list.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rmgr/rmgr_types.h"
#include "orte/mca/rml/rml_types.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * pls proxy commands
 */
typedef uint8_t orte_pls_cmd_flag_t;
#define ORTE_PLS_CMD    ORTE_UINT8
#define ORTE_PLS_LAUNCH_JOB_CMD         1
#define ORTE_PLS_TERMINATE_JOB_CMD      2
#define ORTE_PLS_TERMINATE_PROC_CMD     3
#define ORTE_PLS_SIGNAL_JOB_CMD         4
#define ORTE_PLS_SIGNAL_PROC_CMD        5
#define ORTE_PLS_TERMINATE_ORTEDS_CMD   6

    /**
     * Utility routine to set progress engine schedule
     */
    ORTE_DECLSPEC int orte_pls_base_set_progress_sched(int sched);


    /**
     * Utilities for pls components that use proxy daemons
     */
    ORTE_DECLSPEC int orte_pls_base_orted_exit(struct timeval *timeout, opal_list_t *attrs);
    ORTE_DECLSPEC int orte_pls_base_orted_kill_local_procs(orte_jobid_t job, struct timeval *timeout, opal_list_t *attrs);
    ORTE_DECLSPEC int orte_pls_base_orted_signal_local_procs(orte_jobid_t job, int32_t signal, opal_list_t *attrs);

    ORTE_DECLSPEC int orte_pls_base_launch_apps(orte_job_map_t *map);
    
    ORTE_DECLSPEC int orte_pls_base_daemon_callback(orte_std_cntr_t num_daemons);

    /*
     * communications utilities
     */
    ORTE_DECLSPEC int orte_pls_base_comm_start(void);
    ORTE_DECLSPEC int orte_pls_base_comm_stop(void);
    void orte_pls_base_recv(int status, orte_process_name_t* sender,
                            orte_buffer_t* buffer, orte_rml_tag_t tag,
                            void* cbdata);
        
    /*
     * general utilities
     */
    ORTE_DECLSPEC void orte_pls_base_purge_mca_params(char ***env);

    /**
     * Construct basic ORTE Daemon command line arguments
     */
    ORTE_DECLSPEC int orte_pls_base_orted_append_basic_args(
                                                 int *argc, 
                                                 char ***argv,
                                                 int *proc_name_index,
                                                 int *node_name_index,
                                                 orte_std_cntr_t num_procs);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif  /* MCA_PLS_PRIVATE_H */

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
#define ORTE_PLS_CANCEL_OPERATION_CMD   7

    /*
     * object for daemon information
     */
    typedef struct orte_pls_daemon_info_t {
        opal_list_item_t super;
        orte_cellid_t cell;
        char *nodename;
        orte_process_name_t *name;
        orte_jobid_t active_job;
      } orte_pls_daemon_info_t;
    ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_pls_daemon_info_t);
    
    
    /**
     * Utility routine to set progress engine schedule
     */
    ORTE_DECLSPEC int orte_pls_base_set_progress_sched(int sched);


    /**
     * Utilities for pls components that use proxy daemons
     */
    int orte_pls_base_orted_cancel_operation(void);
    int orte_pls_base_orted_exit(opal_list_t *daemons, struct timeval *timeout);
    int orte_pls_base_orted_kill_local_procs(opal_list_t *daemons, orte_jobid_t job, struct timeval *timeout);
    int orte_pls_base_orted_signal_local_procs(opal_list_t *daemons, int32_t signal);
    int orte_pls_base_orted_add_local_procs(opal_list_t *dmnlist, orte_gpr_notify_data_t *ndat);

    ORTE_DECLSPEC int orte_pls_base_get_active_daemons(opal_list_t *daemons, orte_jobid_t job, opal_list_t *attrs);
    ORTE_DECLSPEC int orte_pls_base_store_active_daemons(opal_list_t *daemons);
    ORTE_DECLSPEC int orte_pls_base_remove_daemon(orte_pls_daemon_info_t *info);
    int orte_pls_base_check_avail_daemons(opal_list_t *daemons, orte_jobid_t job);
    
    ORTE_DECLSPEC int orte_pls_base_launch_on_existing_daemons(orte_job_map_t *map);

    /*
     * communications utilities
     */
    int orte_pls_base_comm_start(void);
    ORTE_DECLSPEC int orte_pls_base_comm_stop(void);
    void orte_pls_base_recv(int status, orte_process_name_t* sender,
                            orte_buffer_t* buffer, orte_rml_tag_t tag,
                            void* cbdata);
        
    /*
     * general utilities
     */
    ORTE_DECLSPEC int orte_pls_base_mca_argv(int *argc, char ***argv);
    void orte_pls_base_purge_mca_params(char ***env);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

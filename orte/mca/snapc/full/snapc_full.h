/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
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

/**
 * @file
 * 
 * FULL SNAPC component
 *
 * Simple, braindead implementation.
 */

#ifndef MCA_SNAPC_FULL_EXPORT_H
#define MCA_SNAPC_FULL_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/event/event.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/snapc/snapc.h"

BEGIN_C_DECLS

/*
 * cmds for base receive
 */
typedef uint8_t orte_snapc_full_cmd_flag_t;
#define ORTE_SNAPC_FULL_CMD  OPAL_UINT8
#define ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD   1
#define ORTE_SNAPC_FULL_UPDATE_PROC_STATE_CMD  2
#define ORTE_SNAPC_FULL_VPID_ASSOC_CMD         3
#define ORTE_SNAPC_FULL_ESTABLISH_DIR_CMD      4

    /*
     * Local Component structures
     */
    struct orte_snapc_full_component_t {
        orte_snapc_base_component_t super;  /** Base SNAPC component */

    };
    typedef struct orte_snapc_full_component_t orte_snapc_full_component_t;
    OPAL_MODULE_DECLSPEC extern orte_snapc_full_component_t mca_snapc_full_component;

    struct orte_snapc_full_global_snapshot_t {
        /** Base SNAPC Global snapshot type */
        orte_snapc_base_snapshot_t super;

        /** Local coordinator associated with this vpid */
        orte_process_name_t local_coord;
    };
    typedef struct orte_snapc_full_global_snapshot_t orte_snapc_full_global_snapshot_t;

    OBJ_CLASS_DECLARATION(orte_snapc_full_global_snapshot_t);

    struct orte_snapc_full_local_snapshot_t {
        /** Base SNAPC Global snapshot type */
        orte_snapc_base_snapshot_t super;

        /** Named Pipe Read and Write */
        char * comm_pipe_r;
        char * comm_pipe_w;
        int    comm_pipe_r_fd;
        int    comm_pipe_w_fd;

        /* An opal event handle for the read pipe */
        struct opal_event comm_pipe_r_eh;
        bool is_eh_active;

        /** State of the process wrt checkpointing */
        int ckpt_state;
    };
    typedef struct orte_snapc_full_local_snapshot_t orte_snapc_full_local_snapshot_t;

    OBJ_CLASS_DECLARATION(orte_snapc_full_local_snapshot_t);

    extern bool orte_snapc_full_skip_filem;

    int orte_snapc_full_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int orte_snapc_full_module_init(bool seed, bool app);
    int orte_snapc_full_module_finalize(void);

    int orte_snapc_full_setup_job(orte_jobid_t jobid);
    int orte_snapc_full_release_job(orte_jobid_t jobid);

    int orte_snapc_full_ft_event(int state);

    /*
     * Global Coordinator Functionality
     */
    int global_coord_init(void);
    int global_coord_finalize(void);
    int global_coord_setup_job(orte_jobid_t jobid);
    int global_coord_release_job(orte_jobid_t jobid);
    int global_coord_vpid_assoc_update(orte_process_name_t local_coord,
                                       orte_process_name_t proc_name);
    int global_coord_vpid_state_update(orte_process_name_t proc_name,
                                       size_t proc_ckpt_state,
                                       char **proc_ckpt_ref,
                                       char **proc_ckpt_loc);
    /*
     * Local Coordinator Functionality
     */
    int local_coord_init(void);
    int local_coord_finalize(void);
    int local_coord_setup_job(orte_jobid_t jobid);
    int local_coord_release_job(orte_jobid_t jobid);
    int local_coord_job_state_update(orte_jobid_t jobid,
                                     size_t job_ckpt_state,
                                     char **job_ckpt_ref,
                                     char **job_ckpt_loc);

    /*
     * Application Coordinator Functionality
     */
    int app_coord_init(void);
    int app_coord_finalize(void);
    int app_coord_ft_event(int state);

END_C_DECLS

#endif /* MCA_SNAPC_FULL_EXPORT_H */

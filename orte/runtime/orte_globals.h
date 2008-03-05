/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Global params for OpenRTE
 */
#ifndef ORTE_RUNTIME_ORTE_GLOBALS_H
#define ORTE_RUNTIME_ORTE_GLOBALS_H

#include "orte_config.h"
#include "orte/types.h"

#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/util/proc_info.h"


#define ORTE_GLOBAL_ARRAY_BLOCK_SIZE    64
#define ORTE_GLOBAL_ARRAY_MAX_SIZE      INT_MAX

/* define a default error return code for ORTE */
#define ORTE_ERROR_DEFAULT_EXIT_CODE    1

BEGIN_C_DECLS

/* global type definitions used by RTE - instanced in orte_globals.c */

/************
* Declare this to allow us to use it before fully
* defining it - resolves potential circular definition
*/
struct orte_proc_t;
/************/

/** Value for orte_app_context_map_t: the data is uninitialized (!) */
#define ORTE_APP_CONTEXT_MAP_INVALID        0
/** Value for orte_app_context_map_t: the data is a comma-delimited
    string of hostnames */
#define ORTE_APP_CONTEXT_MAP_HOSTNAME       1
/** Value for orte_app_context_map_t: the data is a comma-delimited
    list of architecture names */
#define ORTE_APP_CONTEXT_MAP_ARCH           2
/** Value for orte_app_context_map_t: the data is a comma-delimited
    list of C, cX, N, nX mappsing */
#define ORTE_APP_CONTEXT_MAP_CN             3
/** Value for orte_app_context_map_t: the data is a comma-delimited
    string of hostnames that are to be added and used */
#define ORTE_APP_CONTEXT_MAP_ADD_HOSTNAME   4

/**
* Information about mapping requested by the user
 */
typedef struct {
    /** Parent object */
    opal_object_t super;
    /** One of the ORTE_APP_CONTEXT_MAP_* values */
    uint8_t map_type;
    /** String data */
    char *map_data;
} orte_app_context_map_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_app_context_map_t);


/**
* Information about a specific application to be launched in the RTE.
 */
typedef struct {
    /** Parent object */
    opal_object_t super;
    /** Unique index when multiple apps per job */
    orte_std_cntr_t idx;
    /** Absolute pathname of argv[0] */
    char   *app;
    /** Number of copies of this process that are to be launched */
    orte_std_cntr_t num_procs;
    /** Standard argv-style array, including a final NULL pointer */
    char  **argv;
    /** Standard environ-style array, including a final NULL pointer */
    char  **env;
    /** Current working directory for this app */
    char   *cwd;
    /** Whether the cwd was set by the user or by the system */
    bool user_specified_cwd;
    /* Any hostfile that was specified */
    char *hostfile;
    /* Hostfile for adding hosts to an existing allocation */
    char *add_hostfile;
    /** Length of the map_data array, not including the final NULL entry */
    orte_std_cntr_t num_map;
    /** Mapping data about how this app should be laid out across CPUs
        / nodes */
    orte_app_context_map_t **map_data;
    /** Prefix directory for this app (or NULL if no override
        necessary) */
    char *prefix_dir;
    /** Preload the binary on the remote machine (in PLS via FileM) */
    bool preload_binary;
    /** Preload the comma separated list of files to the remote machines cwd */
    char * preload_files;
    /** Destination directory for the preloaded files 
        * If NULL then the absolute and relative paths are obeyed */
    char * preload_files_dest_dir;
} orte_app_context_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_app_context_t);


typedef struct {
    /** Base object so this can be put on a list */
    opal_list_item_t super;
    /* index of this node object in global array */
    orte_std_cntr_t index;
    /** String node name */
    char *name;
    /* an id for the node in case we need it */
    orte_nodeid_t nodeid;
    /* whether or not this node is available for allocation */
    bool allocate;
    /* daemon on this node - it's vpid equates to the nodeid in many environments */
    struct orte_proc_t *daemon;
    /* whether or not this daemon has been launched */
    bool daemon_launched;
    /** Launch id - needed by some systems to launch a proc on this node */
    int32_t launch_id;
    /** number of procs on this node */
    orte_vpid_t num_procs;
    /* array of pointers to procs on this node */
    opal_pointer_array_t *procs;
    /* whether or not we are oversubscribed */
    bool oversubscribed;
    /** The node architecture, as reported by the remote node. This
        * value is a bit-map that identifies whether or not the node
        * is big/little endian, etc.
        */
    int32_t arch;
    /** State of this node */
    orte_node_state_t state;
    /** A "soft" limit on the number of slots available on the node.
        This will typically correspond to the number of physical CPUs
        that we have been allocated on this note and would be the
        "ideal" number of processes for us to launch. */
    orte_std_cntr_t slots;
    /** How many processes have already been launched, used by one or
        more jobs on this node. */
    orte_std_cntr_t slots_inuse;
    /** This represents the number of slots we (the allocator) are
        attempting to allocate to the current job - or the number of
        slots allocated to a specific job on a query for the jobs
        allocations */
    orte_std_cntr_t slots_alloc;
    /** A "hard" limit (if set -- a value of 0 implies no hard limit)
        on the number of slots that can be allocated on a given
        node. This is for some environments (e.g. grid) there may be
        fixed limits on the number of slots that can be used.
        
        This value also could have been a boolean - but we may want to
        allow the hard limit be different than the soft limit - in
        other words allow the node to be oversubscribed up to a
        specified limit.  For example, if we have two processors, we
        may want to allow up to four processes but no more. */
    orte_std_cntr_t slots_max;
    /** Username on this node, if specified */
    char *username;
} orte_node_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_node_t);

typedef struct {
    /** Base object so this can be put on a list */
    opal_list_item_t super;
    /* jobid for this job */
    orte_jobid_t jobid;
    /* app_context array for this job */
    opal_pointer_array_t *apps;
    /* number of app_contexts in the array */
    orte_std_cntr_t num_apps;
    /* whether or not this job is locally spawned */
    bool local_spawn;
    /* total slots allocated to this job */
    orte_std_cntr_t total_slots_alloc;
    /* number of procs in this job */
    orte_vpid_t num_procs;
    /* array of pointers to procs in this job */
    opal_pointer_array_t *procs;
    /* map of the job */
    orte_job_map_t *map;
    /* bookmark for where we are in mapping - this
     * indicates the node where we stopped
     */
    orte_node_t *bookmark;
    /** Whether or not to override oversubscription based on local
     *  hardware - used to indicate uncertainty in number of
     *  actual processors available on this node
     */
    bool oversubscribe_override;
    /* state of the overall job */
    orte_job_state_t state;
    /* number of procs launched */
    orte_vpid_t num_launched;
    /* number of procs reporting contact info */
    orte_vpid_t num_reported;
    /* number of procs terminated */
    orte_vpid_t num_terminated;
    /* did this job abort? */
    bool abort;
    /* proc that caused that to happen */
    struct orte_proc_t *aborted_proc;
#if OPAL_ENABLE_FT == 1
    /* ckpt state */
    size_t ckpt_state;
    /* snapshot reference */
    char *ckpt_snapshot_ref;
    /* snapshot location */
    char *ckpt_snapshot_loc;
#endif
} orte_job_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_job_t);

struct orte_proc_t {
    /** Base object so this can be put on a list */
    opal_list_item_t super;
    /* process name */
    orte_process_name_t name;
    /* pid */
    pid_t pid;
    /* local rank on the node where this is running */
    orte_vpid_t local_rank;
    /* process state */
    orte_proc_state_t state;
    /* exit code */
    orte_exit_code_t exit_code;
    /* the app_context that generated this proc */
    orte_std_cntr_t app_idx;
    /* a cpu list, if specified by the user */
    char *slot_list;
    /* pointer to the node where this proc is executing */
    orte_node_t *node;
    /* name of the node where this proc is executing - this
     * is used simply to pass that info to a calling
     * tool since it may not have a node array available
     */
    char *nodename;
    /* RML contact info */
    char *rml_uri;
#if OPAL_ENABLE_FT == 1
    /* ckpt state */
    size_t ckpt_state;
    /* snapshot reference */
    char *ckpt_snapshot_ref;
    /* snapshot location */
    char *ckpt_snapshot_loc;
#endif
};
typedef struct orte_proc_t orte_proc_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_proc_t);

/**
* Get a job data object
 * We cannot just reference a job data object with its jobid as
 * the jobid is no longer an index into the array. This change
 * was necessitated by modification of the jobid to include
 * an mpirun-unique qualifer to eliminate any global name
 * service
 */
ORTE_DECLSPEC   orte_job_t* orte_get_job_data_object(orte_jobid_t job);

/*
 * Shortcut for some commonly used names
 */
#define ORTE_NAME_WILDCARD      (&orte_globals_name_wildcard)
ORTE_DECLSPEC extern orte_process_name_t orte_globals_name_wildcard;  /** instantiated in orte/runtime/orte_globals.c */

#define ORTE_NAME_INVALID       (&orte_globals_name_invalid)
ORTE_DECLSPEC extern orte_process_name_t orte_globals_name_invalid;  /** instantiated in orte/runtime/orte_globals.c */

#define ORTE_PROC_MY_NAME       (&orte_process_info.my_name)

/* define a special name that belongs to orterun */
#define ORTE_PROC_MY_HNP        (&orte_process_info.my_hnp)

/* define the name of my daemon */
#define ORTE_PROC_MY_DAEMON     (&orte_process_info.my_daemon)


/* global variables used by RTE - instanced in orte_globals.c */
ORTE_DECLSPEC extern bool orte_debug_flag, orte_reuse_daemons, orte_timing;
ORTE_DECLSPEC extern bool orte_debug_daemons_flag, orte_debug_daemons_file_flag;
ORTE_DECLSPEC extern bool orted_spin_flag;

ORTE_DECLSPEC extern int orte_debug_output;

ORTE_DECLSPEC extern bool orte_daemon_died;

ORTE_DECLSPEC extern char **orte_launch_environ;
ORTE_DECLSPEC extern char **orted_cmd_line;
ORTE_DECLSPEC extern int orte_exit, orteds_exit;
ORTE_DECLSPEC extern int orte_exit_status;
ORTE_DECLSPEC extern bool orte_abnormal_term_ordered;
ORTE_DECLSPEC extern bool orte_abort_in_progress;
ORTE_DECLSPEC extern bool orte_wakeup_ordered;
ORTE_DECLSPEC extern int orte_timeout_usec_per_proc;
ORTE_DECLSPEC extern float orte_max_timeout;

/* global arrays for data storage */
ORTE_DECLSPEC extern opal_pointer_array_t *orte_job_data;
ORTE_DECLSPEC extern opal_pointer_array_t *orte_node_pool;

/**
 * Whether ORTE is initialized or we are in orte_finalize
 */
ORTE_DECLSPEC extern bool orte_initialized;
ORTE_DECLSPEC extern bool orte_finalizing;

END_C_DECLS

#endif /* ORTE_RUNTIME_ORTE_GLOBALS_H */

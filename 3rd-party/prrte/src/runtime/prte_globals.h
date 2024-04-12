/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2020 IBM Corporation.  All rights reserved.
 * Copyright (c) 2017-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Global params for PRTE
 */
#ifndef PRTE_RUNTIME_PRTE_GLOBALS_H
#define PRTE_RUNTIME_PRTE_GLOBALS_H

#include "prte_config.h"
#include "types.h"

#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif

#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_pointer_array.h"
#include "src/class/pmix_ring_buffer.h"
#include "src/class/pmix_value_array.h"
#include "src/event/event-internal.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/pmix/pmix-internal.h"
#include "src/threads/pmix_threads.h"

#include "src/mca/plm/plm_types.h"
#include "src/rml/rml_types.h"
#include "src/runtime/runtime.h"
#include "src/util/attr.h"
#include "src/util/pmix_cmd_line.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"

BEGIN_C_DECLS

PRTE_EXPORT extern int prte_debug_verbosity;           /* instantiated in src/runtime/prte_init.c */
PRTE_EXPORT extern char *prte_prohibited_session_dirs; /* instantiated in src/runtime/prte_init.c */
PRTE_EXPORT extern char *prte_job_ident;           /* instantiated in src/runtime/prte_globals.c */
PRTE_EXPORT extern bool prte_execute_quiet;        /* instantiated in src/runtime/prte_globals.c */
PRTE_EXPORT extern bool prte_report_silent_errors; /* instantiated in src/runtime/prte_globals.c */
PRTE_EXPORT extern bool prte_event_base_active;    /* instantiated in src/runtime/prte_init.c */
PRTE_EXPORT extern bool prte_proc_is_bound;        /* instantiated in src/runtime/prte_init.c */
PRTE_EXPORT extern int prte_progress_thread_debug; /* instantiated in src/runtime/prte_init.c */
PRTE_EXPORT extern char *prte_tool_basename;       // argv[0] of prun or one of its symlinks
PRTE_EXPORT extern char *prte_tool_actual;         // actual tool executable
PRTE_EXPORT extern char *prte_progress_thread_cpus;
PRTE_EXPORT extern bool prte_bind_progress_thread_reqd;
PRTE_EXPORT extern bool prte_show_launch_progress;
PRTE_EXPORT extern bool prte_bootstrap_setup;
PRTE_EXPORT extern bool prte_silence_shared_fs;

/**
 * Global indicating where this process was bound to at launch (will
 * be NULL if !prte_proc_is_bound)
 */
PRTE_EXPORT extern hwloc_cpuset_t
    prte_proc_applied_binding; /* instantiated in src/runtime/prte_init.c */

/* Shortcut for some commonly used names */
#define PRTE_NAME_WILDCARD (&prte_name_wildcard)
PRTE_EXPORT extern pmix_proc_t prte_name_wildcard; /** instantiated in src/runtime/prte_init.c */
#define PRTE_NAME_INVALID (&prte_name_invalid)
PRTE_EXPORT extern pmix_proc_t prte_name_invalid; /** instantiated in src/runtime/prte_init.c */
#define PRTE_JOBID_WILDCARD (prte_nspace_wildcard)
PRTE_EXPORT extern pmix_nspace_t
    prte_nspace_wildcard; /** instantiated in src/runtime/prte_init.c */

#define PRTE_PROC_MY_NAME   (&prte_process_info.myproc)
#define PRTE_PROC_MY_PROCID (&prte_process_info.myproc) // backward compatibility synonym

/* define a special name that point to my parent (aka the process that spawned me) */
#define PRTE_PROC_MY_PARENT (&prte_process_info.my_parent)

/* define a special name that belongs to prte master */
#define PRTE_PROC_MY_HNP (&prte_process_info.my_hnp)

/* define some types so we can store the generic
 * values and still *know* how to convert it for PMIx */
typedef int prte_status_t;
typedef uint32_t prte_proc_state_t; // assigned values in src/mca/plm/plm_types.h
#define PRTE_PROC_STATE_T PMIX_UINT32

/* define the results values for comparisons so we can change them in only one place */
#define PRTE_VALUE1_GREATER +1
#define PRTE_VALUE2_GREATER -1
#define PRTE_EQUAL          0

/* error manager callback function */
typedef void (*prte_err_cb_fn_t)(pmix_proc_t *proc, prte_proc_state_t state, void *cbdata);

/* define an object for timer events */
typedef struct {
    pmix_object_t super;
    struct timeval tv;
    prte_event_t *ev;
    void *payload;
} prte_timer_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_timer_t);

PRTE_EXPORT extern int prte_exit_status;

/* define some common keys used in PRTE */
#define PRTE_DB_DAEMON_VPID "prte.daemon.vpid"

/* State Machine lists */
PRTE_EXPORT extern pmix_list_t prte_job_states;
PRTE_EXPORT extern pmix_list_t prte_proc_states;

/* a clean output channel without prefix */
PRTE_EXPORT extern int prte_clean_output;

#define PRTE_GLOBAL_ARRAY_BLOCK_SIZE 64
#define PRTE_GLOBAL_ARRAY_MAX_SIZE   INT_MAX

/* define a default error return code for PRTE */
#define PRTE_ERROR_DEFAULT_EXIT_CODE 1

/**
 * Define a macro for updating the prte_exit_status
 * The macro provides a convenient way of doing this
 * so that we can add thread locking at some point
 * since the prte_exit_status is a global variable.
 *
 * Ensure that we do not overwrite the exit status if it has
 * already been set to some non-zero value. If we don't make
 * this check, then different parts of the code could overwrite
 * each other's exit status in the case of abnormal termination.
 *
 * For example, if a process aborts, we would record the initial
 * exit code from the aborted process. However, subsequent processes
 * will have been aborted by signal as we kill the job. We don't want
 * the subsequent processes to overwrite the original exit code so
 * we can tell the user the exit code from the process that caused
 * the whole thing to happen.
 */
#define PRTE_UPDATE_EXIT_STATUS(newstatus)                                                     \
    do {                                                                                       \
        if (0 == prte_exit_status && 0 != newstatus) {                                         \
            PMIX_OUTPUT_VERBOSE((1, prte_debug_output, "%s:%s(%d) updating exit status to %d", \
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,       \
                                 newstatus));                                                  \
            prte_exit_status = newstatus;                                                      \
        }                                                                                      \
    } while (0);

/* sometimes we need to reset the exit status - for example, when we
 * are restarting a failed process
 */
#define PRTE_RESET_EXIT_STATUS()                                                       \
    do {                                                                               \
        PMIX_OUTPUT_VERBOSE((1, prte_debug_output, "%s:%s(%d) reseting exit status",   \
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__)); \
        prte_exit_status = 0;                                                          \
    } while (0);

/* define a set of flags to control the launch of a job */
typedef uint16_t prte_job_controls_t;
#define PRTE_JOB_CONTROL PRTE_UINT16

/* global type definitions used by RTE - instanced in prte_globals.c */

/************
 * Declare this to allow us to use it before fully
 * defining it - resolves potential circular definition
 */
struct prte_proc_t;
struct prte_job_t;
struct prte_job_map_t;
struct prte_schizo_base_module_t;

/************/

/* define an object for storing node topologies */
typedef struct {
    pmix_object_t super;
    int index;
    hwloc_topology_t topo;
    char *sig;
} prte_topology_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_topology_t);

/**
 * Information about a specific application to be launched in the RTE.
 */
typedef struct {
    /** Parent object */
    pmix_object_t super;
    /** the job this app belongs to */
    struct prte_job_t *job;
    /** Unique index when multiple apps per job */
    prte_app_idx_t idx;
    /** Absolute pathname of argv[0] */
    char *app;
    /** Number of copies of this process that are to be launched */
    int32_t num_procs;
    /** Array of pointers to the proc objects for procs of this app_context
     * NOTE - not always used
     */
    pmix_pointer_array_t procs;
    /** State of the app_context */
    prte_app_state_t state;
    /** First MPI rank of this app_context in the job */
    pmix_rank_t first_rank;
    /** Standard argv-style array, including a final NULL pointer */
    char **argv;
    /** Standard environ-style array, including a final NULL pointer */
    char **env;
    /** Current working directory for this app */
    char *cwd;
    /* flags */
    prte_app_context_flags_t flags;
    /* provide a list of attributes for this app_context in place
     * of having a continually-expanding list of fixed-use values.
     * This is a list of prte_value_t's, with the intent of providing
     * flexibility without constantly expanding the memory footprint
     * every time we want some new (rarely used) option
     */
    pmix_list_t attributes;
    // store the result of parsing this app's cmd line
    pmix_cli_result_t cli;
} prte_app_context_t;

PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_app_context_t);

typedef struct {
    /** Base object so this can be put on a list */
    pmix_list_item_t super;
    /* index of this node object in global array */
    int32_t index;
    /** String node name */
    char *name;
    char *rawname;  // name originally given in allocation, if different from name
    /** aliases */
    char **aliases;
    /* daemon on this node */
    struct prte_proc_t *daemon;
    /* track the unassigned cpus */
    hwloc_cpuset_t available;
    /* cache the cpuset prior to mapping a job for easy reset */
    hwloc_cpuset_t jobcache;
    /** number of procs on this node */
    prte_node_rank_t num_procs;
    /* array of pointers to procs on this node */
    pmix_pointer_array_t *procs;
    /* next node rank on this node */
    prte_node_rank_t next_node_rank;
    /** State of this node */
    prte_node_state_t state;
    /** A "soft" limit on the number of slots available on the node.
        This will typically correspond to the number of physical CPUs
        that we have been allocated on this note and would be the
        "ideal" number of processes for us to launch. */
    int32_t slots;
    /** Slots available for use in the current mapping operation. This
     *  may differ on a per-job basis from the overall allocated slots
     *  thru use of the -host option and possibly other means */
    int32_t slots_available;
    /** How many processes have already been launched, used by one or
        more jobs on this node. */
    int32_t slots_inuse;
    /** A "hard" limit (if set -- a value of 0 implies no hard limit)
        on the number of slots that can be allocated on a given
        node. This is for some environments (e.g. grid) there may be
        fixed limits on the number of slots that can be used.

        This value also could have been a boolean - but we may want to
        allow the hard limit be different than the soft limit - in
        other words allow the node to be oversubscribed up to a
        specified limit.  For example, if we have two processors, we
        may want to allow up to four processes but no more. */
    int32_t slots_max;
    /* system topology for this node */
    prte_topology_t *topology;
    /* flags */
    prte_node_flags_t flags;
    /* list of prte_attribute_t */
    pmix_list_t attributes;
} prte_node_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_node_t);

typedef struct {
    /** Base object so this can be put on a list */
    pmix_list_item_t super;
    /* record the exit status for this job */
    int exit_code;
    /* personality for this job */
    char **personality;
    struct prte_schizo_base_module_t *schizo;
    /* jobid for this job */
    pmix_nspace_t nspace;
    // session directory for this job
    char *session_dir;
    int index; // index in the job array where this is stored
    /* offset to the total number of procs so shared memory
     * components can potentially connect to any spawned jobs*/
    pmix_rank_t offset;
    /* app_context array for this job */
    pmix_pointer_array_t *apps;
    /* number of app_contexts in the array */
    prte_app_idx_t num_apps;
    /* rank desiring stdin - for now, either one rank, all ranks
     * (wildcard), or none (invalid)
     */
    pmix_rank_t stdin_target;
    /* total slots allocated to this job */
    int32_t total_slots_alloc;
    /* number of procs in this job */
    pmix_rank_t num_procs;
    /* array of pointers to procs in this job */
    pmix_pointer_array_t *procs;
    /* map of the job */
    struct prte_job_map_t *map;
    /* bookmark for where we are in mapping - this
     * indicates the node where we stopped
     */
    prte_node_t *bookmark;
    /* state of the overall job */
    prte_job_state_t state;
    /* number of procs mapped */
    pmix_rank_t num_mapped;
    /* number of procs launched */
    pmix_rank_t num_launched;
    /* number of procs reporting contact info */
    pmix_rank_t num_reported;
    /* number of procs terminated */
    pmix_rank_t num_terminated;
    /* number of daemons reported launched so we can track progress */
    pmix_rank_t num_daemons_reported;
    /* number of procs ready for debug */
    pmix_rank_t num_ready_for_debug;
    /* originator of a dynamic spawn */
    pmix_proc_t originator;
    /* number of local procs */
    pmix_rank_t num_local_procs;
    /* flags */
    prte_job_flags_t flags;
    /* attributes */
    pmix_list_t attributes;
    /* launch msg buffer */
    pmix_data_buffer_t launch_msg;
    /* track children of this job */
    pmix_list_t children;
    /* track the launcher of these jobs */
    pmix_nspace_t launcher;
    /* track the number of stack traces recv'd */
    uint32_t ntraces;
    char **traces;
    // store the result of parsing this app's cmd line
    pmix_cli_result_t cli;
} prte_job_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_job_t);

struct prte_proc_t {
    /** Base object so this can be put on a list */
    pmix_list_item_t super;
    /* process name */
    pmix_proc_t name;
    /* the vpid of my parent - the daemon vpid for an app
     * or the vpid of the parent in the routing tree of
     * a daemon */
    pmix_rank_t parent;
    /* pid */
    pid_t pid;
    /* local rank amongst my peers on the node
     * where this is running - this value is
     * needed by MPI procs so that the lowest
     * rank on a node can perform certain fns -
     * e.g., open an sm backing file
     */
    prte_local_rank_t local_rank;
    /* local rank on the node across all procs
     * and jobs known to this HNP - this is
     * needed so that procs can do things like
     * know which static IP port to use
     */
    prte_node_rank_t node_rank;
    /* rank of this proc within its app context - this
     * will just equal its vpid for single app_context
     * applications
     */
    int32_t app_rank;
    /* rank of this proc amongst its peers within the
     * NUMA region to which it is bound */
    prte_local_rank_t numa_rank;
    /* Last state used to trigger the errmgr for this proc */
    prte_proc_state_t last_errmgr_state;
    /* process state */
    prte_proc_state_t state;
    /* exit code */
    prte_exit_code_t exit_code;
    /* the app_context that generated this proc */
    prte_app_idx_t app_idx;
    /* pointer to the node where this proc is executing */
    prte_node_t *node;
    /* pointer to the object on that node where the
     * proc is mapped */
    hwloc_obj_t obj;
    /* cpuset where the proc is bound */
    char *cpuset;
    /* RML contact info */
    char *rml_uri;
    /* some boolean flags */
    prte_proc_flags_t flags;
    /* list of prte_value_t attributes */
    pmix_list_t attributes;
};
typedef struct prte_proc_t prte_proc_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_proc_t);

/**
 * Get a job data object
 * We cannot just reference a job data object with its jobid as
 * the jobid is no longer an index into the array. This change
 * was necessitated by modification of the jobid to include
 * an mpirun-unique qualifer to eliminate any global name
 * service
 */
PRTE_EXPORT prte_job_t *prte_get_job_data_object(const pmix_nspace_t job);

/**
 * Set a job data object - returns an error if it cannot add the object
 * to the array
 */
PRTE_EXPORT int prte_set_job_data_object(prte_job_t *jdata);

/** Pack/unpack a job object */
PRTE_EXPORT int prte_job_pack(pmix_data_buffer_t *bkt, prte_job_t *job);
PRTE_EXPORT int prte_job_unpack(pmix_data_buffer_t *bkt, prte_job_t **job);
PRTE_EXPORT int prte_job_copy(prte_job_t **dest, prte_job_t *src);
PRTE_EXPORT void prte_job_print(char **output, prte_job_t *jdata);

/** Pack/unpack an app-context */
PRTE_EXPORT int prte_app_pack(pmix_data_buffer_t *bkt, prte_app_context_t *app);
PRTE_EXPORT int prte_app_unpack(pmix_data_buffer_t *bkt, prte_app_context_t **app);
PRTE_EXPORT int prte_app_copy(prte_app_context_t **dest, prte_app_context_t *src);
PRTE_EXPORT void prte_app_print(char **output, prte_job_t *jdata, prte_app_context_t *src);

/** Pack/unpack a proc*/
PRTE_EXPORT int prte_proc_pack(pmix_data_buffer_t *bkt, prte_proc_t *proc);
PRTE_EXPORT int prte_proc_unpack(pmix_data_buffer_t *bkt, prte_proc_t **proc);
PRTE_EXPORT int prte_proc_copy(prte_proc_t **dest, prte_proc_t *src);
PRTE_EXPORT void prte_proc_print(char **output, prte_job_t *jdata, prte_proc_t *src);

/** Pack/unpack a job map */
PRTE_EXPORT int prte_map_pack(pmix_data_buffer_t *bkt, struct prte_job_map_t *map);
PRTE_EXPORT int prte_map_unpack(pmix_data_buffer_t *bkt, struct prte_job_map_t **map);
PRTE_EXPORT int prte_map_copy(struct prte_job_map_t **dest, struct prte_job_map_t *src);
PRTE_EXPORT void prte_map_print(char **output, prte_job_t *jdata);

PRTE_EXPORT int prte_node_pack(pmix_data_buffer_t *bkt, prte_node_t *node);
PRTE_EXPORT int prte_node_unpack(pmix_data_buffer_t *bkt, prte_node_t **node);
PRTE_EXPORT int prte_node_copy(prte_node_t **dest, prte_node_t *src);
PRTE_EXPORT void prte_node_print(char **output, prte_job_t *jdata, prte_node_t *src);

/**
 * Get a proc data object
 */
PRTE_EXPORT prte_proc_t *prte_get_proc_object(const pmix_proc_t *proc);

/**
 * Get the daemon vpid hosting a given proc
 */
PRTE_EXPORT pmix_rank_t prte_get_proc_daemon_vpid(const pmix_proc_t *proc);

/* Get the hostname of a proc */
PRTE_EXPORT char *prte_get_proc_hostname(const pmix_proc_t *proc);

/* get the node rank of a proc */
PRTE_EXPORT prte_node_rank_t prte_get_proc_node_rank(const pmix_proc_t *proc);

/* check to see if two nodes match */
PRTE_EXPORT prte_node_t* prte_node_match(pmix_list_t *nodes, const char *name);
PRTE_EXPORT bool prte_nptr_match(prte_node_t *n1, prte_node_t *n2);

/* global variables used by RTE - instanced in prte_globals.c */
PRTE_EXPORT extern bool prte_debug_daemons_flag;
PRTE_EXPORT extern bool prte_debug_daemons_file_flag;
PRTE_EXPORT extern bool prte_leave_session_attached;
PRTE_EXPORT extern char *prte_topo_signature;
PRTE_EXPORT extern char *prte_data_server_uri;
PRTE_EXPORT extern bool prte_dvm_ready;
PRTE_EXPORT extern pmix_pointer_array_t *prte_cache;
PRTE_EXPORT extern bool prte_persistent;
PRTE_EXPORT extern bool prte_allow_run_as_root;
PRTE_EXPORT extern bool prte_fwd_environment;

/* PRTE OOB port flags */
PRTE_EXPORT extern bool prte_static_ports;
PRTE_EXPORT extern char *prte_oob_static_ports;

/* nodename flags */
PRTE_EXPORT extern bool prte_keep_fqdn_hostnames;
PRTE_EXPORT extern bool prte_have_fqdn_allocation;
PRTE_EXPORT extern bool prte_show_resolved_nodenames;
PRTE_EXPORT extern int prte_hostname_cutoff;
PRTE_EXPORT extern bool prte_do_not_resolve;

/* debug flags */
PRTE_EXPORT extern pmix_rank_t prted_debug_failure;
PRTE_EXPORT extern int prted_debug_failure_delay;

PRTE_EXPORT extern bool prte_never_launched;
PRTE_EXPORT extern bool prte_devel_level_output;
PRTE_EXPORT extern bool prte_display_topo_with_map;

PRTE_EXPORT extern char **prte_launch_environ;

PRTE_EXPORT extern bool prte_hnp_is_allocated;
PRTE_EXPORT extern bool prte_allocation_required;
PRTE_EXPORT extern bool prte_managed_allocation;
PRTE_EXPORT extern char *prte_set_slots;
PRTE_EXPORT extern bool prte_set_slots_override;
PRTE_EXPORT extern bool prte_hnp_connected;
PRTE_EXPORT extern bool prte_nidmap_communicated;
PRTE_EXPORT extern bool prte_node_info_communicated;

/* launch agents */
PRTE_EXPORT extern char *prte_launch_agent;
PRTE_EXPORT extern char **prted_cmd_line;

/* exit flags */
PRTE_EXPORT extern bool prte_abnormal_term_ordered;
PRTE_EXPORT extern bool prte_routing_is_enabled;
PRTE_EXPORT extern bool prte_dvm_abort_ordered;
PRTE_EXPORT extern bool prte_prteds_term_ordered;
PRTE_EXPORT extern bool prte_allowed_exit_without_sync;

PRTE_EXPORT extern int prte_timeout_usec_per_proc;
PRTE_EXPORT extern float prte_max_timeout;
PRTE_EXPORT extern prte_timer_t *prte_mpiexec_timeout;

/* global arrays for data storage */
PRTE_EXPORT extern pmix_pointer_array_t *prte_job_data;
PRTE_EXPORT extern pmix_pointer_array_t *prte_node_pool;
PRTE_EXPORT extern pmix_pointer_array_t *prte_node_topologies;
PRTE_EXPORT extern pmix_pointer_array_t *prte_local_children;
PRTE_EXPORT extern pmix_rank_t prte_total_procs;
PRTE_EXPORT extern char *prte_base_compute_node_sig;
PRTE_EXPORT extern bool prte_hetero_nodes;

/* IOF controls */
/* generate new xterm windows to display output from specified ranks */
PRTE_EXPORT extern char *prte_xterm;

/* whether or not to report launch progress */
PRTE_EXPORT extern bool prte_report_launch_progress;

/* allocation specification */
PRTE_EXPORT extern char *prte_default_hostfile;
PRTE_EXPORT extern bool prte_default_hostfile_given;
PRTE_EXPORT extern int prte_num_allocated_nodes;
PRTE_EXPORT extern char *prte_default_dash_host;

/* tool communication controls */
PRTE_EXPORT extern bool prte_report_events;
PRTE_EXPORT extern char *prte_report_events_uri;

/* exit status reporting */
PRTE_EXPORT extern bool prte_report_child_jobs_separately;
PRTE_EXPORT extern struct timeval prte_child_time_to_exit;

/* length of stat history to keep */
PRTE_EXPORT extern int prte_stat_history_size;

/* envars to forward */
PRTE_EXPORT extern char **prte_forwarded_envars;

/* maximum size of virtual machine - used to subdivide allocation */
PRTE_EXPORT extern int prte_max_vm_size;

/* binding directives for daemons to restrict them
 * to certain cores
 */
PRTE_EXPORT extern char *prte_daemon_cores;

/* Max time to wait for stack straces to return */
PRTE_EXPORT extern int prte_stack_trace_wait_timeout;

/* whether or not hwloc shmem support is available */
PRTE_EXPORT extern bool prte_hwloc_shmem_available;

extern char *prte_signal_string;
extern char *prte_stacktrace_output_filename;
extern char *prte_net_private_ipv4;
extern char *prte_set_max_sys_limits;
extern char *prte_if_include;
extern char *prte_if_exclude;

#if PRTE_PICKY_COMPILERS
#define PRTE_HIDE_UNUSED_PARAMS(...)                \
    do {                                            \
        int __x = 3;                                \
        prte_hide_unused_params(__x, __VA_ARGS__);  \
} while(0)

PMIX_EXPORT void prte_hide_unused_params(int x, ...);

#else
#define PRTE_HIDE_UNUSED_PARAMS(...)
#endif

END_C_DECLS

#endif /* PRTE_RUNTIME_PRTE_GLOBALS_H */

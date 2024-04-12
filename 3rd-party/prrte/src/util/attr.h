/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_ATTRS_H
#define PRTE_ATTRS_H

#include "prte_config.h"
#include "types.h"

/*** FLAG FOR SETTING ATTRIBUTES - INDICATES IF THE
 *** ATTRIBUTE IS TO BE SHARED WITH REMOTE PROCS OR NOT
 */
#define PRTE_ATTR_LOCAL  true  // for local use only
#define PRTE_ATTR_GLOBAL false // include when sending this object

/* define the mininum value of the PRTE keys just in
 * case someone someday puts a layer underneath us */
#define PRTE_ATTR_KEY_BASE 0

/*** ATTRIBUTE FLAGS - never sent anywwhere ***/
typedef uint8_t prte_app_context_flags_t;
#define PRTE_APP_FLAG_USED_ON_NODE  0x01    // is being used on the local node
#define PRTE_APP_FLAG_TOOL          0x02    // this app describes daemons to be co-launched
                                            //    with the application procs in the other apps
                                            //    and does not count against allocation

/* APP_CONTEXT ATTRIBUTE KEYS */
#define PRTE_APP_HOSTFILE            1  // string  - hostfile
#define PRTE_APP_ADD_HOSTFILE        2  // string  - hostfile to be added
#define PRTE_APP_DASH_HOST           3  // string  - hosts specified with -host option
#define PRTE_APP_ADD_HOST            4  // string  - hosts to be added
#define PRTE_APP_USER_CWD            5  // bool  - user specified cwd
#define PRTE_APP_SSNDIR_CWD          6  // bool  - use session dir as cwd
#define PRTE_APP_PRELOAD_BIN         7  // bool  - move binaries to remote nodes prior to exec
#define PRTE_APP_PRELOAD_FILES       8  // string  - files to be moved to remote nodes prior to exec
#define PRTE_APP_SSTORE_LOAD         9  // string
#define PRTE_APP_RECOV_DEF          10 // bool  - whether or not a recovery policy was defined
#define PRTE_APP_MAX_RESTARTS       11 // int32 - max number of times a process can be restarted
#define PRTE_APP_MIN_NODES          12 // int64 - min number of nodes required
#define PRTE_APP_MANDATORY          13 // bool - flag if nodes requested in -host are "mandatory" vs "optional"
#define PRTE_APP_MAX_PPN            14 // uint32 - maximum number of procs/node for this app
#define PRTE_APP_PREFIX_DIR         15 // string - prefix directory for this app, if override necessary
#define PRTE_APP_NO_CACHEDIR        16 // bool - flag that a cache dir is not to be specified for a Singularity container
#define PRTE_APP_SET_ENVAR          17 // prte_envar_t - set the given envar to the specified value
#define PRTE_APP_UNSET_ENVAR        18 // string - name of envar to unset, if present
#define PRTE_APP_PREPEND_ENVAR      19 // prte_envar_t - prepend the specified value to the given envar
#define PRTE_APP_APPEND_ENVAR       20 // prte_envar_t - append the specified value to the given envar
#define PRTE_APP_ADD_ENVAR          21 // prte_envar_t - add envar, do not override pre-existing one
#define PRTE_APP_PSET_NAME          23 // string - user-assigned name for the process
                                       //          set containing the given process

#define PRTE_APP_MAX_KEY 100

/*** NODE FLAGS - never sent anywhere ***/
typedef uint8_t prte_node_flags_t;
#define PRTE_NODE_FLAG_DAEMON_LAUNCHED  0x01 // whether or not the daemon on this node has been launched
#define PRTE_NODE_FLAG_LOC_VERIFIED     0x02 // whether or not the location has been verified - used for
                                             // environments where the daemon's final destination is uncertain
#define PRTE_NODE_FLAG_OVERSUBSCRIBED   0x04 // whether or not this node is oversubscribed
#define PRTE_NODE_FLAG_MAPPED           0x08 // whether we have been added to the current map
#define PRTE_NODE_FLAG_SLOTS_GIVEN      0x10 // the number of slots was specified - used only in non-managed environments
#define PRTE_NODE_NON_USABLE            0x20 // the node is hosting a tool and is NOT to be used for jobs

/*** NODE ATTRIBUTE KEYS - never sent anywhere ***/
#define PRTE_NODE_START_KEY PRTE_APP_MAX_KEY

#define PRTE_NODE_USERNAME      (PRTE_NODE_START_KEY + 1)
#define PRTE_NODE_LAUNCH_ID     (PRTE_NODE_START_KEY + 2) // int32 - Launch id needed by some systems to launch a proc on this node
#define PRTE_NODE_HOSTID        (PRTE_NODE_START_KEY + 3) // pmix_rank_t - if this "node" is a coprocessor being hosted on a different node, then
                                                          // we need to know the id of our "host" to help any procs on us to determine locality
#define PRTE_NODE_SERIAL_NUMBER (PRTE_NODE_START_KEY + 5) // string - serial number: used if node is a coprocessor
#define PRTE_NODE_PORT          (PRTE_NODE_START_KEY + 6) // int32 - Alternate port to be passed to plm
#define PRTE_NODE_ADD_SLOTS     (PRTE_NODE_START_KEY + 7) // bool - slots are being added to existing node

#define PRTE_NODE_MAX_KEY (PRTE_NODE_START_KEY + 100)

/*** JOB FLAGS - included in prte_job_t transmissions ***/
typedef uint16_t prte_job_flags_t;
#define PRTE_JOB_FLAGS_T PRTE_UINT16
#define PRTE_JOB_FLAG_UPDATED           0x0001 // job has been updated and needs to be included in the pidmap message
#define PRTE_JOB_FLAG_RESTARTED         0x0004 // some procs in this job are being restarted
#define PRTE_JOB_FLAG_ABORTED           0x0008 // did this job abort?
#define PRTE_JOB_FLAG_FORWARD_OUTPUT    0x0020 // forward output from the apps
#define PRTE_JOB_FLAG_DO_NOT_MONITOR    0x0040 // do not monitor apps for termination
#define PRTE_JOB_FLAG_FORWARD_COMM      0x0080 //
#define PRTE_JOB_FLAG_RESTART           0x0200 //
#define PRTE_JOB_FLAG_PROCS_MIGRATING   0x0400 // some procs in job are migrating from one node to another
#define PRTE_JOB_FLAG_OVERSUBSCRIBED    0x0800 // at least one node in the job is oversubscribed
#define PRTE_JOB_FLAG_TOOL              0x1000 // job is a tool and doesn't count against allocations
#define PRTE_JOB_FLAG_LAUNCHER          0x2000 // job is also a launcher
#define PRTE_JOB_FLAG_ERR_REPORTED      0x4000 // error report for job has been output

/***   JOB ATTRIBUTE KEYS   ***/
#define PRTE_JOB_START_KEY PRTE_NODE_MAX_KEY

#define PRTE_JOB_LAUNCH_MSG_SENT            (PRTE_JOB_START_KEY +   1) // timeval - time launch message was sent
#define PRTE_JOB_LAUNCH_MSG_RECVD           (PRTE_JOB_START_KEY +   2) // timeval - time launch message was recvd
#define PRTE_JOB_MAX_LAUNCH_MSG_RECVD       (PRTE_JOB_START_KEY +   3) // timeval - max time for launch msg to be received
#define PRTE_JOB_CKPT_STATE                 (PRTE_JOB_START_KEY +   5) // size_t - ckpt state
#define PRTE_JOB_SNAPSHOT_REF               (PRTE_JOB_START_KEY +   6) // string - snapshot reference
#define PRTE_JOB_SNAPSHOT_LOC               (PRTE_JOB_START_KEY +   7) // string - snapshot location
#define PRTE_JOB_SNAPC_INIT_BAR             (PRTE_JOB_START_KEY +   8) // prte_grpcomm_coll_id_t - collective id
#define PRTE_JOB_SNAPC_FINI_BAR             (PRTE_JOB_START_KEY +   9) // prte_grpcomm_coll_id_t - collective id
#define PRTE_JOB_NUM_NONZERO_EXIT           (PRTE_JOB_START_KEY +  10) // int32 - number of procs with non-zero exit codes
#define PRTE_SPAWN_TIMEOUT_EVENT            (PRTE_JOB_START_KEY +  11) // prte_ptr (prte_timer_t*) - timer event for failure detect/response
                                                                       // if fails to launch
#define PRTE_JOB_ABORTED_PROC               (PRTE_JOB_START_KEY +  12) // prte_ptr (prte_proc_t*) - proc that caused abort to happen
#define PRTE_JOB_MAPPER                     (PRTE_JOB_START_KEY +  13) // bool - job consists of MapReduce mappers
#define PRTE_JOB_REDUCER                    (PRTE_JOB_START_KEY +  14) // bool - job consists of MapReduce reducers
#define PRTE_JOB_COMBINER                   (PRTE_JOB_START_KEY +  15) // bool - job consists of MapReduce combiners
#define PRTE_JOB_INDEX_ARGV                 (PRTE_JOB_START_KEY +  16) // bool - automatically index argvs
#define PRTE_JOB_NO_VM                      (PRTE_JOB_START_KEY +  17) // bool - do not use VM launch
#define PRTE_JOB_SPIN_FOR_DEBUG             (PRTE_JOB_START_KEY +  18) // bool - the prted's are to spin while waiting for debugger
#define PRTE_JOB_CONTINUOUS                 (PRTE_JOB_START_KEY +  19) // bool - job consists of continuously operating apps
#define PRTE_JOB_RECOVER_DEFINED            (PRTE_JOB_START_KEY +  20) // bool - recovery policy has been defined
#define PRTE_JOB_NON_PRTE_JOB               (PRTE_JOB_START_KEY +  22) // bool - non-prte job
#define PRTE_JOB_STDOUT_TARGET              (PRTE_JOB_START_KEY +  23) // pmix_nspace_t - job that is to receive the stdout (on its
                                                                       //      stdin) from this one
#define PRTE_JOB_POWER                      (PRTE_JOB_START_KEY +  24) // string - power setting for nodes in job
#define PRTE_JOB_MAX_FREQ                   (PRTE_JOB_START_KEY +  25) // string - max freq setting for nodes in job
#define PRTE_JOB_MIN_FREQ                   (PRTE_JOB_START_KEY +  26) // string - min freq setting for nodes in job
#define PRTE_JOB_GOVERNOR                   (PRTE_JOB_START_KEY +  27) // string - governor used for nodes in job
#define PRTE_JOB_FAIL_NOTIFIED              (PRTE_JOB_START_KEY +  28) // bool - abnormal term of proc within job has been reported
#define PRTE_JOB_TERM_NOTIFIED              (PRTE_JOB_START_KEY +  29) // bool - normal term of job has been reported
#define PRTE_JOB_PEER_MODX_ID               (PRTE_JOB_START_KEY +  30) // prte_grpcomm_coll_id_t - collective id
#define PRTE_JOB_INIT_BAR_ID                (PRTE_JOB_START_KEY +  31) // prte_grpcomm_coll_id_t - collective id
#define PRTE_JOB_FINI_BAR_ID                (PRTE_JOB_START_KEY +  32) // prte_grpcomm_coll_id_t - collective id
#define PRTE_JOB_FWDIO_TO_TOOL              (PRTE_JOB_START_KEY +  33) // Forward IO for this job to the tool requesting its spawn
#define PRTE_JOB_LAUNCHED_DAEMONS           (PRTE_JOB_START_KEY +  35) // bool - Job caused new daemons to be spawned
#define PRTE_JOB_REPORT_BINDINGS            (PRTE_JOB_START_KEY +  36) // bool - Report process bindings
#define PRTE_JOB_CPUSET                     (PRTE_JOB_START_KEY +  37) // string - "soft" cgroup envelope for the job
#define PRTE_JOB_NOTIFICATIONS              (PRTE_JOB_START_KEY +  38) // string - comma-separated list of desired notifications+methods
#define PRTE_JOB_ROOM_NUM                   (PRTE_JOB_START_KEY +  39) // int - number of remote request's hotel room
#define PRTE_JOB_LAUNCH_PROXY               (PRTE_JOB_START_KEY +  40) // pmix_proc_t - name of spawn requestor
#define PRTE_JOB_NSPACE_REGISTERED          (PRTE_JOB_START_KEY +  41) // bool - job has been registered with embedded PMIx server
#define PRTE_JOB_FIXED_DVM                  (PRTE_JOB_START_KEY +  42) // bool - do not change the size of the DVM for this job
#define PRTE_JOB_DVM_JOB                    (PRTE_JOB_START_KEY +  43) // bool - job is using a DVM
#define PRTE_JOB_CANCELLED                  (PRTE_JOB_START_KEY +  44) // bool - job was cancelled
#define PRTE_JOB_OUTPUT_TO_FILE             (PRTE_JOB_START_KEY +  45) // string - path to use as basename of files to which
                                                                       //       stdout/err is to be directed
#define PRTE_JOB_MERGE_STDERR_STDOUT        (PRTE_JOB_START_KEY +  46) // bool - merge stderr into stdout stream
#define PRTE_JOB_TAG_OUTPUT                 (PRTE_JOB_START_KEY +  47) // bool - tag stdout/stderr
#define PRTE_JOB_TIMESTAMP_OUTPUT           (PRTE_JOB_START_KEY +  48) // bool - timestamp stdout/stderr
#define PRTE_JOB_MULTI_DAEMON_SIM           (PRTE_JOB_START_KEY +  49) // bool - multiple daemons/node to simulate large cluster
#define PRTE_JOB_NOTIFY_COMPLETION          (PRTE_JOB_START_KEY +  50) // bool - notify parent proc when spawned job terminates
#define PRTE_JOB_TRANSPORT_KEY              (PRTE_JOB_START_KEY +  51) // string - transport keys assigned to this job
#define PRTE_JOB_INFO_CACHE                 (PRTE_JOB_START_KEY +  52) // pmix_list_t - list of prte_value_t to be included in job_info
#define PRTE_JOB_SILENT_TERMINATION         (PRTE_JOB_START_KEY +  54) // bool - do not generate an event notification when job
                                                                       //        normally terminates
#define PRTE_JOB_SET_ENVAR                  (PRTE_JOB_START_KEY +  55) // prte_envar_t - set the given envar to the specified value
#define PRTE_JOB_UNSET_ENVAR                (PRTE_JOB_START_KEY +  56) // string - name of envar to unset, if present
#define PRTE_JOB_PREPEND_ENVAR              (PRTE_JOB_START_KEY +  57) // prte_envar_t - prepend the specified value to the given envar
#define PRTE_JOB_APPEND_ENVAR               (PRTE_JOB_START_KEY +  58) // prte_envar_t - append the specified value to the given envar
#define PRTE_JOB_ADD_ENVAR                  (PRTE_JOB_START_KEY +  59) // prte_envar_t - add envar, do not override pre-existing one
#define PRTE_JOB_APP_SETUP_DATA             (PRTE_JOB_START_KEY +  60) // pmix_byte_object_t - blob containing app setup data
#define PRTE_JOB_OUTPUT_TO_DIRECTORY        (PRTE_JOB_START_KEY +  61) // string - path of directory to which stdout/err is to be directed
#define PRTE_JOB_STOP_ON_EXEC               (PRTE_JOB_START_KEY +  62) // bool - stop on first instruction for debugger attach
#define PRTE_JOB_SPAWN_NOTIFIED             (PRTE_JOB_START_KEY +  63) // bool - process requesting a spawn operation has been notified of result
#define PRTE_JOB_DISPLAY_MAP                (PRTE_JOB_START_KEY +  64) // bool - display job map
#define PRTE_JOB_DISPLAY_DEVEL_MAP          (PRTE_JOB_START_KEY +  65) // bool - display devel level job map
#define PRTE_JOB_DISPLAY_TOPO               (PRTE_JOB_START_KEY +  66) // bool - display topology with job map
// 67 was removed option diffable map
#define PRTE_JOB_DISPLAY_ALLOC              (PRTE_JOB_START_KEY +  68) // bool - display allocation
#define PRTE_JOB_DO_NOT_LAUNCH              (PRTE_JOB_START_KEY +  69) // bool - do not launch job
#define PRTE_JOB_XML_OUTPUT                 (PRTE_JOB_START_KEY +  70) // bool - print in xml format
#define PRTE_JOB_TIMEOUT                    (PRTE_JOB_START_KEY +  71) // int32 - number of seconds job can run before terminating it as timed out
#define PRTE_JOB_STACKTRACES                (PRTE_JOB_START_KEY +  72) // bool - include process stack traces in timeout report
#define PRTE_JOB_REPORT_STATE               (PRTE_JOB_START_KEY +  73) // bool - include process state in timeout report
#define PRTE_JOB_TIMEOUT_EVENT              (PRTE_JOB_START_KEY +  74) // prte_ptr (prte_timer_t*) - timer event for job timeout
#define PRTE_JOB_TRACE_TIMEOUT_EVENT        (PRTE_JOB_START_KEY +  75) // prte_ptr (prte_timer_t*) - timer event for stacktrace collection
#define PRTE_JOB_INHERIT                    (PRTE_JOB_START_KEY +  76) // bool - job inherits parent's mapping/ranking/binding policies
#define PRTE_JOB_PES_PER_PROC               (PRTE_JOB_START_KEY +  77) // uint16_t - number of cpus to be assigned to each process
#define PRTE_JOB_DIST_DEVICE                (PRTE_JOB_START_KEY +  78) // char* - device to use for dist mapping
#define PRTE_JOB_HWT_CPUS                   (PRTE_JOB_START_KEY +  79) // bool - job requests hwthread cpus
#define PRTE_JOB_CORE_CPUS                  (PRTE_JOB_START_KEY +  80) // bool - job requests core cpus
#define PRTE_JOB_PPR                        (PRTE_JOB_START_KEY +  81) // char* - string specifying the procs-per-resource pattern
#define PRTE_JOB_NOINHERIT                  (PRTE_JOB_START_KEY +  82) // bool do NOT inherit parent's mapping/ranking/binding policies
#define PRTE_JOB_FILE                       (PRTE_JOB_START_KEY +  83) // char* - file to use for sequential or rankfile mapping
#define PRTE_JOB_DO_NOT_RESOLVE             (PRTE_JOB_START_KEY +  84) // bool - do not resolve nodes
#define PRTE_JOB_DEBUG_TARGET               (PRTE_JOB_START_KEY +  85) // pmix_proc_t - application proc to co-locate daemons with
#define PRTE_JOB_DEBUG_DAEMONS_PER_NODE     (PRTE_JOB_START_KEY +  86) // uint16_t - Number of debug daemons per node
#define PRTE_JOB_DEBUG_DAEMONS_PER_PROC     (PRTE_JOB_START_KEY +  87) // uint16_t - Number of debug daemons per application proc
#define PRTE_JOB_STOP_IN_INIT               (PRTE_JOB_START_KEY +  88) // bool - stop in PMIx_Init
#define PRTE_JOB_STOP_IN_APP                (PRTE_JOB_START_KEY +  89) // bool - stop at app-determined location
#define PRTE_JOB_ENVARS_HARVESTED           (PRTE_JOB_START_KEY +  90) // envars have already been harvested
#define PRTE_JOB_OUTPUT_NOCOPY              (PRTE_JOB_START_KEY +  91) // bool - do not copy output to stdout/err
#define PRTE_JOB_RANK_OUTPUT                (PRTE_JOB_START_KEY +  92) // bool - tag stdout/stderr with rank
#define PRTE_SPAWN_TIMEOUT                  (PRTE_JOB_START_KEY +  93) // int32 - number of seconds to spawn before terminating it as timed out
#define PRTE_JOB_RAW_OUTPUT                 (PRTE_JOB_START_KEY +  94) // bool - do not buffer output
#define PRTE_JOB_EXEC_AGENT                 (PRTE_JOB_START_KEY +  95) // char* - string specifying the cmd to use when exec'ing the local proc
#define PRTE_JOB_NOAGG_HELP                 (PRTE_JOB_START_KEY +  96) // bool - do not aggregate show_help messages
#define PRTE_JOB_COLOCATE_PROCS             (PRTE_JOB_START_KEY +  97) // pmix_data_array_t - colocate this job's procs with the given ones
#define PRTE_JOB_COLOCATE_NPERPROC          (PRTE_JOB_START_KEY +  98) // uint16_t - number of procs to colocate at each proc
#define PRTE_JOB_COLOCATE_NPERNODE          (PRTE_JOB_START_KEY +  99) // uint16_t - number of procs to colocate on node of each proc
#define PRTE_JOB_TAG_OUTPUT_DETAILED        (PRTE_JOB_START_KEY + 100) // bool - include [hostname:pid] in output stream tag
#define PRTE_JOB_TAG_OUTPUT_FULLNAME        (PRTE_JOB_START_KEY + 101) // bool - use full namespace in output stream tag
#define PRTE_JOB_ERROR_NONZERO_EXIT         (PRTE_JOB_START_KEY + 102) // bool - mark it as an error if a proc exits with non-zero status
#define PRTE_JOB_CONTROLS                   (PRTE_JOB_START_KEY + 103) // char* - Directives controlling job behavior
#define PRTE_JOB_SHOW_PROGRESS              (PRTE_JOB_START_KEY + 104) // bool - show launch progress of this job
#define PRTE_JOB_RECOVERABLE                (PRTE_JOB_START_KEY + 105) // bool - job processes can be recovered, do not terminate upon
                                                                       //        process failure
#define PRTE_JOB_NOTIFY_ERRORS              (PRTE_JOB_START_KEY + 106) // bool - provide PMIx events on errors
#define PRTE_JOB_AUTORESTART                (PRTE_JOB_START_KEY + 107) // bool - automatically restart failed processes
#define PRTE_JOB_OUTPUT_PROCTABLE           (PRTE_JOB_START_KEY + 108) // char* - string specifying where the output is to go, with a '-'
                                                                       //         indicating stdout, '+' indicating stderr, else path
#define PRTE_JOB_DISPLAY_PROCESSORS         (PRTE_JOB_START_KEY + 109) // char* - string displaying nodes whose avail CPUs
                                                                       //         are to be displayed
#define PRTE_JOB_DISPLAY_PARSEABLE_OUTPUT   (PRTE_JOB_START_KEY + 110) // bool - display output in machine parsable format
#define PRTE_JOB_EXTEND_DVM                 (PRTE_JOB_START_KEY + 111) // bool - DVM is being extended

#define PRTE_JOB_MAX_KEY (PRTE_JOB_START_KEY + 200)

/*** PROC FLAGS - never sent anywhere ***/
typedef uint16_t prte_proc_flags_t;
#define PRTE_PROC_FLAG_ALIVE            0x0001 // proc has been launched and has not yet terminated
#define PRTE_PROC_FLAG_ABORT            0x0002 // proc called abort
#define PRTE_PROC_FLAG_UPDATED          0x0004 // proc has been updated and need to be included in the next pidmap message
#define PRTE_PROC_FLAG_LOCAL            0x0008 // indicate that this proc is local
#define PRTE_PROC_FLAG_REPORTED         0x0010 // indicate proc has reported in
#define PRTE_PROC_FLAG_REG              0x0020 // proc has registered
#define PRTE_PROC_FLAG_HAS_DEREG        0x0040 // proc has deregistered
#define PRTE_PROC_FLAG_AS_MPI           0x0080 // proc is MPI process
#define PRTE_PROC_FLAG_IOF_COMPLETE     0x0100 // IOF has completed
#define PRTE_PROC_FLAG_WAITPID          0x0200 // waitpid fired
#define PRTE_PROC_FLAG_RECORDED         0x0400 // termination has been recorded
#define PRTE_PROC_FLAG_DATA_IN_SM       0x0800 // modex data has been stored in the local shared memory region
#define PRTE_PROC_FLAG_DATA_RECVD       0x1000 // modex data for this proc has been received
#define PRTE_PROC_FLAG_SM_ACCESS        0x2000 // indicate if process can read modex data from shared memory region
#define PRTE_PROC_FLAG_TERM_REPORTED    0x4000 // proc termination has been reported


/***   PROCESS ATTRIBUTE KEYS   ***/
#define PRTE_PROC_START_KEY PRTE_JOB_MAX_KEY

#define PRTE_PROC_NOBARRIER         (PRTE_PROC_START_KEY + 1) // bool  - indicates proc should not barrier in prte_init
#define PRTE_PROC_PRIOR_NODE        (PRTE_PROC_START_KEY + 5) // void* - pointer to prte_node_t where this proc last executed
#define PRTE_PROC_NRESTARTS         (PRTE_PROC_START_KEY + 6) // int32 - number of times this process has been restarted
#define PRTE_PROC_RESTART_TIME      (PRTE_PROC_START_KEY + 7) // timeval - time of last restart
#define PRTE_PROC_FAST_FAILS        (PRTE_PROC_START_KEY + 8) // int32 - number of failures in "fast" window
#define PRTE_PROC_CKPT_STATE        (PRTE_PROC_START_KEY + 9)  // size_t - ckpt state
#define PRTE_PROC_SNAPSHOT_REF      (PRTE_PROC_START_KEY + 10) // string - snapshot reference
#define PRTE_PROC_SNAPSHOT_LOC      (PRTE_PROC_START_KEY + 11) // string - snapshot location
#define PRTE_PROC_NODENAME          (PRTE_PROC_START_KEY + 12) // string - node where proc is located, used only by tools
#define PRTE_PROC_CGROUP            (PRTE_PROC_START_KEY + 13) // string - name of cgroup this proc shall be assigned to
#define PRTE_PROC_NBEATS            (PRTE_PROC_START_KEY + 14) // int32 - number of heartbeats in current window

#define PRTE_PROC_MAX_KEY (PRTE_PROC_START_KEY + 100)

/*** RML ATTRIBUTE keys ***/
#define PRTE_RML_START_KEY              PRTE_PROC_MAX_KEY
#define PRTE_RML_TRANSPORT_TYPE         (PRTE_RML_START_KEY + 1) // string - null terminated string containing transport type
#define PRTE_RML_PROTOCOL_TYPE          (PRTE_RML_START_KEY + 2) // string - protocol type (e.g., as returned by fi_info)
#define PRTE_RML_CONDUIT_ID             (PRTE_RML_START_KEY + 3) // prte_rml_conduit_t - conduit_id for this transport
#define PRTE_RML_INCLUDE_COMP_ATTRIB    (PRTE_RML_START_KEY + 4) // string - comma delimited list of RML component names to be considered
#define PRTE_RML_EXCLUDE_COMP_ATTRIB    (PRTE_RML_START_KEY + 5) // string - comma delimited list of RML component names to be excluded
#define PRTE_RML_TRANSPORT_ATTRIB       (PRTE_RML_START_KEY + 6) // string - comma delimited list of transport types to be considered
                                                                 // (e.g., "fabric,ethernet")
#define PRTE_RML_QUALIFIER_ATTRIB       (PRTE_RML_START_KEY + 7) // string - comma delimited list of qualifiers (e.g., routed=direct,bandwidth=xxx)
#define PRTE_RML_PROVIDER_ATTRIB        (PRTE_RML_START_KEY + 8) // string - comma delimited list of provider names to be considered
#define PRTE_RML_PROTOCOL_ATTRIB        (PRTE_RML_START_KEY + 9) // string - comma delimited list of protocols to be considered (e.g., tcp,udp)
#define PRTE_RML_ROUTED_ATTRIB          (PRTE_RML_START_KEY + 10) // string - comma delimited list of routed modules to be considered

#define PRTE_RML_MAX_KEY (PRTE_RML_START_KEY + 100)

#define PRTE_ATTR_KEY_MAX PRTE_RML_MAX_KEY

/*** FLAG OPS ***/
#define PRTE_FLAG_SET(p, f)   ((p)->flags |= (f))
#define PRTE_FLAG_UNSET(p, f) ((p)->flags &= ~(f))
#define PRTE_FLAG_TEST(p, f)  ((p)->flags & (f))

PRTE_EXPORT const char *prte_attr_key_to_str(prte_attribute_key_t key);

/* Retrieve the named attribute from a list */
PRTE_EXPORT bool prte_get_attribute(pmix_list_t *attributes, prte_attribute_key_t key, void **data,
                                    pmix_data_type_t type);

/* Set the named attribute in a list, overwriting any prior entry */
PRTE_EXPORT int prte_set_attribute(pmix_list_t *attributes, prte_attribute_key_t key, bool local,
                                   void *data, pmix_data_type_t type);

/* Remove the named attribute from a list */
PRTE_EXPORT void prte_remove_attribute(pmix_list_t *attributes, prte_attribute_key_t key);

PRTE_EXPORT prte_attribute_t *prte_fetch_attribute(pmix_list_t *attributes, prte_attribute_t *prev,
                                                   prte_attribute_key_t key);

PRTE_EXPORT int prte_prepend_attribute(pmix_list_t *attributes, prte_attribute_key_t key,
                                       bool local, void *data, pmix_data_type_t type);

PRTE_EXPORT int prte_attr_load(prte_attribute_t *kv, void *data, pmix_data_type_t type);

PRTE_EXPORT int prte_attr_unload(prte_attribute_t *kv, void **data, pmix_data_type_t type);

PRTE_EXPORT char *prte_attr_print_list(pmix_list_t *attributes);

/*
 * Register a handler for converting attr keys to strings
 *
 * Handlers will be invoked by prte_attr_key_to_str to return the appropriate value.
 */
typedef char *(*prte_attr2str_fn_t)(prte_attribute_key_t key);

PRTE_EXPORT int prte_attr_register(const char *project, prte_attribute_key_t key_base,
                                   prte_attribute_key_t key_max, prte_attr2str_fn_t converter);

/** FOR DIAGNOSTIC PURPOSES **/
#define PRTE_SHOW_ATTRS(a)                                                          \
    do {                                                                            \
        char *_output = prte_attr_print_list((a));                                  \
        fprintf(stderr, "[%s:%s:%d]\n%s\n", __FILE__, __func__, __LINE__, _output); \
        free(_output);                                                              \
    } while (0)
#endif

// forward declarations
struct prte_proc_t;
struct prte_node_t;
struct prte_job_t;

PRTE_EXPORT char* prte_print_proc_flags(struct prte_proc_t *p);
PRTE_EXPORT char* prte_print_node_flags(struct prte_node_t *p);
PRTE_EXPORT char* prte_print_job_flags(struct prte_job_t *p);

/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_PMIX_TYPES_H
#define OPAL_PMIX_TYPES_H

#include "opal_config.h"

#include "opal/dss/dss_types.h"
#include "opal/util/proc.h"

BEGIN_C_DECLS

/* define a set of "standard" attributes that can
 * be queried. Implementations (and users) are free to extend as
 * desired, so the get functions need to be capable
 * of handling the "not found" condition. Note that these
 * are attributes of the system and the job as opposed to
 * values the application (or underlying MPI library)
 * might choose to expose - i.e., they are values provided
 * by the resource manager as opposed to the application. Thus,
 * these keys are RESERVED */
#define OPAL_PMIX_ATTR_UNDEF      NULL

#define OPAL_PMIX_CPUSET                "pmix.cpuset"       // (char*) hwloc bitmap applied to proc upon launch
#define OPAL_PMIX_CREDENTIAL            "pmix.cred"         // (char*) security credential assigned to proc
#define OPAL_PMIX_SPAWNED               "pmix.spawned"      // (bool) true if this proc resulted from a call to PMIx_Spawn
#define OPAL_PMIX_ARCH                  "pmix.arch"         // (uint32_t) datatype architecture flag
/* scratch directory locations for use by applications */
#define OPAL_PMIX_TMPDIR                "pmix.tmpdir"       // (char*) top-level tmp dir assigned to session
#define OPAL_PMIX_NSDIR                 "pmix.nsdir"        // (char*) sub-tmpdir assigned to namespace
#define OPAL_PMIX_PROCDIR               "pmix.pdir"         // (char*) sub-nsdir assigned to proc
/* information about relative ranks as assigned by the RM */
#define OPAL_PMIX_JOBID                 "pmix.jobid"        // (char*) jobid assigned by scheduler
#define OPAL_PMIX_APPNUM                "pmix.appnum"       // (uint32_t) app number within the job
#define OPAL_PMIX_RANK                  "pmix.rank"         // (uint32_t) process rank within the job
#define OPAL_PMIX_GLOBAL_RANK           "pmix.grank"        // (uint32_t) rank spanning across all jobs in this session
#define OPAL_PMIX_APP_RANK              "pmix.apprank"      // (uint32_t) rank within this app
#define OPAL_PMIX_NPROC_OFFSET          "pmix.offset"       // (uint32_t) starting global rank of this job
#define OPAL_PMIX_LOCAL_RANK            "pmix.lrank"        // (uint16_t) rank on this node within this job
#define OPAL_PMIX_NODE_RANK             "pmix.nrank"        // (uint16_t) rank on this node spanning all jobs
#define OPAL_PMIX_LOCALLDR              "pmix.lldr"         // (uint64_t) opal_identifier of lowest rank on this node within this job
#define OPAL_PMIX_APPLDR                "pmix.aldr"         // (uint32_t) lowest rank in this app within this job
#define OPAL_PMIX_LOCALITY              "pmix.loc"          // (uint16_t) relative locality of two procs
/* proc location-related info */
/* For PMIX_HOSTNAME, three use-cases exist for PMIx_Get:
 *
 * (a) Specifying a namespace with PMIX_RANK_WILDCARD will return
 *     a comma-delimited list of nodes that host procs in that namespace
 *
 * (b) Passing a NULL namespace will return a comma-delimited list of all
 *     nodes known to this session, regardless of whether or not they
 *     currently host procs. The rank argument in PMIx_Get is ignored
 *     for this use-case
 *
 * (c) Specifying a namespace and a rank will return the name of the
 *     host this proc is on
 */
#define OPAL_PMIX_HOSTNAME              "pmix.hname"        // (char*) see above comment
#define OPAL_PMIX_NODEID                "pmix.nodeid"       // (uint32_t) node identifier
#define OPAL_PMIX_LOCAL_PEERS           "pmix.lpeers"       // (char*) comma-delimited string of ranks on this node within the specified nspace
#define OPAL_PMIX_LOCAL_CPUSETS         "pmix.lcpus"        // (char*) colon-delimited cpusets of local peers within the specified nspace
#define OPAL_PMIX_PROC_URI              "pmix.puri"         // (char*) URI containing contact info for proc
/* size info */
#define OPAL_PMIX_UNIV_SIZE             "pmix.univ.size"    // (uint32_t) #procs in this nspace
#define OPAL_PMIX_JOB_SIZE              "pmix.job.size"     // (uint32_t) #procs in this job
#define OPAL_PMIX_LOCAL_SIZE            "pmix.local.size"   // (uint32_t) #procs in this job on this node
#define OPAL_PMIX_NODE_SIZE             "pmix.node.size"    // (uint32_t) #procs across all jobs on this node
#define OPAL_PMIX_MAX_PROCS             "pmix.max.size"     // (uint32_t) max #procs for this job
/* topology info */
#define OPAL_PMIX_NET_TOPO              "pmix.ntopo"        // (char*) xml-representation of network topology
#define OPAL_PMIX_LOCAL_TOPO            "pmix.ltopo"        // (char*) xml-representation of local node topology
/* fault tolerance-related info */
#define OPAL_PMIX_TERMINATE_SESSION     "pmix.term.sess"    // (bool) RM intends to terminate session
#define OPAL_PMIX_TERMINATE_JOB         "pmix.term.job"     // (bool) RM intends to terminate this job
#define OPAL_PMIX_TERMINATE_NODE        "pmix.term.node"    // (bool) RM intends to terminate all procs on this node
#define OPAL_PMIX_TERMINATE_PROC        "pmix.term.proc"    // (bool) RM intends to terminate just this process
#define OPAL_PMIX_ERROR_ACTION_TIMEOUT  "pmix.err.timeout"  // (int) time in sec before RM will execute error response


/* attribute used by host server to pass data to the server convenience library - the
 * data will then be parsed and provided to the local clients */
#define OPAL_PMIX_PROC_DATA             "pmix.pdata"        // (pmix_value_array_t) starts with rank, then contains more data
#define OPAL_PMIX_NODE_MAP              "pmix.nmap"         // (char*) regex of nodes containing procs for this job
#define OPAL_PMIX_PROC_MAP              "pmix.pmap"         // (char*) regex describing procs on each node within this job

/* attributes used internally to communicate data from the server to the client */
#define OPAL_PMIX_PROC_BLOB             "pmix.pblob"        // (pmix_byte_object_t) packed blob of process data
#define OPAL_PMIX_MAP_BLOB              "pmix.mblob"        // (pmix_byte_object_t) packed blob of process location


/* define a scope for data "put" by PMI per the following:
 *
 * OPAL_PMI_LOCAL - the data is intended only for other application
 *                  processes on the same node. Data marked in this way
 *                  will not be included in data packages sent to remote requestors
 * OPAL_PMI_REMOTE - the data is intended solely for applications processes on
 *                   remote nodes. Data marked in this way will not be shared with
 *                   other processes on the same node
 * OPAL_PMI_GLOBAL - the data is to be shared with all other requesting processes,
 *                   regardless of location
 */
#define OPAL_PMIX_SCOPE PMIX_UINT32
typedef enum {
    OPAL_PMIX_SCOPE_UNDEF = 0,
    OPAL_PMIX_LOCAL,           // share to procs also on this node
    OPAL_PMIX_REMOTE,          // share with procs not on this node
    OPAL_PMIX_GLOBAL
} opal_pmix_scope_t; 

/* define a range for data "published" by PMI */
#define OPAL_PMIX_DATA_RANGE OPAL_UINT8
typedef enum {
    OPAL_PMIX_DATA_RANGE_UNDEF = 0,
    OPAL_PMIX_NAMESPACE,       // data is available to procs in the same nspace only
    OPAL_PMIX_UNIVERSAL,       // data available to all
    OPAL_PMIX_USER             // data available to all nspaces owned by this user
} opal_pmix_data_range_t;

/* define a "persistence" policy for data published by
 * clients to the "global" nspace */
typedef enum {
    OPAL_PMIX_PERSIST_INDEF = 0,   // retain until specifically deleted
    OPAL_PMIX_PERSIST_PROC,        // retain until publishing process terminates
    OPAL_PMIX_PERSIST_APP,         // retain until application terminates
    OPAL_PMIX_PERSIST_SESSION      // retain until session/allocation terminates
} opal_pmix_persistence_t;


/****    PMIX INFO STRUCT    ****/
typedef struct {
    opal_list_item_t super;
    char *key;
    opal_value_t value;
} opal_pmix_info_t;
OBJ_CLASS_DECLARATION(opal_pmix_info_t);


/****    PMIX LOOKUP RETURN STRUCT    ****/
typedef struct {
    opal_list_item_t super;
    opal_process_name_t proc;
    char *key;
    opal_value_t value;
} opal_pmix_pdata_t;
OBJ_CLASS_DECLARATION(opal_pmix_pdata_t);


/****    PMIX APP STRUCT    ****/
typedef struct {
    opal_list_item_t super;
    char *cmd;
    int argc;
    char **argv;
    char **env;
    int maxprocs;
    opal_list_t info;
} opal_pmix_app_t;
/* utility macros for working with pmix_app_t structs */
OBJ_CLASS_DECLARATION(opal_pmix_app_t);


/****    PMIX MODEX STRUCT    ****/
typedef struct {
    opal_object_t super;
    opal_process_name_t proc;
    uint8_t *blob;
    size_t size;
} opal_pmix_modex_data_t;
OBJ_CLASS_DECLARATION(opal_pmix_modex_data_t);


/****    CALLBACK FUNCTIONS FOR NON-BLOCKING OPERATIONS    ****/

/* define a callback function that is solely used by servers, and
 * not clients, to return modex data in response to "fence" and "get"
 * operations. The returned blob contains the data collected from each
 * server participating in the operation. */
typedef void (*opal_pmix_modex_cbfunc_t)(int status,
                                         const char *data, size_t ndata,
                                         void *cbdata);

/* define a callback function for calls to spawn_nb - the function
 * will be called upon completion of the spawn command. The status
 * will indicate whether or not the spawn succeeded. The jobid
 * of the spawned processes will be returned, along with any provided
 * callback data. */
typedef void (*opal_pmix_spawn_cbfunc_t)(int status, opal_jobid_t jobid, void *cbdata);

/* define a callback for common operations that simply return
 * a status. Examples include the non-blocking versions of
 * Fence, Connect, and Disconnect */
typedef void (*opal_pmix_op_cbfunc_t)(int status, void *cbdata);

/* define a callback function for calls to lookup_nb - the
 * function will be called upon completion of the command with the
 * status indicating the success of failure of the request. Any
 * retrieved data will be returned in a list of opal_pmix_pdata_t's.
 * The nspace/rank of the process that provided each data element is
 * also returned.
 *
 * Note that these structures will be released upon return from
 * the callback function, so the receiver must copy/protect the
 * data prior to returning if it needs to be retained */

typedef void (*opal_pmix_lookup_cbfunc_t)(int status,
                                          opal_list_t *data,
                                          void *cbdata);

/* define a callback function for the errhandler. Upon receipt of an
 * error notification, the active module will execute the specified notification
 * callback function, providing:
 *
 * status - the error that occurred
 * procs -  the nspace and ranks of the affected processes. A NULL
 *          value indicates that the error occurred in the module
 *          library within this process itself
 * info - any additional info provided regarding the error.
 *
 * Note that different resource managers may provide differing levels
 * of support for error notification to application processes. Thus, the
 * info list may be NULL or may contain detailed information of the error.
 * It is the responsibility of the application to parse any provided info array
 * for defined key-values if it so desires.
 *
 * Possible uses of a pmix_info_t object include:
 *
 * - for the RM to alert the process as to planned actions, such as
 *   to abort the session, in response to the reported error
 *
 * - provide a timeout for alternative action to occur, such as for
 *   the application to request an alternate response to the error
 *
 * For example, the RM might alert the application to the failure of
 * a node that resulted in termination of several processes, and indicate
 * that the overall session will be aborted unless the application
 * requests an alternative behavior in the next 5 seconds. The application
 * then has time to respond with a checkpoint request, or a request to
 * recover from the failure by obtaining replacement nodes and restarting
 * from some earlier checkpoint.
 *
 * Support for these options is left to the discretion of the host RM. Info
 * keys are included in the common definions above, but also may be augmented
 * on a per-RM basis.
 *
 * On the server side, the notification function is used to inform the host
 * server of a detected error in the PMIx subsystem and/or client */
typedef void (*opal_pmix_notification_fn_t)(int status,
                                            opal_list_t *procs,
                                            opal_list_t *info);

/* define a callback function for calls to get_nb. The status
 * indicates if the requested data was found or not - a pointer to the
 * opal_value_t structure containing the found data is returned. The
 * pointer will be NULL if the requested data was not found. */ 
typedef void (*opal_pmix_value_cbfunc_t)(int status,
                                         opal_value_t *kv, void *cbdata);



END_C_DECLS

#endif

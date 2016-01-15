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

/* identification attributes */
#define OPAL_PMIX_USERID                "pmix.euid"         // (uint32_t) effective user id
#define OPAL_PMIX_GRPID                 "pmix.egid"         // (uint32_t) effective group id

/* general proc-level attributes */
#define OPAL_PMIX_CPUSET                "pmix.cpuset"       // (char*) hwloc bitmap applied to proc upon launch
#define OPAL_PMIX_CREDENTIAL            "pmix.cred"         // (char*) security credential assigned to proc
#define OPAL_PMIX_SPAWNED               "pmix.spawned"      // (bool) true if this proc resulted from a call to PMIx_Spawn
#define OPAL_PMIX_ARCH                  "pmix.arch"         // (uint32_t) datatype architecture flag

/* scratch directory locations for use by applications */
#define OPAL_PMIX_TMPDIR                "pmix.tmpdir"       // (char*) top-level tmp dir assigned to session
#define OPAL_PMIX_NSDIR                 "pmix.nsdir"        // (char*) sub-tmpdir assigned to namespace
#define OPAL_PMIX_PROCDIR               "pmix.pdir"         // (char*) sub-nsdir assigned to proc

/* information about relative ranks as assigned by the RM */
#define OPAL_PMIX_JOBID                 "pmix.jobid"        // (uint32_t) jobid assigned by scheduler
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
#define OPAL_PMIX_NODE_LIST             "pmix.nlist"        // (char*) comma-delimited list of nodes running procs for this job
#define OPAL_PMIX_TOPOLOGY              "pmix.topo"         // (hwloc_topology_t) pointer to the PMIx client's internal topology object

/* fault tolerance-related info */
#define OPAL_PMIX_TERMINATE_SESSION     "pmix.term.sess"    // (bool) RM intends to terminate session
#define OPAL_PMIX_TERMINATE_JOB         "pmix.term.job"     // (bool) RM intends to terminate this job
#define OPAL_PMIX_TERMINATE_NODE        "pmix.term.node"    // (bool) RM intends to terminate all procs on this node
#define OPAL_PMIX_TERMINATE_PROC        "pmix.term.proc"    // (bool) RM intends to terminate just this process
#define OPAL_PMIX_ERROR_ACTION_TIMEOUT  "pmix.err.timeout"  // (int) time in sec before RM will execute error response

/* request-related info */
#define OPAL_PMIX_COLLECT_DATA          "pmix.collect"      // (bool) collect data and return it at the end of the operation
#define OPAL_PMIX_TIMEOUT               "pmix.timeout"      // (int) time in sec before specified operation should time out
#define OPAL_PMIX_WAIT                  "pmix.wait"         // (int) caller requests that the server wait until at least the specified
                                                            //       #values are found (0 => all and is the default)
#define OPAL_PMIX_COLLECTIVE_ALGO       "pmix.calgo"        // (char*) comma-delimited list of algorithms to use for collective
#define OPAL_PMIX_COLLECTIVE_ALGO_REQD  "pmix.calreqd"      // (bool) if true, indicates that the requested choice of algo is mandatory
#define OPAL_PMIX_NOTIFY_COMPLETION     "pmix.notecomp"     // (bool) notify parent process upon termination of child job
#define OPAL_PMIX_RANGE                 "pmix.range"        // (int) opal_pmix_data_range_t value for calls to publish/lookup/unpublish
#define OPAL_PMIX_PERSISTENCE           "pmix.persist"      // (int) opal_pmix_persistence_t value for calls to publish
#define OPAL_PMIX_OPTIONAL              "pmix.optional"     // (bool) look only in the immediate data store for the requested value - do
                                                            //        not request data from the server if not found

/* attribute used by host server to pass data to the server convenience library - the
 * data will then be parsed and provided to the local clients */
#define OPAL_PMIX_PROC_DATA             "pmix.pdata"        // (pmix_value_array_t) starts with rank, then contains more data
#define OPAL_PMIX_NODE_MAP              "pmix.nmap"         // (char*) regex of nodes containing procs for this job
#define OPAL_PMIX_PROC_MAP              "pmix.pmap"         // (char*) regex describing procs on each node within this job

/* attributes used internally to communicate data from the server to the client */
#define OPAL_PMIX_PROC_BLOB             "pmix.pblob"        // (pmix_byte_object_t) packed blob of process data
#define OPAL_PMIX_MAP_BLOB              "pmix.mblob"        // (pmix_byte_object_t) packed blob of process location

/* error handler registration  and notification info keys */
#define OPAL_PMIX_ERROR_NAME            "pmix.errname"           /* enum pmix_status_t specific error to be notified */
#define OPAL_PMIX_ERROR_GROUP_COMM      "pmix.errgroup.comm"     /* bool - set true to get comm  errors notification */
#define OPAL_PMIX_ERROR_GROUP_ABORT     "pmix.errgroup.abort"    /* bool -set true to get abort errors notification */
#define OPAL_PMIX_ERROR_GROUP_MIGRATE   "pmix.errgroup.migrate"  /* bool -set true to get migrate errors notification  */
#define OPAL_PMIX_ERROR_GROUP_RESOURCE  "pmix.errgroup.resource" /* bool -set true to get resource errors notification */
#define OPAL_PMIX_ERROR_GROUP_SPAWN     "pmix.errgroup.spawn"    /* bool - set true to get spawn errors notification */
#define OPAL_PMIX_ERROR_GROUP_NODE      "pmix.errgroup.node"     /* bool -set true to get node status errors */
#define OPAL_PMIX_ERROR_GROUP_LOCAL     "pmix.errgroup.local"    /* bool set true to get local errors */
#define OPAL_PMIX_ERROR_GROUP_GENERAL   "pmix.errgroup.gen"      /* bool set true to get notified af generic errors */

/* error notification keys */
#define OPAL_PMIX_ERROR_SCOPE           "pmix.errscope"       /* int (enum pmix_scope_t) scope of error notification*/
#define OPAL_PMIX_ERROR_NODE_NAME       "pmix.errnode.name"   /* name of the node that is in error or which reported the error.*/
#define OPAL_PMIX_ERROR_SEVERITY        "pmix.errseverity"    /* the severity of the notified (reported) error */

/* attributes used to describe "spawm" attributes */
#define OPAL_PMIX_PERSONALITY           "pmix.pers"         // (char*) name of personality to use
#define OPAL_PMIX_HOST                  "pmix.host"         // (char*) comma-delimited list of hosts to use for spawned procs
#define OPAL_PMIX_HOSTFILE              "pmix.hostfile"     // (char*) hostfile to use for spawned procs
#define OPAL_PMIX_ADD_HOST              "pmix.addhost"      // (char*) comma-delimited list of hosts to add to allocation
#define OPAL_PMIX_ADD_HOSTFILE          "pmix.addhostfile"  // (char*) hostfile to add to existing allocation
#define OPAL_PMIX_PREFIX                "pmix.prefix"       // (char*) prefix to use for starting spawned procs
#define OPAL_PMIX_WDIR                  "pmix.wdir"         // (char*) working directory for spawned procs
#define OPAL_PMIX_MAPPER                "pmix.mapper"       // (char*) mapper to use for placing spawned procs
#define OPAL_PMIX_DISPLAY_MAP           "pmix.dispmap"      // (bool) display process map upon spawn
#define OPAL_PMIX_PPR                   "pmix.ppr"          // (char*) #procs to spawn on each identified resource
#define OPAL_PMIX_MAPBY                 "pmix.mapby"        // (char*) mapping policy
#define OPAL_PMIX_RANKBY                "pmix.rankby"       // (char*) ranking policy
#define OPAL_PMIX_BINDTO                "pmix.bindto"       // (char*) binding policy
#define OPAL_PMIX_PRELOAD_BIN           "pmix.preloadbin"   // (bool) preload binaries
#define OPAL_PMIX_PRELOAD_FILES         "pmix.preloadfiles" // (char*) comma-delimited list of files to pre-position
#define OPAL_PMIX_NON_PMI               "pmix.nonpmi"       // (bool) spawned procs will not call PMIx_Init
#define OPAL_PMIX_STDIN_TGT             "pmix.stdin"        // (uint32_t) spawned proc rank that is to receive stdin


/* define a scope for data "put" by PMI per the following - maintain
 * consistent order with the PMIx distro :
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
#define OPAL_PMIX_SCOPE PMIX_UINT
typedef enum {
    OPAL_PMIX_SCOPE_UNDEF = 0,
    OPAL_PMIX_LOCAL,           // share to procs also on this node
    OPAL_PMIX_REMOTE,          // share with procs not on this node
    OPAL_PMIX_GLOBAL
} opal_pmix_scope_t;

/* define a range for data "published" by PMI  - maintain
 * consistent order with the PMIx distro */
#define OPAL_PMIX_DATA_RANGE OPAL_UINT
typedef enum {
    OPAL_PMIX_DATA_RANGE_UNDEF = 0,
    OPAL_PMIX_NAMESPACE,       // data is available to procs in the same nspace only
    OPAL_PMIX_SESSION          // data available to all jobs in this session
} opal_pmix_data_range_t;

/* define a "persistence" policy for data published by clients - maintain
 * consistent order with the PMIx distro */
typedef enum {
    OPAL_PMIX_PERSIST_INDEF = 0,   // retain until specifically deleted
    OPAL_PMIX_PERSIST_FIRST_READ,  // delete upon first access
    OPAL_PMIX_PERSIST_PROC,        // retain until publishing process terminates
    OPAL_PMIX_PERSIST_APP,         // retain until application terminates
    OPAL_PMIX_PERSIST_SESSION      // retain until session/allocation terminates
} opal_pmix_persistence_t;


/****    PMIX INFO STRUCT    ****/

/* NOTE: the pmix_info_t is essentially equivalent to the opal_value_t
 * Hence, we do not define an opal_value_t */


/****    PMIX LOOKUP RETURN STRUCT    ****/
typedef struct {
    opal_list_item_t super;
    opal_process_name_t proc;
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

typedef void (*opal_pmix_release_cbfunc_t)(void *cbdata);

/* define a callback function that is solely used by servers, and
 * not clients, to return modex data in response to "fence" and "get"
 * operations. The returned blob contains the data collected from each
 * server participating in the operation. */
typedef void (*opal_pmix_modex_cbfunc_t)(int status,
                                         const char *data, size_t ndata, void *cbdata,
                                         opal_pmix_release_cbfunc_t relcbfunc, void *relcbdata);

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
 * cbfunc - callback function to execute when the errhandler is
 *          finished with the provided data so it can be released
 * cbdata - pointer to be returned in cbfunc
 *
 * Note that different resource managers may provide differing levels
 * of support for error notification to application processes. Thus, the
 * info list may be NULL or may contain detailed information of the error.
 * It is the responsibility of the application to parse any provided info array
 * for defined key-values if it so desires.
 *
 * Possible uses of the opal_value_t list include:
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
                                            opal_list_t *info,
                                            opal_pmix_release_cbfunc_t cbfunc,
                                            void *cbdata);

/* define a callback function for calls to register_errhandler. The
 * status indicates if the request was successful or not, errhandler_ref is
 * an integer reference assigned to the errhandler by PMIX, this reference
 * must be used to deregister the err handler. A ptr to the original
 * cbdata is returned. */
typedef void (*opal_pmix_errhandler_reg_cbfunc_t)(int status,
                                                  int errhandler_ref,
                                                  void *cbdata);

/* define a callback function for calls to get_nb. The status
 * indicates if the requested data was found or not - a pointer to the
 * opal_value_t structure containing the found data is returned. The
 * pointer will be NULL if the requested data was not found. */
typedef void (*opal_pmix_value_cbfunc_t)(int status,
                                         opal_value_t *kv, void *cbdata);



END_C_DECLS

#endif

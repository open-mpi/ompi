/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer listed
 *   in this license in the documentation and/or other materials
 *   provided with the distribution.
 *
 * - Neither the name of the copyright holders nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * The copyright holders provide no reassurances that the source code
 * provided does not infringe any patent, copyright, or any other
 * intellectual property rights of third parties.  The copyright holders
 * disclaim any liability to any recipient for claims brought against
 * recipient by any third party for infringement of that parties
 * intellectual property rights.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * $HEADER$
 */

#ifndef PMIx_COMMON_H
#define PMIx_COMMON_H

#include <pmix/autogen/config.h>
#include <pmix/rename.h>

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h> /* for struct timeval */
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for uid_t and gid_t */
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h> /* for uid_t and gid_t */
#endif

BEGIN_C_DECLS

/****  PMIX CONSTANTS    ****/

/* define maximum value and key sizes */
#define PMIX_MAX_NSLEN     255
#define PMIX_MAX_KEYLEN    511

/* define a value for requests for job-level data
 * where the info itself isn't associated with any
 * specific rank, or when a request involves
 * a rank that isn't known - e.g., when someone requests
 * info thru one of the legacy interfaces where the rank
 * is typically encoded into the key itself since there is
 * no rank parameter in the API itself */
#define PMIX_RANK_UNDEF     INT32_MAX
/* define a value to indicate that the user wants the
 * data for the given key from every rank that posted
 * that key */
#define PMIX_RANK_WILDCARD  INT32_MAX-1

/* define a set of "standard" PMIx attributes that can
 * be queried. Implementations (and users) are free to extend as
 * desired, so the get functions need to be capable
 * of handling the "not found" condition. Note that these
 * are attributes of the system and the job as opposed to
 * values the application (or underlying MPI library)
 * might choose to expose - i.e., they are values provided
 * by the resource manager as opposed to the application. Thus,
 * these keys are RESERVED */
#define PMIX_ATTR_UNDEF      NULL

/* initialization attributes */
#define PMIX_EVENT_BASE                     "pmix.evbase"           // (struct event_base *) pointer to libevent event_base to use in place
                                                                    //                       of the internal progress thread
#define PMIX_SERVER_TOOL_SUPPORT            "pmix.srvr.tool"        // (bool) The host RM wants to declare itself as willing to
                                                                    //        accept tool connection requests
#define PMIX_SERVER_PIDINFO                 "pmix.srvr.pidinfo"     // (uint32_t) pid of the target server

/* identification attributes */
#define PMIX_USERID                         "pmix.euid"             // (uint32_t) effective user id
#define PMIX_GRPID                          "pmix.egid"             // (uint32_t) effective group id

/* attributes for the rendezvous socket  */
#define PMIX_SOCKET_MODE                    "pmix.sockmode"         // (uint32_t) POSIX mode_t (9 bits valid)

/* general proc-level attributes */
#define PMIX_CPUSET                         "pmix.cpuset"           // (char*) hwloc bitmap applied to proc upon launch
#define PMIX_CREDENTIAL                     "pmix.cred"             // (char*) security credential assigned to proc
#define PMIX_SPAWNED                        "pmix.spawned"          // (bool) true if this proc resulted from a call to PMIx_Spawn
#define PMIX_ARCH                           "pmix.arch"             // (uint32_t) datatype architecture flag

/* scratch directory locations for use by applications */
#define PMIX_TMPDIR                         "pmix.tmpdir"           // (char*) top-level tmp dir assigned to session
#define PMIX_NSDIR                          "pmix.nsdir"            // (char*) sub-tmpdir assigned to namespace
#define PMIX_PROCDIR                        "pmix.pdir"             // (char*) sub-nsdir assigned to proc

/* information about relative ranks as assigned by the RM */
#define PMIX_JOBID                          "pmix.jobid"            // (char*) jobid assigned by scheduler
#define PMIX_APPNUM                         "pmix.appnum"           // (uint32_t) app number within the job
#define PMIX_RANK                           "pmix.rank"             // (uint32_t) process rank within the job
#define PMIX_GLOBAL_RANK                    "pmix.grank"            // (uint32_t) rank spanning across all jobs in this session
#define PMIX_APP_RANK                       "pmix.apprank"          // (uint32_t) rank within this app
#define PMIX_NPROC_OFFSET                   "pmix.offset"           // (uint32_t) starting global rank of this job
#define PMIX_LOCAL_RANK                     "pmix.lrank"            // (uint16_t) rank on this node within this job
#define PMIX_NODE_RANK                      "pmix.nrank"            // (uint16_t) rank on this node spanning all jobs
#define PMIX_LOCALLDR                       "pmix.lldr"             // (uint64_t) opal_identifier of lowest rank on this node within this job
#define PMIX_APPLDR                         "pmix.aldr"             // (uint32_t) lowest rank in this app within this job

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
 *     host that proc is on
 */
#define PMIX_HOSTNAME                       "pmix.hname"            // (char*) see above comment
#define PMIX_NODEID                         "pmix.nodeid"           // (uint32_t) node identifier
#define PMIX_LOCAL_PEERS                    "pmix.lpeers"           // (char*) comma-delimited string of ranks on this node within the specified nspace
#define PMIX_LOCAL_CPUSETS                  "pmix.lcpus"            // (char*) colon-delimited cpusets of local peers within the specified nspace
#define PMIX_PROC_URI                       "pmix.puri"             // (char*) URI containing contact info for proc

/* size info */
#define PMIX_UNIV_SIZE                      "pmix.univ.size"        // (uint32_t) #procs in this nspace
#define PMIX_JOB_SIZE                       "pmix.job.size"         // (uint32_t) #procs in this job
#define PMIX_JOB_NUM_APPS                   "pmix.job.napps"        // (uint32_t) #apps in this job
#define PMIX_APP_SIZE                       "pmix.app.size"         // (uint32_t) #procs in this application
#define PMIX_LOCAL_SIZE                     "pmix.local.size"       // (uint32_t) #procs in this job on this node
#define PMIX_NODE_SIZE                      "pmix.node.size"        // (uint32_t) #procs across all jobs on this node
#define PMIX_MAX_PROCS                      "pmix.max.size"         // (uint32_t) max #procs for this job

/* topology info */
#define PMIX_NET_TOPO                       "pmix.ntopo"            // (char*) xml-representation of network topology
#define PMIX_LOCAL_TOPO                     "pmix.ltopo"            // (char*) xml-representation of local node topology
#define PMIX_NODE_LIST                      "pmix.nlist"            // (char*) comma-delimited list of nodes running procs for this job
#define PMIX_TOPOLOGY                       "pmix.topo"             // (hwloc_topology_t) pointer to the PMIx client's internal topology object

/* request-related info */
#define PMIX_COLLECT_DATA                   "pmix.collect"          // (bool) collect data and return it at the end of the operation
#define PMIX_TIMEOUT                        "pmix.timeout"          // (int) time in sec before specified operation should time out
#define PMIX_WAIT                           "pmix.wait"             // (int) caller requests that the server wait until at least the specified
                                                                    //       #values are found (0 => all and is the default)
#define PMIX_COLLECTIVE_ALGO                "pmix.calgo"            // (char*) comma-delimited list of algorithms to use for collective
#define PMIX_COLLECTIVE_ALGO_REQD           "pmix.calreqd"          // (bool) if true, indicates that the requested choice of algo is mandatory
#define PMIX_NOTIFY_COMPLETION              "pmix.notecomp"         // (bool) notify parent process upon termination of child job
#define PMIX_RANGE                          "pmix.range"            // (int) pmix_data_range_t value for calls to publish/lookup/unpublish
#define PMIX_PERSISTENCE                    "pmix.persist"          // (int) pmix_persistence_t value for calls to publish
#define PMIX_OPTIONAL                       "pmix.optional"         // (bool) look only in the immediate data store for the requested value - do
                                                                    //        not request data from the server if not found
#define PMIX_EMBED_BARRIER                  "pmix.embed.barrier"    // (bool) execute a blocking fence operation before executing the
                                                                    //        specified operation

/* attributes used by host server to pass data to the server convenience library - the
 * data will then be parsed and provided to the local clients */
#define PMIX_PROC_DATA                      "pmix.pdata"            // (pmix_value_array_t) starts with rank, then contains more data
#define PMIX_NODE_MAP                       "pmix.nmap"             // (char*) regex of nodes containing procs for this job
#define PMIX_PROC_MAP                       "pmix.pmap"             // (char*) regex describing procs on each node within this job
#define PMIX_ANL_MAP                        "pmix.anlmap"           // (char*) process mapping in ANL notation (used in PMI-1/PMI-2)

/* attributes used internally to communicate data from the server to the client */
#define PMIX_PROC_BLOB                      "pmix.pblob"            // (pmix_byte_object_t) packed blob of process data
#define PMIX_MAP_BLOB                       "pmix.mblob"            // (pmix_byte_object_t) packed blob of process location

/* event handler registration and notification info keys */
#define PMIX_EVENT_HDLR_NAME                "pmix.evname"           // (char*) string name identifying this handler
#define PMIX_EVENT_JOB_LEVEL                "pmix.evjob"            // (bool) register for job-specific events only
#define PMIX_EVENT_ENVIRO_LEVEL             "pmix.evenv"            // (bool) register for environment events only
#define PMIX_EVENT_ORDER_PREPEND            "pmix.evprepend"        // (bool) prepend this handler to the precedence list
#define PMIX_EVENT_CUSTOM_RANGE             "pmix.evrange"          // (pmix_proc_t*) array of pmix_proc_t defining range of event notification
#define PMIX_EVENT_AFFECTED_PROC            "pmix.evproc"           // (pmix_proc_t) single proc that was affected
#define PMIX_EVENT_AFFECTED_PROCS           "pmix.evaffected"       // (pmix_proc_t*) array of pmix_proc_t defining affected procs
#define PMIX_EVENT_NON_DEFAULT              "pmix.evnondef"         // (bool) event is not to be delivered to default event handlers
/* fault tolerance-related events */
#define PMIX_EVENT_TERMINATE_SESSION        "pmix.evterm.sess"      // (bool) RM intends to terminate session
#define PMIX_EVENT_TERMINATE_JOB            "pmix.evterm.job"       // (bool) RM intends to terminate this job
#define PMIX_EVENT_TERMINATE_NODE           "pmix.evterm.node"      // (bool) RM intends to terminate all procs on this node
#define PMIX_EVENT_TERMINATE_PROC           "pmix.evterm.proc"      // (bool) RM intends to terminate just this process
#define PMIX_EVENT_ACTION_TIMEOUT           "pmix.evtimeout"        // (int) time in sec before RM will execute error response

/* attributes used to describe "spawn" attributes */
#define PMIX_PERSONALITY                    "pmix.pers"              // (char*) name of personality to use
#define PMIX_HOST                           "pmix.host"              // (char*) comma-delimited list of hosts to use for spawned procs
#define PMIX_HOSTFILE                       "pmix.hostfile"          // (char*) hostfile to use for spawned procs
#define PMIX_ADD_HOST                       "pmix.addhost"           // (char*) comma-delimited list of hosts to add to allocation
#define PMIX_ADD_HOSTFILE                   "pmix.addhostfile"       // (char*) hostfile to add to existing allocation
#define PMIX_PREFIX                         "pmix.prefix"            // (char*) prefix to use for starting spawned procs
#define PMIX_WDIR                           "pmix.wdir"              // (char*) working directory for spawned procs
#define PMIX_MAPPER                         "pmix.mapper"            // (char*) mapper to use for placing spawned procs
#define PMIX_DISPLAY_MAP                    "pmix.dispmap"           // (bool) display process map upon spawn
#define PMIX_PPR                            "pmix.ppr"               // (char*) #procs to spawn on each identified resource
#define PMIX_MAPBY                          "pmix.mapby"             // (char*) mapping policy
#define PMIX_RANKBY                         "pmix.rankby"            // (char*) ranking policy
#define PMIX_BINDTO                         "pmix.bindto"            // (char*) binding policy
#define PMIX_PRELOAD_BIN                    "pmix.preloadbin"        // (bool) preload binaries
#define PMIX_PRELOAD_FILES                  "pmix.preloadfiles"      // (char*) comma-delimited list of files to pre-position
#define PMIX_NON_PMI                        "pmix.nonpmi"            // (bool) spawned procs will not call PMIx_Init
#define PMIX_STDIN_TGT                      "pmix.stdin"             // (uint32_t) spawned proc rank that is to receive stdin
#define PMIX_FWD_STDIN                      "pmix.fwd.stdin"         // (bool) forward my stdin to the designated proc
#define PMIX_FWD_STDOUT                     "pmix.fwd.stdout"        // (bool) forward stdout from spawned procs to me
#define PMIX_FWD_STDERR                     "pmix.fwd.stderr"        // (bool) forward stderr from spawned procs to me

/* query attributes */
#define PMIX_QUERY_NAMESPACES               "pmix.qry.ns"            // (char*) request a comma-delimited list of active nspaces
#define PMIX_QUERY_JOB_STATUS               "pmix.qry.jst"           // (pmix_status_t) status of a specified currently executing job
#define PMIX_QUERY_QUEUE_LIST               "pmix.qry.qlst"          // (char*) request a comma-delimited list of scheduler queues
#define PMIX_QUERY_QUEUE_STATUS             "pmix.qry.qst"           // (TBD) status of a specified scheduler queue

/****    PMIX ERROR CONSTANTS    ****/
/* PMIx errors are always negative, with 0 reserved for success */
#define PMIX_ERR_BASE                   0

typedef int pmix_status_t;

#define PMIX_SUCCESS                            (PMIX_ERR_BASE)
#define PMIX_ERROR                              (PMIX_ERR_BASE -  1)    // general error
/* debugger release flag */
#define PMIX_ERR_DEBUGGER_RELEASE               (PMIX_ERR_BASE -  2)
/* fault tolerance */
#define PMIX_ERR_PROC_RESTART                   (PMIX_ERR_BASE -  3)
#define PMIX_ERR_PROC_CHECKPOINT                (PMIX_ERR_BASE -  4)
#define PMIX_ERR_PROC_MIGRATE                   (PMIX_ERR_BASE -  5)
#define PMIX_ERR_UPDATE_ENDPOINTS               (PMIX_ERR_BASE -  6)
/* abort */
#define PMIX_ERR_PROC_ABORTED                   (PMIX_ERR_BASE -  7)
#define PMIX_ERR_PROC_REQUESTED_ABORT           (PMIX_ERR_BASE -  8)
#define PMIX_ERR_PROC_ABORTING                  (PMIX_ERR_BASE -  9)
/* communication failures */
#define PMIX_ERR_UNREACH                        (PMIX_ERR_BASE - 10)
#define PMIX_ERR_LOST_CONNECTION_TO_SERVER      (PMIX_ERR_BASE - 11)
#define PMIX_ERR_LOST_PEER_CONNECTION           (PMIX_ERR_BASE - 12)
#define PMIX_ERR_LOST_CONNECTION_TO_CLIENT      (PMIX_ERR_BASE - 13)
/* operational */
#define PMIX_ERR_NO_PERMISSIONS                 (PMIX_ERR_BASE - 14)
#define PMIX_ERR_TIMEOUT                        (PMIX_ERR_BASE - 15)
#define PMIX_ERR_WOULD_BLOCK                    (PMIX_ERR_BASE - 16)
#define PMIX_EXISTS                             (PMIX_ERR_BASE - 17)
#define PMIX_ERR_SERVER_FAILED_REQUEST          (PMIX_ERR_BASE - 18)
#define PMIX_ERR_NOT_SUPPORTED                  (PMIX_ERR_BASE - 19)
#define PMIX_ERR_NOT_FOUND                      (PMIX_ERR_BASE - 20)
#define PMIX_ERR_BAD_PARAM                      (PMIX_ERR_BASE - 21)
#define PMIX_ERR_DATA_VALUE_NOT_FOUND           (PMIX_ERR_BASE - 22)
#define PMIX_ERR_OUT_OF_RESOURCE                (PMIX_ERR_BASE - 23)
#define PMIX_ERR_INVALID_NAMESPACE              (PMIX_ERR_BASE - 24)
#define PMIX_ERR_INVALID_SIZE                   (PMIX_ERR_BASE - 25)
#define PMIX_ERR_INIT                           (PMIX_ERR_BASE - 26)
#define PMIX_ERR_EVENT_REGISTRATION             (PMIX_ERR_BASE - 27)
/* system failures */
#define PMIX_ERR_NODE_DOWN                      (PMIX_ERR_BASE - 28)
#define PMIX_ERR_NODE_OFFLINE                   (PMIX_ERR_BASE - 29)
/* used by event handlers */
#define PMIX_EVENT_NO_ACTION_TAKEN              (PMIX_ERR_BASE - 30)
#define PMIX_EVENT_PARTIAL_ACTION_TAKEN         (PMIX_ERR_BASE - 31)
#define PMIX_EVENT_ACTION_DEFERRED              (PMIX_ERR_BASE - 32)
#define PMIX_EVENT_ACTION_COMPLETE              (PMIX_ERR_BASE - 33)
/* used by the query system */
#define PMIX_QUERY_PARTIAL_SUCCESS              (PMIX_ERR_BASE - 34)


/* define a starting point for PMIx internal error codes
 * that are never exposed outside the library */
#define PMIX_INTERNAL_ERR_BASE          -1000

/* define a starting point for user-level defined error
 * constants - negative values larger than this are guaranteed
 * not to conflict with PMIx values. Definitions should always
 * be based on the PMIX_EXTERNAL_ERR_BASE constant and -not- a
 * specific value as the value of the constant may change */
#define PMIX_EXTERNAL_ERR_BASE           -2000

/****    PMIX DATA TYPES    ****/
typedef enum {
    PMIX_UNDEF = 0,
    PMIX_BOOL,           // converted to/from native true/false to uint8 for pack/unpack
    PMIX_BYTE,           // a byte of data
    PMIX_STRING,         // NULL-terminated string
    PMIX_SIZE,           // size_t
    PMIX_PID,            // OS-pid

    PMIX_INT,
    PMIX_INT8,
    PMIX_INT16,
    PMIX_INT32,
    PMIX_INT64,

    PMIX_UINT,
    PMIX_UINT8,
    PMIX_UINT16,
    PMIX_UINT32,
    PMIX_UINT64,

    PMIX_FLOAT,
    PMIX_DOUBLE,

    PMIX_TIMEVAL,
    PMIX_TIME,

    PMIX_STATUS,            // needs to be tracked separately from integer for those times
                            // when we are embedded and it needs to be converted to the
                            // host error definitions
    PMIX_HWLOC_TOPO,
    PMIX_VALUE,
    PMIX_INFO_ARRAY,
    PMIX_PROC,
    PMIX_APP,
    PMIX_INFO,
    PMIX_PDATA,
    PMIX_BUFFER,
    PMIX_BYTE_OBJECT,
    PMIX_KVAL,
    PMIX_MODEX,
    PMIX_PERSIST,
    PMIX_POINTER
} pmix_data_type_t;

/* define a scope for data "put" by PMI per the following:
 *
 * PMI_LOCAL - the data is intended only for other application
 *             processes on the same node. Data marked in this way
 *             will not be included in data packages sent to remote requestors
 * PMI_REMOTE - the data is intended solely for applications processes on
 *              remote nodes. Data marked in this way will not be shared with
 *              other processes on the same node
 * PMI_GLOBAL - the data is to be shared with all other requesting processes,
 *              regardless of location
 */
#define PMIX_SCOPE PMIX_UINT
typedef enum {
    PMIX_SCOPE_UNDEF = 0,
    PMIX_LOCAL,           // share to procs also on this node
    PMIX_REMOTE,          // share with procs not on this node
    PMIX_GLOBAL,          // share with all procs (local + remote)
} pmix_scope_t;

/* define a range for data "published" by PMI
 */
#define PMIX_DATA_RANGE PMIX_UINT
typedef enum {
    PMIX_RANGE_UNDEF = 0,
    PMIX_RANGE_RM,          // data is intended for the host resource manager
    PMIX_RANGE_LOCAL,       // available on local node only
    PMIX_RANGE_NAMESPACE,   // data is available to procs in the same nspace only
    PMIX_RANGE_SESSION,     // data available to all procs in session
    PMIX_RANGE_GLOBAL,      // data available to all procs
    PMIX_RANGE_CUSTOM       // range is specified in a pmix_info_t
} pmix_data_range_t;

/* define a "persistence" policy for data published by clients */
typedef enum {
    PMIX_PERSIST_INDEF = 0,   // retain until specifically deleted
    PMIX_PERSIST_FIRST_READ,  // delete upon first access
    PMIX_PERSIST_PROC,        // retain until publishing process terminates
    PMIX_PERSIST_APP,         // retain until application terminates
    PMIX_PERSIST_SESSION      // retain until session/allocation terminates
} pmix_persistence_t;

/****    PMIX BYTE OBJECT    ****/
typedef struct pmix_byte_object {
    char *bytes;
    size_t size;
} pmix_byte_object_t;


/****    PMIX PROC OBJECT    ****/
typedef struct pmix_proc {
    char nspace[PMIX_MAX_NSLEN+1];
    int rank;
} pmix_proc_t;
#define PMIX_PROC_CREATE(m, n)                                  \
    do {                                                        \
        (m) = (pmix_proc_t*)malloc((n) * sizeof(pmix_proc_t));  \
        memset((m), 0, (n) * sizeof(pmix_proc_t));              \
    } while (0)

#define PMIX_PROC_RELEASE(m)                    \
    do {                                        \
        PMIX_PROC_FREE((m));                    \
    } while (0)

#define PMIX_PROC_CONSTRUCT(m)                  \
    do {                                        \
        memset((m), 0, sizeof(pmix_proc_t));    \
    } while (0)

#define PMIX_PROC_DESTRUCT(m)

#define PMIX_PROC_FREE(m, n)                    \
    do {                                        \
        if (NULL != (m)) {                      \
            free((m));                          \
        }                                       \
    } while (0)



/****    PMIX VALUE STRUCT    ****/
typedef struct pmix_info_t pmix_info_t;

typedef struct pmix_info_array {
    size_t size;
    pmix_info_t *array;
} pmix_info_array_t;
/* NOTE: operations can supply a collection of values under
 * a single key by passing a pmix_value_t containing an
 * array of type PMIX_INFO_ARRAY, with each array element
 * containing its own pmix_info_t object */

typedef struct pmix_value {
    pmix_data_type_t type;
    union {
        bool flag;
        uint8_t byte;
        char *string;
        size_t size;
        pid_t pid;
        int integer;
        int8_t int8;
        int16_t int16;
        int32_t int32;
        int64_t int64;
        unsigned int uint;
        uint8_t uint8;
        uint16_t uint16;
        uint32_t uint32;
        uint64_t uint64;
        float fval;
        double dval;
        struct timeval tv;
        pmix_status_t status;
        pmix_proc_t proc;
        pmix_info_array_t array;
        pmix_byte_object_t bo;
        void *ptr;
    } data;
} pmix_value_t;
/* allocate and initialize a specified number of value structs */
#define PMIX_VALUE_CREATE(m, n)                                         \
    do {                                                                \
        int _ii;                                                        \
        (m) = (pmix_value_t*)malloc((n) * sizeof(pmix_value_t));        \
        memset((m), 0, (n) * sizeof(pmix_value_t));                     \
        for (_ii=0; _ii < (int)(n); _ii++) {                            \
            (m)[_ii].type = PMIX_UNDEF;                                 \
        }                                                               \
    } while (0)

/* release a single pmix_value_t struct, including its data */
#define PMIX_VALUE_RELEASE(m)                                           \
    do {                                                                \
        PMIX_VALUE_DESTRUCT((m));                                       \
        free((m));                                                      \
    } while (0)

/* initialize a single value struct */
#define PMIX_VALUE_CONSTRUCT(m)                 \
    do {                                        \
        memset((m), 0, sizeof(pmix_value_t));   \
        (m)->type = PMIX_UNDEF;                 \
    } while (0)

/* release the memory in the value struct data field */
#define PMIX_VALUE_DESTRUCT(m)                                          \
    do {                                                                \
        if (PMIX_STRING == (m)->type) {                                 \
            if (NULL != (m)->data.string) {                             \
                free((m)->data.string);                                 \
            }                                                           \
        } else if (PMIX_BYTE_OBJECT == (m)->type) {                     \
            if (NULL != (m)->data.bo.bytes) {                           \
                free((m)->data.bo.bytes);                               \
            }                                                           \
        } else if (PMIX_INFO_ARRAY == (m)->type) {                      \
            size_t _n;                                                  \
            pmix_info_t *_p = (pmix_info_t*)((m)->data.array.array);    \
            for (_n=0; _n < (m)->data.array.size; _n++) {               \
                if (PMIX_STRING == _p[_n].value.type) {                 \
                    if (NULL != _p[_n].value.data.string) {             \
                        free(_p[_n].value.data.string);                 \
                    }                                                   \
                } else if (PMIX_BYTE_OBJECT == _p[_n].value.type) {     \
                    if (NULL != _p[_n].value.data.bo.bytes) {           \
                        free(_p[_n].value.data.bo.bytes);               \
                    }                                                   \
                }                                                       \
            }                                                           \
            free(_p);                                                   \
        }                                                               \
    } while (0)

#define PMIX_VALUE_FREE(m, n)                           \
    do {                                                \
        size_t _s;                                      \
        if (NULL != (m)) {                              \
            for (_s=0; _s < (n); _s++) {                \
                PMIX_VALUE_DESTRUCT(&((m)[_s]));        \
            }                                           \
            free((m));                                  \
        }                                               \
    } while (0)

/* expose two functions that are resolved in the
 * PMIx library, but part of a header that
 * includes internal functions - we don't
 * want to expose the entire header here
 */
void pmix_value_load(pmix_value_t *v, void *data, pmix_data_type_t type);
pmix_status_t pmix_value_xfer(pmix_value_t *kv, pmix_value_t *src);




/****    PMIX INFO STRUCT    ****/
struct pmix_info_t {
    char key[PMIX_MAX_KEYLEN+1];  // ensure room for the NULL terminator
    bool required;                // defaults to optional (i.e., required=false)
    pmix_value_t value;
};

/* utility macros for working with pmix_info_t structs */
#define PMIX_INFO_CREATE(m, n)                                  \
    do {                                                        \
        (m) = (pmix_info_t*)malloc((n) * sizeof(pmix_info_t));  \
        memset((m), 0, (n) * sizeof(pmix_info_t));              \
    } while (0)

#define PMIX_INFO_CONSTRUCT(m)                  \
    do {                                        \
        memset((m), 0, sizeof(pmix_info_t));    \
        (m)->value.type = PMIX_UNDEF;           \
    } while (0)

#define PMIX_INFO_DESTRUCT(m) \
    do {                                        \
        PMIX_VALUE_DESTRUCT(&(m)->value);       \
    } while (0)

#define PMIX_INFO_FREE(m, n)                    \
    do {                                        \
        size_t _s;                              \
        if (NULL != (m)) {                      \
            for (_s=0; _s < (n); _s++) {        \
                PMIX_INFO_DESTRUCT(&((m)[_s])); \
            }                                   \
            free((m));                          \
        }                                       \
    } while (0)

#define PMIX_INFO_LOAD(m, k, v, t)                      \
    do {                                                \
        (void)strncpy((m)->key, (k), PMIX_MAX_KEYLEN);  \
        pmix_value_load(&((m)->value), (v), (t));       \
    } while (0)
#define PMIX_INFO_XFER(d, s)                                \
    do {                                                    \
        (void)strncpy((d)->key, (s)->key, PMIX_MAX_KEYLEN); \
        (d)->required = (s)->required;                      \
        pmix_value_xfer(&(d)->value, &(s)->value);          \
    } while(0)

#define PMIX_INFO_REQUIRED(m)       \
    (m)->required = true;
#define PMIX_INFO_OPTIONAL(m)       \
    (m)->required = false;

/****    PMIX LOOKUP RETURN STRUCT    ****/
typedef struct pmix_pdata {
    pmix_proc_t proc;
    char key[PMIX_MAX_KEYLEN+1];  // ensure room for the NULL terminator
    pmix_value_t value;
} pmix_pdata_t;

/* utility macros for working with pmix_pdata_t structs */
#define PMIX_PDATA_CREATE(m, n)                                         \
    do {                                                                \
        (m) = (pmix_pdata_t*)malloc((n) * sizeof(pmix_pdata_t));        \
        memset((m), 0, (n) * sizeof(pmix_pdata_t));                     \
    } while (0)

#define PMIX_PDATA_RELEASE(m)                   \
    do {                                        \
        PMIX_VALUE_DESTRUCT(&(m)->value);       \
        free((m));                              \
    } while (0)

#define PMIX_PDATA_CONSTRUCT(m)                 \
    do {                                        \
        memset((m), 0, sizeof(pmix_pdata_t));   \
        (m)->value.type = PMIX_UNDEF;           \
    } while (0)

#define PMIX_PDATA_DESTRUCT(m)                  \
    do {                                        \
        PMIX_VALUE_DESTRUCT(&(m)->value);       \
    } while (0)

#define PMIX_PDATA_FREE(m, n)                           \
    do {                                                \
        size_t _s;                                      \
        if (NULL != (m)) {                              \
            for (_s=0; _s < (n); _s++) {                \
                PMIX_PDATA_DESTRUCT(&((m)[_s]));        \
            }                                           \
            free((m));                                  \
        }                                               \
    } while (0)

#define PMIX_PDATA_LOAD(m, p, k, v, t)                                  \
    do {                                                                \
    if (NULL != (m)) {                                                  \
        (void)strncpy((m)->proc.nspace, (p)->nspace, PMIX_MAX_NSLEN);   \
        (m)->proc.rank = (p)->rank;                                     \
        (void)strncpy((m)->key, (k), PMIX_MAX_KEYLEN);                  \
        pmix_value_load(&((m)->value), (v), (t));                       \
    } while (0)


/****    PMIX APP STRUCT    ****/
typedef struct pmix_app {
    char *cmd;
    int argc;
    char **argv;
    char **env;
    int maxprocs;
    pmix_info_t *info;
    size_t ninfo;
} pmix_app_t;
/* utility macros for working with pmix_app_t structs */
#define PMIX_APP_CREATE(m, n)                                   \
    do {                                                        \
        (m) = (pmix_app_t*)malloc((n) * sizeof(pmix_app_t));    \
        memset((m), 0, (n) * sizeof(pmix_app_t));               \
    } while (0)

#define PMIX_APP_RELEASE(m)                     \
    do {                                        \
        PMIX_APP_DESTRUCT((m));                 \
        free((m));                              \
    } while (0)

#define PMIX_APP_CONSTRUCT(m)                   \
    do {                                        \
        memset((m), 0, sizeof(pmix_app_t));     \
    } while (0)

#define PMIX_APP_DESTRUCT(m)                                    \
    do {                                                        \
        size_t _ii;                                             \
        if (NULL != (m)->cmd) {                                 \
            free((m)->cmd);                                     \
        }                                                       \
        if (NULL != (m)->argv) {                                \
            for (_ii=0; NULL != (m)->argv[_ii]; _ii++) {        \
                free((m)->argv[_ii]);                           \
            }                                                   \
            free((m)->argv);                                    \
        }                                                       \
        if (NULL != (m)->env) {                                 \
            for (_ii=0; NULL != (m)->env[_ii]; _ii++) {         \
                free((m)->env[_ii]);                            \
            }                                                   \
            free((m)->env);                                     \
        }                                                       \
        if (NULL != (m)->info) {                                \
            for (_ii=0; _ii < (m)->ninfo; _ii++) {              \
                PMIX_INFO_DESTRUCT(&(m)->info[_ii]);            \
            }                                                   \
        }                                                       \
    } while (0)

#define PMIX_APP_FREE(m, n)                     \
    do {                                        \
        size_t _s;                              \
        if (NULL != (m)) {                      \
            for (_s=0; _s < (n); _s++) {        \
                PMIX_APP_DESTRUCT(&((m)[_s]));  \
            }                                   \
            free((m));                          \
        }                                       \
    } while (0)

/****    PMIX MODEX STRUCT    ****/
typedef struct pmix_modex_data {
    char nspace[PMIX_MAX_NSLEN+1];
    int rank;
    uint8_t *blob;
    size_t size;
} pmix_modex_data_t;
/* utility macros for working with pmix_modex_t structs */
#define PMIX_MODEX_CREATE(m, n)                                         \
    do {                                                                \
        (m) = (pmix_modex_data_t*)malloc((n) * sizeof(pmix_modex_data_t)); \
        memset((m), 0, (n) * sizeof(pmix_modex_data_t));                \
    } while (0)

#define PMIX_MODEX_RELEASE(m)                   \
    do {                                        \
        PMIX_MODEX_DESTRUCT((m));               \
        free((m));                              \
    } while (0)

#define PMIX_MODEX_CONSTRUCT(m)                         \
    do {                                                \
        memset((m), 0, sizeof(pmix_modex_data_t));      \
    } while (0)

#define PMIX_MODEX_DESTRUCT(m)                  \
    do {                                        \
        if (NULL != (m)->blob) {                \
            free((m)->blob);                    \
        }                                       \
    } while (0)

#define PMIX_MODEX_FREE(m, n)                           \
    do {                                                \
        size_t _s;                                      \
        if (NULL != (m)) {                              \
            for (_s=0; _s < (n); _s++) {                \
                PMIX_MODEX_DESTRUCT(&((m)[_s]));        \
            }                                           \
            free((m));                                  \
        }                                               \
    } while (0)


/****    CALLBACK FUNCTIONS FOR NON-BLOCKING OPERATIONS    ****/

typedef void (*pmix_release_cbfunc_t)(void *cbdata);

/* define a callback function that is solely used by servers, and
 * not clients, to return modex data in response to "fence" and "get"
 * operations. The returned blob contains the data collected from each
 * server participating in the operation.
 *
 * As the data is "owned" by the host server, provide a secondary
 * callback function to notify the host server that we are done
 * with the data so it can be released */
typedef void (*pmix_modex_cbfunc_t)(pmix_status_t status,
                                    const char *data, size_t ndata,
                                    void *cbdata,
                                    pmix_release_cbfunc_t release_fn,
                                    void *release_cbdata);

/* define a callback function for calls to PMIx_Spawn_nb - the function
 * will be called upon completion of the spawn command. The status
 * will indicate whether or not the spawn succeeded. The nspace
 * of the spawned processes will be returned, along with any provided
 * callback data. Note that the returned nspace value will be
 * released by the library upon return from the callback function, so
 * the receiver must copy it if it needs to be retained */
typedef void (*pmix_spawn_cbfunc_t)(pmix_status_t status,
                                    char nspace[], void *cbdata);

/* define a callback for common operations that simply return
 * a status. Examples include the non-blocking versions of
 * Fence, Connect, and Disconnect */
typedef void (*pmix_op_cbfunc_t)(pmix_status_t status, void *cbdata);

/* define a callback function for calls to PMIx_Lookup_nb - the
 * function will be called upon completion of the command with the
 * status indicating the success of failure of the request. Any
 * retrieved data will be returned in an array of pmix_pdata_t structs.
 * The nspace/rank of the process that provided each data element is
 * also returned.
 *
 * Note that these structures will be released upon return from
 * the callback function, so the receiver must copy/protect the
 * data prior to returning if it needs to be retained */

typedef void (*pmix_lookup_cbfunc_t)(pmix_status_t status,
                                     pmix_pdata_t data[], size_t ndata,
                                     void *cbdata);

/* define a callback by which an event handler can notify the PMIx library
 * that it has completed its response to the notification. The handler
 * is _required_ to execute this callback so the library can determine
 * if additional handlers need to be called. The handler shall return
 * PMIX_SUCCESS if no further action is required. The return status
 * of each event handler and any returned pmix_info_t structures
 * will be added to the array of pmix_info_t passed to any subsequent
 * event handlers to help guide their operation.
 *
 * If non-NULL, the provided callback function will be called to allow
 * the event handler to release the provided info array.
 */
typedef void (*pmix_event_notification_cbfunc_fn_t)(pmix_status_t status,
                                                    pmix_info_t *results, size_t nresults,
                                                    pmix_op_cbfunc_t cbfunc, void *thiscbdata,
                                                    void *notification_cbdata);

/* define a callback function for the event handler. Upon receipt of an
 * event notification, PMIx will execute the specified notification
 * callback function, providing:
 *
 * evhdlr_registration_id - the returned registration number of
 *                          the event handler being called
 * status - the event that occurred
 * source - the nspace and rank of the process that generated
 *          the event. If the source is the resource manager,
 *          then the nspace will be empty and the rank will
 *          be PMIX_RANK_UNDEF
 * info - any additional info provided regarding the event.
 * ninfo - the number of info objects in the info array
 * results - any provided results from event handlers called
 *           prior to this one.
 * nresults - number of info objects in the results array
 * cbfunc - the function to be called upon completion of the handler
 * cbdata - pointer to be returned in the completion cbfunc
 *
 * Note that different resource managers may provide differing levels
 * of support for event notification to application processes. Thus, the
 * info array may be NULL or may contain detailed information of the event.
 * It is the responsibility of the application to parse any provided info array
 * for defined key-values if it so desires.
 *
 * Possible uses of the pmix_info_t object include:
 *
 * - for the RM to alert the process as to planned actions, such as
 *   to abort the session, in response to the reported event
 *
 * - provide a timeout for alternative action to occur, such as for
 *   the application to request an alternate response to the event
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
 * server of a detected event in the PMIx subsystem and/or client
 */
typedef void (*pmix_notification_fn_t)(size_t evhdlr_registration_id,
                                       pmix_status_t status,
                                       const pmix_proc_t *source,
                                       pmix_info_t info[], size_t ninfo,
                                       pmix_info_t *results, size_t nresults,
                                       pmix_event_notification_cbfunc_fn_t cbfunc,
                                       void *cbdata);

/* define a callback function for calls to PMIx_Register_evhdlr. The
 * status indicates if the request was successful or not, evhdlr_ref is
 * an integer reference assigned to the event handler by PMIx, this reference
 * must be used to deregister the err handler. A ptr to the original
 * cbdata is returned. */
typedef void (*pmix_evhdlr_reg_cbfunc_t)(pmix_status_t status,
                                         size_t evhdlr_ref,
                                         void *cbdata);

/* define a callback function for calls to PMIx_Get_nb. The status
 * indicates if the requested data was found or not - a pointer to the
 * pmix_value_t structure containing the found data is returned. The
 * pointer will be NULL if the requested data was not found. */
typedef void (*pmix_value_cbfunc_t)(pmix_status_t status,
                                    pmix_value_t *kv, void *cbdata);

/* define a callback function for calls to PMIx_Query. The status
 * indicates if requested data was found or not - an array of
 * pmix_info_t will contain the key/value pairs. */
typedef void (*pmix_info_cbfunc_t)(pmix_status_t status,
                                   pmix_info_t *info, size_t ninfo,
                                   void *cbdata,
                                   pmix_release_cbfunc_t release_fn,
                                   void *release_cbdata);


/****    COMMON SUPPORT FUNCTIONS    ****/
/* Register an event handler to report events. Three types of events
 * can be reported:
 *
 * (a) those that occur within the client library, but are not
 *     reportable via the API itself (e.g., loss of connection to
 *     the server). These events typically occur during behind-the-scenes
 *     non-blocking operations.
 *
 * (b) job-related events such as the failure of another process in
 *     the job or in any connected job, impending failure of hardware
 *     within the job's usage footprint, etc.
 *
 * (c) system notifications that are made available by the local
 *     administrators
 *
 * By default, only events that directly affect the process and/or
 * any process to which it is connected (via the PMIx_Connect call)
 * will be reported. Options to modify that behavior can be provided
 * in the info array
 *
 * Both the client application and the resource manager can register
 * err handlers for specific events. PMIx client/server calls the registered
 * err handler upon receiving event notify notification (via PMIx_Notify_event)
 * from the other end (Resource Manager/Client application).
 *
 * Multiple err handlers can be registered for different events. PMIX returns
 * an integer reference to each register handler in the callback fn. The caller
 * must retain the reference in order to deregister the evhdlr.
 * Modification of the notification behavior can be accomplished by
 * deregistering the current evhdlr, and then registering it
 * using a new set of info values.
 *
 * See pmix_common.h for a description of the notification function */
void PMIx_Register_event_handler(pmix_status_t codes[], size_t ncodes,
                                 pmix_info_t info[], size_t ninfo,
                                 pmix_notification_fn_t evhdlr,
                                 pmix_evhdlr_reg_cbfunc_t cbfunc,
                                 void *cbdata);

/* Deregister an event handler
 * evhdlr_ref is the reference returned by PMIx from the call to
 * PMIx_Register_event_handler. If non-NULL, the provided cbfunc
 * will be called to confirm removal of the designated handler */
void PMIx_Deregister_event_handler(size_t evhdlr_ref,
                                   pmix_op_cbfunc_t cbfunc,
                                   void *cbdata);

/* Report an event to a process for notification via any
 * registered evhdlr. The evhdlr registration can be
 * called by both the server and the client application. On the
 * server side, the evhdlr is used to report events detected
 * by PMIx to the host server for handling. On the client side,
 * the evhdlr is used to notify the process of events
 * reported by the server - e.g., the failure of another process.
 *
 * This function allows the host server to direct the server
 * convenience library to notify all registered local procs of
 * an event. The event can be local, or anywhere in the cluster.
 * The status indicates the event being reported.
 *
 * The client application can also call this function to notify the
 * resource manager of an event it encountered. It can request the host
 * server to notify the indicated processes about the event.
 *
 * The array of procs identifies the processes that will be impacted
 * by the event. This could consist of a single process, or a number
 * of processes.
 *
 * The info array contains any further info the RM can and/or chooses
 * to provide.
 *
 * The callback function will be called upon completion of the
 * notify_event function's actions. Note that any messages will
 * have been queued, but may not have been transmitted by this
 * time. Note that the caller is required to maintain the input
 * data until the callback function has been executed!
*/
pmix_status_t PMIx_Notify_event(pmix_status_t status,
                                const pmix_proc_t *source,
                                pmix_data_range_t range,
                                pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Provide a string representation of a pmix_status_t value. Note
 * that the provided string is statically defined and must NOT be
 * free'd */
const char* PMIx_Error_string(pmix_status_t status);

/* Get the PMIx version string. Note that the provided string is
 * statically defined and must NOT be free'd  */
const char* PMIx_Get_version(void);

/* Store some data locally for retrieval by other areas of the
 * proc. This is data that has only internal scope - it will
 * never be "pushed" externally */
pmix_status_t PMIx_Store_internal(const pmix_proc_t *proc,
                                  const char *key, pmix_value_t *val);


/* Key-Value pair management macros */
// TODO: add all possible types/fields here.

#define PMIX_VAL_FIELD_int(x)       ((x)->data.integer)
#define PMIX_VAL_FIELD_uint32_t(x)  ((x)->data.uint32)
#define PMIX_VAL_FIELD_uint16_t(x)  ((x)->data.uint16)
#define PMIX_VAL_FIELD_string(x)    ((x)->data.string)
#define PMIX_VAL_FIELD_float(x)     ((x)->data.fval)
#define PMIX_VAL_FIELD_byte(x)      ((x)->data.byte)
#define PMIX_VAL_FIELD_flag(x)      ((x)->data.flag)

#define PMIX_VAL_TYPE_int      PMIX_INT
#define PMIX_VAL_TYPE_uint32_t PMIX_UINT32
#define PMIX_VAL_TYPE_uint16_t PMIX_UINT16
#define PMIX_VAL_TYPE_string   PMIX_STRING
#define PMIX_VAL_TYPE_float    PMIX_FLOAT
#define PMIX_VAL_TYPE_byte     PMIX_BYTE
#define PMIX_VAL_TYPE_flag     PMIX_BOOL

#define PMIX_VAL_set_assign(_v, _field, _val )   \
    do {                                                            \
        (_v)->type = PMIX_VAL_TYPE_ ## _field;                      \
        PMIX_VAL_FIELD_ ## _field((_v)) = _val;                     \
    } while (0)

#define PMIX_VAL_set_strdup(_v, _field, _val )       \
    do {                                                                \
        (_v)->type = PMIX_VAL_TYPE_ ## _field;                          \
        PMIX_VAL_FIELD_ ## _field((_v)) = strdup(_val);                 \
    } while (0)

#define PMIX_VAL_SET_int        PMIX_VAL_set_assign
#define PMIX_VAL_SET_uint32_t   PMIX_VAL_set_assign
#define PMIX_VAL_SET_uint16_t   PMIX_VAL_set_assign
#define PMIX_VAL_SET_string     PMIX_VAL_set_strdup
#define PMIX_VAL_SET_float      PMIX_VAL_set_assign
#define PMIX_VAL_SET_byte       PMIX_VAL_set_assign
#define PMIX_VAL_SET_flag       PMIX_VAL_set_assign

#define PMIX_VAL_SET(_v, _field, _val )   \
    PMIX_VAL_SET_ ## _field(_v, _field, _val)

#define PMIX_VAL_cmp_val(_val1, _val2)      ((_val1) != (_val2))
#define PMIX_VAL_cmp_float(_val1, _val2)    (((_val1)>(_val2))?(((_val1)-(_val2))>0.000001):(((_val2)-(_val1))>0.000001))
#define PMIX_VAL_cmp_ptr(_val1, _val2)      strncmp(_val1, _val2, strlen(_val1)+1)

#define PMIX_VAL_CMP_int        PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_uint32_t   PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_uint16_t   PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_float      PMIX_VAL_cmp_float
#define PMIX_VAL_CMP_string     PMIX_VAL_cmp_ptr
#define PMIX_VAL_CMP_byte       PMIX_VAL_cmp_val
#define PMIX_VAL_CMP_flag       PMIX_VAL_cmp_val

#define PMIX_VAL_CMP(_field, _val1, _val2) \
    PMIX_VAL_CMP_ ## _field(_val1, _val2)

#define PMIX_VAL_FREE(_v) \
     PMIx_free_value_data(_v)

END_C_DECLS
#endif

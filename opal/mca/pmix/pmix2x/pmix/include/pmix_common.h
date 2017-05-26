/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
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
 * $HEADER$
 */

#ifndef PMIx_COMMON_H
#define PMIx_COMMON_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <sys/time.h> /* for struct timeval */
#include <unistd.h> /* for uid_t and gid_t */
#include <sys/types.h> /* for uid_t and gid_t */
#include <pmix_rename.h>
#include <pmix_version.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/****  PMIX CONSTANTS    ****/

/* define maximum value and key sizes */
#define PMIX_MAX_NSLEN     255
#define PMIX_MAX_KEYLEN    511

/* define a type for rank values */
typedef uint32_t pmix_rank_t;

/* define a value for requests for job-level data
 * where the info itself isn't associated with any
 * specific rank, or when a request involves
 * a rank that isn't known - e.g., when someone requests
 * info thru one of the legacy interfaces where the rank
 * is typically encoded into the key itself since there is
 * no rank parameter in the API itself */
#define PMIX_RANK_UNDEF     UINT32_MAX
/* define a value to indicate that the user wants the
 * data for the given key from every rank that posted
 * that key */
#define PMIX_RANK_WILDCARD  UINT32_MAX-1

/* other special rank values will be used to define
 * groups of ranks for use in collectives */
#define PMIX_RANK_LOCAL_NODE    UINT32_MAX-2        // all ranks on local node


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
#define PMIX_SERVER_REMOTE_CONNECTIONS      "pmix.srvr.remote"      // (bool) Allow connections from remote tools (do not use loopback device)
#define PMIX_SERVER_SYSTEM_SUPPORT          "pmix.srvr.sys"         // (bool) The host RM wants to declare itself as being the local
                                                                    //        system server for PMIx connection requests
#define PMIX_SERVER_PIDINFO                 "pmix.srvr.pidinfo"     // (pid_t) pid of the target server
#define PMIX_SERVER_HOSTNAME                "pmix.srvr.host"        // (char*) node where target server is located
#define PMIX_SERVER_TMPDIR                  "pmix.srvr.tmpdir"      // (char*) temp directory where PMIx server will place
                                                                    //        client rendezvous points and contact info
#define PMIX_SYSTEM_TMPDIR                  "pmix.sys.tmpdir"       // (char*) temp directory for this system, where PMIx
                                                                    //        server will place tool rendezvous points and contact info
#define PMIX_CONNECT_TO_SYSTEM              "pmix.cnct.sys"         // (bool) The requestor requires that a connection be made only to
                                                                    //        a local system-level PMIx server
#define PMIX_CONNECT_SYSTEM_FIRST           "pmix.cnct.sys.first"   // (bool) Preferentially look for a system-level PMIx server first
#define PMIX_REGISTER_NODATA                "pmix.reg.nodata"       // (bool) Registration is for nspace only, do not copy job data
#define PMIX_SERVER_ENABLE_MONITORING       "pmix.srv.monitor"      // (bool) Enable PMIx internal monitoring by server


/* identification attributes */
#define PMIX_USERID                         "pmix.euid"             // (uint32_t) effective user id
#define PMIX_GRPID                          "pmix.egid"             // (uint32_t) effective group id
#define PMIX_DSTPATH                        "pmix.dstpath"          // (char*) path to dstore files
#define PMIX_VERSION_INFO                   "pmix.version"          // (char*) PMIx version of contactor
#define PMIX_PROGRAMMING_MODEL              "pmix.pgm.model"        // (char*) programming model being initialized (e.g., "MPI" or "OpenMP")
#define PMIX_MODEL_LIBRARY_NAME             "pmix.mdl.name"         // (char*) programming model implementation ID (e.g., "OpenMPI" or "MPICH")
#define PMIX_MODEL_LIBRARY_VERSION          "pmix.mld.vrs"          // (char*) programming model version string (e.g., "2.1.1")
#define PMIX_THREADING_MODEL                "pmix.threads"          // (char*) threading model used (e.g., "pthreads")


/* attributes for the USOCK rendezvous socket  */
#define PMIX_USOCK_DISABLE                  "pmix.usock.disable"    // (bool) disable legacy usock support
#define PMIX_SOCKET_MODE                    "pmix.sockmode"         // (uint32_t) POSIX mode_t (9 bits valid)
#define PMIX_SINGLE_LISTENER                "pmix.sing.listnr"      // (bool) use only one rendezvous socket, letting priorities and/or
                                                                    //        MCA param select the active transport

/* attributes for TCP connections */
#define PMIX_TCP_URI                        "pmix.tcp.uri"          // (char*) URI of server to connect to
#define PMIX_TCP_IF_INCLUDE                 "pmix.tcp.ifinclude"    // (char*) comma-delimited list of devices and/or CIDR notation
#define PMIX_TCP_IF_EXCLUDE                 "pmix.tcp.ifexclude"    // (char*) comma-delimited list of devices and/or CIDR notation
#define PMIX_TCP_IPV4_PORT                  "pmix.tcp.ipv4"         // (int) IPv4 port to be used
#define PMIX_TCP_IPV6_PORT                  "pmix.tcp.ipv6"         // (int) IPv6 port to be used
#define PMIX_TCP_DISABLE_IPV4               "pmix.tcp.disipv4"      // (bool) true to disable IPv4 family
#define PMIX_TCP_DISABLE_IPV6               "pmix.tcp.disipv6"      // (bool) true to disable IPv6 family

/* attributes for GDS */
#define PMIX_GDS_MODULE                     "pmix.gds.mod"          // (char*) comma-delimited string of desired modules


/* general proc-level attributes */
#define PMIX_CPUSET                         "pmix.cpuset"           // (char*) hwloc bitmap applied to proc upon launch
#define PMIX_CREDENTIAL                     "pmix.cred"             // (char*) security credential assigned to proc
#define PMIX_SPAWNED                        "pmix.spawned"          // (bool) true if this proc resulted from a call to PMIx_Spawn
#define PMIX_ARCH                           "pmix.arch"             // (uint32_t) datatype architecture flag

/* scratch directory locations for use by applications */
#define PMIX_TMPDIR                         "pmix.tmpdir"           // (char*) top-level tmp dir assigned to session
#define PMIX_NSDIR                          "pmix.nsdir"            // (char*) sub-tmpdir assigned to namespace
#define PMIX_PROCDIR                        "pmix.pdir"             // (char*) sub-nsdir assigned to proc
#define PMIX_TDIR_RMCLEAN                   "pmix.tdir.rmclean"     // (bool)  Resource Manager will clean session directories

/* information about relative ranks as assigned by the RM */
#define PMIX_PROCID                         "pmix.procid"           // (pmix_proc_t) process identifier
#define PMIX_NSPACE                         "pmix.nspace"           // (char*) nspace of a job
#define PMIX_JOBID                          "pmix.jobid"            // (char*) jobid assigned by scheduler
#define PMIX_APPNUM                         "pmix.appnum"           // (uint32_t) app number within the job
#define PMIX_RANK                           "pmix.rank"             // (pmix_rank_t) process rank within the job
#define PMIX_GLOBAL_RANK                    "pmix.grank"            // (pmix_rank_t) rank spanning across all jobs in this session
#define PMIX_APP_RANK                       "pmix.apprank"          // (pmix_rank_t) rank within this app
#define PMIX_NPROC_OFFSET                   "pmix.offset"           // (pmix_rank_t) starting global rank of this job
#define PMIX_LOCAL_RANK                     "pmix.lrank"            // (uint16_t) rank on this node within this job
#define PMIX_NODE_RANK                      "pmix.nrank"            // (uint16_t) rank on this node spanning all jobs
#define PMIX_LOCALLDR                       "pmix.lldr"             // (pmix_rank_t) lowest rank on this node within this job
#define PMIX_APPLDR                         "pmix.aldr"             // (pmix_rank_t) lowest rank in this app within this job
#define PMIX_PROC_PID                       "pmix.ppid"             // (pid_t) pid of specified proc
#define PMIX_SESSION_ID                     "pmix.session.id"       // (uint32_t) session identifier

#define PMIX_NODE_LIST                      "pmix.nlist"            // (char*) comma-delimited list of nodes running procs for the specified nspace
#define PMIX_ALLOCATED_NODELIST             "pmix.alist"            // (char*) comma-delimited list of all nodes in this allocation regardless of
                                                                    //         whether or not they currently host procs.
#define PMIX_HOSTNAME                       "pmix.hname"            // (char*) name of the host the specified proc is on
#define PMIX_NODEID                         "pmix.nodeid"           // (uint32_t) node identifier where the specified proc is located
#define PMIX_LOCAL_PEERS                    "pmix.lpeers"           // (char*) comma-delimited string of ranks on this node within the specified nspace
#define PMIX_LOCAL_PROCS                    "pmix.lprocs"           // (pmix_proc_t array) array of pmix_proc_t of procs on the specified node
#define PMIX_LOCAL_CPUSETS                  "pmix.lcpus"            // (char*) colon-delimited cpusets of local peers within the specified nspace
#define PMIX_PROC_URI                       "pmix.puri"             // (char*) URI containing contact info for proc
#define PMIX_LOCALITY                       "pmix.loc"              // (uint16_t) relative locality of two procs

/* size info */
#define PMIX_UNIV_SIZE                      "pmix.univ.size"        // (uint32_t) #procs in this nspace
#define PMIX_JOB_SIZE                       "pmix.job.size"         // (uint32_t) #procs in this job
#define PMIX_JOB_NUM_APPS                   "pmix.job.napps"        // (uint32_t) #apps in this job
#define PMIX_APP_SIZE                       "pmix.app.size"         // (uint32_t) #procs in this application
#define PMIX_LOCAL_SIZE                     "pmix.local.size"       // (uint32_t) #procs in this job on this node
#define PMIX_NODE_SIZE                      "pmix.node.size"        // (uint32_t) #procs across all jobs on this node
#define PMIX_MAX_PROCS                      "pmix.max.size"         // (uint32_t) max #procs for this job
#define PMIX_NUM_NODES                      "pmix.num.nodes"        // (uint32_t) #nodes in this nspace

/* Memory info */
#define PMIX_AVAIL_PHYS_MEMORY              "pmix.pmem"             // (uint64_t) total available physical memory on this node
#define PMIX_DAEMON_MEMORY                  "pmix.dmn.mem"          // (float) Mbytes of memory currently used by daemon
#define PMIX_CLIENT_AVG_MEMORY              "pmix.cl.mem.avg"       // (float) Average Mbytes of memory used by client processes

/* topology info */
#define PMIX_NET_TOPO                       "pmix.ntopo"            // (char*) xml-representation of network topology
#define PMIX_LOCAL_TOPO                     "pmix.ltopo"            // (char*) xml-representation of local node topology
#define PMIX_NODE_LIST                      "pmix.nlist"            // (char*) comma-delimited list of nodes running procs for this job
#define PMIX_TOPOLOGY                       "pmix.topo"             // (hwloc_topology_t) pointer to the PMIx client's internal topology object
#define PMIX_TOPOLOGY_SIGNATURE             "pmix.toposig"          // (char*) topology signature string
#define PMIX_LOCALITY_STRING                "pmix.locstr"           // (char*) string describing a proc's location

/* request-related info */
#define PMIX_COLLECT_DATA                   "pmix.collect"          // (bool) collect data and return it at the end of the operation
#define PMIX_TIMEOUT                        "pmix.timeout"          // (int) time in sec before specified operation should time out (0 => infinite)
#define PMIX_IMMEDIATE                      "pmix.immediate"        // (bool) specified operation should immediately return an error if requested
                                                                    //        data cannot be found - do not request it from the host RM
#define PMIX_WAIT                           "pmix.wait"             // (int) caller requests that the server wait until at least the specified
                                                                    //       #values are found (0 => all and is the default)
#define PMIX_COLLECTIVE_ALGO                "pmix.calgo"            // (char*) comma-delimited list of algorithms to use for collective
#define PMIX_COLLECTIVE_ALGO_REQD           "pmix.calreqd"          // (bool) if true, indicates that the requested choice of algo is mandatory
#define PMIX_NOTIFY_COMPLETION              "pmix.notecomp"         // (bool) notify parent process upon termination of child job
#define PMIX_RANGE                          "pmix.range"            // (pmix_data_range_t) value for calls to publish/lookup/unpublish or for
                                                                    //        monitoring event notifications
#define PMIX_PERSISTENCE                    "pmix.persist"          // (pmix_persistence_t) value for calls to publish
#define PMIX_DATA_SCOPE                     "pmix.scope"            // (pmix_scope_t) scope of the data to be found in a PMIx_Get call
#define PMIX_OPTIONAL                       "pmix.optional"         // (bool) look only in the immediate data store for the requested value - do
                                                                    //        not request data from the server if not found
#define PMIX_EMBED_BARRIER                  "pmix.embed.barrier"    // (bool) execute a blocking fence operation before executing the
                                                                    //        specified operation

/* attributes used by host server to pass data to the server convenience library - the
 * data will then be parsed and provided to the local clients */
#define PMIX_PROC_DATA                      "pmix.pdata"            // (pmix_data_array_t) starts with rank, then contains more data
#define PMIX_NODE_MAP                       "pmix.nmap"             // (char*) regex of nodes containing procs for this job
#define PMIX_PROC_MAP                       "pmix.pmap"             // (char*) regex describing procs on each node within this job
#define PMIX_ANL_MAP                        "pmix.anlmap"           // (char*) process mapping in ANL notation (used in PMI-1/PMI-2)
#define PMIX_APP_MAP_TYPE                   "pmix.apmap.type"       // (char*) type of mapping used to layout the application (e.g., cyclic)
#define PMIX_APP_MAP_REGEX                  "pmix.apmap.regex"      // (char*) regex describing the result of the mapping

/* attributes used internally to communicate data from the server to the client */
#define PMIX_PROC_BLOB                      "pmix.pblob"            // (pmix_byte_object_t) packed blob of process data
#define PMIX_MAP_BLOB                       "pmix.mblob"            // (pmix_byte_object_t) packed blob of process location

/* event handler registration and notification info keys */
#define PMIX_EVENT_HDLR_NAME                "pmix.evname"           // (char*) string name identifying this handler
#define PMIX_EVENT_JOB_LEVEL                "pmix.evjob"            // (bool) register for job-specific events only
#define PMIX_EVENT_ENVIRO_LEVEL             "pmix.evenv"            // (bool) register for environment events only
#define PMIX_EVENT_HDLR_FIRST               "pmix.evfirst"          // (bool) invoke this event handler before any other handlers
#define PMIX_EVENT_HDLR_LAST                "pmix.evlast"           // (bool) invoke this event handler after all other handlers have been called
#define PMIX_EVENT_HDLR_FIRST_IN_CATEGORY   "pmix.evfirstcat"       // (bool) invoke this event handler before any other handlers in this category
#define PMIX_EVENT_HDLR_LAST_IN_CATEGORY    "pmix.evlastcat"        // (bool) invoke this event handler after all other handlers in this category have been called
#define PMIX_EVENT_HDLR_BEFORE              "pmix.evbefore"         // (char*) put this event handler immediately before the one specified in the (char*) value
#define PMIX_EVENT_HDLR_AFTER               "pmix.evafter"          // (char*) put this event handler immediately after the one specified in the (char*) value
#define PMIX_EVENT_HDLR_PREPEND             "pmix.evprepend"        // (bool) prepend this handler to the precedence list within its category
#define PMIX_EVENT_HDLR_APPEND              "pmix.evappend"         // (bool) append this handler to the precedence list within its category
#define PMIX_EVENT_CUSTOM_RANGE             "pmix.evrange"          // (pmix_data_array_t*) array of pmix_proc_t defining range of event notification
#define PMIX_EVENT_AFFECTED_PROC            "pmix.evproc"           // (pmix_proc_t) single proc that was affected
#define PMIX_EVENT_AFFECTED_PROCS           "pmix.evaffected"       // (pmix_data_array_t*) array of pmix_proc_t defining affected procs
#define PMIX_EVENT_NON_DEFAULT              "pmix.evnondef"         // (bool) event is not to be delivered to default event handlers
#define PMIX_EVENT_RETURN_OBJECT            "pmix.evobject"         // (void*) object to be returned whenever the registered cbfunc is invoked
                                                                    //     NOTE: the object will _only_ be returned to the process that
                                                                    //           registered it

/* fault tolerance-related events */
#define PMIX_EVENT_TERMINATE_SESSION        "pmix.evterm.sess"      // (bool) RM intends to terminate session
#define PMIX_EVENT_TERMINATE_JOB            "pmix.evterm.job"       // (bool) RM intends to terminate this job
#define PMIX_EVENT_TERMINATE_NODE           "pmix.evterm.node"      // (bool) RM intends to terminate all procs on this node
#define PMIX_EVENT_TERMINATE_PROC           "pmix.evterm.proc"      // (bool) RM intends to terminate just this process
#define PMIX_EVENT_ACTION_TIMEOUT           "pmix.evtimeout"        // (int) time in sec before RM will execute error response
#define PMIX_EVENT_NO_TERMINATION           "pmix.evnoterm"         // (bool) indicates that the handler has satisfactorily handled
                                                                    //        the event and believes termination of the application is not required
#define PMIX_EVENT_WANT_TERMINATION         "pmix.evterm"           // (bool) indicates that the handler has determined that the application should be terminated


/* attributes used to describe "spawn" attributes */
#define PMIX_PERSONALITY                    "pmix.pers"             // (char*) name of personality to use
#define PMIX_HOST                           "pmix.host"             // (char*) comma-delimited list of hosts to use for spawned procs
#define PMIX_HOSTFILE                       "pmix.hostfile"         // (char*) hostfile to use for spawned procs
#define PMIX_ADD_HOST                       "pmix.addhost"          // (char*) comma-delimited list of hosts to add to allocation
#define PMIX_ADD_HOSTFILE                   "pmix.addhostfile"      // (char*) hostfile to add to existing allocation
#define PMIX_PREFIX                         "pmix.prefix"           // (char*) prefix to use for starting spawned procs
#define PMIX_WDIR                           "pmix.wdir"             // (char*) working directory for spawned procs
#define PMIX_MAPPER                         "pmix.mapper"           // (char*) mapper to use for placing spawned procs
#define PMIX_DISPLAY_MAP                    "pmix.dispmap"          // (bool) display process map upon spawn
#define PMIX_PPR                            "pmix.ppr"              // (char*) #procs to spawn on each identified resource
#define PMIX_MAPBY                          "pmix.mapby"            // (char*) mapping policy
#define PMIX_RANKBY                         "pmix.rankby"           // (char*) ranking policy
#define PMIX_BINDTO                         "pmix.bindto"           // (char*) binding policy
#define PMIX_PRELOAD_BIN                    "pmix.preloadbin"       // (bool) preload binaries
#define PMIX_PRELOAD_FILES                  "pmix.preloadfiles"     // (char*) comma-delimited list of files to pre-position
#define PMIX_NON_PMI                        "pmix.nonpmi"           // (bool) spawned procs will not call PMIx_Init
#define PMIX_STDIN_TGT                      "pmix.stdin"            // (uint32_t) spawned proc rank that is to receive stdin
#define PMIX_FWD_STDIN                      "pmix.fwd.stdin"        // (bool) forward my stdin to the designated proc
#define PMIX_FWD_STDOUT                     "pmix.fwd.stdout"       // (bool) forward stdout from spawned procs to me
#define PMIX_FWD_STDERR                     "pmix.fwd.stderr"       // (bool) forward stderr from spawned procs to me
#define PMIX_DEBUGGER_DAEMONS               "pmix.debugger"         // (bool) spawned app consists of debugger daemons
#define PMIX_COSPAWN_APP                    "pmix.cospawn"          // (bool) designated app is to be spawned as a disconnected
                                                                    //     job - i.e., not part of the "comm_world" of the job

/* query attributes */
#define PMIX_QUERY_NAMESPACES               "pmix.qry.ns"           // (char*) request a comma-delimited list of active nspaces
#define PMIX_QUERY_JOB_STATUS               "pmix.qry.jst"          // (pmix_status_t) status of a specified currently executing job
#define PMIX_QUERY_QUEUE_LIST               "pmix.qry.qlst"         // (char*) request a comma-delimited list of scheduler queues
#define PMIX_QUERY_QUEUE_STATUS             "pmix.qry.qst"          // (TBD) status of a specified scheduler queue
#define PMIX_QUERY_PROC_TABLE               "pmix.qry.ptable"       // (char*) input nspace of job whose info is being requested
                                                                    //         returns (pmix_data_array_t) an array of pmix_proc_info_t
#define PMIX_QUERY_LOCAL_PROC_TABLE         "pmix.qry.lptable"      // (char*) input nspace of job whose info is being requested
                                                                    //         returns (pmix_data_array_t) an array of pmix_proc_info_t for
                                                                    //         procs in job on same node
#define PMIX_QUERY_AUTHORIZATIONS           "pmix.qry.auths"        // (bool) return operations tool is authorized to perform
#define PMIX_QUERY_SPAWN_SUPPORT            "pmix.qry.spawn"        // (bool) return a comma-delimited list of supported spawn attributes
#define PMIX_QUERY_DEBUG_SUPPORT            "pmix.qry.debug"        // (bool) return a comma-delimited list of supported debug attributes
#define PMIX_QUERY_MEMORY_USAGE             "pmix.qry.mem"          // (bool) return info on memory usage for the procs indicated in the qualifiers
#define PMIX_QUERY_LOCAL_ONLY               "pmix.qry.local"        // (bool) constrain the query to local information only
#define PMIX_QUERY_REPORT_AVG               "pmix.qry.avg"          // (bool) report average values
#define PMIX_QUERY_REPORT_MINMAX            "pmix.qry.minmax"       // (bool) report minimum and maximum value
#define PMIX_QUERY_ALLOC_STATUS             "pmix.query.alloc"      // (char*) string identifier of the allocation whose status
                                                                    //         is being requested
#define PMIX_TIME_REMAINING                 "pmix.time.remaining"   // (char*) query number of seconds (uint32_t) remaining in allocation
                                                                    //         for the specified nspace

/* log attributes */
#define PMIX_LOG_STDERR                     "pmix.log.stderr"       // (char*) log string to stderr
#define PMIX_LOG_STDOUT                     "pmix.log.stdout"       // (char*) log string to stdout
#define PMIX_LOG_SYSLOG                     "pmix.log.syslog"       // (char*) log data to syslog - defaults to ERROR priority unless
#define PMIX_LOG_MSG                        "pmix.log.msg"          // (pmix_byte_object_t) message blob to be sent somewhere
#define PMIX_LOG_EMAIL                      "pmix.log.email"        // (pmix_data_array_t) log via email based on pmix_info_t containing directives
#define PMIX_LOG_EMAIL_ADDR                 "pmix.log.emaddr"       // (char*) comma-delimited list of email addresses that are to recv msg
#define PMIX_LOG_EMAIL_SUBJECT              "pmix.log.emsub"        // (char*) subject line for email
#define PMIX_LOG_EMAIL_MSG                  "pmix.log.emmsg"        // (char*) msg to be included in email

/* debugger attributes */
#define PMIX_DEBUG_STOP_ON_EXEC             "pmix.dbg.exec"         // (bool) job is being spawned under debugger - instruct it to pause on start
#define PMIX_DEBUG_STOP_IN_INIT             "pmix.dbg.init"         // (bool) instruct job to stop during PMIx init
#define PMIX_DEBUG_WAIT_FOR_NOTIFY          "pmix.dbg.notify"       // (bool) block at desired point until receiving debugger release notification
#define PMIX_DEBUG_JOB                      "pmix.dbg.job"          // (char*) nspace of the job to be debugged - the RM/PMIx server are
#define PMIX_DEBUG_WAITING_FOR_NOTIFY       "pmix.dbg.waiting"      // (bool) job to be debugged is waiting for a release

/* Resource Manager identification */
#define PMIX_RM_NAME                        "pmix.rm.name"          // (char*) string name of the resource manager
#define PMIX_RM_VERSION                     "pmix.rm.version"       // (char*) RM version string

/* attributes for setting envars */
#define PMIX_SET_ENVAR                      "pmix.set.envar"        // (char*) string "key=value" value shall be put into the environment
#define PMIX_UNSET_ENVAR                    "pmix.unset.envar"      // (char*) unset envar specified in string

/* attributes relating to allocations */
#define PMIX_ALLOC_ID                       "pmix.alloc.id"         // (char*) provide a string identifier for this allocation request
                                                                    //         which can later be used to query status of the request
#define PMIX_ALLOC_NUM_NODES                "pmix.alloc.nnodes"     // (uint64_t) number of nodes
#define PMIX_ALLOC_NODE_LIST                "pmix.alloc.nlist"      // (char*) regex of specific nodes
#define PMIX_ALLOC_NUM_CPUS                 "pmix.alloc.ncpus"      // (uint64_t) number of cpus
#define PMIX_ALLOC_NUM_CPU_LIST             "pmix.alloc.ncpulist"   // (char*) regex of #cpus for each node
#define PMIX_ALLOC_CPU_LIST                 "pmix.alloc.cpulist"    // (char*) regex of specific cpus indicating the cpus involved.
#define PMIX_ALLOC_MEM_SIZE                 "pmix.alloc.msize"      // (float) number of Mbytes
#define PMIX_ALLOC_NETWORK                  "pmix.alloc.net"        // (array) array of pmix_info_t describing network resources. If not
                                                                    //         given as part of an info struct that identifies the
                                                                    //         impacted nodes, then the description will be applied
                                                                    //         across all nodes in the requestor's allocation
#define PMIX_ALLOC_NETWORK_ID               "pmix.alloc.netid"      // (char*) name of network
#define PMIX_ALLOC_BANDWIDTH                "pmix.alloc.bw"         // (float) Mbits/sec
#define PMIX_ALLOC_NETWORK_QOS              "pmix.alloc.netqos"     // (char*) quality of service level
#define PMIX_ALLOC_TIME                     "pmix.alloc.time"       // (uint32_t) time in seconds

/* job control attributes */
#define PMIX_JOB_CTRL_ID                    "pmix.jctrl.id"         // (char*) provide a string identifier for this request
#define PMIX_JOB_CTRL_PAUSE                 "pmix.jctrl.pause"      // (bool) pause the specified processes
#define PMIX_JOB_CTRL_RESUME                "pmix.jctrl.resume"     // (bool) "un-pause" the specified processes
#define PMIX_JOB_CTRL_CANCEL                "pmix.jctrl.cancel"     // (char*) cancel the specified request
                                                                    //         (NULL => cancel all requests from this requestor)
#define PMIX_JOB_CTRL_KILL                  "pmix.jctrl.kill"       // (bool) forcibly terminate the specified processes and cleanup
#define PMIX_JOB_CTRL_RESTART               "pmix.jctrl.restart"    // (char*) restart the specified processes using the given checkpoint ID
#define PMIX_JOB_CTRL_CHECKPOINT            "pmix.jctrl.ckpt"       // (char*) checkpoint the specified processes and assign the given ID to it
#define PMIX_JOB_CTRL_CHECKPOINT_EVENT      "pmix.jctrl.ckptev"     // (bool) use event notification to trigger process checkpoint
#define PMIX_JOB_CTRL_CHECKPOINT_SIGNAL     "pmix.jctrl.ckptsig"    // (int) use the given signal to trigger process checkpoint
#define PMIX_JOB_CTRL_CHECKPOINT_TIMEOUT    "pmix.jctrl.ckptsig"    // (int) time in seconds to wait for checkpoint to complete
#define PMIX_JOB_CTRL_CHECKPOINT_METHOD     "pmix.jctrl.ckmethod"   // (pmix_data_array_t) array of pmix_info_t declaring each
                                                                    //      method and value supported by this application
#define PMIX_JOB_CTRL_SIGNAL                "pmix.jctrl.sig"        // (int) send given signal to specified processes
#define PMIX_JOB_CTRL_PROVISION             "pmix.jctrl.pvn"        // (char*) regex identifying nodes that are to be provisioned
#define PMIX_JOB_CTRL_PROVISION_IMAGE       "pmix.jctrl.pvnimg"     // (char*) name of the image that is to be provisioned
#define PMIX_JOB_CTRL_PREEMPTIBLE           "pmix.jctrl.preempt"    // (bool) job can be pre-empted

/* monitoring attributes */
#define PMIX_MONITOR_ID                     "pmix.monitor.id"       // (char*) provide a string identifier for this request
#define PMIX_MONITOR_CANCEL                 "pmix.monitor.cancel"   // (char*) identifier to be canceled (NULL = cancel all
                                                                    //         monitoring for this process)
#define PMIX_MONITOR_APP_CONTROL            "pmix.monitor.appctrl"  // (bool) the application desires to control the response to
                                                                    //        a monitoring event
#define PMIX_MONITOR_HEARTBEAT              "pmix.monitor.mbeat"    // (void) register to have the server monitor the requestor for heartbeats
#define PMIX_SEND_HEARTBEAT                 "pmix.monitor.beat"     // (void) send heartbeat to local server
#define PMIX_MONITOR_HEARTBEAT_TIME         "pmix.monitor.btime"    // (uint32_t) time in seconds before declaring heartbeat missed
#define PMIX_MONITOR_HEARTBEAT_DROPS        "pmix.monitor.bdrop"    // (uint32_t) number of heartbeats that can be missed before
                                                                    //            generating the event
#define PMIX_MONITOR_FILE                   "pmix.monitor.fmon"     // (char*) register to monitor file for signs of life
#define PMIX_MONITOR_FILE_SIZE              "pmix.monitor.fsize"    // (bool) monitor size of given file is growing to determine app is running
#define PMIX_MONITOR_FILE_ACCESS            "pmix.monitor.faccess"  // (char*) monitor time since last access of given file to determine app is running
#define PMIX_MONITOR_FILE_MODIFY            "pmix.monitor.fmod"     // (char*) monitor time since last modified of given file to determine app is running
#define PMIX_MONITOR_FILE_CHECK_TIME        "pmix.monitor.ftime"    // (uint32_t) time in seconds between checking file
#define PMIX_MONITOR_FILE_DROPS             "pmix.monitor.fdrop"    // (uint32_t) number of file checks that can be missed before
                                                                    //            generating the event

/****    PROCESS STATE DEFINITIONS    ****/
typedef uint8_t pmix_proc_state_t;
#define PMIX_PROC_STATE_UNDEF                    0  /* undefined process state */
#define PMIX_PROC_STATE_PREPPED                  1  /* process is ready to be launched */
#define PMIX_PROC_STATE_LAUNCH_UNDERWAY          2  /* launch process underway */
#define PMIX_PROC_STATE_RESTART                  3  /* the proc is ready for restart */
#define PMIX_PROC_STATE_TERMINATE                4  /* process is marked for termination */
#define PMIX_PROC_STATE_RUNNING                  5  /* daemon has locally fork'd process */
#define PMIX_PROC_STATE_CONNECTED                6  /* proc connected to PMIx server */
/*
* Define a "boundary" so users can easily and quickly determine
* if a proc is still running or not - any value less than
* this one means that the proc has not terminated
*/
#define PMIX_PROC_STATE_UNTERMINATED            15

#define PMIX_PROC_STATE_TERMINATED              20  /* process has terminated and is no longer running */
/* Define a boundary so users can easily and quickly determine
* if a proc abnormally terminated - leave a little room
* for future expansion
*/
#define PMIX_PROC_STATE_ERROR                   50
/* Define specific error code values */
#define PMIX_PROC_STATE_KILLED_BY_CMD           (PMIX_PROC_STATE_ERROR +  1)  /* process was killed by cmd */
#define PMIX_PROC_STATE_ABORTED                 (PMIX_PROC_STATE_ERROR +  2)  /* process aborted */
#define PMIX_PROC_STATE_FAILED_TO_START         (PMIX_PROC_STATE_ERROR +  3)  /* process failed to start */
#define PMIX_PROC_STATE_ABORTED_BY_SIG          (PMIX_PROC_STATE_ERROR +  4)  /* process aborted by signal */
#define PMIX_PROC_STATE_TERM_WO_SYNC            (PMIX_PROC_STATE_ERROR +  5)  /* process exit'd w/o calling PMIx_Finalize */
#define PMIX_PROC_STATE_COMM_FAILED             (PMIX_PROC_STATE_ERROR +  6)  /* process communication has failed */
#define PMIX_PROC_STATE_CALLED_ABORT            (PMIX_PROC_STATE_ERROR +  7)  /* process called "PMIx_Abort" */
#define PMIX_PROC_STATE_MIGRATING               (PMIX_PROC_STATE_ERROR +  8)  /* process failed and is waiting for resources before restarting */
#define PMIX_PROC_STATE_CANNOT_RESTART          (PMIX_PROC_STATE_ERROR +  9)  /* process failed and cannot be restarted */
#define PMIX_PROC_STATE_TERM_NON_ZERO           (PMIX_PROC_STATE_ERROR + 10)  /* process exited with a non-zero status, indicating abnormal */
#define PMIX_PROC_STATE_FAILED_TO_LAUNCH        (PMIX_PROC_STATE_ERROR + 11)  /* unable to launch process */


/****    PMIX ERROR CONSTANTS    ****/
/* PMIx errors are always negative, with 0 reserved for success */
#define PMIX_ERR_BASE                   0

typedef int pmix_status_t;

/* v1.x error values - must be fixed in place for backward
 * compatability. Note that some number of these have been
 * deprecated and may not be returned by v2.x and above
 * clients or servers. However, they must always be
 * at least defined to ensure older codes will compile */
#define PMIX_SUCCESS                                 0
#define PMIX_ERROR                                  -1          // general error
#define PMIX_ERR_SILENT                             -2          // internal-only
/* debugger release flag */
#define PMIX_ERR_DEBUGGER_RELEASE                   -3
/* fault tolerance */
#define PMIX_ERR_PROC_RESTART                       -4
#define PMIX_ERR_PROC_CHECKPOINT                    -5
#define PMIX_ERR_PROC_MIGRATE                       -6
/* abort */
#define PMIX_ERR_PROC_ABORTED                       -7
#define PMIX_ERR_PROC_REQUESTED_ABORT               -8
#define PMIX_ERR_PROC_ABORTING                      -9
/* communication failures */
#define PMIX_ERR_SERVER_FAILED_REQUEST              -10
#define PMIX_EXISTS                                 -11
#define PMIX_ERR_INVALID_CRED                       -12         // internal-only
#define PMIX_ERR_HANDSHAKE_FAILED                   -13         // internal-only
#define PMIX_ERR_READY_FOR_HANDSHAKE                -14         // internal-only
#define PMIX_ERR_WOULD_BLOCK                        -15
#define PMIX_ERR_UNKNOWN_DATA_TYPE                  -16         // internal-only
#define PMIX_ERR_PROC_ENTRY_NOT_FOUND               -17         // internal-only
#define PMIX_ERR_TYPE_MISMATCH                      -18         // internal-only
#define PMIX_ERR_UNPACK_INADEQUATE_SPACE            -19         // internal-only
#define PMIX_ERR_UNPACK_FAILURE                     -20         // internal-only
#define PMIX_ERR_PACK_FAILURE                       -21         // internal-only
#define PMIX_ERR_PACK_MISMATCH                      -22         // internal-only
#define PMIX_ERR_NO_PERMISSIONS                     -23
#define PMIX_ERR_TIMEOUT                            -24
#define PMIX_ERR_UNREACH                            -25
#define PMIX_ERR_IN_ERRNO                           -26         // internal-only
#define PMIX_ERR_BAD_PARAM                          -27
#define PMIX_ERR_RESOURCE_BUSY                      -28         // internal-only
#define PMIX_ERR_OUT_OF_RESOURCE                    -29
#define PMIX_ERR_DATA_VALUE_NOT_FOUND               -30
#define PMIX_ERR_INIT                               -31
#define PMIX_ERR_NOMEM                              -32         // internal-only
#define PMIX_ERR_INVALID_ARG                        -33         // internal-only
#define PMIX_ERR_INVALID_KEY                        -34         // internal-only
#define PMIX_ERR_INVALID_KEY_LENGTH                 -35         // internal-only
#define PMIX_ERR_INVALID_VAL                        -36         // internal-only
#define PMIX_ERR_INVALID_VAL_LENGTH                 -37         // internal-only
#define PMIX_ERR_INVALID_LENGTH                     -38         // internal-only
#define PMIX_ERR_INVALID_NUM_ARGS                   -39         // internal-only
#define PMIX_ERR_INVALID_ARGS                       -40         // internal-only
#define PMIX_ERR_INVALID_NUM_PARSED                 -41         // internal-only
#define PMIX_ERR_INVALID_KEYVALP                    -42         // internal-only
#define PMIX_ERR_INVALID_SIZE                       -43
#define PMIX_ERR_INVALID_NAMESPACE                  -44
#define PMIX_ERR_SERVER_NOT_AVAIL                   -45         // internal-only
#define PMIX_ERR_NOT_FOUND                          -46
#define PMIX_ERR_NOT_SUPPORTED                      -47
#define PMIX_ERR_NOT_IMPLEMENTED                    -48
#define PMIX_ERR_COMM_FAILURE                       -49
#define PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER     -50         // internal-only

/* define a starting point for v2.x error values */
#define PMIX_ERR_V2X_BASE                   -100

/* v2.x communication errors */
#define PMIX_ERR_LOST_CONNECTION_TO_SERVER      (PMIX_ERR_V2X_BASE -  1)
#define PMIX_ERR_LOST_PEER_CONNECTION           (PMIX_ERR_V2X_BASE -  2)
#define PMIX_ERR_LOST_CONNECTION_TO_CLIENT      (PMIX_ERR_V2X_BASE -  3)
/* used by the query system */
#define PMIX_QUERY_PARTIAL_SUCCESS              (PMIX_ERR_V2X_BASE -  4)
/* request responses */
#define PMIX_NOTIFY_ALLOC_COMPLETE              (PMIX_ERR_V2X_BASE -  5)
/* job control */
#define PMIX_JCTRL_CHECKPOINT                   (PMIX_ERR_V2X_BASE -  6)    // monitored by client to trigger checkpoint operation
#define PMIX_JCTRL_CHECKPOINT_COMPLETE          (PMIX_ERR_V2X_BASE -  7)    // sent by client and monitored by server to notify that requested
                                                                            //     checkpoint operation has completed
#define PMIX_JCTRL_PREEMPT_ALERT                (PMIX_ERR_V2X_BASE -  8)    // monitored by client to detect RM intends to preempt
/* monitoring */
#define PMIX_MONITOR_HEARTBEAT_ALERT            (PMIX_ERR_V2X_BASE -  9)
#define PMIX_MONITOR_FILE_ALERT                 (PMIX_ERR_V2X_BASE - 10)

/* define a starting point for operational error constants so
 * we avoid renumbering when making additions */
#define PMIX_ERR_OP_BASE    PMIX_ERR_V2X_BASE-30

/* operational */
#define PMIX_ERR_EVENT_REGISTRATION             (PMIX_ERR_OP_BASE - 14)
#define PMIX_ERR_JOB_TERMINATED                 (PMIX_ERR_OP_BASE - 15)
#define PMIX_ERR_UPDATE_ENDPOINTS               (PMIX_ERR_OP_BASE - 16)
#define PMIX_MODEL_DECLARED                     (PMIX_ERR_OP_BASE - 17)
#define PMIX_GDS_ACTION_COMPLETE                (PMIX_ERR_OP_BASE - 18)

/* define a starting point for system error constants so
 * we avoid renumbering when making additions */
#define PMIX_ERR_SYS_BASE    PMIX_ERR_OP_BASE-100

/* system failures */
#define PMIX_ERR_NODE_DOWN                      (PMIX_ERR_SYS_BASE -  1)
#define PMIX_ERR_NODE_OFFLINE                   (PMIX_ERR_SYS_BASE -  2)


/* define a starting point for event handler error constants so
 * we avoid renumbering when making additions */
#define PMIX_ERR_EVHDLR_BASE    PMIX_ERR_SYS_BASE-100

/* used by event handlers */
#define PMIX_EVENT_NO_ACTION_TAKEN              (PMIX_ERR_EVHDLR_BASE -  1)
#define PMIX_EVENT_PARTIAL_ACTION_TAKEN         (PMIX_ERR_EVHDLR_BASE -  2)
#define PMIX_EVENT_ACTION_DEFERRED              (PMIX_ERR_EVHDLR_BASE -  3)
#define PMIX_EVENT_ACTION_COMPLETE              (PMIX_ERR_EVHDLR_BASE -  4)


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
typedef uint16_t pmix_data_type_t;
#define PMIX_UNDEF               0
#define PMIX_BOOL                1  // converted to/from native true/false to uint8 for pack/unpack
#define PMIX_BYTE                2  // a byte of data
#define PMIX_STRING              3  // NULL-terminated string
#define PMIX_SIZE                4  // size_t
#define PMIX_PID                 5  // OS-pid
#define PMIX_INT                 6
#define PMIX_INT8                7
#define PMIX_INT16               8
#define PMIX_INT32               9
#define PMIX_INT64              10
#define PMIX_UINT               11
#define PMIX_UINT8              12
#define PMIX_UINT16             13
#define PMIX_UINT32             14
#define PMIX_UINT64             15
#define PMIX_FLOAT              16
#define PMIX_DOUBLE             17
#define PMIX_TIMEVAL            18
#define PMIX_TIME               19
#define PMIX_STATUS             20  // needs to be tracked separately from integer for those times
                                    // when we are embedded and it needs to be converted to the
                                    // host error definitions
#define PMIX_VALUE              21
#define PMIX_PROC               22
#define PMIX_APP                23
#define PMIX_INFO               24
#define PMIX_PDATA              25
#define PMIX_BUFFER             26
#define PMIX_BYTE_OBJECT        27
#define PMIX_KVAL               28
#define PMIX_MODEX              29
#define PMIX_PERSIST            30
#define PMIX_POINTER            31
#define PMIX_SCOPE              32
#define PMIX_DATA_RANGE         33
#define PMIX_COMMAND            34
#define PMIX_INFO_DIRECTIVES    35
#define PMIX_DATA_TYPE          36
#define PMIX_PROC_STATE         37
#define PMIX_PROC_INFO          38
#define PMIX_DATA_ARRAY         39
#define PMIX_PROC_RANK          40
#define PMIX_QUERY              41
#define PMIX_COMPRESSED_STRING  42  // string compressed with zlib
#define PMIX_ALLOC_DIRECTIVE    43
/**** DEPRECATED ****/
#define PMIX_INFO_ARRAY         44
/********************/

/* define a boundary for implementers so they can add their own data types */
#define PMIX_DATA_TYPE_MAX     500


/* define a scope for data "put" by PMIx per the following:
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
typedef uint8_t pmix_scope_t;
#define PMIX_SCOPE_UNDEF    0
#define PMIX_LOCAL          1   // share to procs also on this node
#define PMIX_REMOTE         2   // share with procs not on this node
#define PMIX_GLOBAL         3   // share with all procs (local + remote)
#define PMIX_INTERNAL       4   // store data in the internal tables

/* define a range for data "published" by PMI
 */
typedef uint8_t pmix_data_range_t;
#define PMIX_RANGE_UNDEF        0
#define PMIX_RANGE_RM           1   // data is intended for the host resource manager
#define PMIX_RANGE_LOCAL        2   // available on local node only
#define PMIX_RANGE_NAMESPACE    3   // data is available to procs in the same nspace only
#define PMIX_RANGE_SESSION      4   // data available to all procs in session
#define PMIX_RANGE_GLOBAL       5   // data available to all procs
#define PMIX_RANGE_CUSTOM       6   // range is specified in a pmix_info_t
#define PMIX_RANGE_PROC_LOCAL   7   // restrict range to the local proc

/* define a "persistence" policy for data published by clients */
typedef uint8_t pmix_persistence_t;
#define PMIX_PERSIST_INDEF          0   // retain until specifically deleted
#define PMIX_PERSIST_FIRST_READ     1   // delete upon first access
#define PMIX_PERSIST_PROC           2   // retain until publishing process terminates
#define PMIX_PERSIST_APP            3   // retain until application terminates
#define PMIX_PERSIST_SESSION        4   // retain until session/allocation terminates

/* define a set of bit-mask flags for specifying behavior of
 * command directives via pmix_info_t arrays */
typedef uint32_t pmix_info_directives_t;
#define PMIX_INFO_REQD          0x0001


/* define a set of directives for allocation requests */
typedef uint8_t pmix_alloc_directive_t;
#define PMIX_ALLOC_NEW          1  // new allocation is being requested. The resulting allocation will be
                                   // disjoint (i.e., not connected in a job sense) from the requesting allocation
#define PMIX_ALLOC_EXTEND       2  // extend the existing allocation, either in time or as additional resources
#define PMIX_ALLOC_RELEASE      3  // release part of the existing allocation. Attributes in the accompanying
                                   // pmix\_info\_t array may be used to specify permanent release of the
                                   // identified resources, or "lending" of those resources for some period
                                   // of time.
#define PMIX_ALLOC_REAQUIRE     4  // reacquire resources that were previously "lent" back to the scheduler

/* define a value boundary beyond which implementers are free
 * to define their own directive values */
#define PMIX_ALLOC_EXTERNAL     128


/****    PMIX BYTE OBJECT    ****/
typedef struct pmix_byte_object {
    char *bytes;
    size_t size;
} pmix_byte_object_t;
#define PMIX_BYTE_OBJECT_DESTRUCT(m)    \
    do {                                \
        if (NULL != (m)->bytes) {       \
            free((m)->bytes);           \
        }                               \
    } while(0)

#define PMIX_BYTE_OBJECT_FREE(m, n)         \
    do {                                    \
        size_t _n;                          \
        for (_n=0; _n < n; _n++) {          \
            if (NULL != (m)[_n].bytes) {    \
                free((m)[_n].bytes);        \
            }                               \
        }                                   \
        free((m));                          \
    } while(0)


/****    PMIX PROC OBJECT    ****/
typedef struct pmix_proc {
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_rank_t rank;
} pmix_proc_t;
#define PMIX_PROC_CREATE(m, n)                                  \
    do {                                                        \
        (m) = (pmix_proc_t*)calloc((n) , sizeof(pmix_proc_t));  \
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


/****    PMIX PROC INFO STRUCT    ****/
typedef struct pmix_proc_info {
    pmix_proc_t proc;
    char *hostname;
    char *executable_name;
    pid_t pid;
    int exit_code;
    pmix_proc_state_t state;
} pmix_proc_info_t;
#define PMIX_PROC_INFO_CREATE(m, n)                                         \
    do {                                                                    \
        (m) = (pmix_proc_info_t*)calloc((n) , sizeof(pmix_proc_info_t));    \
    } while (0)

#define PMIX_PROC_INFO_RELEASE(m)   \
    do {                            \
        PMIX_PROC_INFO_FREE((m));   \
    } while (0)

#define PMIX_PROC_INFO_CONSTRUCT(m)                 \
    do {                                            \
        memset((m), 0, sizeof(pmix_proc_info_t));   \
    } while (0)

#define PMIX_PROC_INFO_DESTRUCT(m)              \
    do {                                        \
        if (NULL != (m)->hostname) {            \
            free((m)->hostname);                \
        }                                       \
        if (NULL != (m)->executable_name) {     \
            free((m)->executable_name);         \
        }                                       \
    } while(0)

#define PMIX_PROC_INFO_FREE(m, n)                   \
    do {                                            \
        size_t _k;                                  \
        if (NULL != (m)) {                          \
            for (_k=0; _k < (n); _k++) {            \
                PMIX_PROC_INFO_DESTRUCT(&(m)[_k]);  \
            }                                       \
            free((m));                              \
        }                                           \
    } while (0)


/****    PMIX VALUE STRUCT    ****/
typedef struct pmix_info_t pmix_info_t;

typedef struct pmix_data_array {
    pmix_data_type_t type;
    size_t size;
    void *array;
} pmix_data_array_t;

typedef struct pmix_info_array {
    size_t size;
    pmix_info_t *array;
} pmix_info_array_t;
/********************/

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
        time_t time;
        pmix_status_t status;
        pmix_rank_t rank;
        pmix_proc_t *proc;
        pmix_byte_object_t bo;
        pmix_persistence_t persist;
        pmix_scope_t scope;
        pmix_data_range_t range;
        pmix_proc_state_t state;
        pmix_proc_info_t *pinfo;
        pmix_data_array_t *darray;
        void *ptr;
        pmix_alloc_directive_t adir;
        /**** DEPRECATED ****/
        pmix_info_array_t *array;
        /********************/
    } data;
} pmix_value_t;
/* allocate and initialize a specified number of value structs */
#define PMIX_VALUE_CREATE(m, n)                                 \
    do {                                                        \
        int _ii;                                                \
        pmix_value_t *_v;                                       \
        (m) = (pmix_value_t*)calloc((n), sizeof(pmix_value_t)); \
        _v = (pmix_value_t*)(m);                                \
        if (NULL != (m)) {                                      \
            for (_ii=0; _ii < (int)(n); _ii++) {                \
                _v[_ii].type = PMIX_UNDEF;                     \
            }                                                   \
        }                                                       \
    } while (0)

/* release a single pmix_value_t struct, including its data */
#define PMIX_VALUE_RELEASE(m)       \
    do {                            \
        PMIX_VALUE_DESTRUCT((m));   \
        free((m));                  \
    } while (0)

/* initialize a single value struct */
#define PMIX_VALUE_CONSTRUCT(m)                 \
    do {                                        \
        memset((m), 0, sizeof(pmix_value_t));   \
        (m)->type = PMIX_UNDEF;                 \
    } while (0)

/* release the memory in the value struct data field */
#define PMIX_VALUE_DESTRUCT(m)                                                  \
    do {                                                                        \
        size_t _n;                                                              \
        if (PMIX_STRING == (m)->type) {                                         \
            if (NULL != (m)->data.string) {                                     \
                free((m)->data.string);                                         \
            }                                                                   \
        } else if ((PMIX_BYTE_OBJECT == (m)->type) ||                           \
                   (PMIX_COMPRESSED_STRING == (m)->type)) {                     \
            if (NULL != (m)->data.bo.bytes) {                                   \
                free((m)->data.bo.bytes);                                       \
            }                                                                   \
        } else if (PMIX_DATA_ARRAY == (m)->type) {                              \
            if (PMIX_STRING == (m)->data.darray->type) {                        \
                char **_str = (char**)(m)->data.darray->array;                  \
                for (_n=0; _n < (m)->data.darray->size; _n++) {                 \
                    if (NULL != _str[_n]) {                                     \
                        free(_str[_n]);                                         \
                    }                                                           \
                }                                                               \
            } else if (PMIX_PROC_INFO == (m)->data.darray->type) {              \
                pmix_proc_info_t *_info =                                       \
                            (pmix_proc_info_t*)(m)->data.darray->array;         \
                for (_n=0; _n < (m)->data.darray->size; _n++) {                 \
                    PMIX_PROC_INFO_DESTRUCT(&_info[_n]);                        \
                }                                                               \
            } else if (PMIX_INFO == (m)->data.darray->type) {                   \
                pmix_info_t *_info =                                            \
                            (pmix_info_t*)(m)->data.darray->array;              \
                for (_n=0; _n < (m)->data.darray->size; _n++) {                 \
                    /* cannot use info destruct as that loops back */           \
                    if (PMIX_STRING == _info[_n].value.type) {                  \
                        if (NULL != _info[_n].value.data.string) {              \
                            free(_info[_n].value.data.string);                  \
                        }                                                       \
                    } else if (PMIX_BYTE_OBJECT == _info[_n].value.type) {      \
                        if (NULL != _info[_n].value.data.bo.bytes) {            \
                            free(_info[_n].value.data.bo.bytes);                \
                        }                                                       \
                    } else if (PMIX_PROC_INFO == _info[_n].value.type) {        \
                        PMIX_PROC_INFO_DESTRUCT(_info[_n].value.data.pinfo);    \
                    }                                                           \
                }                                                               \
            } else if (PMIX_BYTE_OBJECT == (m)->data.darray->type) {            \
                pmix_byte_object_t *_obj =                                      \
                            (pmix_byte_object_t*)(m)->data.darray->array;       \
                for (_n=0; _n < (m)->data.darray->size; _n++) {                 \
                    if (NULL != _obj[_n].bytes) {                               \
                        free(_obj[_n].bytes);                                   \
                    }                                                           \
                }                                                               \
            }                                                                   \
            if (NULL != (m)->data.darray->array) {                              \
                free((m)->data.darray->array);                                  \
            }                                                                   \
            free((m)->data.darray);                                             \
        /**** DEPRECATED ****/                                                  \
        } else if (PMIX_INFO_ARRAY == (m)->type) {                              \
            pmix_info_t *_p = (pmix_info_t*)((m)->data.array->array);           \
            for (_n=0; _n < (m)->data.array->size; _n++) {                      \
                if (PMIX_STRING == _p[_n].value.type) {                         \
                    if (NULL != _p[_n].value.data.string) {                     \
                        free(_p[_n].value.data.string);                         \
                    }                                                           \
                } else if (PMIX_BYTE_OBJECT == _p[_n].value.type) {             \
                    if (NULL != _p[_n].value.data.bo.bytes) {                   \
                        free(_p[_n].value.data.bo.bytes);                       \
                    }                                                           \
                } else if (PMIX_PROC_INFO == _p[_n].value.type) {               \
                    PMIX_PROC_INFO_DESTRUCT(_p[_n].value.data.pinfo);           \
                }                                                               \
            }                                                                   \
            free(_p);                                                           \
        /********************/                                                  \
        }                                                                       \
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

/* expose some functions that are resolved in the
 * PMIx library, but part of a header that
 * includes internal functions - we don't
 * want to expose the entire header here. For
 * consistency, we provide macro versions as well
 */
void pmix_value_load(pmix_value_t *v, const void *data, pmix_data_type_t type);
#define PMIX_VALUE_LOAD(v, d, t) \
    pmix_value_load((v), (d), (t))

pmix_status_t pmix_value_xfer(pmix_value_t *kv, pmix_value_t *src);
#define PMIX_VALUE_XFER(r, v, s)                                \
    do {                                                        \
        if (NULL == (v)) {                                      \
            (v) = (pmix_value_t*)malloc(sizeof(pmix_value_t));  \
            if (NULL == (v)) {                                  \
                (r) = PMIX_ERR_NOMEM;                           \
            } else {                                            \
                (r) = pmix_value_xfer((v), (s));                \
            }                                                   \
        } else {                                                \
            (r) = pmix_value_xfer((v), (s));                    \
        }                                                       \
    } while(0)

pmix_status_t pmix_argv_append_nosize(char ***argv, const char *arg);
#define PMIX_ARGV_APPEND(r, a, b) \
    (r) = pmix_argv_append_nosize(&(a), (b))

pmix_status_t pmix_setenv(const char *name, const char *value,
                              bool overwrite, char ***env);
#define PMIX_SETENV(r, a, b, c) \
    (r) = pmix_setenv((a), (b), true, (c))


/****    PMIX INFO STRUCT    ****/
struct pmix_info_t {
    char key[PMIX_MAX_KEYLEN+1];    // ensure room for the NULL terminator
    pmix_info_directives_t flags;   // bit-mask of flags
    pmix_value_t value;
};

/* utility macros for working with pmix_info_t structs */
#define PMIX_INFO_CREATE(m, n)                                  \
    do {                                                        \
        (m) = (pmix_info_t*)calloc((n), sizeof(pmix_info_t));   \
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
        (d)->flags = (s)->flags;                      \
        pmix_value_xfer(&(d)->value, &(s)->value);          \
    } while(0)

#define PMIX_INFO_REQUIRED(m)       \
    (m)->flags |= PMIX_INFO_REQD;
#define PMIX_INFO_OPTIONAL(m)       \
    (m)->flags &= ~PMIX_INFO_REQD;


/****    PMIX LOOKUP RETURN STRUCT    ****/
typedef struct pmix_pdata {
    pmix_proc_t proc;
    char key[PMIX_MAX_KEYLEN+1];  // ensure room for the NULL terminator
    pmix_value_t value;
} pmix_pdata_t;

/* utility macros for working with pmix_pdata_t structs */
#define PMIX_PDATA_CREATE(m, n)                                 \
    do {                                                        \
        (m) = (pmix_pdata_t*)calloc((n), sizeof(pmix_pdata_t)); \
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

#define PMIX_PDATA_LOAD(m, p, k, v, t)                                      \
    do {                                                                    \
        if (NULL != (m)) {                                                  \
            memset((m), 0, sizeof(pmix_pdata_t));                           \
            (void)strncpy((m)->proc.nspace, (p)->nspace, PMIX_MAX_NSLEN);   \
            (m)->proc.rank = (p)->rank;                                     \
            (void)strncpy((m)->key, (k), PMIX_MAX_KEYLEN);                  \
            pmix_value_load(&((m)->value), (v), (t));                       \
        }                                                                   \
    } while (0)

#define PMIX_PDATA_XFER(d, s)                                                   \
    do {                                                                        \
        if (NULL != (d)) {                                                      \
            memset((d), 0, sizeof(pmix_pdata_t));                               \
            (void)strncpy((d)->proc.nspace, (s)->proc.nspace, PMIX_MAX_NSLEN);  \
            (d)->proc.rank = (s)->proc.rank;                                    \
            (void)strncpy((d)->key, (s)->key, PMIX_MAX_KEYLEN);                 \
            pmix_value_xfer(&((d)->value), &((s)->value));                      \
        }                                                                       \
    } while (0)


/****    PMIX APP STRUCT    ****/
typedef struct pmix_app {
    char *cmd;
    char **argv;
    char **env;
    char *cwd;
    int maxprocs;
    pmix_info_t *info;
    size_t ninfo;
} pmix_app_t;
/* utility macros for working with pmix_app_t structs */
#define PMIX_APP_CREATE(m, n)                                   \
    do {                                                        \
        (m) = (pmix_app_t*)calloc((n), sizeof(pmix_app_t));     \
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
        if (NULL != (m)->cwd) {                                 \
            free((m)->cwd);                                     \
        }                                                       \
        if (NULL != (m)->info) {                                \
            for (_ii=0; _ii < (m)->ninfo; _ii++) {              \
                PMIX_INFO_DESTRUCT(&(m)->info[_ii]);            \
            }                                                   \
            free((m)->info);                                    \
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


/****    PMIX QUERY STRUCT    ****/
typedef struct pmix_query {
    char **keys;
    pmix_info_t *qualifiers;
    size_t nqual;
} pmix_query_t;
/* utility macros for working with pmix_query_t structs */
#define PMIX_QUERY_CREATE(m, n)                                     \
    do {                                                            \
        (m) = (pmix_query_t*)calloc((n) , sizeof(pmix_query_t));    \
    } while (0)

#define PMIX_QUERY_RELEASE(m)       \
    do {                            \
        PMIX_QUERY_DESTRUCT((m));   \
        free((m));                  \
    } while (0)

#define PMIX_QUERY_CONSTRUCT(m)                 \
    do {                                        \
        memset((m), 0, sizeof(pmix_query_t));   \
    } while (0)

#define PMIX_QUERY_DESTRUCT(m)                                  \
    do {                                                        \
        size_t _ii;                                             \
        if (NULL != (m)->keys) {                                \
            for (_ii=0; NULL != (m)->keys[_ii]; _ii++) {        \
                free((m)->keys[_ii]);                           \
            }                                                   \
            free((m)->keys);                                    \
        }                                                       \
        if (NULL != (m)->qualifiers) {                          \
            for (_ii=0; _ii < (m)->nqual; _ii++) {              \
                PMIX_INFO_DESTRUCT(&(m)->qualifiers[_ii]);      \
            }                                                   \
            free((m)->qualifiers);                              \
        }                                                       \
    } while (0)

#define PMIX_QUERY_FREE(m, n)                       \
    do {                                            \
        size_t _s;                                  \
        if (NULL != (m)) {                          \
            for (_s=0; _s < (n); _s++) {            \
                PMIX_QUERY_DESTRUCT(&((m)[_s]));    \
            }                                       \
            free((m));                              \
        }                                           \
    } while (0)




/****    PMIX MODEX STRUCT    ****/
typedef struct pmix_modex_data {
    char nspace[PMIX_MAX_NSLEN+1];
    int rank;
    uint8_t *blob;
    size_t size;
} pmix_modex_data_t;
/* utility macros for working with pmix_modex_t structs */
#define PMIX_MODEX_CREATE(m, n)                                             \
    do {                                                                    \
        (m) = (pmix_modex_data_t*)calloc((n) , sizeof(pmix_modex_data_t));  \
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

/* Provide a string representation for several types of value. Note
 * that the provided string is statically defined and must NOT be
 * free'd. Supported value types:
 *
 * - pmix_status_t (PMIX_STATUS)
 * - pmix_scope_t   (PMIX_SCOPE)
 * - pmix_persistence_t  (PMIX_PERSIST)
 * - pmix_data_range_t   (PMIX_DATA_RANGE)
 * - pmix_info_directives_t   (PMIX_INFO_DIRECTIVES)
 * - pmix_data_type_t   (PMIX_DATA_TYPE)
 * - pmix_alloc_directive_t  (PMIX_ALLOC_DIRECTIVE)
 */
const char* PMIx_Error_string(pmix_status_t status);
const char* PMIx_Proc_state_string(pmix_proc_state_t state);
const char* PMIx_Scope_string(pmix_scope_t scope);
const char* PMIx_Persistence_string(pmix_persistence_t persist);
const char* PMIx_Data_range_string(pmix_data_range_t range);
const char* PMIx_Info_directives_string(pmix_info_directives_t directives);
const char* PMIx_Data_type_string(pmix_data_type_t type);
const char* PMIx_Alloc_directive_string(pmix_alloc_directive_t directive);

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

#define PMIX_VAL_ASSIGN(_v, _field, _val) \
    PMIX_VAL_set_assign(_v, _field, _val)

#define PMIX_VAL_CMP(_field, _val1, _val2) \
    PMIX_VAL_CMP_ ## _field(_val1, _val2)

#define PMIX_VAL_FREE(_v) \
     PMIx_free_value_data(_v)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

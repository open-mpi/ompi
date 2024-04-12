/*
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
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
 * $HEADER$
 *
 * PMIx provides a "function-shipping" approach to support for
 * implementing the server-side of the protocol. This method allows
 * resource managers to implement the server without being burdened
 * with PMIx internal details. Accordingly, each PMIx API is mirrored
 * here in a function call to be provided by the server. When a
 * request is received from the client, the corresponding server function
 * will be called with the information.
 *
 * Any functions not supported by the RM can be indicated by a NULL for
 * the function pointer. Client calls to such functions will have a
 * "not supported" error returned.
 */

#ifndef PMIx_DEPRECATED_H
#define PMIx_DEPRECATED_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/***** v4 DECPRECATIONS/REMOVALS *****/
/* APIs */
PMIX_EXPORT pmix_status_t PMIx_tool_connect_to_server(pmix_proc_t *proc,
                                                      pmix_info_t info[], size_t ninfo);

/* DATATYPES */
#define PMIX_BUFFER                     26

/****    PMIX ERROR CONSTANTS    ****/
#define PMIX_ERR_SILENT                             -2
#define PMIX_ERR_DEBUGGER_RELEASE                   -3
#define PMIX_ERR_PROC_ABORTED                       -7
#define PMIX_ERR_PROC_ABORTING                      -9
#define PMIX_ERR_SERVER_FAILED_REQUEST              -10
#define PMIX_EXISTS                                 -11
#define PMIX_ERR_HANDSHAKE_FAILED                   -13
#define PMIX_ERR_READY_FOR_HANDSHAKE                -14
#define PMIX_ERR_PROC_ENTRY_NOT_FOUND               -17
#define PMIX_ERR_PACK_MISMATCH                      -22
#define PMIX_ERR_IN_ERRNO                           -26
#define PMIX_ERR_DATA_VALUE_NOT_FOUND               -30
#define PMIX_ERR_INVALID_ARG                        -33
#define PMIX_ERR_INVALID_KEY                        -34
#define PMIX_ERR_INVALID_KEY_LENGTH                 -35
#define PMIX_ERR_INVALID_VAL                        -36
#define PMIX_ERR_INVALID_VAL_LENGTH                 -37
#define PMIX_ERR_INVALID_LENGTH                     -38
#define PMIX_ERR_INVALID_NUM_ARGS                   -39
#define PMIX_ERR_INVALID_ARGS                       -40
#define PMIX_ERR_INVALID_NUM_PARSED                 -41
#define PMIX_ERR_INVALID_KEYVALP                    -42
#define PMIX_ERR_INVALID_SIZE                       -43
#define PMIX_ERR_INVALID_NAMESPACE                  -44
#define PMIX_ERR_SERVER_NOT_AVAIL                   -45
#define PMIX_ERR_NOT_IMPLEMENTED                    -48
#define PMIX_DEBUG_WAITING_FOR_NOTIFY               -58
#define PMIX_ERR_FATAL                              -63
#define PMIX_ERR_NOT_AVAILABLE                      -64
#define PMIX_ERR_VALUE_OUT_OF_BOUNDS                -65
#define PMIX_ERR_FILE_OPEN_FAILURE                  -67
#define PMIX_ERR_FILE_READ_FAILURE                  -68
#define PMIX_ERR_FILE_WRITE_FAILURE                 -69
#define PMIX_ERR_SYS_LIMITS_PIPES                   -70
#define PMIX_ERR_SYS_LIMITS_CHILDREN                -71
#define PMIX_ERR_PIPE_SETUP_FAILURE                 -72
#define PMIX_ERR_EXE_NOT_ACCESSIBLE                 -73
#define PMIX_ERR_JOB_WDIR_NOT_ACCESSIBLE            -74
#define PMIX_ERR_SYS_LIMITS_FILES                   -75
#define PMIX_ERR_LOST_CONNECTION_TO_SERVER          -101
#define PMIX_ERR_LOST_PEER_CONNECTION               -102
#define PMIX_ERR_LOST_CONNECTION_TO_CLIENT          -103
#define PMIX_NOTIFY_ALLOC_COMPLETE                  -105
#define PMIX_ERR_INVALID_TERMINATION                -112
#define PMIX_ERR_JOB_TERMINATED                     -145    // DEPRECATED NAME - non-error termination
#define PMIX_ERR_UPDATE_ENDPOINTS                   -146
#define PMIX_GDS_ACTION_COMPLETE                    -148
#define PMIX_PROC_HAS_CONNECTED                     -149
#define PMIX_CONNECT_REQUESTED                      -150
#define PMIX_ERR_NODE_DOWN                          -231
#define PMIX_ERR_NODE_OFFLINE                       -232

#define PMIX_ERR_SYS_BASE                           PMIX_EVENT_SYS_BASE
#define PMIX_ERR_SYS_OTHER                          PMIX_EVENT_SYS_OTHER

#define PMIX_JOB_STATE_PREPPED                   1  // job is ready to be launched

/* ATTRIBUTES */
#define PMIX_EVENT_BASE                     "pmix.evbase"           // (struct event_base *) pointer to libevent event_base
                                                                    // to use in place of the internal progress thread ***** DEPRECATED *****
#define PMIX_TOPOLOGY                       "pmix.topo"             // (hwloc_topology_t) ***** DEPRECATED ***** pointer to the PMIx client's internal
                                                                    //         topology object
#define PMIX_DEBUG_JOB                      "pmix.dbg.job"          // (char*) ***** DEPRECATED ***** nspace of the job assigned to this debugger to be debugged. Note
                                                                    //         that id's, pids, and other info on the procs is available
                                                                    //         via a query for the nspace's local or global proctable
#define PMIX_RECONNECT_SERVER               "pmix.cnct.recon"       // (bool) tool is requesting to change server connections

/* attributes for the USOCK rendezvous socket  */
#define PMIX_USOCK_DISABLE                  "pmix.usock.disable"    // (bool) disable legacy usock support
#define PMIX_SOCKET_MODE                    "pmix.sockmode"         // (uint32_t) POSIX mode_t (9 bits valid)
#define PMIX_SINGLE_LISTENER                "pmix.sing.listnr"      // (bool) use only one rendezvous socket, letting priorities and/or
                                                                    //        MCA param select the active transport
#define PMIX_ALLOC_NETWORK                  "pmix.alloc.net"        // (pmix_data_array_t*) ***** DEPRECATED *****
#define PMIX_ALLOC_NETWORK_ID               "pmix.alloc.netid"      // (char*) ***** DEPRECATED *****
#define PMIX_ALLOC_NETWORK_QOS              "pmix.alloc.netqos"     // (char*) ***** DEPRECATED *****
#define PMIX_ALLOC_NETWORK_TYPE             "pmix.alloc.nettype"    // (char*) ***** DEPRECATED *****
#define PMIX_ALLOC_NETWORK_PLANE            "pmix.alloc.netplane"   // (char*) ***** DEPRECATED *****
#define PMIX_ALLOC_NETWORK_ENDPTS           "pmix.alloc.endpts"     // (size_t) ***** DEPRECATED *****
#define PMIX_ALLOC_NETWORK_ENDPTS_NODE      "pmix.alloc.endpts.nd"  // (size_t) ***** DEPRECATED *****
#define PMIX_ALLOC_NETWORK_SEC_KEY          "pmix.alloc.nsec"       // (pmix_byte_object_t) ***** DEPRECATED *****
#define PMIX_PROC_DATA                      "pmix.pdata"            // (pmix_data_array_t*) ***** DEPRECATED ***** starts with rank, then contains more data
#define PMIX_LOCALITY                       "pmix.loc"              // (uint16_t)  ***** DEPRECATED *****relative locality of two procs

#define PMIX_LOCAL_TOPO                     "pmix.ltopo"            // (char*)  ***** DEPRECATED *****xml-representation of local node topology
#define PMIX_TOPOLOGY_XML                   "pmix.topo.xml"         // (char*)  ***** DEPRECATED *****XML-based description of topology
#define PMIX_TOPOLOGY_FILE                  "pmix.topo.file"        // (char*)  ***** DEPRECATED *****full path to file containing XML topology description
#define PMIX_TOPOLOGY_SIGNATURE             "pmix.toposig"          // (char*)  ***** DEPRECATED *****topology signature string
#define PMIX_HWLOC_SHMEM_ADDR               "pmix.hwlocaddr"        // (size_t)  ***** DEPRECATED *****address of HWLOC shared memory segment
#define PMIX_HWLOC_SHMEM_SIZE               "pmix.hwlocsize"        // (size_t)  ***** DEPRECATED *****size of HWLOC shared memory segment
#define PMIX_HWLOC_SHMEM_FILE               "pmix.hwlocfile"        // (char*)  ***** DEPRECATED *****path to HWLOC shared memory file
#define PMIX_HWLOC_XML_V1                   "pmix.hwlocxml1"        // (char*)  ***** DEPRECATED ***** XML representation of local topology using HWLOC v1.x format
#define PMIX_HWLOC_XML_V2                   "pmix.hwlocxml2"        // (char*) ***** DEPRECATED ***** XML representation of local topology using HWLOC v2.x format
#define PMIX_HWLOC_SHARE_TOPO               "pmix.hwlocsh"          // (bool) ***** DEPRECATED ***** Share the HWLOC topology via shared memory
#define PMIX_HWLOC_HOLE_KIND                "pmix.hwlocholek"       // (char*) ***** DEPRECATED ***** Kind of VM "hole" HWLOC should use for shared memory
#define PMIX_DSTPATH                        "pmix.dstpath"          // (char*) ***** DEPRECATED ***** path to dstore files
#define PMIX_COLLECTIVE_ALGO                "pmix.calgo"            // (char*) ***** DEPRECATED ***** comma-delimited list of algorithms to use for collective
#define PMIX_COLLECTIVE_ALGO_REQD           "pmix.calreqd"          // (bool) ***** DEPRECATED ***** if true, indicates that the requested choice of algo is mandatory
#define PMIX_PROC_BLOB                      "pmix.pblob"            // (pmix_byte_object_t) ***** DEPRECATED ***** packed blob of process data
#define PMIX_MAP_BLOB                       "pmix.mblob"            // (pmix_byte_object_t) ***** DEPRECATED ***** packed blob of process location
#define PMIX_MAPPER                         "pmix.mapper"           // (char*) ***** DEPRECATED ***** mapper to use for placing spawned procs
#define PMIX_NON_PMI                        "pmix.nonpmi"           // (bool) ***** DEPRECATED ***** spawned procs will not call PMIx_Init
#define PMIX_PROC_URI                       "pmix.puri"             // (char*) ***** DEPRECATED ***** URI containing contact info for proc
#define PMIX_ARCH                           "pmix.arch"             // (uint32_t) ***** DEPRECATED ***** datatype architecture flag

#define PMIX_DEBUG_JOB_DIRECTIVES           "pmix.dbg.jdirs"        // (pmix_data_array_t*) ***** DEPRECATED ***** array of job-level directives
#define PMIX_DEBUG_APP_DIRECTIVES           "pmix.dbg.adirs"        // (pmix_data_array_t*) ***** DEPRECATED ***** array of app-level directives
#define PMIX_EVENT_NO_TERMINATION           "pmix.evnoterm"         // (bool) ***** DEPRECATED ***** indicates that the handler has satisfactorily handled
                                                                    //        the event and believes termination of the application is not required
#define PMIX_EVENT_WANT_TERMINATION         "pmix.evterm"           // (bool) ***** DEPRECATED ***** indicates that the handler has determined that the
                                                                    //        application should be terminated
#define PMIX_TAG_OUTPUT                     "pmix.tagout"           // (bool) ***** DEPRECATED ***** tag application output with the ID of the source
#define PMIX_TIMESTAMP_OUTPUT               "pmix.tsout"            // (bool) ***** DEPRECATED ***** timestamp output from applications
#define PMIX_MERGE_STDERR_STDOUT            "pmix.mergeerrout"      // (bool) ***** DEPRECATED ***** merge stdout and stderr streams from application procs
#define PMIX_OUTPUT_TO_FILE                 "pmix.outfile"          // (char*) ***** DEPRECATED ***** direct application output into files of form
                                                                    //         "<filename>.rank" with both stdout and stderr redirected into it
#define PMIX_OUTPUT_TO_DIRECTORY            "pmix.outdir"           // (char*) ***** DEPRECATED ***** direct application output into files of form
                                                                    //         "<directory>/<jobid>/rank.<rank>/stdout[err]"
#define PMIX_OUTPUT_NOCOPY                  "pmix.nocopy"           // (bool) ***** DEPRECATED ***** output only into designated files - do not also output
                                                                    //        a copy to stdout/stderr

/* attributes for GDS */
#define PMIX_GDS_MODULE                     "pmix.gds.mod"          // (char*) ***** DEPRECATED ***** comma-delimited string of desired modules
#define PMIX_BFROPS_MODULE                  "pmix.bfrops.mod"       // (char*) ***** INTERNAL ***** name of bfrops plugin in-use by a given nspace
#define PMIX_PNET_SETUP_APP                 "pmix.pnet.setapp"      // (pmix_byte_object_t) ***** INTERNAL ***** blob containing info to be given to pnet framework on remote nodes

#define PMIX_IOF_STOP                       "pmix.iof.stop"         // (bool) ***** DEPRECATED ***** Stop forwarding the specified channel(s)
#define PMIX_NOTIFY_LAUNCH                  "pmix.note.lnch"        // (bool) ***** DEPRECATED ***** notify the requestor upon launch of the child job and return
                                                                    //        its namespace in the event


/* DUPLICATES */

/* Bring some function definitions across from pmix.h for now-deprecated
 * macros that utilize them. We have to do this as there are people who
 * only included pmix_common.h if they were using macros but not APIs */

PMIX_EXPORT void PMIx_Load_key(pmix_key_t key, const char *src);
PMIX_EXPORT bool PMIx_Check_key(const char *key, const char *str);
PMIX_EXPORT bool PMIx_Check_reserved_key(const char *key);
PMIX_EXPORT void PMIx_Load_nspace(pmix_nspace_t nspace, const char *str);
PMIX_EXPORT bool PMIx_Check_nspace(const char *key1, const char *key2);
PMIX_EXPORT bool PMIx_Nspace_invalid(const char *nspace);
PMIX_EXPORT void PMIx_Load_procid(pmix_proc_t *p,
                                  const char *ns,
                                  pmix_rank_t rk);
PMIX_EXPORT void PMIx_Xfer_procid(pmix_proc_t *dst,
                                  const pmix_proc_t *src);
PMIX_EXPORT bool PMIx_Check_procid(const pmix_proc_t *a,
                                   const pmix_proc_t *b);
PMIX_EXPORT bool PMIx_Check_rank(pmix_rank_t a,
                                 pmix_rank_t b);
PMIX_EXPORT bool PMIx_Procid_invalid(const pmix_proc_t *p);

PMIX_EXPORT int PMIx_Argv_count(char **a);
PMIX_EXPORT pmix_status_t PMIx_Argv_append_nosize(char ***argv, const char *arg);
PMIX_EXPORT pmix_status_t PMIx_Argv_prepend_nosize(char ***argv, const char *arg);
PMIX_EXPORT pmix_status_t PMIx_Argv_append_unique_nosize(char ***argv, const char *arg);
PMIX_EXPORT void PMIx_Argv_free(char **argv);
PMIX_EXPORT char **PMIx_Argv_split_inter(const char *src_string,
                                         int delimiter,
                                         bool include_empty);
PMIX_EXPORT char **PMIx_Argv_split_with_empty(const char *src_string, int delimiter);
PMIX_EXPORT char **PMIx_Argv_split(const char *src_string, int delimiter);
PMIX_EXPORT char *PMIx_Argv_join(char **argv, int delimiter);
PMIX_EXPORT char **PMIx_Argv_copy(char **argv);
PMIX_EXPORT pmix_status_t PMIx_Setenv(const char *name,
                                      const char *value,
                                      bool overwrite,
                                      char ***env);


PMIX_EXPORT void PMIx_Value_construct(pmix_value_t *val);
PMIX_EXPORT void PMIx_Value_destruct(pmix_value_t *val);
PMIX_EXPORT pmix_value_t* PMIx_Value_create(size_t n);
PMIX_EXPORT void PMIx_Value_free(pmix_value_t *v, size_t n);
PMIX_EXPORT pmix_boolean_t PMIx_Value_true(const pmix_value_t *v);
PMIX_EXPORT pmix_status_t PMIx_Value_load(pmix_value_t *val,
                                          const void *data,
                                          pmix_data_type_t type);
PMIX_EXPORT pmix_status_t PMIx_Value_unload(pmix_value_t *val,
                                            void **data,
                                            size_t *sz);
PMIX_EXPORT pmix_status_t PMIx_Value_xfer(pmix_value_t *dest,
                                          const pmix_value_t *src);
PMIX_EXPORT pmix_value_cmp_t PMIx_Value_compare(pmix_value_t *v1,
                                                pmix_value_t *v2);


PMIX_EXPORT void PMIx_Info_construct(pmix_info_t *p);
PMIX_EXPORT void PMIx_Info_destruct(pmix_info_t *p);
PMIX_EXPORT pmix_info_t* PMIx_Info_create(size_t n);
PMIX_EXPORT void PMIx_Info_free(pmix_info_t *p, size_t n);
PMIX_EXPORT pmix_boolean_t PMIx_Info_true(const pmix_info_t *p);
PMIX_EXPORT pmix_status_t PMIx_Info_load(pmix_info_t *info,
                                         const char *key,
                                         const void *data,
                                         pmix_data_type_t type);
PMIX_EXPORT pmix_status_t PMIx_Info_xfer(pmix_info_t *dest,
                                         const pmix_info_t *src);
PMIX_EXPORT void PMIx_Info_required(pmix_info_t *p);
PMIX_EXPORT void PMIx_Info_optional(pmix_info_t *p);
PMIX_EXPORT bool PMIx_Info_is_required(const pmix_info_t *p);
PMIX_EXPORT bool PMIx_Info_is_optional(const pmix_info_t *p);
PMIX_EXPORT void PMIx_Info_processed(pmix_info_t *p);
PMIX_EXPORT bool PMIx_Info_was_processed(const pmix_info_t *p);
PMIX_EXPORT void PMIx_Info_set_end(pmix_info_t *p);
PMIX_EXPORT bool PMIx_Info_is_end(const pmix_info_t *p);
PMIX_EXPORT void PMIx_Info_qualifier(pmix_info_t *p);
PMIX_EXPORT bool PMIx_Info_is_qualifier(const pmix_info_t *p);
PMIX_EXPORT void PMIx_Info_persistent(pmix_info_t *p);
PMIX_EXPORT bool PMIx_Info_is_persistent(const pmix_info_t *p);


PMIX_EXPORT void PMIx_Coord_construct(pmix_coord_t *m);
PMIX_EXPORT void PMIx_Coord_destruct(pmix_coord_t *m);
PMIX_EXPORT pmix_coord_t* PMIx_Coord_create(size_t dims,
                                            size_t number);
PMIX_EXPORT void PMIx_Coord_free(pmix_coord_t *m, size_t number);


PMIX_EXPORT void PMIx_Topology_construct(pmix_topology_t *t);
PMIX_EXPORT void PMIx_Topology_destruct(pmix_topology_t *topo);
PMIX_EXPORT pmix_topology_t* PMIx_Topology_create(size_t n);
PMIX_EXPORT void PMIx_Topology_free(pmix_topology_t *t, size_t n);


PMIX_EXPORT void PMIx_Cpuset_construct(pmix_cpuset_t *c);
PMIX_EXPORT void PMIx_Cpuset_destruct(pmix_cpuset_t *c);
PMIX_EXPORT pmix_cpuset_t* PMIx_Cpuset_create(size_t n);
PMIX_EXPORT void PMIx_Cpuset_free(pmix_cpuset_t *c, size_t n);


PMIX_EXPORT void PMIx_Geometry_construct(pmix_geometry_t *g);
PMIX_EXPORT void PMIx_Geometry_destruct(pmix_geometry_t *g);
PMIX_EXPORT pmix_geometry_t* PMIx_Geometry_create(size_t n);
PMIX_EXPORT void PMIx_Geometry_free(pmix_geometry_t *g, size_t n);


PMIX_EXPORT void PMIx_Device_distance_construct(pmix_device_distance_t *d);
PMIX_EXPORT void PMIx_Device_distance_destruct(pmix_device_distance_t *d);
PMIX_EXPORT pmix_device_distance_t* PMIx_Device_distance_create(size_t n);
PMIX_EXPORT void PMIx_Device_distance_free(pmix_device_distance_t *d, size_t n);


PMIX_EXPORT void PMIx_Byte_object_construct(pmix_byte_object_t *b);
PMIX_EXPORT void PMIx_Byte_object_destruct(pmix_byte_object_t *b);
PMIX_EXPORT pmix_byte_object_t* PMIx_Byte_object_create(size_t n);
PMIX_EXPORT void PMIx_Byte_object_free(pmix_byte_object_t *b, size_t n);
PMIX_EXPORT void PMIx_Byte_object_load(pmix_byte_object_t *b,
                                       char *d, size_t sz);


PMIX_EXPORT void PMIx_Endpoint_construct(pmix_endpoint_t *e);
PMIX_EXPORT void PMIx_Endpoint_destruct(pmix_endpoint_t *e);
PMIX_EXPORT pmix_endpoint_t* PMIx_Endpoint_create(size_t n);
PMIX_EXPORT void PMIx_Endpoint_free(pmix_endpoint_t *e, size_t n);


PMIX_EXPORT void PMIx_Envar_construct(pmix_envar_t *e);
PMIX_EXPORT void PMIx_Envar_destruct(pmix_envar_t *e);
PMIX_EXPORT pmix_envar_t* PMIx_Envar_create(size_t n);
PMIX_EXPORT void PMIx_Envar_free(pmix_envar_t *e, size_t n);
PMIX_EXPORT void PMIx_Envar_load(pmix_envar_t *e,
                                 char *var,
                                 char *value,
                                 char separator);


PMIX_EXPORT void PMIx_Data_buffer_construct(pmix_data_buffer_t *b);
PMIX_EXPORT void PMIx_Data_buffer_destruct(pmix_data_buffer_t *b);
PMIX_EXPORT pmix_data_buffer_t* PMIx_Data_buffer_create(void);
PMIX_EXPORT void PMIx_Data_buffer_release(pmix_data_buffer_t *b);
PMIX_EXPORT void PMIx_Data_buffer_load(pmix_data_buffer_t *b,
                                       char *bytes, size_t sz);
PMIX_EXPORT void PMIx_Data_buffer_unload(pmix_data_buffer_t *b,
                                         char **bytes, size_t *sz);


PMIX_EXPORT void PMIx_Proc_construct(pmix_proc_t *p);
PMIX_EXPORT void PMIx_Proc_destruct(pmix_proc_t *p);
PMIX_EXPORT pmix_proc_t* PMIx_Proc_create(size_t n);
PMIX_EXPORT void PMIx_Proc_free(pmix_proc_t *p, size_t n);
PMIX_EXPORT void PMIx_Proc_load(pmix_proc_t *p,
                                const char *nspace, pmix_rank_t rank);
PMIX_EXPORT void PMIx_Multicluster_nspace_construct(pmix_nspace_t target,
                                                    pmix_nspace_t cluster,
                                                    pmix_nspace_t nspace);
PMIX_EXPORT void PMIx_Multicluster_nspace_parse(pmix_nspace_t target,
                                                pmix_nspace_t cluster,
                                                pmix_nspace_t nspace);


PMIX_EXPORT void PMIx_Proc_info_construct(pmix_proc_info_t *p);
PMIX_EXPORT void PMIx_Proc_info_destruct(pmix_proc_info_t *p);
PMIX_EXPORT pmix_proc_info_t* PMIx_Proc_info_create(size_t n);
PMIX_EXPORT void PMIx_Proc_info_free(pmix_proc_info_t *p, size_t n);


PMIX_EXPORT void PMIx_Proc_stats_construct(pmix_proc_stats_t *p);
PMIX_EXPORT void PMIx_Proc_stats_destruct(pmix_proc_stats_t *p);
PMIX_EXPORT pmix_proc_stats_t* PMIx_Proc_stats_create(size_t n);
PMIX_EXPORT void PMIx_Proc_stats_free(pmix_proc_stats_t *p, size_t n);


PMIX_EXPORT void PMIx_Disk_stats_construct(pmix_disk_stats_t *p);
PMIX_EXPORT void PMIx_Disk_stats_destruct(pmix_disk_stats_t *p);
PMIX_EXPORT pmix_disk_stats_t* PMIx_Disk_stats_create(size_t n);
PMIX_EXPORT void PMIx_Disk_stats_free(pmix_disk_stats_t *p, size_t n);


PMIX_EXPORT void PMIx_Net_stats_construct(pmix_net_stats_t *p);
PMIX_EXPORT void PMIx_Net_stats_destruct(pmix_net_stats_t *p);
PMIX_EXPORT pmix_net_stats_t* PMIx_Net_stats_create(size_t n);
PMIX_EXPORT void PMIx_Net_stats_free(pmix_net_stats_t *p, size_t n);


PMIX_EXPORT void PMIx_Node_stats_construct(pmix_node_stats_t *p);
PMIX_EXPORT void PMIx_Node_stats_destruct(pmix_node_stats_t *p);
PMIX_EXPORT pmix_node_stats_t* PMIx_Node_stats_create(size_t n);
PMIX_EXPORT void PMIx_Node_stats_free(pmix_node_stats_t *p, size_t n);


PMIX_EXPORT void PMIx_Pdata_construct(pmix_pdata_t *p);
PMIX_EXPORT void PMIx_Pdata_destruct(pmix_pdata_t *p);
PMIX_EXPORT pmix_pdata_t* PMIx_Pdata_create(size_t n);
PMIX_EXPORT void PMIx_Pdata_free(pmix_pdata_t *p, size_t n);


PMIX_EXPORT void PMIx_App_construct(pmix_app_t *p);
PMIX_EXPORT void PMIx_App_destruct(pmix_app_t *p);
PMIX_EXPORT pmix_app_t* PMIx_App_create(size_t n);
PMIX_EXPORT void PMIx_App_info_create(pmix_app_t *p, size_t n);
PMIX_EXPORT void PMIx_App_free(pmix_app_t *p, size_t n);
PMIX_EXPORT void PMIx_App_release(pmix_app_t *p);


PMIX_EXPORT void PMIx_Query_construct(pmix_query_t *p);
PMIX_EXPORT void PMIx_Query_destruct(pmix_query_t *p);
PMIX_EXPORT pmix_query_t* PMIx_Query_create(size_t n);
PMIX_EXPORT void PMIx_Query_qualifiers_create(pmix_query_t *p, size_t n);
PMIX_EXPORT void PMIx_Query_free(pmix_query_t *p, size_t n);
PMIX_EXPORT void PMIx_Query_release(pmix_query_t *p);


PMIX_EXPORT void PMIx_Regattr_construct(pmix_regattr_t *p);
PMIX_EXPORT void PMIx_Regattr_destruct(pmix_regattr_t *p);
PMIX_EXPORT pmix_regattr_t* PMIx_Regattr_create(size_t n);
PMIX_EXPORT void PMIx_Regattr_free(pmix_regattr_t *p, size_t n);
PMIX_EXPORT void PMIx_Regattr_load(pmix_regattr_t *info,
                                   const char *name,
                                   const char *key,
                                   pmix_data_type_t type,
                                   const char *description);
PMIX_EXPORT void PMIx_Regattr_xfer(pmix_regattr_t *dest,
                                   const pmix_regattr_t *src);


PMIX_EXPORT void PMIx_Fabric_construct(pmix_regattr_t *p);


PMIX_EXPORT void PMIx_Data_array_init(pmix_data_array_t *p,
                                      pmix_data_type_t type);
PMIX_EXPORT void PMIx_Data_array_construct(pmix_data_array_t *p,
                                           size_t num, pmix_data_type_t type);
PMIX_EXPORT void PMIx_Data_array_destruct(pmix_data_array_t *d);
PMIX_EXPORT pmix_data_array_t* PMIx_Data_array_create(size_t n, pmix_data_type_t type);
PMIX_EXPORT void PMIx_Data_array_free(pmix_data_array_t *p);


PMIX_EXPORT void* PMIx_Info_list_start(void);

PMIX_EXPORT pmix_status_t PMIx_Info_list_add(void *ptr,
                                             const char *key,
                                             const void *value,
                                             pmix_data_type_t type);

PMIX_EXPORT pmix_status_t PMIx_Info_list_prepend(void *ptr,
                                                 const char *key,
                                                 const void *value,
                                                 pmix_data_type_t type);

PMIX_EXPORT pmix_status_t PMIx_Info_list_insert(void *ptr, pmix_info_t *info);

PMIX_EXPORT pmix_status_t PMIx_Info_list_xfer(void *ptr,
                                              const pmix_info_t *info);

PMIX_EXPORT pmix_status_t PMIx_Info_list_convert(void *ptr, pmix_data_array_t *par);

PMIX_EXPORT void PMIx_Info_list_release(void *ptr);

PMIX_EXPORT pmix_info_t* PMIx_Info_list_get_info(void *ptr, void *prev, void **next);

/* Macros that have been converted to functions */

#define PMIX_LOAD_KEY(a, b) \
    PMIx_Load_key(a, b)

#define PMIX_CHECK_KEY(a, b) \
    PMIx_Check_key((a)->key, b)

#define PMIX_CHECK_RESERVED_KEY(a) \
    PMIx_Check_reserved_key(a)

#define PMIX_LOAD_NSPACE(a, b) \
    PMIx_Load_nspace(a, b)

/* define a convenience macro for checking nspaces */
#define PMIX_CHECK_NSPACE(a, b) \
    PMIx_Check_nspace(a, b)

#define PMIX_NSPACE_INVALID(a) \
    PMIx_Nspace_invalid(a)

#define PMIX_LOAD_PROCID(a, b, c) \
    PMIx_Load_procid(a, b, c)

#define PMIX_XFER_PROCID(a, b) \
    PMIx_Xfer_procid(a, b)

#define PMIX_PROCID_XFER(a, b) PMIX_XFER_PROCID(a, b)

#define PMIX_CHECK_PROCID(a, b) \
    PMIx_Check_procid(a, b)

#define PMIX_CHECK_RANK(a, b) \
    PMIx_Check_rank(a, b)

#define PMIX_PROCID_INVALID(a)  \
    PMIx_Procid_invalid(a)

#define PMIX_ARGV_COUNT(r, a) \
    (r) = PMIx_Argv_count(a)

#define PMIX_ARGV_APPEND(r, a, b) \
    (r) = PMIx_Argv_append_nosize((char***)&(a), (b))

#define PMIX_ARGV_PREPEND(r, a, b) \
    (r) = PMIx_Argv_prepend_nosize((char***)&(a), b)

#define PMIX_ARGV_APPEND_UNIQUE(r, a, b) \
    (r) = PMIx_Argv_append_unique_nosize(a, b)

// free(a) is called inside PMIx_Argv_free().
#define PMIX_ARGV_FREE(a)    \
    do {                     \
        PMIx_Argv_free((a)); \
        (a) = NULL;          \
    } while (0)

#define PMIX_ARGV_SPLIT(a, b, c) \
    (a) = PMIx_Argv_split(b, c)

#define PMIX_ARGV_JOIN(a, b, c) \
    (a) = PMIx_Argv_join(b, c)

#define PMIX_ARGV_COPY(a, b) \
    (a) = PMIx_Argv_copy(b)

#define PMIX_SETENV(r, a, b, c) \
    (r) = PMIx_Setenv((a), (b), true, (c))

#define PMIX_VALUE_CONSTRUCT(m) \
    PMIx_Value_construct(m)

#define PMIX_VALUE_DESTRUCT(m) \
    PMIx_Value_destruct(m)

#define PMIX_VALUE_CREATE(m, n) \
    (m) = PMIx_Value_create(n)

// free(m) is called inside PMIx_Value_free().
#define PMIX_VALUE_RELEASE(m)  \
    do {                       \
        PMIx_Value_free(m, 1); \
        (m) = NULL;            \
    } while (0)

// free(m) is called inside PMIx_Value_free().
#define PMIX_VALUE_FREE(m, n)   \
    do {                        \
        PMIx_Value_free(m, n);  \
        (m) = NULL;             \
    } while (0)

#define PMIX_CHECK_TRUE(a) \
    (PMIX_BOOL_TRUE == PMIx_Value_true(a) ? true : false)

#define PMIX_CHECK_BOOL(a) \
    (PMIX_NON_BOOL == PMIx_Value_true(a) ? false : true)

#define PMIX_VALUE_LOAD(v, d, t) \
    PMIx_Value_load((v), (d), (t))

#define PMIX_VALUE_UNLOAD(r, k, d, s)      \
    (r) = PMIx_Value_unload((k), (d), (s))

#define PMIX_VALUE_XFER(r, v, s)                                \
    do {                                                        \
        if (NULL == (v)) {                                      \
            (v) = (pmix_value_t*)pmix_malloc(sizeof(pmix_value_t));  \
            if (NULL == (v)) {                                  \
                (r) = PMIX_ERR_NOMEM;                           \
            } else {                                            \
                (r) = PMIx_Value_xfer((v), (s));                \
            }                                                   \
        } else {                                                \
            (r) = PMIx_Value_xfer((v), (s));                    \
        }                                                       \
    } while(0)

#define PMIX_VALUE_XFER_DIRECT(r, v, s)     \
    (r) = PMIx_Value_xfer((v), (s))

#define PMIX_INFO_CONSTRUCT(m) \
    PMIx_Info_construct(m)

#define PMIX_INFO_DESTRUCT(m) \
    PMIx_Info_destruct(m)

#define PMIX_INFO_CREATE(m, n) \
    (m) = PMIx_Info_create(n)

// free(m) is called inside PMIx_Info_free().
#define PMIX_INFO_FREE(m, n)  \
    do {                      \
        PMIx_Info_free(m, n); \
        (m) = NULL;           \
    } while (0)

#define PMIX_INFO_REQUIRED(m) \
    PMIx_Info_required(m)

#define PMIX_INFO_OPTIONAL(m) \
    PMIx_Info_optional(m)

#define PMIX_INFO_IS_REQUIRED(m) \
    PMIx_Info_is_required(m)

#define PMIX_INFO_IS_OPTIONAL(m) \
    PMIx_Info_is_optional(m)

#define PMIX_INFO_PROCESSED(m)  \
    PMIx_Info_processed(m)

#define PMIX_INFO_WAS_PROCESSED(m)  \
    PMIx_Info_was_processed(m)

#define PMIX_INFO_SET_END(m)    \
    PMIx_Info_set_end(m)
#define PMIX_INFO_IS_END(m)         \
    PMIx_Info_is_end(m)

#define PMIX_INFO_SET_QUALIFIER(i)   \
    PMIx_Info_qualifier(i)

#define PMIX_INFO_IS_QUALIFIER(i)    \
    PMIx_Info_is_qualifier(i)

#define PMIX_INFO_SET_PERSISTENT(ii) \
    PMIx_Info_persistent(ii)

#define PMIX_INFO_IS_PERSISTENT(ii)  \
    PMIx_Info_is_persistent(ii)

#define PMIX_INFO_LOAD(i, k, d, t)  \
    (void) PMIx_Info_load(i, k, d, t)

#define PMIX_INFO_XFER(d, s)    \
    (void) PMIx_Info_xfer(d, s)

#define PMIX_PDATA_LOAD(m, p, k, v, t)                                      \
    do {                                                                    \
        if (NULL != (m)) {                                                  \
            memset((m), 0, sizeof(pmix_pdata_t));                           \
            PMIX_LOAD_NSPACE((m)->proc.nspace, (p)->nspace);                \
            (m)->proc.rank = (p)->rank;                                     \
            PMIX_LOAD_KEY((m)->key, k);                                     \
            PMIx_Value_load(&((m)->value), (v), (t));                       \
        }                                                                   \
    } while (0)

#define PMIX_PDATA_XFER(d, s)                                                   \
    do {                                                                        \
        if (NULL != (d)) {                                                      \
            memset((d), 0, sizeof(pmix_pdata_t));                               \
            PMIX_LOAD_NSPACE((d)->proc.nspace, (s)->proc.nspace);               \
            (d)->proc.rank = (s)->proc.rank;                                    \
            PMIX_LOAD_KEY((d)->key, (s)->key);                                  \
            PMIx_Value_xfer(&((d)->value), &((s)->value));                      \
        }                                                                       \
    } while (0)

#define PMIX_INFO_TRUE(m)   \
    (PMIX_BOOL_TRUE == PMIx_Info_true(m) ? true : false)

#define PMIX_INFO_LIST_START(p)    \
    (p) = PMIx_Info_list_start()

#define PMIX_INFO_LIST_ADD(r, p, a, v, t)     \
    (r) = PMIx_Info_list_add((p), (a), (v), (t))

#define PMIX_INFO_LIST_PREPEND(r, p, a, v, t)     \
    (r) = PMIx_Info_list_prepend((p), (a), (v), (t))

#define PMIX_INFO_LIST_INSERT(r, p, i)     \
    (r) = PMIx_Info_list_insert((p), (i))

#define PMIX_INFO_LIST_XFER(r, p, a)     \
    (r) = PMIx_Info_list_xfer((p), (a))

#define PMIX_INFO_LIST_CONVERT(r, p, m)     \
    (r) = PMIx_Info_list_convert((p), (m))

#define PMIX_INFO_LIST_RELEASE(p) \
    PMIx_Info_list_release((p))

#define PMIX_TOPOLOGY_CONSTRUCT(m) \
    PMIx_Topology_construct(m)

#define PMIX_TOPOLOGY_CREATE(m, n) \
    (m) = PMIx_Topology_create(n)

#define PMIX_TOPOLOGY_DESTRUCT(x) \
    PMIx_Topology_destruct(x)

// free(m) is called inside PMIx_Topology_free().
#define PMIX_TOPOLOGY_FREE(m, n)  \
    do {                          \
        PMIx_Topology_free(m, n); \
        (m) = NULL;               \
    } while (0)

#define PMIX_COORD_CREATE(m, n, d)  \
    (m) = PMIx_Coord_create(d, n)

#define PMIX_COORD_CONSTRUCT(m) \
    PMIx_Coord_construct(m)

#define PMIX_COORD_DESTRUCT(m)  \
    PMIx_Coord_destruct(m)

// free(m) is called inside PMIx_Coord_free().
#define PMIX_COORD_FREE(m, n)   \
    do {                        \
        PMIx_Coord_free(m, n);  \
        (m) = NULL;             \
    } while(0)

#define PMIX_CPUSET_CONSTRUCT(m) \
    PMIx_Cpuset_construct(m)

#define PMIX_CPUSET_DESTRUCT(m) \
    PMIx_Cpuset_destruct(m)

#define PMIX_CPUSET_CREATE(m, n) \
    (m) = PMIx_Cpuset_create(n)

// free(m) is called inside PMIx_Cpuset_free().
#define PMIX_CPUSET_FREE(m, n)    \
    do {                          \
        PMIx_Cpuset_free(m, n);   \
        (m) = NULL;               \
    } while(0)

#define PMIX_GEOMETRY_CONSTRUCT(m) \
    PMIx_Geometry_construct(m)

#define PMIX_GEOMETRY_DESTRUCT(m) \
    PMIx_Geometry_destruct(m)

#define PMIX_GEOMETRY_CREATE(m, n) \
    (m) = PMIx_Geometry_create(n)

// free(m) is called inside PMIx_Geometry_free().
#define PMIX_GEOMETRY_FREE(m, n)    \
    do {                            \
        PMIx_Geometry_free(m, n);   \
        (m) = NULL;                 \
    } while(0)

#define PMIX_DEVICE_DIST_CONSTRUCT(m) \
    PMIx_Device_distance_construct(m)

#define PMIX_DEVICE_DIST_DESTRUCT(m) \
    PMIx_Device_distance_destruct(m)

#define PMIX_DEVICE_DIST_CREATE(m, n) \
    (m) = PMIx_Device_distance_create(n)

// free(m) is called inside PMIx_Device_distance_free().
#define PMIX_DEVICE_DIST_FREE(m, n)      \
    do {                                 \
        PMIx_Device_distance_free(m, n); \
        (m) = NULL;                      \
    } while(0)

#define PMIX_BYTE_OBJECT_CONSTRUCT(m) \
    PMIx_Byte_object_construct(m)

#define PMIX_BYTE_OBJECT_DESTRUCT(m) \
    PMIx_Byte_object_destruct(m)

#define PMIX_BYTE_OBJECT_CREATE(m, n) \
    (m) = PMIx_Byte_object_create(n)

// free(m) is called inside PMIx_Byte_object_free().
#define PMIX_BYTE_OBJECT_FREE(m, n)  \
    do {                             \
        PMIx_Byte_object_free(m, n); \
        (m) = NULL;                  \
    } while(0)

#define PMIX_BYTE_OBJECT_LOAD(b, d, s)  \
    do {                                \
        PMIx_Byte_object_load(b, d, s); \
        (d) = NULL;                     \
        (s) = 0;                        \
    } while(0)

#define PMIX_ENDPOINT_CONSTRUCT(m) \
    PMIx_Endpoint_construct(m)

#define PMIX_ENDPOINT_DESTRUCT(m) \
    PMIx_Endpoint_destruct(m)

#define PMIX_ENDPOINT_CREATE(m, n) \
    (m) = PMIx_Endpoint_create(n)

// free(m) is called inside PMIx_Endpoint_free().
#define PMIX_ENDPOINT_FREE(m, n)  \
    do {                          \
        PMIx_Endpoint_free(m, n); \
        (m) = NULL;               \
    } while(0)

#define PMIX_ENVAR_CREATE(m, n) \
    (m) = PMIx_Envar_create(n)

// free(m) is called inside PMIx_Envar_free().
#define PMIX_ENVAR_FREE(m, n)   \
    do {                        \
        PMIx_Envar_free(m, n);  \
        (m) = NULL;             \
    } while(0)

#define PMIX_ENVAR_CONSTRUCT(m) \
    PMIx_Envar_construct(m)

#define PMIX_ENVAR_DESTRUCT(m) \
    PMIx_Envar_destruct(m)

#define PMIX_ENVAR_LOAD(m, e, v, s) \
    PMIx_Envar_load(m, e, v, s)

#define PMIX_DATA_BUFFER_CREATE(m)  \
    (m) = PMIx_Data_buffer_create()

// free(m) is called inside PMIx_Data_buffer_release().
#define PMIX_DATA_BUFFER_RELEASE(m)  \
    do {                             \
        PMIx_Data_buffer_release(m); \
        (m) = NULL;                  \
    } while (0)

#define PMIX_DATA_BUFFER_CONSTRUCT(m)       \
    PMIx_Data_buffer_construct(m)

#define PMIX_DATA_BUFFER_DESTRUCT(m)        \
    PMIx_Data_buffer_destruct(m)

#define PMIX_DATA_BUFFER_LOAD(b, d, s)  \
    PMIx_Data_buffer_load(b, d, s)

#define PMIX_DATA_BUFFER_UNLOAD(b, d, s)    \
    PMIx_Data_buffer_unload(b, (char**)&(d), (size_t*)&(s))

#define PMIX_PROC_CREATE(m, n) \
    (m) = PMIx_Proc_create(n)

#define PMIX_PROC_CONSTRUCT(m) \
    PMIx_Proc_construct(m)

#define PMIX_PROC_DESTRUCT(m) \
    PMIx_Proc_destruct(m)

// free(m) is called inside PMIx_Proc_free().
#define PMIX_PROC_FREE(m, n)    \
    do {                        \
        PMIx_Proc_free(m, n);   \
        (m) = NULL;             \
    } while (0)

// free(m) is called inside PMIx_Proc_free().
#define PMIX_PROC_RELEASE(m)    \
do {                            \
    PMIx_Proc_free(m, 1);       \
    (m) = NULL;                 \
} while(0)

#define PMIX_PROC_LOAD(m, n, r) \
    PMIx_Proc_load(m, n, r)

#define PMIX_MULTICLUSTER_NSPACE_CONSTRUCT(t, c, n) \
    PMIx_Multicluster_nspace_construct(t, c, n)

#define PMIX_MULTICLUSTER_NSPACE_PARSE(t, c, n) \
    PMIx_Multicluster_nspace_parse(t, c, n)

#define PMIX_PROC_INFO_CREATE(m, n) \
    (m) = PMIx_Proc_info_create(n)

#define PMIX_PROC_INFO_CONSTRUCT(m) \
    PMIx_Proc_info_construct(m)

#define PMIX_PROC_INFO_DESTRUCT(m) \
    PMIx_Proc_info_destruct(m)

// free(m) is called inside PMIx_Proc_info_free().
#define PMIX_PROC_INFO_FREE(m, n)  \
    do {                           \
        PMIx_Proc_info_free(m, n); \
        (m) = NULL;                \
    } while (0)

// free(m) is called inside PMIx_Proc_info_free().
#define PMIX_PROC_INFO_RELEASE(m)   \
do {                                \
    PMIx_Proc_info_free(m, 1)       \
    (m) = NULL;                     \
} while(0)

#define PMIX_PROC_STATS_CONSTRUCT(m) \
    PMIx_Proc_stats_construct(m)

#define PMIX_PROC_STATS_DESTRUCT(m) \
    PMIx_Proc_stats_destruct(m)

#define PMIX_PROC_STATS_CREATE(m, n) \
    (m) = PMIx_Proc_stats_create(n)

// free(m) is called inside PMIx_Proc_stats_free().
#define PMIX_PROC_STATS_FREE(m, n)  \
do {                                \
    PMIx_Proc_stats_free(m, n);     \
    (m) = NULL;                     \
} while(0)

// free(m) is called inside PMIx_Proc_stats_free().
#define PMIX_PROC_STATS_RELEASE(m)  \
do {                                \
    PMIx_Proc_stats_free(m, 1);     \
    (m) = NULL;                     \
} while(0)

#define PMIX_DISK_STATS_CONSTRUCT(m) \
    PMIx_Disk_stats_construct(m)

#define PMIX_DISK_STATS_DESTRUCT(m) \
    PMIx_Disk_stats_destruct(m)

#define PMIX_DISK_STATS_CREATE(m, n) \
    (m) = PMIx_Disk_stats_create(n)

// free(m) is called inside PMIx_Disk_stats_free().
#define PMIX_DISK_STATS_FREE(m, n)  \
do {                                \
    PMIx_Disk_stats_free(m, n);     \
    (m) = NULL;                     \
} while(0)

// free(m) is called inside PMIx_Disk_stats_free().
#define PMIX_DISK_STATS_RELEASE(m) \
do {                               \
    PMIx_Disk_stats_free(m, 1);    \
    (m) = NULL;                    \
} while(0)

#define PMIX_NET_STATS_CONSTRUCT(m) \
    PMIx_Net_stats_construct(m)

#define PMIX_NET_STATS_DESTRUCT(m) \
    PMIx_Net_stats_destruct(m)

#define PMIX_NET_STATS_CREATE(m, n) \
    (m) = PMIx_Net_stats_create(n)

// free(m) is called inside PMIx_Net_stats_free().
#define PMIX_NET_STATS_FREE(m, n)  \
do {                               \
    PMIx_Net_stats_free(m, n);     \
    (m) = NULL;                    \
} while(0)

#define PMIX_NET_STATS_RELEASE(m)  \
do {                               \
    PMIx_Net_stats_free(m, 1);     \
    (m) = NULL;                    \
} while(0)

#define PMIX_NODE_STATS_CONSTRUCT(m) \
    PMIx_Node_stats_construct(m)

#define PMIX_NODE_STATS_DESTRUCT(m) \
    PMIx_Node_stats_destruct(m)

#define PMIX_NODE_STATS_CREATE(m, n) \
    (m) = PMIx_Node_stats_create(n)

// free(m) is called inside PMIx_Node_stats_free().
#define PMIX_NODE_STATS_FREE(m, n)  \
do {                                \
    PMIx_Node_stats_free(m, n);     \
    (m) = NULL;                     \
} while(0)

// free(m) is called inside PMIx_Node_stats_free().
#define PMIX_NODE_STATS_RELEASE(m)  \
do {                                \
    PMIx_Node_stats_free(m, 1);     \
    (m) = NULL;                     \
} while(0)

#define PMIX_PDATA_CONSTRUCT(m) \
    PMIx_Pdata_construct(m)

#define PMIX_PDATA_DESTRUCT(m) \
    PMIx_Pdata_destruct(m)

#define PMIX_PDATA_CREATE(m, n) \
    (m) = PMIx_Pdata_create(n)

// free(m) is called inside PMIx_Pdata_free().
#define PMIX_PDATA_FREE(m, n)   \
do {                            \
    PMIx_Pdata_free(m, n);      \
    (m) = NULL;                 \
} while(0)

// free(m) is called inside PMIx_Pdata_free().
#define PMIX_PDATA_RELEASE(m)   \
do {                            \
    PMIx_Pdata_free(m, 1);      \
    (m) = NULL;                 \
} while(0)

#define PMIX_APP_CONSTRUCT(m) \
    PMIx_App_construct(m)

#define PMIX_APP_DESTRUCT(m) \
    PMIx_App_destruct(m)

#define PMIX_APP_CREATE(m, n) \
    (m) = PMIx_App_create(n)

#define PMIX_APP_INFO_CREATE(m, n) \
    PMIx_App_info_create(m, n)

// free(m) is called inside PMIx_App_free().
#define PMIX_APP_RELEASE(m)     \
    do {                        \
        PMIx_App_free(m, 1);    \
        (m) = NULL;             \
    } while (0)

// free(m) is called inside PMIx_App_free().
#define PMIX_APP_FREE(m, n)     \
    do {                        \
        PMIx_App_free(m, n);    \
        (m) = NULL;             \
    } while (0)

#define PMIX_QUERY_CONSTRUCT(m) \
    PMIx_Query_construct(m)

#define PMIX_QUERY_DESTRUCT(m) \
    PMIx_Query_destruct(m)

#define PMIX_QUERY_CREATE(m, n) \
    (m) = PMIx_Query_create(n)

#define PMIX_QUERY_QUALIFIERS_CREATE(m, n) \
    PMIx_Query_qualifiers_create(m, n)

// free(m) is called inside PMIx_Query_release().
#define PMIX_QUERY_RELEASE(m)       \
    do {                            \
        PMIx_Query_release((m));    \
        (m) = NULL;                 \
    } while (0)

// free(m) is called inside PMIx_Query_free().
#define PMIX_QUERY_FREE(m, n)   \
    do {                        \
        PMIx_Query_free(m, n);  \
        (m) = NULL;             \
    } while (0)

#define PMIX_REGATTR_CONSTRUCT(a) \
    PMIx_Regattr_construct(a)

#define PMIX_REGATTR_LOAD(a, n, k, t, v) \
    PMIx_Regattr_load(a, n, k, t, v)

#define PMIX_REGATTR_DESTRUCT(a) \
    PMIx_Regattr_destruct(a)

#define PMIX_REGATTR_CREATE(m, n) \
    (m)= PMIx_Regattr_create(n)

// free(m) is called inside PMIx_Regattr_free().
#define PMIX_REGATTR_FREE(m, n)     \
    do {                            \
        PMIx_Regattr_free(m, n);    \
        (m) = NULL;                 \
    } while (0)

#define PMIX_REGATTR_XFER(a, b) \
    PMIx_Regattr_xfer(a, b)

#define PMIX_DATA_ARRAY_INIT(m, t) \
    PMIx_Data_array_init(m, t)

#define PMIX_DATA_ARRAY_CONSTRUCT(m, n, t) \
    PMIx_Data_array_construct(m, n, t)

#define PMIX_DATA_ARRAY_DESTRUCT(m) \
    PMIx_Data_array_destruct(m)

#define PMIX_DATA_ARRAY_CREATE(m, n, t) \
    (m) = PMIx_Data_array_create(n, t)

// free(m) is called inside PMIx_Data_array_free().
#define PMIX_DATA_ARRAY_FREE(m)     \
    do {                            \
        PMIx_Data_array_free(m);    \
        (m) = NULL;                 \
    } while(0)


// functions that are no longer visible from inside
// the PMIx library

#define pmix_argv_append_nosize(a, b) \
    PMIx_Argv_append_nosize(a, b)

#define pmix_argv_append_unique_nosize(a, b) \
    PMIx_Argv_append_unique_nosize(a, b)

#define pmix_argv_split(a, b) \
    PMIx_Argv_split(a, b)

#define pmix_argv_split_with_empty(a, b) \
    PMIx_Argv_split_with_empty(a, b)

#define pmix_argv_free(a) \
    PMIx_Argv_free(a)

#define pmix_argv_count(a) \
    PMIx_Argv_count(a)

#define pmix_argv_join(a, b) \
    PMIx_Argv_join(a, b)

#define pmix_argv_prepend_nosize(a, b) \
    PMIx_Argv_prepend_nosize(a, b)

#define pmix_argv_copy(a) \
    PMIx_Argv_copy(a)

#define pmix_setenv(a, b, c, d) \
    PMIx_Setenv(a, b, c, d)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

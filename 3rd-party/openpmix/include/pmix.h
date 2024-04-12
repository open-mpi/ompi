/*
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
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
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIx_H
#define PMIx_H

/* Structure and constant definitions */
#include <pmix_common.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/****    PMIX API    ****/

/* Initialize the PMIx client, returning the process identifier assigned
 * to this client's application in the provided pmix_proc_t struct.
 * Passing a parameter of _NULL_ for this parameter is allowed if the user
 * wishes solely to initialize the PMIx system and does not require
 * return of the identifier at that time.
 *
 * When called the PMIx client will check for the required connection
 * information of the local PMIx server and will establish the connection.
 * If the information is not found, or the server connection fails, then
 * an appropriate error constant will be returned.
 *
 * If successful, the function will return PMIX_SUCCESS and will fill the
 * provided structure with the server-assigned namespace and rank of the
 * process within the application.
 *
 * Note that the PMIx client library is referenced counted, and so multiple
 * calls to PMIx_Init are allowed. Thus, one way to obtain the namespace and
 * rank of the process is to simply call PMIx_Init with a non-NULL parameter.
 *
 * The info array is used to pass user requests pertaining to the init
 * and subsequent operations. Pass a _NULL_ value for the array pointer
 * is supported if no directives are desired.
 */
PMIX_EXPORT pmix_status_t PMIx_Init(pmix_proc_t *proc,
                                    pmix_info_t info[], size_t ninfo);

/* Finalize the PMIx client, closing the connection to the local server.
 * An error code will be returned if, for some reason, the connection
 * cannot be closed.
 *
 * The info array is used to pass user requests regarding the finalize
 * operation. This can include:
 *
 * (a) PMIX_EMBED_BARRIER - By default, PMIx_Finalize does not include an
 * internal barrier operation. This attribute directs PMIx_Finalize to
 * execute a barrier as part of the finalize operation.
 */
PMIX_EXPORT pmix_status_t PMIx_Finalize(const pmix_info_t info[], size_t ninfo);


/* Returns _true_ if the PMIx client has been successfully initialized,
 * returns _false_ otherwise. Note that the function only reports the
 * internal state of the PMIx client - it does not verify an active
 * connection with the server, nor that the server is functional. */
PMIX_EXPORT int PMIx_Initialized(void);


/* Request that the provided array of procs be aborted, returning the
 * provided _status_ and printing the provided message. A _NULL_
 * for the proc array indicates that all processes in the caller's
 * nspace are to be aborted.
 *
 * The response to this request is somewhat dependent on the specific resource
 * manager and its configuration (e.g., some resource managers will
 * not abort the application if the provided _status_ is zero unless
 * specifically configured to do so), and thus lies outside the control
 * of PMIx itself. However, the client will inform the RM of
 * the request that the application be aborted, regardless of the
 * value of the provided _status_.
 *
 * Passing a _NULL_ msg parameter is allowed. Note that race conditions
 * caused by multiple processes calling PMIx_Abort are left to the
 * server implementation to resolve with regard to which status is
 * returned and what messages (if any) are printed. */
PMIX_EXPORT pmix_status_t PMIx_Abort(int status, const char msg[],
                                     pmix_proc_t procs[], size_t nprocs);


/* Push a value into the client's namespace. The client library will cache
 * the information locally until _PMIx_Commit_ is called. The provided scope
 * value is passed to the local PMIx server, which will distribute the data
 * as directed. */
PMIX_EXPORT pmix_status_t PMIx_Put(pmix_scope_t scope,
                                   const char key[],
                                   pmix_value_t *val);


/* Push all previously _PMIx_Put_ values to the local PMIx server.
 * This is an asynchronous operation - the library will immediately
 * return to the caller while the data is transmitted to the local
 * server in the background */
PMIX_EXPORT pmix_status_t PMIx_Commit(void);


/* Execute a blocking barrier across the processes identified in the
 * specified array. Passing a _NULL_ pointer as the _procs_ parameter
 * indicates that the barrier is to span all processes in the client's
 * namespace. Each provided pmix_proc_t struct can pass PMIX_RANK_WILDCARD to
 * indicate that all processes in the given namespace are
 * participating.
 *
 * The info array is used to pass user requests regarding the fence
 * operation. This can include:
 *
 * (a) PMIX_COLLECT_DATA - a boolean indicating whether or not the barrier
 *     operation is to return the _put_ data from all participating processes.
 *     A value of _false_ indicates that the callback is just used as a release
 *     and no data is to be returned at that time. A value of _true_ indicates
 *     that all _put_ data is to be collected by the barrier. Returned data is
 *     cached at the server to reduce memory footprint, and can be retrieved
 *     as needed by calls to PMIx_Get(nb).
 *
 *     Note that for scalability reasons, the default behavior for PMIx_Fence
 *     is to _not_ collect the data.
 *
 * (b) PMIX_COLLECTIVE_ALGO - a comma-delimited string indicating the algos
 *     to be used for executing the barrier, in priority order.
 *
 * (c) PMIX_COLLECTIVE_ALGO_REQD - instructs the host RM that it should return
 *     an error if none of the specified algos are available. Otherwise, the RM
 *     is to use one of the algos if possible, but is otherwise free to use any
 *     of its available methods to execute the operation.
 *
 * (d) PMIX_TIMEOUT - maximum time for the fence to execute before declaring
 *     an error. By default, the RM shall terminate the operation and notify participants
 *     if one or more of the indicated procs fails during the fence. However,
 *     the timeout parameter can help avoid "hangs" due to programming errors
 *     that prevent one or more procs from reaching the "fence".
 */
PMIX_EXPORT pmix_status_t PMIx_Fence(const pmix_proc_t procs[], size_t nprocs,
                                     const pmix_info_t info[], size_t ninfo);

/* Non-blocking version of PMIx_Fence. Note that the function will return
 * an error if a _NULL_ callback function is given. */
PMIX_EXPORT pmix_status_t PMIx_Fence_nb(const pmix_proc_t procs[], size_t nprocs,
                                        const pmix_info_t info[], size_t ninfo,
                                        pmix_op_cbfunc_t cbfunc, void *cbdata);


/* Retrieve information for the specified _key_ as published by the process
 * identified in the given pmix_proc_t, returning a pointer to the value in the
 * given address.
 *
 * This is a blocking operation - the caller will block until
 * the specified data has been _PMIx_Put_ by the specified rank. The caller is
 * responsible for freeing all memory associated with the returned value when
 * no longer required.
 *
 * The info array is used to pass user requests regarding the get
 * operation. This can include:
 *
 * (a) PMIX_TIMEOUT - maximum time for the get to execute before declaring
 *     an error. The timeout parameter can help avoid "hangs" due to programming
 *     errors that prevent the target proc from ever exposing its data.
 */
PMIX_EXPORT pmix_status_t PMIx_Get(const pmix_proc_t *proc, const char key[],
                                   const pmix_info_t info[], size_t ninfo,
                                   pmix_value_t **val);

/* A non-blocking operation version of PMIx_Get - the callback function will
 * be executed once the specified data has been _PMIx_Put_
 * by the identified process and retrieved by the local server. The info
 * array is used as described above for the blocking form of this call. */
PMIX_EXPORT pmix_status_t PMIx_Get_nb(const pmix_proc_t *proc, const char key[],
                                      const pmix_info_t info[], size_t ninfo,
                                      pmix_value_cbfunc_t cbfunc, void *cbdata);


/* Publish the data in the info array for lookup. By default,
 * the data will be published into the PMIX_SESSION range and
 * with PMIX_PERSIST_APP persistence. Changes to those values,
 * and any additional directives, can be included in the pmix_info_t
 * array.
 *
 * Note that the keys must be unique within the specified
 * data range or else an error will be returned (first published
 * wins). Attempts to access the data by procs outside of
 * the provided data range will be rejected.
 *
 * The persistence parameter instructs the server as to how long
 * the data is to be retained.
 *
 * The blocking form will block until the server confirms that the
 * data has been posted and is available. The non-blocking form will
 * return immediately, executing the callback when the server confirms
 * availability of the data.
 */
PMIX_EXPORT pmix_status_t PMIx_Publish(const pmix_info_t info[], size_t ninfo);
PMIX_EXPORT pmix_status_t PMIx_Publish_nb(const pmix_info_t info[], size_t ninfo,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata);


/* Lookup information published by this or another process. By default,
 * the search will be conducted across the PMIX_SESSION range. Changes
 * to the range, and any additional directives, can be provided
 * in the pmix_info_t array. Note that the search is also constrained
 * to only data published by the current user ID - i.e., the search
 * will not return data published by an application being executed
 * by another user. There currently is no option to override this
 * behavior - such an option may become available later via an
 * appropriate pmix_info_t directive.
 *
 * The "data" parameter consists of an array of pmix_pdata_t struct with the
 * keys specifying the requested information. Data will be returned
 * for each key in the associated info struct - any key that cannot
 * be found will return with a data type of "PMIX_UNDEF". The function
 * will return SUCCESS if _any_ values can be found, so the caller
 * must check each data element to ensure it was returned.
 *
 * The proc field in each pmix_pdata_t struct will contain the
 * nspace/rank of the process that published the data.
 *
 * Note: although this is a blocking function, it will _not_ wait
 * by default for the requested data to be published. Instead, it
 * will block for the time required by the server to lookup its current
 * data and return any found items. Thus, the caller is responsible for
 * ensuring that data is published prior to executing a lookup, or
 * for retrying until the requested data is found
 *
 * Optionally, the info array can be used to modify this behavior
 * by including:
 *
 * (a) PMIX_WAIT - wait for the requested data to be published. The
 *     server is to wait until all data has become available.
 *
 * (b) PMIX_TIMEOUT - max time to wait for data to become available.
 *
 */
PMIX_EXPORT pmix_status_t PMIx_Lookup(pmix_pdata_t data[], size_t ndata,
                                      const pmix_info_t info[], size_t ninfo);

/* Non-blocking form of the _PMIx_Lookup_ function. Data for
 * the provided NULL-terminated keys array will be returned
 * in the provided callback function. As above, the default
 * behavior is to _not_ wait for data to be published. The
 * info keys can be used to modify the behavior as previously
 * described */
PMIX_EXPORT pmix_status_t PMIx_Lookup_nb(char **keys, const pmix_info_t info[], size_t ninfo,
                                         pmix_lookup_cbfunc_t cbfunc, void *cbdata);


/* Unpublish data posted by this process using the given keys.
 * The function will block until the data has been removed by
 * the server. A value of _NULL_ for the keys parameter instructs
 * the server to remove _all_ data published by this process.
 *
 * By default, the range is assumed to be PMIX_SESSION. Changes
 * to the range, and any additional directives, can be provided
 * in the pmix_info_t array */
PMIX_EXPORT pmix_status_t PMIx_Unpublish(char **keys,
                                         const pmix_info_t info[], size_t ninfo);

/* Non-blocking form of the _PMIx_Unpublish_ function. The
 * callback function will be executed once the server confirms
 * removal of the specified data. */
PMIX_EXPORT pmix_status_t PMIx_Unpublish_nb(char **keys,
                                            const pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata);


/* Spawn a new job. The assigned namespace of the spawned applications
 * is returned in the nspace parameter - a _NULL_ value in that
 * location indicates that the caller doesn't wish to have the
 * namespace returned. The nspace array must be at least of size
 * PMIX_MAX_NSLEN+1. Behavior of individual resource managers
 * may differ, but it is expected that failure of any application
 * process to start will result in termination/cleanup of _all_
 * processes in the newly spawned job and return of an error
 * code to the caller.
 *
 * By default, the spawned processes will be PMIx "connected" to
 * the parent process upon successful launch (see PMIx_Connect
 * description for details). Note that this only means that the
 * parent process (a) will be given a copy of the  new job's
 * information so it can query job-level info without
 * incurring any communication penalties, and (b) will receive
 * notification of errors from process in the child job.
 *
 * Job-level directives can be specified in the job_info array. This
 * can include:
 *
 * (a) PMIX_NON_PMI - processes in the spawned job will
 *     not be calling PMIx_Init
 *
 * (b) PMIX_TIMEOUT - declare the spawn as having failed if the launched
 *     procs do not call PMIx_Init within the specified time
 *
 * (c) PMIX_NOTIFY_COMPLETION - notify the parent process when the
 *     child job terminates, either normally or with error
 */
PMIX_EXPORT pmix_status_t PMIx_Spawn(const pmix_info_t job_info[], size_t ninfo,
                                     const pmix_app_t apps[], size_t napps,
                                     pmix_nspace_t nspace);


/* Non-blocking form of the _PMIx_Spawn_ function. The callback
 * will be executed upon launch of the specified applications,
 * or upon failure to launch any of them. */
PMIX_EXPORT pmix_status_t PMIx_Spawn_nb(const pmix_info_t job_info[], size_t ninfo,
                                        const pmix_app_t apps[], size_t napps,
                                        pmix_spawn_cbfunc_t cbfunc, void *cbdata);

/* Record the specified processes as "connected". Both blocking and non-blocking
 * versions are provided. This means that the resource manager should treat the
 * failure of any process in the specified group as a reportable event, and take
 * appropriate action. Note that different resource managers may respond to
 * failures in different manners.
 *
 * The callback function is to be called once all participating processes have
 * called connect. The server is required to return any job-level info for the
 * connecting processes that might not already have - i.e., if the connect
 * request involves procs from different nspaces, then each proc shall receive
 * the job-level info from those nspaces other than their own.
 *
 * Note: a process can only engage in _one_ connect operation involving the identical
 * set of processes at a time. However, a process _can_ be simultaneously engaged
 * in multiple connect operations, each involving a different set of processes
 *
 * As in the case of the fence operation, the info array can be used to pass
 * user-level directives regarding the algorithm to be used for the collective
 * operation involved in the "connect", timeout constraints, and other options
 * available from the host RM */
PMIX_EXPORT pmix_status_t PMIx_Connect(const pmix_proc_t procs[], size_t nprocs,
                                       const pmix_info_t info[], size_t ninfo);

PMIX_EXPORT pmix_status_t PMIx_Connect_nb(const pmix_proc_t procs[], size_t nprocs,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Disconnect a previously connected set of processes. An error will be returned
 * if the specified set of procs was not previously "connected". As above, a process
 * may be involved in multiple simultaneous disconnect operations. However, a process
 * is not allowed to reconnect to a set of procs that has not fully completed
 * disconnect - i.e., you have to fully disconnect before you can reconnect to the
 * _same_ group of processes. The info array is used as above. */
PMIX_EXPORT pmix_status_t PMIx_Disconnect(const pmix_proc_t procs[], size_t nprocs,
                                          const pmix_info_t info[], size_t ninfo);

PMIX_EXPORT pmix_status_t PMIx_Disconnect_nb(const pmix_proc_t ranges[], size_t nprocs,
                                             const pmix_info_t info[], size_t ninfo,
                                             pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Given a node name, return an array of processes within the specified nspace
 * on that node. If the nspace is NULL, then all processes on the node will
 * be returned. If the specified node does not currently host any processes,
 * then the returned array will be NULL, and nprocs=0. The caller is responsible
 * for releasing the array when done with it - the PMIX_PROC_FREE macro is
 * provided for this purpose.
 */
PMIX_EXPORT pmix_status_t PMIx_Resolve_peers(const char *nodename,
                                             const pmix_nspace_t nspace,
                                             pmix_proc_t **procs, size_t *nprocs);


/* Given an nspace, return the list of nodes hosting processes within
 * that nspace. The returned string will contain a comma-delimited list
 * of nodenames. The caller is responsible for releasing the string
 * when done with it */
PMIX_EXPORT pmix_status_t PMIx_Resolve_nodes(const pmix_nspace_t nspace, char **nodelist);

/* Query information about the system in general - can include
 * a list of active nspaces, network topology, etc. Also can be
 * used to query node-specific info such as the list of peers
 * executing on a given node. We assume that the host RM will
 * exercise appropriate access control on the information.
 *
 * The following return status codes are provided in the callback:
 *
 * PMIX_SUCCESS - all data has been returned
 * PMIX_ERR_NOT_FOUND - none of the requested data was available
 * PMIX_ERR_PARTIAL_SUCCESS - some of the data has been returned
 * PMIX_ERR_NOT_SUPPORTED - the host RM does not support this function
 */
PMIX_EXPORT pmix_status_t PMIx_Query_info(pmix_query_t queries[], size_t nqueries,
                                          pmix_info_t **results, size_t *nresults);

PMIX_EXPORT pmix_status_t PMIx_Query_info_nb(pmix_query_t queries[], size_t nqueries,
                                             pmix_info_cbfunc_t cbfunc, void *cbdata);

/* Log data to a central data service/store, subject to the
 * services offered by the host resource manager. The data to
 * be logged is provided in the data array. The (optional) directives
 * can be used to request specific storage options and direct
 * the choice of storage option.
 *
 * The callback function will be executed when the log operation
 * has been completed. The data array must be maintained until
 * the callback is provided
 */
PMIX_EXPORT pmix_status_t PMIx_Log(const pmix_info_t data[], size_t ndata,
                                   const pmix_info_t directives[], size_t ndirs);

PMIX_EXPORT pmix_status_t PMIx_Log_nb(const pmix_info_t data[], size_t ndata,
                                      const pmix_info_t directives[], size_t ndirs,
                                      pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Request an allocation operation from the host scheduler.
 * Several broad categories are envisioned, including the ability to:
 *
 * - request allocation of additional resources, including memory,
 *   bandwidth, and compute. This should be accomplished in a
 *   non-blocking manner so that the application can continue to
 *   progress while waiting for resources to become available. Note
 *   that the new allocation will be disjoint from (i.e., not
 *   affiliated with) the allocation of the requestor - thus the
 *   termination of one allocation will not impact the other.
 *
 * - extend the reservation on currently allocated resources, subject
 *   to scheduling availability and priorities. This includes extending
 *   the time limit on current resources, and/or requesting additional
 *   resources be allocated to the requesting job. Any additional
 *   allocated resources will be considered as part of the current
 *   allocation, and thus will be released at the same time.
 *
 * - release currently allocated resources that are no longer required.
 *   This is intended to support partial release of resources since all
 *   resources are normally released upon termination of the job. The
 *   identified use-cases include resource variations across discrete steps
 *   of a workflow, as well as applications that spawn sub-jobs and/or
 *   dynamically grow/shrink over time
 *
 * - "lend" resources back to the scheduler with an expectation of getting
 *   them back at some later time in the job. This can be a proactive
 *   operation (e.g., to save on computing costs when resources are
 *   temporarily not required), or in response to scheduler requests in
 *   lieue of preemption. A corresponding ability to "reacquire" resources
 *   previously released is included.
 */
PMIX_EXPORT pmix_status_t PMIx_Allocation_request(pmix_alloc_directive_t directive,
                                                  pmix_info_t *info, size_t ninfo,
                                                  pmix_info_t **results, size_t *nresults);

PMIX_EXPORT pmix_status_t PMIx_Allocation_request_nb(pmix_alloc_directive_t directive,
                                                     pmix_info_t *info, size_t ninfo,
                                                     pmix_info_cbfunc_t cbfunc, void *cbdata);

/* Request a session control action. The sessionID identifies the session
 * to which the specified control action is to be applied. A NULL
 * value can be used to indicate all sessions under the caller's control.
 *
 * The directives are provided as pmix_info_t structs in the directives
 * array. The callback function provides a status to indicate whether or
 * not the request was granted, and to provide some information as to the
 * reason for any denial in the pmix_info_cbfunc_t' array of pmix_info_t
 * structures. If non-NULL, then the specified release_fn must be called
 * when the callback function completes - this will be used to release any
 * provided pmix_info_t array.

 * Passing NULL as the cbfunc to this call indicates that it shall be treated
 * as a blocking operation, with the return status indicative of the overall
 * operation's completion.
 */
PMIX_EXPORT pmix_status_t PMIx_Session_control(uint32_t sessionID,
                                               const pmix_info_t directives[], size_t ndirs,
                                               pmix_info_cbfunc_t cbfunc, void *cbdata);

/* Request a job control action. The targets array identifies the
 * processes to which the requested job control action is to be applied.
 * A NULL value can be used to indicate all processes in the caller's
 * nspace. The use of PMIX_RANK_WILDARD can also be used to indicate
 * that all processes in the given nspace are to be included.
 *
 * The directives are provided as pmix_info_t structs in the directives
 * array. The callback function provides a status to indicate whether or
 * not the request was granted, and to provide some information as to
 * the reason for any denial in the pmix_info_cbfunc_t array of pmix_info_t
 * structures. If non-NULL, then the specified release_fn must be called
 * when the callback function completes - this will be used to release
 * any provided pmix_info_t array.
 */
PMIX_EXPORT pmix_status_t PMIx_Job_control(const pmix_proc_t targets[], size_t ntargets,
                                           const pmix_info_t directives[], size_t ndirs,
                                           pmix_info_t **results, size_t *nresults);

PMIX_EXPORT pmix_status_t PMIx_Job_control_nb(const pmix_proc_t targets[], size_t ntargets,
                                              const pmix_info_t directives[], size_t ndirs,
                                              pmix_info_cbfunc_t cbfunc, void *cbdata);

/* Request that something be monitored - e.g., that the server monitor
 * this process for periodic heartbeats as an indication that the process
 * has not become "wedged". When a monitor detects the specified alarm
 * condition, it will generate an event notification using the provided
 * error code and passing along any available relevant information. It is
 * up to the caller to register a corresponding event handler.
 *
 * Params:
 *
 * monitor: attribute indicating the type of monitor being requested - e.g.,
 *          PMIX_MONITOR_FILE to indicate that the requestor is asking that
 *          a file be monitored.
 *
 * error: the status code to be used when generating an event notification
 *        alerting that the monitor has been triggered. The range of the
 *        notification defaults to PMIX_RANGE_NAMESPACE - this can be
 *        changed by providing a PMIX_RANGE directive
 *
 * directives: characterize the monitoring request (e.g., monitor file size)
 *             and frequency of checking to be done
 *
 * cbfunc: provides a status to indicate whether or not the request was granted,
 *         and to provide some information as to the reason for any denial in
 *         the pmix_info_cbfunc_t array of pmix_info_t structures.
 *
 * Note: a process can send a heartbeat to the server using the PMIx_Heartbeat
 * macro provided below*/
PMIX_EXPORT pmix_status_t PMIx_Process_monitor(const pmix_info_t *monitor, pmix_status_t error,
                                               const pmix_info_t directives[], size_t ndirs,
                                               pmix_info_t **results, size_t *nresults);

PMIX_EXPORT pmix_status_t PMIx_Process_monitor_nb(const pmix_info_t *monitor, pmix_status_t error,
                                                  const pmix_info_t directives[], size_t ndirs,
                                                  pmix_info_cbfunc_t cbfunc, void *cbdata);

/* define a special macro to simplify sending of a heartbeat */
#define PMIx_Heartbeat()                                                    \
    do {                                                                    \
        pmix_info_t _in;                                                    \
        PMIX_INFO_CONSTRUCT(&_in);                                          \
        PMIX_INFO_LOAD(&_in, PMIX_SEND_HEARTBEAT, NULL, PMIX_POINTER);      \
        PMIx_Process_monitor_nb(&_in, PMIX_SUCCESS, NULL, 0, NULL, NULL);   \
        PMIX_INFO_DESTRUCT(&_in);                                           \
    } while(0)

/* Request a credential from the PMIx server/SMS.
 * Input values include:
 *
 * info - an array of pmix_info_t structures containing any directives the
 *        caller may wish to pass. Typical usage might include:
 *            PMIX_TIMEOUT - how long to wait (in seconds) for a credential
 *                           before timing out and returning an error
 *            PMIX_CRED_TYPE - a prioritized, comma-delimited list of desired
 *                             credential types for use in environments where
 *                             multiple authentication mechanisms may be
 *                             available
 *
 * ninfo - number of elements in the info array
 *
 * cbfunc - the pmix_credential_cbfunc_t function to be called upon completion
 *          of the request
 *
 * cbdata - pointer to an object to be returned when cbfunc is called
 *
 * Returned values:
 * PMIX_SUCCESS - indicates that the request has been successfully communicated to
 *                the local PMIx server. The response will be coming in the provided
 *                callback function.
 *
 * Any other value indicates an appropriate error condition. The callback function
 * will _not_ be called in such cases.
 */
PMIX_EXPORT pmix_status_t PMIx_Get_credential(const pmix_info_t info[], size_t ninfo,
                                              pmix_byte_object_t *credential);

PMIX_EXPORT pmix_status_t PMIx_Get_credential_nb(const pmix_info_t info[], size_t ninfo,
                                                 pmix_credential_cbfunc_t cbfunc, void *cbdata);

/* Request validation of a credential by the PMIx server/SMS
 * Input values include:
 *
 * cred - pointer to a pmix_byte_object_t containing the credential
 *
 * info - an array of pmix_info_t structures containing any directives the
 *        caller may wish to pass. Typical usage might include:
 *            PMIX_TIMEOUT - how long to wait (in seconds) for validation
 *                           before timing out and returning an error
 *            PMIX_USERID - the expected effective userid of the credential
 *                          to be validated
 *            PMIX_GROUPID - the expected effective group id of the credential
 *                          to be validated
 *
 * ninfo - number of elements in the info array
 *
 * cbfunc - the pmix_validation_cbfunc_t function to be called upon completion
 *          of the request
 *
 * cbdata - pointer to an object to be returned when cbfunc is called
 *
 * Returned values:
 * PMIX_SUCCESS - indicates that the request has been successfully communicated to
 *                the local PMIx server. The response will be coming in the provided
 *                callback function.
 *
 * Any other value indicates an appropriate error condition. The callback function
 * will _not_ be called in such cases.
 */
PMIX_EXPORT pmix_status_t PMIx_Validate_credential(const pmix_byte_object_t *cred,
                                                   const pmix_info_t info[], size_t ninfo,
                                                   pmix_info_t **results, size_t *nresults);

PMIX_EXPORT pmix_status_t PMIx_Validate_credential_nb(const pmix_byte_object_t *cred,
                                                      const pmix_info_t info[], size_t ninfo,
                                                      pmix_validation_cbfunc_t cbfunc, void *cbdata);


/* Construct a new group composed of the specified processes and identified with
 * the provided group identifier. Both blocking and non-blocking versions
 * are provided (the callback function for the non-blocking form will be called
 * once all specified processes have joined the group). The group identifier is
 * a user-defined, NULL-terminated character array of length less than or equal
 * to PMIX_MAX_NSLEN. Only characters accepted by standard string comparison
 * functions (e.g., strncmp) are supported.
 *
 * Processes may engage in multiple simultaneous group construct operations as
 * desired so long as each is provided with a unique group ID. The info array
 * can be used to pass user-level directives regarding timeout constraints and
 * other options available from the PMIx server.
 *
 * The construct leader (if PMIX_GROUP_LEADER is provided) or all participants
 * will receive events (if registered for the PMIX_GROUP_MEMBER_FAILED event)
 * whenever a process fails or terminates prior to calling
 * PMIx_Group_construct(_nb) – the events will contain the identifier of the
 * process that failed to join plus any other information that the resource
 * manager provided. This provides an opportunity for the leader to react to
 * the event – e.g., to invite an alternative member to the group or to decide
 * to proceed with a smaller group. The decision to proceed with a smaller group
 * is communicated to the PMIx library in the results array at the end of the
 * event handler. This allows PMIx to properly adjust accounting for procedure
 * completion. When construct is complete, the participating PMIx servers will
 * be alerted to any change in participants and each group member will (if
 * registered) receive a PMIX_GROUP_MEMBERSHIP_UPDATE event updating the group
 * membership.
 *
 * Processes in a group under construction are not allowed to leave the group
 * until group construction is complete. Upon completion of the construct
 * procedure, each group member will have access to the job-level information
 * of all nspaces represented in the group and the contact information for
 * every group member.
 *
 * Failure of the leader at any time will cause a PMIX_GROUP_LEADER_FAILED event
 * to be delivered to all participants so they can optionally declare a new leader.
 * A new leader is identified by providing the PMIX_GROUP_LEADER attribute in
 * the results array in the return of the event handler. Only one process is
 * allowed to return that attribute, declaring itself as the new leader. Results
 * of the leader selection will be communicated to all participants via a
 * PMIX_GROUP_LEADER_SELECTED event identifying the new leader. If no leader
 * was selected, then the status code provided in the event handler will provide
 * an error value so the participants can take appropriate action.
 *
 * Any participant that returns PMIX_GROUP_CONSTRUCT_ABORT from the leader failed
 * event handler will cause the construct process to abort. Those processes
 * engaged in the blocking construct will return from the call with the
 * PMIX_GROUP_CONSTRUCT_ABORT status. Non-blocking participants will have
 * their callback function executed with that status.
 *
 * Some relevant attributes for this operation:
 *    PMIX_GROUP_LEADER - declare this process to be the leader of the construction
 *                        procedure. If a process provides this attribute, then
 *                        failure notification for any participating process will
 *                        go only to that one process. In the absence of a
 *                        declared leader, failure events go to all participants.
 *    PMIX_GROUP_OPTIONAL - participation is optional - do not return an error if
 *                          any of the specified processes terminate
 *                          without having joined (default=false)
 *    PMIX_GROUP_NOTIFY_TERMINATION - notify remaining members when another member
 *                                    terminates without first leaving the
 *                                    group (default=false)
 *    PMIX_GROUP_ASSIGN_CONTEXT_ID - requests that the RM assign a unique context
 *                                   ID (size_t) to the group. The value is returned
 *                                   in the PMIX_GROUP_CONSTRUCT_COMPLETE event
 *    PMIX_TIMEOUT - return an error if the group doesn't assemble within the
 *                   specified number of seconds. Targets the scenario where a
 *                   process fails to call PMIx_Group_connect due to hanging
 *
 */
PMIX_EXPORT pmix_status_t PMIx_Group_construct(const char grp[],
                                               const pmix_proc_t procs[], size_t nprocs,
                                               const pmix_info_t directives[], size_t ndirs,
                                               pmix_info_t **results, size_t *nresults);

PMIX_EXPORT pmix_status_t PMIx_Group_construct_nb(const char grp[],
                                                  const pmix_proc_t procs[], size_t nprocs,
                                                  const pmix_info_t info[], size_t ninfo,
                                                  pmix_info_cbfunc_t cbfunc, void *cbdata);

/* Explicitly invite specified processes to join a group.
 *
 * Each invited process will be notified of the invitation via the PMIX_GROUP_INVITED
 * event. The processes being invited must have registered for the PMIX_GROUP_INVITED
 * event in order to be notified of the invitation. When ready to respond, each invited
 * process provides a response using the appropriate form of PMIx_Group_join. This will
 * notify the inviting process that the invitation was either accepted (via the
 * PMIX_GROUP_INVITE_ACCEPTED event) or declined (via the PMIX_GROUP_INVITE_DECLINED event).
 * The inviting process will also receive PMIX_GROUP_MEMBER_FAILED events whenever a
 * process fails or terminates prior to responding to the invitation.
 *
 * Upon accepting the invitation, both the inviting and invited process will receive
 * access to the job-level information of each other’s nspaces and the contact
 * information of the other process.
 *
 * Some relevant attributes for this operation:
 *    PMIX_GROUP_ASSIGN_CONTEXT_ID - requests that the RM assign a unique context
 *                                   ID (size_t) to the group. The value is returned
 *                                   in the PMIX_GROUP_CONSTRUCT_COMPLETE event
 *    PMIX_TIMEOUT (int): return an error if the group doesn’t assemble within the
 *                        specified number of seconds. Targets the scenario where a
 *                        process fails to call PMIx_Group_connect due to hanging
 *
 * The inviting process is automatically considered the leader of the asynchronous
 * group construction procedure and will receive all failure or termination events
 * for invited members prior to completion. The inviting process is required to
 * provide a PMIX_GROUP_CONSTRUCT_COMPLETE event once the group has been fully
 * assembled – this event will be distributed to all participants along with the
 * final membership.
 *
 * Failure of the leader at any time will cause a PMIX_GROUP_LEADER_FAILED event
 * to be delivered to all participants so they can optionally declare a new leader.
 * A new leader is identified by providing the PMIX_GROUP_LEADER attribute in
 * the results array in the return of the event handler. Only one process is
 * allowed to return that attribute, declaring itself as the new leader. Results
 * of the leader selection will be communicated to all participants via a
 * PMIX_GROUP_LEADER_SELECTED event identifying the new leader. If no leader
 * was selected, then the status code provided in the event handler will provide
 * an error value so the participants can take appropriate action.
 *
 * Any participant that returns PMIX_GROUP_CONSTRUCT_ABORT from the event
 * handler will cause all participants to receive an event notifying them
 * of that status.
 */
PMIX_EXPORT pmix_status_t PMIx_Group_invite(const char grp[],
                                            const pmix_proc_t procs[], size_t nprocs,
                                            const pmix_info_t info[], size_t ninfo,
                                            pmix_info_t **results, size_t *nresult);

PMIX_EXPORT pmix_status_t PMIx_Group_invite_nb(const char grp[],
                                               const pmix_proc_t procs[], size_t nprocs,
                                               const pmix_info_t info[], size_t ninfo,
                                               pmix_info_cbfunc_t cbfunc, void *cbdata);

/* Respond to an invitation to join a group that is being asynchronously constructed.
 *
 * The process must have registered for the PMIX_GROUP_INVITED event in order to be
 * notified of the invitation. When ready to respond, the process provides a response
 * using the appropriate form of PMIx_Group_join.
 *
 * Critical Note: Since the process is alerted to the invitation in a PMIx event handler,
 * the process must not use the blocking form of this call unless it first “thread shifts”
 * out of the handler and into its own thread context. Likewise, while it is safe to call
 * the non-blocking form of the API from the event handler, the process must not block
 * in the handler while waiting for the callback function to be called.
 *
 * Calling this function causes the group “leader” to be notified that the process has
 * either accepted or declined the request. The blocking form of the API will return
 * once the group has been completely constructed or the group’s construction has failed
 * (as determined by the leader) – likewise, the callback function of the non-blocking
 * form will be executed upon the same conditions.
 *
 * Failure of the leader at any time will cause a PMIX_GROUP_LEADER_FAILED event
 * to be delivered to all participants so they can optionally declare a new leader.
 * A new leader is identified by providing the PMIX_GROUP_LEADER attribute in
 * the results array in the return of the event handler. Only one process is
 * allowed to return that attribute, declaring itself as the new leader. Results
 * of the leader selection will be communicated to all participants via a
 * PMIX_GROUP_LEADER_SELECTED event identifying the new leader. If no leader
 * was selected, then the status code provided in the event handler will provide
 * an error value so the participants can take appropriate action.
 *
 * Any participant that returns PMIX_GROUP_CONSTRUCT_ABORT from the leader failed
 * event handler will cause all participants to receive an event notifying them
 * of that status. Similarly, the leader may elect to abort the procedure
 * by either returning PMIX_GROUP_CONSTRUCT_ABORT from the handler assigned
 * to the PMIX_GROUP_INVITE_ACCEPTED or PMIX_GROUP_INVITE_DECLINED codes, or
 * by generating an event for the abort code. Abort events will be sent to
 * all invited participants.
 */
PMIX_EXPORT pmix_status_t PMIx_Group_join(const char grp[],
                                          const pmix_proc_t *leader,
                                          pmix_group_opt_t opt,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_info_t **results, size_t *nresult);

PMIX_EXPORT pmix_status_t PMIx_Group_join_nb(const char grp[],
                                             const pmix_proc_t *leader,
                                             pmix_group_opt_t opt,
                                             const pmix_info_t info[], size_t ninfo,
                                             pmix_info_cbfunc_t cbfunc, void *cbdata);

/* Leave a PMIx Group. Calls to PMIx_Group_leave (or its non-blocking form) will cause
 * a PMIX_GROUP_LEFT event to be generated notifying all members of the group of the
 * caller’s departure. The function will return (or the non-blocking function will
 * execute the specified callback function) once the event has been locally generated
 * and is not indicative of remote receipt. All PMIx-based collectives such as
 * PMIx_Fence in action across the group will automatically be adjusted if the
 * collective was called with the PMIX_GROUP_FT_COLLECTIVE attribute (default is
 * false) – otherwise, the standard error return behavior will be provided.
 *
 * Critical Note: The PMIx_Group_leave API is intended solely for asynchronous
 * departures of individual processes from a group as it is not a scalable
 * operation – i.e., when a process determines it should no longer be a part of a
 * defined group, but the remainder of the group retains a valid reason to continue
 * in existence. Developers are advised to use PMIx_Group_destruct (or its
 * non-blocking form) for all other scenarios as it represents a more scalable
 * operation.
 */
PMIX_EXPORT pmix_status_t PMIx_Group_leave(const char grp[],
                                           const pmix_info_t info[], size_t ninfo);

PMIX_EXPORT pmix_status_t PMIx_Group_leave_nb(const char grp[],
                                              const pmix_info_t info[], size_t ninfo,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Destruct a group identified by the provided group identifier. Both blocking and
 * non-blocking versions are provided (the callback function for the non-blocking
 * form will be called once all members of the group have called “destruct”).
 * Processes may engage in multiple simultaneous group destruct operations as
 * desired so long as each involves a unique group ID. The info array can be used
 * to pass user-level directives regarding timeout constraints and other options
 * available from the PMIx server.
 *
 * Some relevant attributes for this operation:
 *
 *    PMIX_TIMEOUT (int): return an error if the group doesn’t destruct within the
 *                        specified number of seconds. Targets the scenario where
 *                        a process fails to call PMIx_Group_destruct due to hanging
 *
 * The destruct API will return an error if any group process fails or terminates
 * prior to calling PMIx_Group_destruct or its non-blocking version unless the
 * PMIX_GROUP_NOTIFY_TERMINATION attribute was provided (with a value of true) at
 * time of group construction. If notification was requested, then a event will
 * be delivered (using PMIX_GROUP_MEMBER_FAILED) for each process that fails to
 * call destruct and the destruct tracker updated to account for the lack of
 * participation. The PMIx_Group_destruct operation will subsequently return
 * PMIX_SUCCESS when the remaining processes have all called destruct – i.e., the
 * event will serve in place of return of an error.
 */
PMIX_EXPORT pmix_status_t PMIx_Group_destruct(const char grp[],
                                              const pmix_info_t info[], size_t ninfo);

PMIX_EXPORT pmix_status_t PMIx_Group_destruct_nb(const char grp[],
                                                 const pmix_info_t info[], size_t ninfo,
                                                 pmix_op_cbfunc_t cbfunc, void *cbdata);

/****************************************/
/****    COMMON SUPPORT FUNCTIONS    ****/
/****************************************/

/******     EVENT NOTIFICATION SUPPORT      ******/
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
 * If cbfunc is NULL, then this is treated as a BLOCKING call - a positive
 * return value represents the reference ID for the request, while
 * negative values indicate the corresponding error
 *
 * See pmix_common.h for a description of the notification function */
PMIX_EXPORT pmix_status_t PMIx_Register_event_handler(pmix_status_t codes[], size_t ncodes,
                                                      pmix_info_t info[], size_t ninfo,
                                                      pmix_notification_fn_t evhdlr,
                                                      pmix_hdlr_reg_cbfunc_t cbfunc,
                                                      void *cbdata);

/* Deregister an event handler
 * evhdlr_ref is the reference returned by PMIx from the call to
 * PMIx_Register_event_handler. If non-NULL, the provided cbfunc
 * will be called to confirm removal of the designated handler */
PMIX_EXPORT pmix_status_t PMIx_Deregister_event_handler(size_t evhdlr_ref,
                                                        pmix_op_cbfunc_t cbfunc,
                                                        void *cbdata);

/* Report an event for notification via any
 * registered evhdlr.
 *
 * This function allows the host server to direct the server
 * convenience library to notify all registered local procs of
 * an event. The event can be local, or anywhere in the cluster.
 * The status indicates the event being reported.
 *
 * The client application can also call this function to notify the
 * resource manager and/or other processes of an event it encountered.
 * It can also be used to asynchronously notify other parts of its
 * own internal process - e.g., for one library to notify another
 * when initialized inside the process.
 *
 * status - status code indicating the event being reported
 *
 * source - the process that generated the event
 *
 * range - the range in which the event is to be reported. For example,
 *         a value of PMIX_RANGE_LOCAL would instruct the system
 *         to only notify procs on the same local node as the
 *         event generator.
 *
 * info - an array of pmix_info_t structures provided by the event
 *        generator to pass any additional information about the
 *        event. This can include an array of pmix_proc_t structs
 *        describing the processes impacted by the event, the nature
 *        of the event and its severity, etc. The precise contents
 *        of the array will depend on the event generator.
 *
 * ninfo - number of elements in the info array
 *
 * cbfunc - callback function to be called upon completion of the
 *          notify_event function's actions. Note that any messages
 *          will have been queued, but may not have been transmitted
 *          by this time. Note that the caller is required to maintain
 *          the input data until the callback function has been executed!
 *          If cbfunc is NULL, then this is treated as a BLOCKING call and
 *          the result of the operation is provided in the returned
 *          status
 *
 * cbdata - the caller's provided void* object
 */
PMIX_EXPORT pmix_status_t PMIx_Notify_event(pmix_status_t status,
                                            const pmix_proc_t *source,
                                            pmix_data_range_t range,
                                            const pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata);


/******    FABRIC-RELATED APIS    ******/
/* Register for access to fabric-related information, including
 * communication cost matrix. This call must be made prior to
 * requesting information from a fabric.
 *
 * fabric - address of a pmix_fabric_t (backed by storage). User
 *          may populate the "name" field at will - PMIx does not
 *          utilize this field
 *
 * directives - an optional array of values indicating desired
 *              behaviors and/or fabric to be accessed. If NULL,
 *              then the highest priority available fabric will
 *              be used
 *
 * ndirs - number of elements in the directives array
 *
 * Return values include:
 *
 * PMIX_SUCCESS - indicates success
 */
PMIX_EXPORT pmix_status_t PMIx_Fabric_register(pmix_fabric_t *fabric,
                                               const pmix_info_t directives[],
                                               size_t ndirs);

PMIX_EXPORT pmix_status_t PMIx_Fabric_register_nb(pmix_fabric_t *fabric,
												  const pmix_info_t directives[],
											      size_t ndirs,
												  pmix_op_cbfunc_t cbfunc, void *cbdata);


/* Update fabric-related information. This call can be made at any time to request an update of the
 * fabric information contained in the provided pmix_fabric_t object. The caller is not allowed
 * to access the provided pmix_fabric_t until the call has returned.
 *
 * fabric - pointer to the pmix_fabric_t struct provided to
 *          the registration function
 *
 * Return values include:
 *
 * PMIX_SUCCESS - indicates successful update
 */
PMIX_EXPORT pmix_status_t PMIx_Fabric_update(pmix_fabric_t *fabric);

PMIX_EXPORT pmix_status_t PMIx_Fabric_update_nb(pmix_fabric_t *fabric,
											    pmix_op_cbfunc_t cbfunc, void *cbdata);


/* Deregister a fabric object, providing an opportunity for
 * the PMIx server library to cleanup any information
 * (e.g., cost matrix) associated with it
 *
 * fabric - pointer to the pmix_fabric_t struct provided
 *          to the registration function
 */
PMIX_EXPORT pmix_status_t PMIx_Fabric_deregister(pmix_fabric_t *fabric);

PMIX_EXPORT pmix_status_t PMIx_Fabric_deregister_nb(pmix_fabric_t *fabric,
												    pmix_op_cbfunc_t cbfunc, void *cbdata);


/* Compute the distance information for the current process
 * Returns an array of distances from the current process
 * location to each of the local devices of the specified type(s)
 *
 * distances - pointer to location where the array of
 *             distances is to be returned
 *
 * ndist - number of elements in the distances array
 *
 * Return values include:
 *
 * PMIX_SUCCESS - distance array was successfully returned
 * Other error
 */
PMIX_EXPORT pmix_status_t PMIx_Compute_distances(pmix_topology_t *topo,
                                                 pmix_cpuset_t *cpuset,
                                                 pmix_info_t info[], size_t ninfo,
                                                 pmix_device_distance_t *distances[],
                                                 size_t *ndist);

PMIX_EXPORT pmix_status_t PMIx_Compute_distances_nb(pmix_topology_t *topo,
                                                    pmix_cpuset_t *cpuset,
                                                    pmix_info_t info[], size_t ninfo,
                                                    pmix_device_dist_cbfunc_t cbfunc,
                                                    void *cbdata);

/* Load the local hwardware topology description
 *
 * topo - pointer to a pmix_topology_t object. This object
 *        must be initialized! If the a particular "source"
 *        for the topology is required (e.g., "hwloc"), then
 *        the "source" field of the object must be set to
 *        that value
 *
 * Return values include:
 * PMIX_SUCCESS - indicates return of a valid value
 * PMIX_ERR_NOT_FOUND - provided source is not available
 * PMIX_ERR_NOT_SUPPORTED - current implementation does not support this option
 */
PMIX_EXPORT pmix_status_t PMIx_Load_topology(pmix_topology_t *topo);

/* Get the PU binding bitmap from its string representation
 *
 * cpuset_string - string representation of the binding bitmap
 *                 (as returned by PMIx_Get using the PMIX_CPUSET key)
 *
 * cpuset - pointer to a pmix_cpuset_t object where the result
 *          is to be stored
 *
 * Return values include:
 * PMIX_SUCCESS - indicates return of a valid value
 * PMIX_ERR_NOT_FOUND - provided source is not available
 * PMIX_ERR_NOT_SUPPORTED - current implementation does not support this option
 */
PMIX_EXPORT pmix_status_t PMIx_Parse_cpuset_string(const char *cpuset_string,
	                                               pmix_cpuset_t *cpuset);

PMIX_EXPORT pmix_status_t PMIx_Get_cpuset(pmix_cpuset_t *cpuset, pmix_bind_envelope_t ref);

/* Get the relative locality of two local processes given their locality strings.
 *
 * locality1 - String returned by the PMIx_server_generate_locality_string API
 *
 * locality2 - String returned by the PMIx_server_generate_locality_string API
 *
 * locality - Pointer to the location where the relative locality bitmask is
 *            to be constructed
 *
 * Return values include:
 * PMIX_SUCCESS - indicates return of a valid value
 * other error constant
 */
PMIX_EXPORT pmix_status_t PMIx_Get_relative_locality(const char *locality1,
	                                                 const char *locality2,
	                                                 pmix_locality_t *locality);

PMIX_EXPORT void PMIx_Progress(void);

/******    PRETTY-PRINT DEFINED VALUE TYPES     ******/
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
 * - pmix_iof_channel_t  (PMIX_IOF_CHANNEL)
 * - pmix_job_state_t  (PMIX_JOB_STATE)
 * - pmix_proc_state_t  (PMIX_PROC_STATE)
 * - attribute string value of provided name
 * - attribute name corresponding to provided string
 * - pmix_link_state_t (PMIX_LINK_STATE)
 * - pmix_device_type_t (PMIX_DEVTYPE)
 * - pmix_value_cmp_t (enum)
 * - pmix_info_t (PMIX_INFO)
 * - pmix_value_t (PMIX_VALUE)
 * - pmix_info_directives_t (PMIX_INFO_DIRECTIVES)
 * - pmix_app_t (PMIX_APP)
 */
PMIX_EXPORT const char* PMIx_Error_string(pmix_status_t status);
PMIX_EXPORT pmix_status_t PMIx_Error_code(const char *errname);
PMIX_EXPORT const char* PMIx_Proc_state_string(pmix_proc_state_t state);
PMIX_EXPORT const char* PMIx_Scope_string(pmix_scope_t scope);
PMIX_EXPORT const char* PMIx_Persistence_string(pmix_persistence_t persist);
PMIX_EXPORT const char* PMIx_Data_range_string(pmix_data_range_t range);
PMIX_EXPORT const char* PMIx_Data_type_string(pmix_data_type_t type);
PMIX_EXPORT const char* PMIx_Alloc_directive_string(pmix_alloc_directive_t directive);
PMIX_EXPORT const char* PMIx_IOF_channel_string(pmix_iof_channel_t channel);
PMIX_EXPORT const char* PMIx_Job_state_string(pmix_job_state_t state);
PMIX_EXPORT const char* PMIx_Get_attribute_string(const char *attribute);
PMIX_EXPORT const char* PMIx_Get_attribute_name(const char *attrstring);
PMIX_EXPORT const char* PMIx_Link_state_string(pmix_link_state_t state);
PMIX_EXPORT const char* PMIx_Device_type_string(pmix_device_type_t type);
PMIX_EXPORT const char* PMIx_Value_comparison_string(pmix_value_cmp_t cmp);

/* the following print statements return ALLOCATED strings
 * that the user must release when done */
PMIX_EXPORT char* PMIx_Info_string(const pmix_info_t *info);
PMIX_EXPORT char* PMIx_Value_string(const pmix_value_t *value);
PMIX_EXPORT char* PMIx_Info_directives_string(pmix_info_directives_t directives);
PMIX_EXPORT char* PMIx_App_string(const pmix_app_t *app);
PMIX_EXPORT char* PMIx_Proc_string(const pmix_proc_t *proc);

/* Get the PMIx version string. Note that the provided string is
 * statically defined and must NOT be free'd  */
PMIX_EXPORT const char* PMIx_Get_version(void);

/* Store some data locally for retrieval by other areas of the
 * proc. This is data that has only internal scope - it will
 * never be "pushed" externally */
PMIX_EXPORT pmix_status_t PMIx_Store_internal(const pmix_proc_t *proc,
                                              const char key[], pmix_value_t *val);


/* Compute and return the size (in bytes) of the data
 * payload in a pmix_value_t structure. Returns:
 *
 * - PMIX_SUCCESS if the value could be computed
 *
 * - an appropriate error value (e.g., PMIX_ERR_UNKNOWN_DATA_TYPE
 *   if the data type is unknown) if the value could not be computed.
 */
PMIX_EXPORT pmix_status_t PMIx_Value_get_size(const pmix_value_t *val,
                                              size_t *size);

/* Compute and return the size (in bytes) of the data
 * payload in a pmix_info_t structure. Returns:
 *
 * - PMIX_SUCCESS if the value could be computed
 *
 * - an appropriate error value (e.g., PMIX_ERR_UNKNOWN_DATA_TYPE
 *   if the data type is unknown) if the value could not be computed.
 */
PMIX_EXPORT pmix_status_t PMIx_Info_get_size(const pmix_info_t *val,
                                             size_t *size);


/******    DATA BUFFER PACK/UNPACK SUPPORT    ******/
/**
 * Top-level interface function to pack one or more values into a
 * buffer.
 *
 * The pack function packs one or more values of a specified type into
 * the specified buffer.  The buffer must have already been
 * initialized via the PMIX_DATA_BUFFER_CREATE or PMIX_DATA_BUFFER_CONSTRUCT
 * call - otherwise, the pack_value function will return an error.
 * Providing an unsupported type flag will likewise be reported as an error.
 *
 * Note that any data to be packed that is not hard type cast (i.e.,
 * not type cast to a specific size) may lose precision when unpacked
 * by a non-homogeneous recipient.  The PACK function will do its best to deal
 * with heterogeneity issues between the packer and unpacker in such
 * cases. Sending a number larger than can be handled by the recipient
 * will return an error code (generated upon unpacking) -
 * the error cannot be detected during packing.
 *
 * The identity of the intended recipient of the packed buffer (i.e., the
 * process that will be unpacking it) is used solely to resolve any data type
 * differences between PMIx versions. The recipient must, therefore, be
 * known to the user prior to calling the pack function so that the
 * PMIx library is aware of the version the recipient is using.
 *
 * @param *target Pointer to a pmix_proc_t structure containing the
 * nspace/rank of the process that will be unpacking the final buffer.
 * A NULL value may be used to indicate that the target is based on
 * the same PMIx version as the caller.
 *
 * @param *buffer A pointer to the buffer into which the value is to
 * be packed.
 *
 * @param *src A void* pointer to the data that is to be packed. Note
 * that strings are to be passed as (char **) - i.e., the caller must
 * pass the address of the pointer to the string as the void*. This
 * allows PMIx to use a single pack function, but still allow
 * the caller to pass multiple strings in a single call.
 *
 * @param num_values An int32_t indicating the number of values that are
 * to be packed, beginning at the location pointed to by src. A string
 * value is counted as a single value regardless of length. The values
 * must be contiguous in memory. Arrays of pointers (e.g., string
 * arrays) should be contiguous, although (obviously) the data pointed
 * to need not be contiguous across array entries.
 *
 * @param type The type of the data to be packed - must be one of the
 * PMIX defined data types.
 *
 * @retval PMIX_SUCCESS The data was packed as requested.
 *
 * @retval PMIX_ERROR(s) An appropriate PMIX error code indicating the
 * problem encountered. This error code should be handled
 * appropriately.
 *
 * @code
 * pmix_data_buffer_t *buffer;
 * int32_t src;
 *
 * PMIX_DATA_BUFFER_CREATE(buffer);
 * status_code = PMIx_Data_pack(buffer, &src, 1, PMIX_INT32);
 * @endcode
 */
PMIX_EXPORT pmix_status_t PMIx_Data_pack(const pmix_proc_t *target,
                                         pmix_data_buffer_t *buffer,
                                         void *src, int32_t num_vals,
                                         pmix_data_type_t type);

/**
 * Unpack values from a buffer.
 *
 * The unpack function unpacks the next value (or values) of a
 * specified type from the specified buffer.
 *
 * The buffer must have already been initialized via an PMIX_DATA_BUFFER_CREATE or
 * PMIX_DATA_BUFFER_CONSTRUCT call (and assumedly filled with some data) -
 * otherwise, the unpack_value function will return an
 * error. Providing an unsupported type flag will likewise be reported
 * as an error, as will specifying a data type that DOES NOT match the
 * type of the next item in the buffer. An attempt to read beyond the
 * end of the stored data held in the buffer will also return an
 * error.
 *
 * NOTE: it is possible for the buffer to be corrupted and that
 * PMIx will *think* there is a proper variable type at the
 * beginning of an unpack region - but that the value is bogus (e.g., just
 * a byte field in a string array that so happens to have a value that
 * matches the specified data type flag). Therefore, the data type error check
 * is NOT completely safe. This is true for ALL unpack functions.
 *
 *
 * Unpacking values is a "nondestructive" process - i.e., the values are
 * not removed from the buffer. It is therefore possible for the caller
 * to re-unpack a value from the same buffer by resetting the unpack_ptr.
 *
 * Warning: The caller is responsible for providing adequate memory
 * storage for the requested data. As noted below, the user
 * must provide a parameter indicating the maximum number of values that
 * can be unpacked into the allocated memory. If more values exist in the
 * buffer than can fit into the memory storage, then the function will unpack
 * what it can fit into that location and return an error code indicating
 * that the buffer was only partially unpacked.
 *
 * Note that any data that was not hard type cast (i.e., not type cast
 * to a specific size) when packed may lose precision when unpacked by
 * a non-homogeneous recipient.  PMIx will do its best to deal with
 * heterogeneity issues between the packer and unpacker in such
 * cases. Sending a number larger than can be handled by the recipient
 * will return an error code generated upon unpacking - these errors
 * cannot be detected during packing.
 *
 * The identity of the source of the packed buffer (i.e., the
 * process that packed it) is used solely to resolve any data type
 * differences between PMIx versions. The source must, therefore, be
 * known to the user prior to calling the unpack function so that the
 * PMIx library is aware of the version the source used.
 *
 * @param *source Pointer to a pmix_proc_t structure containing the
 * nspace/rank of the process that packed the provided buffer.
 * A NULL value may be used to indicate that the source is based on
 * the same PMIx version as the caller.
 *
 * @param *buffer A pointer to the buffer from which the value will be
 * extracted.
 *
 * @param *dest A void* pointer to the memory location into which the
 * data is to be stored. Note that these values will be stored
 * contiguously in memory. For strings, this pointer must be to (char
 * **) to provide a means of supporting multiple string
 * operations. The unpack function will allocate memory for each
 * string in the array - the caller must only provide adequate memory
 * for the array of pointers.
 *
 * @param type The type of the data to be unpacked - must be one of
 * the BFROP defined data types.
 *
 * @retval *max_num_values The number of values actually unpacked. In
 * most cases, this should match the maximum number provided in the
 * parameters - but in no case will it exceed the value of this
 * parameter.  Note that if you unpack fewer values than are actually
 * available, the buffer will be in an unpackable state - the function will
 * return an error code to warn of this condition.
 *
 * @note The unpack function will return the actual number of values
 * unpacked in this location.
 *
 * @retval PMIX_SUCCESS The next item in the buffer was successfully
 * unpacked.
 *
 * @retval PMIX_ERROR(s) The unpack function returns an error code
 * under one of several conditions: (a) the number of values in the
 * item exceeds the max num provided by the caller; (b) the type of
 * the next item in the buffer does not match the type specified by
 * the caller; or (c) the unpack failed due to either an error in the
 * buffer or an attempt to read past the end of the buffer.
 *
 * @code
 * pmix_data_buffer_t *buffer;
 * int32_t dest;
 * char **string_array;
 * int32_t num_values;
 *
 * num_values = 1;
 * status_code = PMIx_Data_unpack(buffer, (void*)&dest, &num_values, PMIX_INT32);
 *
 * num_values = 5;
 * string_array = pmix_malloc(num_values*sizeof(char *));
 * status_code = PMIx_Data_unpack(buffer, (void*)(string_array), &num_values, PMIX_STRING);
 *
 * @endcode
 */
PMIX_EXPORT pmix_status_t PMIx_Data_unpack(const pmix_proc_t *source,
                                           pmix_data_buffer_t *buffer, void *dest,
                                           int32_t *max_num_values,
                                           pmix_data_type_t type);

/**
 * Copy a data value from one location to another.
 *
 * Since registered data types can be complex structures, the system
 * needs some way to know how to copy the data from one location to
 * another (e.g., for storage in the registry). This function, which
 * can call other copy functions to build up complex data types, defines
 * the method for making a copy of the specified data type.
 *
 * @param **dest The address of a pointer into which the
 * address of the resulting data is to be stored.
 *
 * @param *src A pointer to the memory location from which the
 * data is to be copied.
 *
 * @param type The type of the data to be copied - must be one of
 * the PMIx defined data types.
 *
 * @retval PMIX_SUCCESS The value was successfully copied.
 *
 * @retval PMIX_ERROR(s) An appropriate error code.
 *
 */
PMIX_EXPORT pmix_status_t PMIx_Data_copy(void **dest, void *src,
                                         pmix_data_type_t type);

/**
 * Print a data value.
 *
 * Since registered data types can be complex structures, the system
 * needs some way to know how to print them (i.e., convert them to a string
 * representation). Provided for debug purposes.
 *
 * @retval PMIX_SUCCESS The value was successfully printed.
 *
 * @retval PMIX_ERROR(s) An appropriate error code.
 */
PMIX_EXPORT pmix_status_t PMIx_Data_print(char **output, char *prefix,
                                          void *src, pmix_data_type_t type);

/**
 * Copy a payload from one buffer to another
 *
 * This function will append a copy of the payload in one buffer into
 * another buffer.
 * NOTE: This is NOT a destructive procedure - the
 * source buffer's payload will remain intact, as will any pre-existing
 * payload in the destination's buffer.
 */
PMIX_EXPORT pmix_status_t PMIx_Data_copy_payload(pmix_data_buffer_t *dest,
                                                 pmix_data_buffer_t *src);

/**
 * Unload a buffer into a byte object
 *
 * The unload function provides the caller with a pointer to the data
 * payload within the buffer and the size of that payload. This allows
 * the user to directly access the payload.
 *
 * @note This is a destructive operation. While the payload is
 * undisturbed, the function will clear the buffer's pointers to the
 * payload. Thus, the buffer and the payload are completely separated,
 * leaving the caller free to release the buffer.
 *
 * @param buffer A pointer to the buffer whose payload is to be
 * unloaded.
 *
 * @param payload The address of a pmix_byte_object_t into which
 * the buffer is to be unloaded
 *
 * @retval PMIX_SUCCESS The request was successfully completed.
 *
 * @retval PMIX_ERROR(s) An appropriate error code indicating the
 * problem will be returned. This should be handled appropriately by
 * the caller.
 *
 * @code
 * pmix_data_buffer_t *buffer;
 * pmix_byte_object_t payload;
 *
 * status_code = PMIx_Data_unload(buffer, &payload);
 * @endcode
 */
PMIX_EXPORT pmix_status_t PMIx_Data_unload(pmix_data_buffer_t *buffer,
                                           pmix_byte_object_t *payload);

/**
 * Load a data payload into a buffer.
 *
 * The load function allows the caller to replace the payload in a
 * buffer with one provided by the caller. If a payload already exists
 * in the buffer, the function will "free" the existing data to
 * release it, and then replace the data payload with the one provided
 * by the caller.
 *
 * @note The buffer must be allocated in advance - failing to do so
 * will cause the load function to return an error code.
 *
 * @note The caller is responsible for pre-packing the provided
 * payload - the load function cannot convert to network byte order
 * any data contained in the provided payload.
 *
 * @note The "payload" object will be empty upon completion of
 * this operation.
 *
 * @param buffer A pointer to the buffer into which the payload is to
 * be loaded.
 *
 * @param payload A pointer to the pmix_byte_object_t .containing the
 * desired payload
 *
 * @retval PMIX_SUCCESS The request was successfully completed
 *
 * @retval PMIX_ERROR(s) An appropriate error code indicating the
 * problem will be returned. This should be handled appropriately by
 * the caller.
 *
 * @code
 * pmix_data_buffer_t *buffer;
 * pmix_byte_object_t payload;
 *
 * PMIX_DATA_BUFFER_CREATE(buffer);
 * status_code = PMIx_Data_load(buffer, &payload);
 * @endcode
 */
PMIX_EXPORT pmix_status_t PMIx_Data_load(pmix_data_buffer_t *buffer,
                                         pmix_byte_object_t *payload);

/**
* Embed a data payload into a buffer.
*
* The embed function is identical in operation to PMIx_Data_load
* except that it does NOT "clear" the payload upon completion.
*
* @note The buffer must be allocated in advance - failing to do so
* will cause the function to return an error code.
*
* @note The caller is responsible for pre-packing the provided
* payload - the load function cannot convert to network byte order
* any data contained in the provided payload.
*
* @note The "payload" object is unaltered by this operation.
*
* @param buffer A pointer to the buffer into which the payload is to
* be loaded.
*
* @param payload A pointer to the pmix_byte_object_t .containing the
* desired payload
*
* @retval PMIX_SUCCESS The request was successfully completed
*
* @retval PMIX_ERROR(s) An appropriate error code indicating the
* problem will be returned. This should be handled appropriately by
* the caller.
*
* @code
* pmix_data_buffer_t *buffer;
* pmix_byte_object_t payload;
*
* PMIX_DATA_BUFFER_CREATE(buffer);
* status_code = PMIx_Data_embed(buffer, &payload);
* @endcode
*/
PMIX_EXPORT pmix_status_t PMIx_Data_embed(pmix_data_buffer_t *buffer,
                                          const pmix_byte_object_t *payload);

/**
* Compress data using loss-less compression algorithm.
*
* Compress the provided data block. Destination memory
* will be allocated if successful operation is concluded. Caller
* is responsible for release of the allocated region. The input
* data block will remain unaltered.
*
* Note: the compress function will return "false" if the operation
* would not result in a smaller data block.
*
* @param inbytes A pointer to the data to be compressed
*
* @param size Number of bytes in the input data region
*
* @param outbytes Address where a pointer to the compressed
* data region is to be returned
*
* @param nbytes Address where the number of bytes in the
* compressed data region is to be returned
*
* @retval true The input data was compressed.
*
* @retval false The input data was not compressed
*
*/
PMIX_EXPORT bool PMIx_Data_compress(const uint8_t *inbytes,
                                    size_t size,
                                    uint8_t **outbytes,
                                    size_t *nbytes);

/**
* Decompress data.
*
* Decompress the provided data block. Destination memory
* will be allocated if successful operation is concluded. Caller
* is responsible for release of the allocated region. The input
* data block will remain unaltered.
*
* note: only data compressed using PMIx_Data_compress can
* be input to this function
*
* @param inbytes A pointer to the data to be decompressed
*
* @param size Number of bytes in the input data region
*
* @param outbytes Address where a pointer to the decompressed
* data region is to be returned
*
* @param nbytes Address where the number of bytes in the
* decompressed data region is to be returned
*
* @retval true The input data was decompressed
*
* @retval false The input data was not decompressed
*
*/
PMIX_EXPORT bool PMIx_Data_decompress(const uint8_t *inbytes,
                                      size_t size,
                                      uint8_t **outbytes,
                                      size_t *nbytes);


/* We had to put some function definitions into pmix_deprecated.h for
 * now-deprecated macros that utilize them as there are people who only
 * included pmix_common.h if they were using macros but not APIs.
 * However, we really want those APIs here so people will
 * see them and know they exist. So include them here as well. */

#ifndef PMIx_DEPRECATED_H

/* load a key */
PMIX_EXPORT void PMIx_Load_key(pmix_key_t key, const char *src);

/* check a key */
PMIX_EXPORT bool PMIx_Check_key(const char *key, const char *str);

/* check to see if a key is a "reserved" key */
PMIX_EXPORT bool PMIx_Check_reserved_key(const char *key);

/* load a string into a pmix_nspace_t struct */
PMIX_EXPORT void PMIx_Load_nspace(pmix_nspace_t nspace, const char *str);

/* check two nspace structs for equality */
PMIX_EXPORT bool PMIx_Check_nspace(const char *key1, const char *key2);

/* check if a namespace is invalid */
PMIX_EXPORT bool PMIx_Nspace_invalid(const char *nspace);

/* load a process ID struct */
PMIX_EXPORT void PMIx_Load_procid(pmix_proc_t *p, 
                                  const char *ns,
                                  pmix_rank_t rk);

/* transfer a process ID struct (non-destructive) */
PMIX_EXPORT void PMIx_Xfer_procid(pmix_proc_t *dst,
                                  const pmix_proc_t *src);

/* check two procIDs for equality */
PMIX_EXPORT bool PMIx_Check_procid(const pmix_proc_t *a,
                                   const pmix_proc_t *b);

/* check two ranks for equality */
PMIX_EXPORT bool PMIx_Check_rank(pmix_rank_t a,
                                 pmix_rank_t b);

/* check if procID is invalid */
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

/* initialize a value struct */
PMIX_EXPORT void PMIx_Value_construct(pmix_value_t *val);

/* free memory stored inside a value struct */
PMIX_EXPORT void PMIx_Value_destruct(pmix_value_t *val);

/* create and initialize an array of value structs */
PMIX_EXPORT pmix_value_t* PMIx_Value_create(size_t n);

/* free memory stored inside an array of coord structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Value_free(pmix_value_t *v, size_t n);

/* Check the given value struct to determine if it includes
 * a boolean value (includes strings for "true" and "false",
 * including abbreviations such as "t" or "f"), and if so,
 * then its value. A value type of PMIX_UNDEF is taken to imply
 * a boolean "true". */
PMIX_EXPORT pmix_boolean_t PMIx_Value_true(const pmix_value_t *v);

/* Load data into a pmix_value_t structure. The data can be of any
 * PMIx data type - which means the load can be somewhat complex
 * to implement (e.g., in the case of a pmix_data_array_t). The
 * data is COPIED into the value struct
 */
PMIX_EXPORT pmix_status_t PMIx_Value_load(pmix_value_t *val,
                                          const void *data,
                                          pmix_data_type_t type);

/* Unload data from a pmix_value_t structure. */
PMIX_EXPORT pmix_status_t PMIx_Value_unload(pmix_value_t *val,
                                            void **data,
                                            size_t *sz);

/* Transfer data from one pmix_value_t to another - this is actually
 * executed as a COPY operation, so the original data is not altered.
 */
PMIX_EXPORT pmix_status_t PMIx_Value_xfer(pmix_value_t *dest,
                                          const pmix_value_t *src);

/* Compare the contents of two pmix_value_t structures */
PMIX_EXPORT pmix_value_cmp_t PMIx_Value_compare(pmix_value_t *v1,
                                                pmix_value_t *v2);



PMIX_EXPORT void PMIx_Data_array_init(pmix_data_array_t *p,
                                      pmix_data_type_t type);
PMIX_EXPORT void PMIx_Data_array_construct(pmix_data_array_t *p,
                                           size_t num, pmix_data_type_t type);
PMIX_EXPORT void PMIx_Data_array_destruct(pmix_data_array_t *d);
PMIX_EXPORT pmix_data_array_t* PMIx_Data_array_create(size_t n, pmix_data_type_t type);
PMIX_EXPORT void PMIx_Data_array_free(pmix_data_array_t *p);


/* initialize an info struct */
PMIX_EXPORT void PMIx_Info_construct(pmix_info_t *p);

/* free memory stored inside an info struct */
PMIX_EXPORT void PMIx_Info_destruct(pmix_info_t *p);

/* create and initialize an array of info structs */
PMIX_EXPORT pmix_info_t* PMIx_Info_create(size_t n);

/* free memory stored inside an array of coord structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Info_free(pmix_info_t *p, size_t n);

/* Check the given info struct to determine if it includes
 * a boolean value (includes strings for "true" and "false",
 * including abbreviations such as "t" or "f"), and if so,
 * then its value. A value type of PMIX_UNDEF is taken to imply
 * a boolean "true" as the presence of the key defaults to
 * indicating "true". */
PMIX_EXPORT pmix_boolean_t PMIx_Info_true(const pmix_info_t *p);

/* Load key/value data into a pmix_info_t struct. Note that this
 * effectively is a PMIX_LOAD_KEY operation to copy the key,
 * followed by a PMIx_Value_load to COPY the data into the
 * pmix_value_t in the provided info struct */
PMIX_EXPORT pmix_status_t PMIx_Info_load(pmix_info_t *info,
                                         const char *key,
                                         const void *data,
                                         pmix_data_type_t type);

/* Transfer data from one pmix_info_t to another - this is actually
 * executed as a COPY operation, so the original data is not altered */
PMIX_EXPORT pmix_status_t PMIx_Info_xfer(pmix_info_t *dest,
                                         const pmix_info_t *src);

/* mark the info struct as required */
PMIX_EXPORT void PMIx_Info_required(pmix_info_t *p);

/* mark the info struct as optional */
PMIX_EXPORT void PMIx_Info_optional(pmix_info_t *p);

/* check if the info struct is required */
PMIX_EXPORT bool PMIx_Info_is_required(const pmix_info_t *p);

/* check if the info struct is optional */
PMIX_EXPORT bool PMIx_Info_is_optional(const pmix_info_t *p);

/* mark the info struct as processed */
PMIX_EXPORT void PMIx_Info_processed(pmix_info_t *p);

/* check if the info struct has been processed */
PMIX_EXPORT bool PMIx_Info_was_processed(const pmix_info_t *p);

/* mark the info struct as the end of an array */
PMIX_EXPORT void PMIx_Info_set_end(pmix_info_t *p);

/* check if the info struct is the end of an array */
PMIX_EXPORT bool PMIx_Info_is_end(const pmix_info_t *p);

/* mark the info as a qualifier */
PMIX_EXPORT void PMIx_Info_qualifier(pmix_info_t *p);

/* check if the info struct is a qualifier */
PMIX_EXPORT bool PMIx_Info_is_qualifier(const pmix_info_t *p);

/* mark the info struct as persistent - do NOT release its contents */
PMIX_EXPORT void PMIx_Info_persistent(pmix_info_t *p);

/* check if the info struct is persistent */
PMIX_EXPORT bool PMIx_Info_is_persistent(const pmix_info_t *p);


/* initialize a coord struct */
PMIX_EXPORT void PMIx_Coord_construct(pmix_coord_t *m);

/* free memory stored inside a coord struct */
PMIX_EXPORT void PMIx_Coord_destruct(pmix_coord_t *m);

/* create and initialize an array of coord structs */
PMIX_EXPORT pmix_coord_t* PMIx_Coord_create(size_t dims,
                                            size_t number);

/* free memory stored inside an array of coord structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Coord_free(pmix_coord_t *m, size_t number);


/* initialize a topology struct */
PMIX_EXPORT void PMIx_Topology_construct(pmix_topology_t *t);

/* free memory stored inside a topology struct */
PMIX_EXPORT void PMIx_Topology_destruct(pmix_topology_t *topo);

/* create and initialize an array of topology structs */
PMIX_EXPORT pmix_topology_t* PMIx_Topology_create(size_t n);

/* free memory stored inside an array of topology structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Topology_free(pmix_topology_t *t, size_t n);

/* initialize a cpuset struct */
PMIX_EXPORT void PMIx_Cpuset_construct(pmix_cpuset_t *cpuset);

/* free memory stored inside a cpuset struct */
PMIX_EXPORT void PMIx_Cpuset_destruct(pmix_cpuset_t *cpuset);

/* create and initialize an array of cpuset structs */
PMIX_EXPORT pmix_cpuset_t* PMIx_Cpuset_create(size_t n);

/* free memory stored inside an array of cpuset structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Cpuset_free(pmix_cpuset_t *c, size_t n);

/* initialize a geometry struct */
PMIX_EXPORT void PMIx_Geometry_construct(pmix_geometry_t *g);

/* free memory stored inside a cpuset struct */
PMIX_EXPORT void PMIx_Geometry_destruct(pmix_geometry_t *g);

/* create and initialize an array of cpuset structs */
PMIX_EXPORT pmix_geometry_t* PMIx_Geometry_create(size_t n);

/* free memory stored inside an array of cpuset structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Geometry_free(pmix_geometry_t *g, size_t n);

/* initialize a device distance struct */
PMIX_EXPORT void PMIx_Device_distance_construct(pmix_device_distance_t *d);

/* free memory stored inside a device distance struct */
PMIX_EXPORT void PMIx_Device_distance_destruct(pmix_device_distance_t *d);

/* create and initialize an array of device distance structs */
PMIX_EXPORT pmix_device_distance_t* PMIx_Device_distance_create(size_t n);

/* free memory stored inside an array of device distance structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Device_distance_free(pmix_device_distance_t *d, size_t n);


/* initialize a byte object struct */
PMIX_EXPORT void PMIx_Byte_object_construct(pmix_byte_object_t *b);

/* free memory stored inside a byte object struct */
PMIX_EXPORT void PMIx_Byte_object_destruct(pmix_byte_object_t *g);

/* create and initialize an array of byte object structs */
PMIX_EXPORT pmix_byte_object_t* PMIx_Byte_object_create(size_t n);

/* free memory stored inside an array of byte object structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Byte_object_free(pmix_byte_object_t *g, size_t n);

/* load a byte object */
PMIX_EXPORT void PMIx_Byte_object_load(pmix_byte_object_t *b,
                                       char *d, size_t sz);

/* initialize an endpoint struct */
PMIX_EXPORT void PMIx_Endpoint_construct(pmix_endpoint_t *e);

/* free memory stored inside an endpoint struct */
PMIX_EXPORT void PMIx_Endpoint_destruct(pmix_endpoint_t *e);

/* create and initialize an array of endpoint structs */
PMIX_EXPORT pmix_endpoint_t* PMIx_Endpoint_create(size_t n);

/* free memory stored inside an array of endpoint structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Endpoint_free(pmix_endpoint_t *e, size_t n);


/* initialize an envar struct */
PMIX_EXPORT void PMIx_Envar_construct(pmix_envar_t *e);

/* free memory stored inside an envar struct */
PMIX_EXPORT void PMIx_Envar_destruct(pmix_envar_t *e);

/* create and initialize an array of envar structs */
PMIX_EXPORT pmix_envar_t* PMIx_Envar_create(size_t n);

/* free memory stored inside an array of envar structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Envar_free(pmix_envar_t *e, size_t n);

/* load an envar struct */
PMIX_EXPORT void PMIx_Envar_load(pmix_envar_t *e,
                                 char *var,
                                 char *value,
                                 char separator);

/* initialize a data buffer struct */
PMIX_EXPORT void PMIx_Data_buffer_construct(pmix_data_buffer_t *b);

/* free memory stored inside a data buffer struct */
PMIX_EXPORT void PMIx_Data_buffer_destruct(pmix_data_buffer_t *b);

/* create a data buffer struct */
PMIX_EXPORT pmix_data_buffer_t* PMIx_Data_buffer_create(void);

/* free memory stored inside a data buffer struct */
PMIX_EXPORT void PMIx_Data_buffer_release(pmix_data_buffer_t *b);

/* load a data buffer struct */
PMIX_EXPORT void PMIx_Data_buffer_load(pmix_data_buffer_t *b,
                                       char *bytes, size_t sz);

/* unload a data buffer struct */
PMIX_EXPORT void PMIx_Data_buffer_unload(pmix_data_buffer_t *b,
                                         char **bytes, size_t *sz);


/* initialize a proc struct */
PMIX_EXPORT void PMIx_Proc_construct(pmix_proc_t *p);

/* clear memory inside a proc struct */
PMIX_EXPORT void PMIx_Proc_destruct(pmix_proc_t *p);

/* create and initialize an array of proc structs */
PMIX_EXPORT pmix_proc_t* PMIx_Proc_create(size_t n);

/* free memory stored inside an array of proc structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Proc_free(pmix_proc_t *p, size_t n);

/* load a proc struct */
PMIX_EXPORT void PMIx_Proc_load(pmix_proc_t *p,
                                char *nspace, pmix_rank_t rank);

/* construct a multicluster nspace struct from cluster and nspace values */
PMIX_EXPORT void PMIx_Multicluster_nspace_construct(pmix_nspace_t target,
                                                    pmix_nspace_t cluster,
                                                    pmix_nspace_t nspace);

/* parse a multicluster nspace struct to separate out the cluster
 * and nspace portions */
PMIX_EXPORT void PMIx_Multicluster_nspace_parse(pmix_nspace_t target,
                                                pmix_nspace_t cluster,
                                                pmix_nspace_t nspace);


/* initialize a proc info struct */
PMIX_EXPORT void PMIx_Proc_info_construct(pmix_proc_info_t *p);

/* clear memory inside a proc info struct */
PMIX_EXPORT void PMIx_Proc_info_destruct(pmix_proc_info_t *p);

/* create and initialize an array of proc info structs */
PMIX_EXPORT pmix_proc_info_t* PMIx_Proc_info_create(size_t n);

/* free memory stored inside an array of proc info structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Proc_info_free(pmix_proc_info_t *p, size_t n);


/* initialize a proc stats struct */
PMIX_EXPORT void PMIx_Proc_stats_construct(pmix_proc_stats_t *p);

/* clear memory inside a proc stats struct */
PMIX_EXPORT void PMIx_Proc_stats_destruct(pmix_proc_stats_t *p);

/* create and initialize an array of proc stats structs */
PMIX_EXPORT pmix_proc_stats_t* PMIx_Proc_stats_create(size_t n);

/* free memory stored inside an array of proc stats structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Proc_stats_free(pmix_proc_stats_t *p, size_t n);


/* initialize a disk stats struct */
PMIX_EXPORT void PMIx_Disk_stats_construct(pmix_disk_stats_t *p);

/* clear memory inside a disk stats struct */
PMIX_EXPORT void PMIx_Disk_stats_destruct(pmix_disk_stats_t *p);

/* create and initialize an array of disk stats structs */
PMIX_EXPORT pmix_disk_stats_t* PMIx_Disk_stats_create(size_t n);

/* free memory stored inside an array of disk stats structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Disk_stats_free(pmix_disk_stats_t *p, size_t n);


/* initialize a net stats struct */
PMIX_EXPORT void PMIx_Net_stats_construct(pmix_net_stats_t *p);

/* clear memory inside a net stats struct */
PMIX_EXPORT void PMIx_Net_stats_destruct(pmix_net_stats_t *p);

/* create and initialize an array of net stats structs */
PMIX_EXPORT pmix_net_stats_t* PMIx_Net_stats_create(size_t n);

/* free memory stored inside an array of net stats structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Net_stats_free(pmix_net_stats_t *p, size_t n);


/* initialize a pdata struct */
PMIX_EXPORT void PMIx_Pdata_construct(pmix_pdata_t *p);

/* clear memory inside a pdata struct */
PMIX_EXPORT void PMIx_Pdata_destruct(pmix_pdata_t *p);

/* create and initialize an array of pdata structs */
PMIX_EXPORT pmix_pdata_t* PMIx_Pdata_create(size_t n);

/* free memory stored inside an array of pdata structs (does
 * not free the struct memory itself */
PMIX_EXPORT void PMIx_Pdata_free(pmix_pdata_t *p, size_t n);


PMIX_EXPORT void PMIx_App_construct(pmix_app_t *p);
PMIX_EXPORT void PMIx_App_destruct(pmix_app_t *p);
PMIX_EXPORT pmix_app_t* PMIx_App_create(size_t n);
PMIX_EXPORT void PMIx_App_info_create(pmix_app_t *p, size_t n);
PMIX_EXPORT void PMIx_App_free(pmix_app_t *p, size_t n);
PMIX_EXPORT void PMIx_App_release(pmix_app_t *p);

/* Constructing arrays of pmix_info_t for passing to an API can
 * be tedious since the pmix_info_t itself is not a "list object".
 * Since this is a very frequent operation, a set of APIs has been
 * provided that opaquely manipulates internal PMIx list structures
 * for this purpose. The user only need provide a void* pointer to
 * act as the caddy for the internal list object.
 */

/* Initialize a list of pmix_info_t structures */
PMIX_EXPORT void* PMIx_Info_list_start(void);

/* Add data to a list of pmix_info_t structs. The "ptr" passed
 * here is the pointer returned by PMIx_Info_list_start.
 */
PMIX_EXPORT pmix_status_t PMIx_Info_list_add(void *ptr,
                                             const char *key,
                                             const void *value,
                                             pmix_data_type_t type);

PMIX_EXPORT pmix_status_t PMIx_Info_list_prepend(void *ptr,
                                                 const char *key,
                                                 const void *value,
                                                 pmix_data_type_t type);

PMIX_EXPORT pmix_status_t PMIx_Info_list_insert(void *ptr, pmix_info_t *info);

/* Transfer the data in an existing pmix_info_t struct to a list. This
 * is executed as a COPY operation, so the original data is not altered.
 * The "ptr" passed here is the pointer returned by PMIx_Info_list_start
 */
PMIX_EXPORT pmix_status_t PMIx_Info_list_xfer(void *ptr,
                                              const pmix_info_t *info);

/* Convert the constructed list of pmix_info_t structs to a pmix_data_array_t
 * of pmix_info_t. Data on the list is COPIED to the array elements.
 */
PMIX_EXPORT pmix_status_t PMIx_Info_list_convert(void *ptr, pmix_data_array_t *par);

/* Release all data on the list and destruct all internal tracking */
PMIX_EXPORT void PMIx_Info_list_release(void *ptr);

/* retrieve the next info on the list - passing a NULL
 * to the "prev" parameter will return the first pmix_info_t
 * on the list. A return of NULL indicates the end of the list
 */
PMIX_EXPORT pmix_info_t* PMIx_Info_list_get_info(void *ptr, void *prev, void **next);

#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

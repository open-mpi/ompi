/*
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
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

#ifndef PMIx_H
#define PMIx_H

#include <pmix/autogen/config.h>

#include <stdint.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h> /* for struct timeval */
#endif

/* Symbol transforms */
#include <pmix/rename.h>

/* Structure and constant definitions */
#include <pmix/pmix_common.h>


BEGIN_C_DECLS

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
 * rank of the process is to simply call PMIx_Init with a non-NULL parameter. */
pmix_status_t PMIx_Init(pmix_proc_t *proc);

/* Finalize the PMIx client, closing the connection to the local server.
 * An error code will be returned if, for some reason, the connection
 * cannot be closed. */
pmix_status_t PMIx_Finalize(void);

/* Returns _true_ if the PMIx client has been successfully initialized,
 * returns _false_ otherwise. Note that the function only reports the
 * internal state of the PMIx client - it does not verify an active
 * connection with the server, nor that the server is functional. */
int PMIx_Initialized(void);


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
pmix_status_t PMIx_Abort(int status, const char msg[],
                         pmix_proc_t procs[], size_t nprocs);


/* Push a value into the client's namespace. The client library will cache
 * the information locally until _PMIx_Commit_ is called. The provided scope
 * value is passed to the local PMIx server, which will distribute the data
 * as directed. */
pmix_status_t PMIx_Put(pmix_scope_t scope, const char key[], pmix_value_t *val);


/* Push all previously _PMIx_Put_ values to the local PMIx server.
 * This is an asynchronous operation - the library will immediately
 * return to the caller while the data is transmitted to the local
 * server in the background */
pmix_status_t PMIx_Commit(void);


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
pmix_status_t PMIx_Fence(const pmix_proc_t procs[], size_t nprocs,
                         const pmix_info_t info[], size_t ninfo);

/* Non-blocking version of PMIx_Fence. Note that the function will return
 * an error if a _NULL_ callback function is given. */
pmix_status_t PMIx_Fence_nb(const pmix_proc_t procs[], size_t nprocs,
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
pmix_status_t PMIx_Get(const pmix_proc_t *proc, const char key[],
                       const pmix_info_t info[], size_t ninfo,
                       pmix_value_t **val);

/* A non-blocking operation version of PMIx_Get - the callback function will
 * be executed once the specified data has been _PMIx_Put_
 * by the identified process and retrieved by the local server. The info
 * array is used as described above for the blocking form of this call. */
pmix_status_t PMIx_Get_nb(const pmix_proc_t *proc, const char key[],
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
pmix_status_t PMIx_Publish(const pmix_info_t info[], size_t ninfo);
pmix_status_t PMIx_Publish_nb(const pmix_info_t info[], size_t ninfo,
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
pmix_status_t PMIx_Lookup(pmix_pdata_t data[], size_t ndata,
                          const pmix_info_t info[], size_t ninfo);

/* Non-blocking form of the _PMIx_Lookup_ function. Data for
 * the provided NULL-terminated keys array will be returned
 * in the provided callback function. As above, the default
 * behavior is to _not_ wait for data to be published. The
 * info keys can be used to modify the behavior as previously
 * described */
pmix_status_t PMIx_Lookup_nb(char **keys, const pmix_info_t info[], size_t ninfo,
                             pmix_lookup_cbfunc_t cbfunc, void *cbdata);


/* Unpublish data posted by this process using the given keys.
 * The function will block until the data has been removed by
 * the server. A value of _NULL_ for the keys parameter instructs
 * the server to remove _all_ data published by this process.
 *
 * By default, the range is assumed to be PMIX_SESSION. Changes
 * to the range, and any additional directives, can be provided
 * in the pmix_info_t array */
pmix_status_t PMIx_Unpublish(char **keys,
                             const pmix_info_t info[], size_t ninfo);

/* Non-blocking form of the _PMIx_Unpublish_ function. The
 * callback function will be executed once the server confirms
 * removal of the specified data. */
pmix_status_t PMIx_Unpublish_nb(char **keys,
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
pmix_status_t PMIx_Spawn(const pmix_info_t job_info[], size_t ninfo,
                         const pmix_app_t apps[], size_t napps,
                         char nspace[]);


/* Non-blocking form of the _PMIx_Spawn_ function. The callback
 * will be executed upon launch of the specified applications,
 * or upon failure to launch any of them. */
pmix_status_t PMIx_Spawn_nb(const pmix_info_t job_info[], size_t ninfo,
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
pmix_status_t PMIx_Connect(const pmix_proc_t procs[], size_t nprocs,
                           const pmix_info_t info[], size_t ninfo);

pmix_status_t PMIx_Connect_nb(const pmix_proc_t procs[], size_t nprocs,
                              const pmix_info_t info[], size_t ninfo,
                              pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Disconnect a previously connected set of processes. An error will be returned
 * if the specified set of procs was not previously "connected". As above, a process
 * may be involved in multiple simultaneous disconnect operations. However, a process
 * is not allowed to reconnect to a set of procs that has not fully completed
 * disconnect - i.e., you have to fully disconnect before you can reconnect to the
 * _same_ group of processes. The info array is used as above. */
pmix_status_t PMIx_Disconnect(const pmix_proc_t procs[], size_t nprocs,
                              const pmix_info_t info[], size_t ninfo);

pmix_status_t PMIx_Disconnect_nb(const pmix_proc_t ranges[], size_t nprocs,
                                 const pmix_info_t info[], size_t ninfo,
                                 pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Given a node name, return an array of processes within the specified nspace
 * on that node. If the nspace is NULL, then all processes on the node will
 * be returned. If the specified node does not currently host any processes,
 * then the returned array will be NULL, and nprocs=0. The caller is responsible
 * for releasing the array when done with it - the PMIX_PROC_FREE macro is
 * provided for this purpose.
 */
pmix_status_t PMIx_Resolve_peers(const char *nodename, const char *nspace,
                                 pmix_proc_t **procs, size_t *nprocs);


/* Given an nspace, return the list of nodes hosting processes within
 * that nspace. The returned string will contain a comma-delimited list
 * of nodenames. The caller is responsible for releasing the string
 * when done with it */
pmix_status_t PMIx_Resolve_nodes(const char *nspace, char **nodelist);

END_C_DECLS
#endif

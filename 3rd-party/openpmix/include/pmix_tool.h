/*
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
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

#ifndef PMIx_TOOL_API_H
#define PMIx_TOOL_API_H

/* provide access to the rest of the client functions */
#include <pmix.h>
#include <pmix_server.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/****    TOOL INIT/FINALIZE FUNCTIONS    ****/

/* Initialize the PMIx tool, returning the process identifier assigned
 * to this tool in the provided pmix_proc_t struct.
 *
 * When called the PMIx tool library will check for the required connection
 * information of the local PMIx server and will establish the connection.
 * If the information is not found, or the server connection fails, then
 * an appropriate error constant will be returned.
 *
 * If successful, the function will return PMIX_SUCCESS and will fill the
 * provided structure with the server-assigned namespace and rank of the tool.
 *
 * Note that the PMIx tool library is referenced counted, and so multiple
 * calls to PMIx_tool_init are allowed. Thus, one way to obtain the namespace and
 * rank of the process is to simply call PMIx_tool_init with a non-NULL parameter.
 *
 * The info array is used to pass user requests pertaining to the init
 * and subsequent operations. Passing a _NULL_ value for the array pointer
 * is supported if no directives are desired.
 */
PMIX_EXPORT pmix_status_t PMIx_tool_init(pmix_proc_t *proc,
                                         pmix_info_t info[], size_t ninfo);

/* Provide an entry point by which a tool can provide a server function
 * pointer module.
 */
PMIX_EXPORT pmix_status_t PMIx_tool_set_server_module(pmix_server_module_t *module);

/* Finalize the PMIx tool library, closing the connection to the local server.
 * An error code will be returned if, for some reason, the connection
 * cannot be closed.
 *
 * The info array is used to pass user requests regarding the finalize
 * operation. */
PMIX_EXPORT pmix_status_t PMIx_tool_finalize(void);

/* Check if the tool is connected to a PMIx server */
PMIX_EXPORT bool PMIx_tool_is_connected(void);

/* Establish a connection to a PMIx server. The target server can
 * be given as:
 *
 * - PMIX_CONNECT_TO_SYSTEM: connect solely to the system server
 *
 * - PMIX_CONNECT_SYSTEM_FIRST: a request to use the system server first,
 *   if existing, and then look for the server specified in a different
 *   attribute
 *
 * - PMIX_SERVER_URI: connect to the server at the given URI
 *
 * - PMIX_SERVER_NSPACE: connect to the server of a given nspace
 *
 * - PMIX_SERVER_PIDINFO: connect to a server embedded in the process with
 *   the given pid
 *
 * Passing a _NULL_ value for the info array pointer is not allowed and will
 * result in return of an error.
 *
 * NOTE: PMIx does not currently support on-the-fly changes to the tool's
 * identifier. Thus, the new server must be under the same nspace manager
 * (e.g., host RM) as the prior server so that the original nspace remains
 * a unique assignment. The proc parameter is included here for obsolence
 * protection in case this constraint is someday removed. Meantime, the
 * proc parameter will be filled with the tool's existing nspace/rank, and
 * the caller is welcome to pass _NULL_ in that location
 */
/* REPLACES CONNECT_TO_SERVER, ADDING ABILITY TO RETURN
 * IDENTIFIER OF SERVER TO WHICH TOOL ATTACHED
 */
PMIX_EXPORT pmix_status_t PMIx_tool_attach_to_server(pmix_proc_t *myproc, pmix_proc_t *server,
                                                     pmix_info_t info[], size_t ninfo);


/* Disconnect the PMIx tool from the specified server connection while
 * leaving the tool library initialized.
 *
 * server - Process identifier of the server from which the tool is
 *          to be disconnected
 *
 * Returns PMIX_SUCCESS or a PMIx error constant
 */
PMIX_EXPORT pmix_status_t PMIx_tool_disconnect(const pmix_proc_t *server);


/* Get an array containing the pmix_proc_t process identifiers of all
 * servers to which the tool is currently connected.
 *
 * servers - Address where the pointer to an array of pmix_proc_t
 *           structures shall be returned
 *
 * nservers - Address where the number of elements in servers
 *            shall be returned
 *
 * Returns PMIX_SUCCESS or a PMIx error constant
 */
PMIX_EXPORT pmix_status_t PMIx_tool_get_servers(pmix_proc_t *servers[], size_t *nservers);


/* Designate a server as the tool’s primary server.
 *
 * server - Process identifier of the target server
 *
 * Returns PMIX_SUCCESS or a PMIx error constant
 */
PMIX_EXPORT pmix_status_t PMIx_tool_set_server(const pmix_proc_t *server,
                                               pmix_info_t info[], size_t ninfo);


/* Define a callback function for delivering forwarded IO to a process
 * This function will be called whenever data becomes available, or a
 * specified buffering size and/or time has been met. The function
 * will be passed the following values:
 *
 * iofhdlr - the returned registration number of the handler being invoked.
 *           This is required when deregistering the handler.
 *
 * channel - a bitmask identifying the channel the data arrived on
 *
 * source - the nspace/rank of the process that generated the data
 *
 * payload - pointer to a PMIx byte object containing the data. Note that
 *           multiple strings may be included, and that the data may
 *           _not_ be NULL terminated
 *
 * info - an optional array of info provided by the source containing
 *        metadata about the payload. This could include PMIX_IOF_COMPLETE
 *
 * ninfo - number of elements in the optional info array
 */
 typedef void (*pmix_iof_cbfunc_t)(size_t iofhdlr, pmix_iof_channel_t channel,
                                   pmix_proc_t *source, pmix_byte_object_t *payload,
                                   pmix_info_t info[], size_t ninfo);


/* Register to receive output forwarded from a remote process.
 *
 * procs - array of identifiers for sources whose IO is being
 *         requested. Wildcard rank indicates that all procs
 *         in the specified nspace are included in the request
 *
 * nprocs - number of identifiers in the procs array
 *
 * directives - optional array of attributes to control the
 *              behavior of the request. For example, this
 *              might include directives on buffering IO
 *              before delivery, and/or directives to include
 *              or exclude any backlogged data
 *
 * ndirs - number of elements in the directives array
 *
 * channel - bitmask of IO channels included in the request.
 *           NOTE: STDIN is not supported as it will always
 *           be delivered to the stdin file descriptor
 *
 * cbfunc - function to be called when relevant IO is received. A
 *          NULL indicates that the IO is to be written to stdout
 *          or stderr as per the originating channel
 *
 * regcbfunc - since registration is async, this is the
 *             function to be called when registration is
 *             completed. The function itself will return
 *             a non-success error if the registration cannot
 *             be submitted - in this case, the regcbfunc
 *             will _not_ be called.
 *             If regcbfunc is NULL, then this will be treated
 *             as a BLOCKING call - a positive return value
 *             represents the reference ID for the request,
 *             while negative values indicate the corresponding
 *             error
 *
 * cbdata - pointer to object to be returned in regcbfunc
 */
PMIX_EXPORT pmix_status_t PMIx_IOF_pull(const pmix_proc_t procs[], size_t nprocs,
                                        const pmix_info_t directives[], size_t ndirs,
                                        pmix_iof_channel_t channel, pmix_iof_cbfunc_t cbfunc,
                                        pmix_hdlr_reg_cbfunc_t regcbfunc, void *regcbdata);

/* Deregister from output forwarded from a remote process.
 *
 * iofhdlr - the registration number returned from the
 *           call to PMIx_IOF_pull
 *
 * directives - optional array of attributes to control the
 *              behavior of the request. For example, this
 *              might include directives regarding what to
 *              do with any data currently in the IO buffer
 *              for this process
 *
 * cbfunc - function to be called when deregistration has
 *          been completed. Note that any IO to be flushed
 *          may continue to be received after deregistration
 *          has completed. If cbfunc is NULL, then this is
 *          treated as a BLOCKING call and the result of
 *          the operation will be provided in the returned status
 *
 * cbdata - pointer to object to be returned in cbfunc
 */
PMIX_EXPORT pmix_status_t PMIx_IOF_deregister(size_t iofhdlr,
                                              const pmix_info_t directives[], size_t ndirs,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata);

/* Push data collected locally (typically from stdin) to
 * stdin of target recipients.
 *
 * targets - array of process identifiers to which the data is to be delivered. Note
 *           that a WILDCARD rank indicates that all procs in the given nspace are
 *           to receive a copy of the data
 *
 * ntargets - number of procs in the targets array
 *
 * directives - optional array of attributes to control the
 *              behavior of the request. For example, this
 *              might include directives on buffering IO
 *              before delivery, and/or directives to include
 *              or exclude any backlogged data
 *
 * ndirs - number of elements in the directives array
 *
 * bo - pointer to a byte object containing the stdin data
 *
 * cbfunc - callback function when the data has been forwarded. If
 *          cbfunc is NULL, then this is treated as a BLOCKING call
 *          and the result of the operation will be provided in the
 *          returned status
 *
 * cbdata - object to be returned in cbfunc
 */
PMIX_EXPORT pmix_status_t PMIx_IOF_push(const pmix_proc_t targets[], size_t ntargets,
                                        pmix_byte_object_t *bo,
                                        const pmix_info_t directives[], size_t ndirs,
                                        pmix_op_cbfunc_t cbfunc, void *cbdata);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

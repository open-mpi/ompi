/*
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

/* Finalize the PMIx tool library, closing the connection to the local server.
 * An error code will be returned if, for some reason, the connection
 * cannot be closed.
 *
 * The info array is used to pass user requests regarding the finalize
 * operation. */
PMIX_EXPORT pmix_status_t PMIx_tool_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

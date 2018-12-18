/*
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
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

#ifndef PMIx_EXTEND_H
#define PMIx_EXTEND_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* declare a convenience macro for checking keys */
#define PMIX_CHECK_KEY(a, b) \
    (0 == strncmp((a)->key, (b), PMIX_MAX_KEYLEN))

/* define a convenience macro for checking nspaces */
#define PMIX_CHECK_NSPACE(a, b) \
    (0 == strncmp((a), (b), PMIX_MAX_NSLEN))

/* define a convenience macro for checking names */
#define PMIX_CHECK_PROCID(a, b) \
    (PMIX_CHECK_NSPACE((a)->nspace, (b)->nspace) && ((a)->rank == (b)->rank || (PMIX_RANK_WILDCARD == (a)->rank || PMIX_RANK_WILDCARD == (b)->rank)))

/* expose some functions that are resolved in the
 * PMIx library, but part of a header that
 * includes internal functions - we don't
 * want to expose the entire header here. For
 * consistency, we provide macro versions as well
 */
void pmix_value_load(pmix_value_t *v, const void *data, pmix_data_type_t type);

pmix_status_t pmix_value_unload(pmix_value_t *kv, void **data, size_t *sz);

pmix_status_t pmix_value_xfer(pmix_value_t *kv, const pmix_value_t *src);

pmix_status_t pmix_argv_append_nosize(char ***argv, const char *arg);

pmix_status_t pmix_argv_prepend_nosize(char ***argv, const char *arg);

pmix_status_t pmix_argv_append_unique_nosize(char ***argv, const char *arg, bool overwrite);

void pmix_argv_free(char **argv);

char **pmix_argv_split(const char *src_string, int delimiter);

int pmix_argv_count(char **argv);

char *pmix_argv_join(char **argv, int delimiter);

char **pmix_argv_copy(char **argv);

pmix_status_t pmix_setenv(const char *name, const char *value,
                          bool overwrite, char ***env);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

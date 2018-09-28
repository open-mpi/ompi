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

/* define a convenience macro for loading nspaces */
#define PMIX_LOAD_NSPACE(a, b)                      \
    do {                                            \
        memset((a), 0, PMIX_MAX_NSLEN+1);           \
        (void)strncpy((a), (b), PMIX_MAX_NSLEN);    \
    }while(0)

/* define a convenience macro for checking nspaces */
#define PMIX_CHECK_NSPACE(a, b) \
    (0 == strncmp((a), (b), PMIX_MAX_NSLEN))

/* define a convenience macro for loading names */
#define PMIX_LOAD_PROCID(a, b, c)               \
    do {                                        \
        PMIX_LOAD_NSPACE((a)->nspace, (b));     \
        (a)->rank = (c);                        \
    }while(0)

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
#define PMIX_VALUE_LOAD(v, d, t) \
    pmix_value_load((v), (d), (t))

pmix_status_t pmix_value_unload(pmix_value_t *kv, void **data, size_t *sz);
#define PMIX_VALUE_UNLOAD(r, k, d, s)      \
    (r) = pmix_value_unload((k), (d), (s))

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


#define PMIX_INFO_LOAD(m, k, v, t)                          \
    do {                                                    \
        if (NULL != (k)) {                                  \
            (void)strncpy((m)->key, (k), PMIX_MAX_KEYLEN);  \
        }                                                   \
        (m)->flags = 0;                                     \
        pmix_value_load(&((m)->value), (v), (t));           \
    } while (0)
#define PMIX_INFO_XFER(d, s)                                        \
    do {                                                            \
        if (NULL != (s)->key) {                                     \
            (void)strncpy((d)->key, (s)->key, PMIX_MAX_KEYLEN);     \
        }                                                           \
        (d)->flags = (s)->flags;                                    \
        pmix_value_xfer(&(d)->value, (pmix_value_t*)&(s)->value);   \
    } while(0)


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

/* Append a string (by value) to an new or existing NULL-terminated
 * argv array.
 *
 * @param argv Pointer to an argv array.
 * @param str Pointer to the string to append.
 *
 * @retval PMIX_SUCCESS On success
 * @retval PMIX_ERROR On failure
 *
 * This function adds a string to an argv array of strings by value;
 * it is permissable to pass a string on the stack as the str
 * argument to this function.
 *
 * To add the first entry to an argv array, call this function with
 * (*argv == NULL).  This function will allocate an array of length
 * 2; the first entry will point to a copy of the string passed in
 * arg, the second entry will be set to NULL.
 *
 * If (*argv != NULL), it will be realloc'ed to be 1 (char*) larger,
 * and the next-to-last entry will point to a copy of the string
 * passed in arg.  The last entry will be set to NULL.
 *
 * Just to reinforce what was stated above: the string is copied by
 * value into the argv array; there is no need to keep the original
 * string (i.e., the arg parameter) after invoking this function.
 */
pmix_status_t pmix_argv_append_nosize(char ***argv, const char *arg);
#define PMIX_ARGV_APPEND(r, a, b) \
    (r) = pmix_argv_append_nosize(&(a), (b))

/* Prepend a string to a new or existing NULL-terminated
 * argv array - same as above only prepend
 */
pmix_status_t pmix_argv_prepend_nosize(char ***argv, const char *arg);
#define PMIX_ARGV_PREPEND(r, a, b) \
    (r) = pmix_argv_prepend_nosize(a, b)

/* Append to an argv-style array, but only if the provided argument
 * doesn't already exist somewhere in the array. Ignore the size of the array.
 *
 * @param argv Pointer to an argv array.
 * @param str Pointer to the string to append.
 * @param bool Whether or not to overwrite a matching value if found
 *
 * @retval PMIX_SUCCESS On success
 * @retval PMIX_ERROR On failure
 *
 * This function is identical to the pmix_argv_append_nosize() function
 * except that it only appends the provided argument if it does not already
 * exist in the provided array, or overwrites it if it is.
 */
pmix_status_t pmix_argv_append_unique_nosize(char ***argv, const char *arg, bool overwrite);
#define PMIX_ARGV_APPEND_UNIQUE(r, a, b, c) \
    (r) = pmix_argv_append_unique_nosize(a, b, c)

/*
 * Free a NULL-terminated argv array.
 *
 * @param argv Argv array to free.
 *
 * This function frees an argv array and all of the strings that it
 * contains.  Since the argv parameter is passed by value, it is not
 * set to NULL in the caller's scope upon return.
 *
 * It is safe to invoke this function with a NULL pointer.  It is
 * not safe to invoke this function with a non-NULL-terminated argv
 * array.
 */
void pmix_argv_free(char **argv);
#define PMIX_ARGV_FREE(a)  pmix_argv_free(a)

/*
 * Split a string into a NULL-terminated argv array. Do not include empty
 * strings in result array.
 *
 * @param src_string Input string.
 * @param delimiter Delimiter character.
 *
 * @retval argv pointer to new argv array on success
 * @retval NULL on error
 *
 * All strings are insertted into the argv array by value; the
 * newly-allocated array makes no references to the src_string
 * argument (i.e., it can be freed after calling this function
 * without invalidating the output argv).
 */
char **pmix_argv_split(const char *src_string, int delimiter);
#define PMIX_ARGV_SPLIT(a, b, c) \
    (a) = pmix_argv_split(b, c)

/*
 * Return the length of a NULL-terminated argv array.
 *
 * @param argv The input argv array.
 *
 * @retval 0 If NULL is passed as argv.
 * @retval count Number of entries in the argv array.
 *
 * The argv array must be NULL-terminated.
 */
int pmix_argv_count(char **argv);
#define PMIX_ARGV_COUNT(r, a) \
    (r) = pmix_argv_count(a)

/*
 * Join all the elements of an argv array into a single
 * newly-allocated string.
 *
 * @param argv The input argv array.
 * @param delimiter Delimiter character placed between each argv string.
 *
 * @retval new_string Output string on success.
 * @retval NULL On failure.
 *
 * Similar to the Perl join function, this function takes an input
 * argv and joins them into into a single string separated by the
 * delimiter character.
 *
 * It is the callers responsibility to free the returned string.
 */
char *pmix_argv_join(char **argv, int delimiter);
#define PMIX_ARGV_JOIN(a, b, c) \
    (a) = pmix_argv_join(b, c)

/*
 * Copy a NULL-terminated argv array.
 *
 * @param argv The input argv array.
 *
 * @retval argv Copied argv array on success.
 * @retval NULL On failure.
 *
 * Copy an argv array, including copying all off its strings.
 * Specifically, the output argv will be an array of the same length
 * as the input argv, and strcmp(argv_in[i], argv_out[i]) will be 0.
 */
char **pmix_argv_copy(char **argv);
#define PMIX_ARGV_COPY(a, b) \
    (a) = pmix_argv_copy(b)

/*
 * Set an environmenal paramter in an env array
 *
 * @retval r Return pmix_status_t status
 *
 * @param a Name of the environmental param
 *
 * @param b String value of the environmental param
 *
 * @param c Address of the NULL-terminated env array
 */
pmix_status_t pmix_setenv(const char *name, const char *value,
                          bool overwrite, char ***env);
#define PMIX_SETENV(r, a, b, c) \
    (r) = pmix_setenv((a), (b), true, (c))


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

/* -*- C -*-
 *
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PMIX_PTL_BASE_HANDSHAKE_H_
#define PMIX_PTL_BASE_HANDSHAKE_H_

#include "src/include/pmix_config.h"

#ifdef HAVE_STRING_H
#    include <string.h>
#endif

/* The following macros are for use in the PTL connection
 * handler. For simplicity, they include variables that are
 * defined in the functions where they are used - thus, these
 * macros are NOT portable for use elsewhere
 *
 * A symmetric pair of macros are provided specifically to
 * make it easier to read and compare the two ends of the
 * handshake */

BEGIN_C_DECLS

/* define flag values that indicate the type of process attempting
 * to connect to a server:
 *  0 => simple client process
 *  1 => legacy tool - may or may not have an identifier
 *  2 => legacy launcher - may or may not have an identifier
 * ------------------------------------------
 *  3 => self-started tool process that needs an identifier
 *  4 => self-started tool process that was given an identifier by caller
 *  5 => tool that was started by a PMIx server - identifier specified by server
 *  6 => self-started launcher that needs an identifier
 *  7 => self-started launcher that was given an identifier by caller
 *  8 => launcher that was started by a PMIx server - identifier specified by server
 *  9 => singleton client - treated like a tool that has an identifier
 * ------------------------------------------
 * 10 => scheduler
 */
typedef uint8_t pmix_rnd_flag_t;
#define PMIX_SIMPLE_CLIENT      0
#define PMIX_LEGACY_TOOL        1
#define PMIX_LEGACY_LAUNCHER    2
#define PMIX_TOOL_NEEDS_ID      3
#define PMIX_TOOL_GIVEN_ID      4
#define PMIX_TOOL_CLIENT        5
#define PMIX_LAUNCHER_NEEDS_ID  6
#define PMIX_LAUNCHER_GIVEN_ID  7
#define PMIX_LAUNCHER_CLIENT    8
#define PMIX_SINGLETON_CLIENT   9
#define PMIX_SCHEDULER_WITH_ID 10

/* The following macros are used in the ptl_base_connection_hdlr.c
 * file to parse the handshake message and extract its fields */

#define PMIX_PTL_GET_STRING(n)                  \
    do {                                        \
        size_t _l;                              \
        PMIX_STRNLEN(_l, mg, cnt);              \
        if (_l < cnt) {                         \
            (n) = strdup(mg);                   \
            mg += strlen((n)) + 1;              \
            cnt -= strlen((n)) + 1;             \
        } else {                                \
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM); \
            goto error;                         \
        }                                       \
    } while (0)

/* numerical value must be converted back to host
 * order, so we use an intermediate storage place
 * for that purpose */
#define PMIX_PTL_GET_U32_NOERROR(r, n)          \
    do {                                        \
        uint32_t _u;                            \
        if (sizeof(uint32_t) <= cnt) {          \
            memcpy(&_u, mg, sizeof(uint32_t));  \
            (n) = ntohl(_u);                    \
            mg += sizeof(uint32_t);             \
            cnt -= sizeof(uint32_t);            \
            (r) = PMIX_SUCCESS;                 \
        } else {                                \
            (r) = PMIX_ERR_BAD_PARAM;           \
        }                                       \
    } while (0)

#define PMIX_PTL_GET_U32(n)                     \
    do {                                        \
        uint32_t _u;                            \
        if (sizeof(uint32_t) <= cnt) {          \
            memcpy(&_u, mg, sizeof(uint32_t));  \
            (n) = ntohl(_u);                    \
            mg += sizeof(uint32_t);             \
            cnt -= sizeof(uint32_t);            \
        } else {                                \
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM); \
            goto error;                         \
        }                                       \
    } while (0)

#define PMIX_PTL_GET_BLOB(b, l)                 \
    do {                                        \
        if (0 < (l)) {                          \
            (b) = (char *) malloc((l));         \
            if (NULL == (b)) {                  \
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM); \
                goto error;                     \
            }                                   \
            memcpy((b), mg, (l));               \
            mg += (l);                          \
            cnt -= (l);                         \
        }                                       \
    } while (0)

#define PMIX_PTL_GET_U8(n)                      \
    do {                                        \
        if (sizeof(uint8_t) <= cnt) {           \
            memcpy(&(n), mg, sizeof(uint8_t));  \
            mg += sizeof(uint8_t);              \
            cnt -= sizeof(uint8_t);             \
        } else {                                \
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM); \
            goto error;                         \
        }                                       \
    } while (0)

#define PMIX_PTL_GET_PROCID(p)              \
    do {                                    \
        char *n;                            \
        uint32_t _r;                        \
        pmix_status_t _rc;                  \
        PMIX_PTL_GET_STRING(n);             \
        PMIX_PTL_GET_U32_NOERROR(_rc, _r);  \
        if (PMIX_SUCCESS != _rc) {          \
            PMIX_ERROR_LOG(_rc);            \
            free(n);                        \
            goto error;                     \
        }                                   \
        PMIX_LOAD_PROCID(&(p), n, _r);      \
        free(n);                            \
    } while (0)

/* The following macros are for use in the ptl_base_fns.c
 * when constructing the handshake message. */
#define PMIX_PTL_PUT_STRING(n)                 \
    do {                                       \
        memcpy(msg + csize, (n), strlen((n))); \
        csize += strlen((n)) + 1;              \
    } while (0)

/* numerical value must be converted to network byte
 * order, so we use an intermediate storage place
 * for that purpose */
#define PMIX_PTL_PUT_U32(n)                         \
    do {                                            \
        uint32_t _u;                                \
        _u = htonl((uint32_t)(n));                  \
        memcpy(msg + csize, &_u, sizeof(uint32_t)); \
        csize += sizeof(uint32_t);                  \
    } while (0)

#define PMIX_PTL_PUT_BLOB(b, l)            \
    do {                                   \
        if (0 < (l)) {                     \
            memcpy(msg + csize, (b), (l)); \
            csize += (l);                  \
        }                                  \
    } while (0)

#define PMIX_PTL_PUT_U8(n)                          \
    do {                                            \
        memcpy(msg + csize, &(n), sizeof(uint8_t)); \
        csize += sizeof(uint8_t);                   \
    } while (0)

#define PMIX_PTL_PUT_PROCID(p)           \
    do {                                 \
        PMIX_PTL_PUT_STRING((p).nspace); \
        PMIX_PTL_PUT_U32((p).rank);      \
    } while (0)

/* the following macros are for use in the ptl_base_fns.c
 * when sending/recving values during the handshake */
#define PMIX_PTL_RECV_NSPACE(s, n)                                              \
    do {                                                                        \
        pmix_status_t r;                                                        \
        r = pmix_ptl_base_recv_blocking((s), (char *) (n), PMIX_MAX_NSLEN + 1); \
        (n)[PMIX_MAX_NSLEN] = '\0'; /* ensure NULL termination */               \
        if (PMIX_SUCCESS != r) {                                                \
            return r;                                                           \
        }                                                                       \
    } while (0)

#define PMIX_PTL_RECV_U32(s, n)                                               \
    do {                                                                      \
        pmix_status_t r;                                                      \
        uint32_t _u;                                                          \
        r = pmix_ptl_base_recv_blocking((s), (char *) &_u, sizeof(uint32_t)); \
        if (PMIX_SUCCESS != r) {                                              \
            return r;                                                         \
        }                                                                     \
        (n) = htonl(_u);                                                      \
    } while (0)

END_C_DECLS

#endif

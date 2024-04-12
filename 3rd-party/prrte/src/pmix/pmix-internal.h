/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_PMIX_H
#define PRTE_PMIX_H

#include "prte_config.h"

#ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#endif

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"
#include "src/include/hash_string.h"
#include "src/mca/mca.h"
#include "src/threads/pmix_threads.h"
#include "src/util/error.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_printf.h"
#include "src/util/proc_info.h"

#include <pmix.h>
#include <pmix_server.h>
#include <pmix_tool.h>
#include <pmix_version.h>

BEGIN_C_DECLS

PRTE_EXPORT extern int prte_pmix_verbose_output;

typedef struct {
    pmix_list_item_t super;
    pmix_app_t app;
    void *info;
} prte_pmix_app_t;
PMIX_CLASS_DECLARATION(prte_pmix_app_t);

/* define a caddy for pointing to pmix_info_t that
 * are to be included in an answer */
typedef struct {
    pmix_list_item_t super;
    pmix_proc_t source;
    pmix_info_t *info;
    pmix_persistence_t persistence;
} prte_ds_info_t;
PMIX_CLASS_DECLARATION(prte_ds_info_t);

/* define another caddy for putting statically defined
 * pmix_info_t objects on a list */
typedef struct {
    pmix_list_item_t super;
    pmix_info_t info;
} prte_info_item_t;
PMIX_CLASS_DECLARATION(prte_info_item_t);

typedef struct {
    pmix_list_item_t super;
    pmix_list_t infolist;
} prte_info_array_item_t;
PMIX_CLASS_DECLARATION(prte_info_array_item_t);

typedef struct {
    pmix_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    int status;
    char *msg;
} prte_pmix_lock_t;

typedef struct {
    pmix_list_item_t super;
    pmix_value_t value;
} prte_value_t;
PMIX_CLASS_DECLARATION(prte_value_t);

#if !defined(WORDS_BIGENDIAN)
#    define PMIX_PROC_NTOH(guid) pmix_proc_ntoh_intr(&(guid))
static inline __prte_attribute_always_inline__ void pmix_proc_ntoh_intr(pmix_proc_t *name)
{
    name->rank = ntohl(name->rank);
}
#    define PMIX_PROC_HTON(guid) pmix_proc_hton_intr(&(guid))
static inline __prte_attribute_always_inline__ void pmix_proc_hton_intr(pmix_proc_t *name)
{
    name->rank = htonl(name->rank);
}
#else
#    define PMIX_PROC_NTOH(guid)
#    define PMIX_PROC_HTON(guid)
#endif

#define prte_pmix_condition_wait(a, b) pthread_cond_wait(a, &(b)->m_lock_pthread)

#define PRTE_PMIX_CONSTRUCT_LOCK(l)                \
    do {                                           \
        PMIX_CONSTRUCT(&(l)->mutex, pmix_mutex_t); \
        pthread_cond_init(&(l)->cond, NULL);       \
        (l)->active = true;                        \
        (l)->status = 0;                           \
        (l)->msg = NULL;                           \
        PMIX_POST_OBJECT((l));                     \
    } while (0)

#define PRTE_PMIX_DESTRUCT_LOCK(l)        \
    do {                                  \
        PMIX_ACQUIRE_OBJECT((l));         \
        PMIX_DESTRUCT(&(l)->mutex);       \
        pthread_cond_destroy(&(l)->cond); \
        if (NULL != (l)->msg) {           \
            free((l)->msg);               \
        }                                 \
    } while (0)

#if PRTE_ENABLE_DEBUG
#    define PRTE_PMIX_ACQUIRE_THREAD(lck)                                       \
        do {                                                                    \
            pmix_mutex_lock(&(lck)->mutex);                                     \
            while ((lck)->active) {                                             \
                prte_pmix_condition_wait(&(lck)->cond, &(lck)->mutex);          \
            }                                                                   \
            (lck)->active = true;                                               \
        } while (0)
#else
#    define PRTE_PMIX_ACQUIRE_THREAD(lck)                              \
        do {                                                           \
            pmix_mutex_lock(&(lck)->mutex);                            \
            while ((lck)->active) {                                    \
                prte_pmix_condition_wait(&(lck)->cond, &(lck)->mutex); \
            }                                                          \
            (lck)->active = true;                                      \
        } while (0)
#endif

#if PRTE_ENABLE_DEBUG
#    define PRTE_PMIX_WAIT_THREAD(lck)                                          \
        do {                                                                    \
            pmix_mutex_lock(&(lck)->mutex);                                     \
            while ((lck)->active) {                                             \
                prte_pmix_condition_wait(&(lck)->cond, &(lck)->mutex);          \
            }                                                                   \
            PMIX_ACQUIRE_OBJECT(&lck);                                          \
            pmix_mutex_unlock(&(lck)->mutex);                                   \
        } while (0)
#else
#    define PRTE_PMIX_WAIT_THREAD(lck)                                 \
        do {                                                           \
            pmix_mutex_lock(&(lck)->mutex);                            \
            while ((lck)->active) {                                    \
                prte_pmix_condition_wait(&(lck)->cond, &(lck)->mutex); \
            }                                                          \
            PMIX_ACQUIRE_OBJECT(lck);                                  \
            pmix_mutex_unlock(&(lck)->mutex);                          \
        } while (0)
#endif

#if PRTE_ENABLE_DEBUG
#    define PRTE_PMIX_RELEASE_THREAD(lck)                                     \
        do {                                                                  \
            (lck)->active = false;                                            \
            pthread_cond_broadcast(&(lck)->cond);                             \
            pmix_mutex_unlock(&(lck)->mutex);                                 \
        } while (0)
#else
#    define PRTE_PMIX_RELEASE_THREAD(lck)                   \
        do {                                                \
            assert(0 != pmix_mutex_trylock(&(lck)->mutex)); \
            (lck)->active = false;                          \
            pthread_cond_broadcast(&(lck)->cond);           \
            pmix_mutex_unlock(&(lck)->mutex);               \
        } while (0)
#endif

#define PRTE_PMIX_WAKEUP_THREAD(lck)          \
    do {                                      \
        pmix_mutex_lock(&(lck)->mutex);       \
        (lck)->active = false;                \
        PMIX_POST_OBJECT(lck);                \
        pthread_cond_broadcast(&(lck)->cond); \
        pmix_mutex_unlock(&(lck)->mutex);     \
    } while (0)

/*
 * Count the hash for the the external RM
 */
#define PRTE_HASH_JOBID(str, hash) \
    {                              \
        PRTE_HASH_STR(str, hash);  \
        hash &= ~(0x8000);         \
    }

/**
 * Provide a simplified macro for retrieving modex data
 * from another process when we don't want the PMIx module
 * to request it from the server if not found:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the pmix_proc_t of the proc that posted
 *     the data (pmix_proc_t*)
 * d - pointer to a location wherein the data object
 *     is to be returned
 * t - the expected data type
 */
#define PRTE_MODEX_RECV_VALUE_OPTIONAL(r, s, p, d, t)                                  \
    do {                                                                               \
        pmix_value_t *_kv = NULL;                                                      \
        pmix_info_t _info;                                                             \
        size_t _sz;                                                                    \
        PMIX_OUTPUT_VERBOSE((1, prte_pmix_verbose_output,                              \
                             "%s[%s:%d] MODEX RECV VALUE OPTIONAL FOR PROC %s KEY %s", \
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,   \
                             PRTE_NAME_PRINT((p)), (s)));                              \
        PMIX_INFO_LOAD(&_info, PMIX_OPTIONAL, NULL, PMIX_BOOL);                        \
        (r) = PMIx_Get((p), (s), &(_info), 1, &(_kv));                                 \
        if (NULL == _kv) {                                                             \
            (r) = PMIX_ERR_NOT_FOUND;                                                  \
        } else if (_kv->type != (t)) {                                                 \
            (r) = PMIX_ERR_TYPE_MISMATCH;                                              \
        } else if (PMIX_SUCCESS == (r)) {                                              \
            PMIX_VALUE_UNLOAD((r), _kv, (void **) (d), &_sz);                          \
        }                                                                              \
        if (NULL != _kv) {                                                             \
            PMIX_VALUE_RELEASE(_kv);                                                   \
        }                                                                              \
    } while (0);

#define PRTE_PMIX_SHOW_HELP "prte.show.help"

/* PRTE attribute */
typedef uint16_t prte_attribute_key_t;
#define PRTE_ATTR_KEY_T PRTE_UINT16
typedef struct {
    pmix_list_item_t super;   /* required for this to be on lists */
    prte_attribute_key_t key; /* key identifier */
    bool local;               // whether or not to pack/send this value
    pmix_value_t data;
} prte_attribute_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_attribute_t);

/* some helper functions */
PRTE_EXPORT pmix_proc_state_t prte_pmix_convert_state(int state);
PRTE_EXPORT int prte_pmix_convert_pstate(pmix_proc_state_t);
PRTE_EXPORT pmix_status_t prte_pmix_convert_rc(int rc);
PRTE_EXPORT int prte_pmix_convert_status(pmix_status_t status);
PRTE_EXPORT pmix_status_t prte_pmix_convert_job_state_to_error(int state);
PRTE_EXPORT pmix_status_t prte_pmix_convert_proc_state_to_error(int state);

PRTE_EXPORT int prte_pmix_register_cleanup(char *path, bool directory, bool ignore, bool jobscope);

#ifndef PMIX_DATA_BUFFER_STATIC_INIT
    #define PMIX_DATA_BUFFER_STATIC_INIT    \
    {                                       \
        .base_ptr = NULL,                   \
        .pack_ptr = NULL,                   \
        .unpack_ptr = NULL,                 \
        .bytes_allocated = 0,               \
        .bytes_used = 0                     \
    }
#endif

#define PRTE_MCA_BASE_VERSION_3_0_0(type, type_major, type_minor, type_release) \
    PMIX_MCA_BASE_VERSION_2_1_0("prte", PRTE_MAJOR_VERSION, PRTE_MINOR_VERSION, \
                                PRTE_RELEASE_VERSION, type, type_major, type_minor, type_release)

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_JOIN_COMPAT(a, b) \
        pmix_argv_join(a, b)
#else
#define PMIX_ARGV_JOIN_COMPAT(a, b) \
        PMIx_Argv_join(a, b)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_SPLIT_COMPAT(a, b) \
        pmix_argv_split(a, b)
#else
#define PMIX_ARGV_SPLIT_COMPAT(a, b) \
        PMIx_Argv_split(a, b)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_SPLIT_WITH_EMPTY_COMPAT(a, b) \
        pmix_argv_split_with_empty(a, b)
#else
#define PMIX_ARGV_SPLIT_WITH_EMPTY_COMPAT(a, b) \
        PMIx_Argv_split_with_empty(a, b)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_COUNT_COMPAT(a) \
        pmix_argv_count(a)
#else
#define PMIX_ARGV_COUNT_COMPAT(a) \
        PMIx_Argv_count(a)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_FREE_COMPAT(a) \
        pmix_argv_free(a)
#else
#define PMIX_ARGV_FREE_COMPAT(a) \
        PMIx_Argv_free(a)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_APPEND_UNIQUE_COMPAT(a, b) \
        pmix_argv_append_unique_nosize(a, b)
#else
#define PMIX_ARGV_APPEND_UNIQUE_COMPAT(a, b) \
        PMIx_Argv_append_unique_nosize(a, b)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_APPEND_NOSIZE_COMPAT(a, b) \
        pmix_argv_append_nosize(a, b)
#else
#define PMIX_ARGV_APPEND_NOSIZE_COMPAT(a, b) \
        PMIx_Argv_append_nosize(a, b)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_ARGV_COPY_COMPAT(a) \
        pmix_argv_copy(a)
#else
#define PMIX_ARGV_COPY_COMPAT(a) \
        PMIx_Argv_copy(a)
#endif

#if PMIX_NUMERIC_VERSION < 0x00040203
#define PMIX_SETENV_COMPAT(a, b, c, d) \
        pmix_setenv(a, b, c, d)
#else
#define PMIX_SETENV_COMPAT(a, b, c, d) \
        PMIx_Setenv(a, b, c, d)
#endif

END_C_DECLS

#endif

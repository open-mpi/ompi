/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_PMIX_H
#define OPAL_PMIX_H

#include "opal_config.h"
#include "opal/types.h"

#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"
#include "opal/util/error.h"
#include "opal/util/proc.h"
#include "opal/hash_string.h"

/* include implementation to call */
#include MCA_pmix_IMPLEMENTATION_HEADER


BEGIN_C_DECLS

/* provide access to the framework verbose output without
 * exposing the entire base */
OPAL_DECLSPEC extern bool opal_pmix_collect_all_data;
OPAL_DECLSPEC extern bool opal_pmix_base_async_modex;
OPAL_DECLSPEC extern int opal_pmix_verbose_output;

/* define a caddy for pointing to pmix_info_t that
 * are to be included in an answer */
typedef struct {
    opal_list_item_t super;
    pmix_proc_t source;
    pmix_info_t *info;
    pmix_persistence_t persistence;
} opal_ds_info_t;
OBJ_CLASS_DECLARATION(opal_ds_info_t);

/* define another caddy for putting statically defined
 * pmix_info_t objects on a list */
typedef struct {
    opal_list_item_t super;
    pmix_info_t info;
} opal_info_item_t;
OBJ_CLASS_DECLARATION(opal_info_item_t);


typedef struct {
    opal_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    int status;
    char *msg;
} opal_pmix_lock_t;

#define opal_pmix_condition_wait(a,b)   pthread_cond_wait(a, &(b)->m_lock_pthread)

#define OPAL_PMIX_CONSTRUCT_LOCK(l)                     \
    do {                                                \
        OBJ_CONSTRUCT(&(l)->mutex, opal_mutex_t);       \
        pthread_cond_init(&(l)->cond, NULL);            \
        (l)->active = true;                             \
        (l)->status = 0;                                \
        (l)->msg = NULL;                                \
        OPAL_POST_OBJECT((l));                          \
    } while(0)

#define OPAL_PMIX_DESTRUCT_LOCK(l)          \
    do {                                    \
        OPAL_ACQUIRE_OBJECT((l));           \
        OBJ_DESTRUCT(&(l)->mutex);          \
        pthread_cond_destroy(&(l)->cond);   \
        if (NULL != (l)->msg) {             \
            free((l)->msg);                 \
        }                                   \
    } while(0)


#if OPAL_ENABLE_DEBUG
#define OPAL_PMIX_ACQUIRE_THREAD(lck)                               \
    do {                                                            \
        opal_mutex_lock(&(lck)->mutex);                             \
        if (opal_debug_threads) {                                   \
            opal_output(0, "Waiting for thread %s:%d",              \
                        __FILE__, __LINE__);                        \
        }                                                           \
        while ((lck)->active) {                                     \
            opal_pmix_condition_wait(&(lck)->cond, &(lck)->mutex);  \
        }                                                           \
        if (opal_debug_threads) {                                   \
            opal_output(0, "Thread obtained %s:%d",                 \
                        __FILE__, __LINE__);                        \
        }                                                           \
        (lck)->active = true;                                       \
    } while(0)
#else
#define OPAL_PMIX_ACQUIRE_THREAD(lck)                               \
    do {                                                            \
        opal_mutex_lock(&(lck)->mutex);                             \
        while ((lck)->active) {                                     \
            opal_pmix_condition_wait(&(lck)->cond, &(lck)->mutex);  \
        }                                                           \
        (lck)->active = true;                                       \
    } while(0)
#endif


#if OPAL_ENABLE_DEBUG
#define OPAL_PMIX_WAIT_THREAD(lck)                                  \
    do {                                                            \
        opal_mutex_lock(&(lck)->mutex);                             \
        if (opal_debug_threads) {                                   \
            opal_output(0, "Waiting for thread %s:%d",              \
                        __FILE__, __LINE__);                        \
        }                                                           \
        while ((lck)->active) {                                     \
            opal_pmix_condition_wait(&(lck)->cond, &(lck)->mutex);  \
        }                                                           \
        if (opal_debug_threads) {                                   \
            opal_output(0, "Thread obtained %s:%d",                 \
                        __FILE__, __LINE__);                        \
        }                                                           \
        OPAL_ACQUIRE_OBJECT(&lck);                                  \
        opal_mutex_unlock(&(lck)->mutex);                           \
    } while(0)
#else
#define OPAL_PMIX_WAIT_THREAD(lck)                                  \
    do {                                                            \
        opal_mutex_lock(&(lck)->mutex);                             \
        while ((lck)->active) {                                     \
            opal_pmix_condition_wait(&(lck)->cond, &(lck)->mutex);  \
        }                                                           \
        OPAL_ACQUIRE_OBJECT(lck);                                   \
        opal_mutex_unlock(&(lck)->mutex);                           \
    } while(0)
#endif


#if OPAL_ENABLE_DEBUG
#define OPAL_PMIX_RELEASE_THREAD(lck)                   \
    do {                                                \
        if (opal_debug_threads) {                       \
            opal_output(0, "Releasing thread %s:%d",    \
                        __FILE__, __LINE__);            \
        }                                               \
        (lck)->active = false;                          \
        pthread_cond_broadcast(&(lck)->cond);           \
        opal_mutex_unlock(&(lck)->mutex);               \
    } while(0)
#else
#define OPAL_PMIX_RELEASE_THREAD(lck)                   \
    do {                                                \
        assert(0 != opal_mutex_trylock(&(lck)->mutex)); \
        (lck)->active = false;                          \
        pthread_cond_broadcast(&(lck)->cond);           \
        opal_mutex_unlock(&(lck)->mutex);               \
    } while(0)
#endif


#define OPAL_PMIX_WAKEUP_THREAD(lck)                    \
    do {                                                \
        opal_mutex_lock(&(lck)->mutex);                 \
        (lck)->active = false;                          \
        OPAL_POST_OBJECT(lck);                          \
        pthread_cond_broadcast(&(lck)->cond);           \
        opal_mutex_unlock(&(lck)->mutex);               \
    } while(0)

/*
 * Count the fash for the the external RM
 */
#define OPAL_HASH_JOBID( str, hash ){               \
    OPAL_HASH_STR( str, hash );                     \
    hash &= ~(0x8000);                              \
}

/**
 * Provide a simplified macro for sending data via modex
 * to other processes. The macro requires four arguments:
 *
 * r - the integer return status from the modex op
 * sc - the PMIX scope of the data
 * s - the key to tag the data being posted
 * d - pointer to the data object being posted
 * t - the type of the data
 */
#define OPAL_MODEX_SEND_VALUE(r, sc, s, d, t)   \
    do {                                        \
        pmix_value_t _kv;                       \
        PMIX_VALUE_LOAD(&_kv, (d), (t));        \
        (r) = PMIx_Put((sc), (s), &(_kv));      \
                OPAL_ERROR_LOG((r));            \
    } while(0);

/**
 * Provide a simplified macro for sending data via modex
 * to other processes. The macro requires four arguments:
 *
 * r - the integer return status from the modex op
 * sc - the PMIX scope of the data
 * s - the key to tag the data being posted
 * d - the data object being posted
 * sz - the number of bytes in the data object
 */
#define OPAL_MODEX_SEND_STRING(r, sc, s, d, sz)     \
    do {                                            \
        pmix_value_t _kv;                           \
        _kv.type = PMIX_BYTE_OBJECT;                \
        _kv.data.bo.bytes = (char*)(d);             \
        _kv.data.bo.size = (sz);                    \
        (r) = PMIx_Put(sc, (s), &(_kv));            \
    } while(0);

/**
 * Provide a simplified macro for sending data via modex
 * to other processes. The macro requires four arguments:
 *
 * r - the integer return status from the modex op
 * sc - the PMIX scope of the data
 * s - the MCA component that is posting the data
 * d - the data object being posted
 * sz - the number of bytes in the data object
 */
#define OPAL_MODEX_SEND(r, sc, s, d, sz)                        \
    do {                                                        \
        char *_key;                                             \
        _key = mca_base_component_to_string((s));               \
        OPAL_MODEX_SEND_STRING((r), (sc), _key, (d), (sz));     \
        free(_key);                                             \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process when we don't want the PMIx module
 * to request it from the server if not found:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the opal_process_name_t of the proc that posted
 *     the data (opal_process_name_t*)
 * d - pointer to a location wherein the data object
 *     is to be returned
 * t - the expected data type
 */
#define OPAL_MODEX_RECV_VALUE_OPTIONAL(r, s, p, d, t)                                   \
    do {                                                                                \
        pmix_proc_t _proc;                                                              \
        pmix_value_t *_kv = NULL;                                                       \
        pmix_info_t _info;                                                              \
        size_t _sz;                                                                     \
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,                               \
                            "%s[%s:%d] MODEX RECV VALUE OPTIONAL FOR PROC %s KEY %s",   \
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),                         \
                            __FILE__, __LINE__,                                         \
                            OPAL_NAME_PRINT(*(p)), (s)));                               \
        OPAL_PMIX_CONVERT_NAME(&_proc, (p));                                            \
        PMIX_INFO_LOAD(&_info, PMIX_OPTIONAL, NULL, PMIX_BOOL);                         \
        (r) = PMIx_Get(&(_proc), (s), &(_info), 1, &(_kv));                             \
        if (NULL == _kv) {                                                              \
            (r) = PMIX_ERR_NOT_FOUND;                                                   \
        } else if (_kv->type != (t)) {                                                  \
            (r) = PMIX_ERR_TYPE_MISMATCH;                                               \
        } else if (PMIX_SUCCESS == (r)) {                                               \
            PMIX_VALUE_UNLOAD((r), _kv, (void**)(d), &_sz);                             \
        }                                                                               \
        if (NULL != _kv) {                                                              \
            PMIX_VALUE_RELEASE(_kv);                                                    \
        }                                                                               \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process when we want the PMIx module
 * to request it from the server if not found, but do not
 * want the server to go find it if the server doesn't
 * already have it:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the opal_process_name_t of the proc that posted
 *     the data (opal_process_name_t*)
 * d - pointer to a location wherein the data object
 *     is to be returned
 * t - the expected data type
 */
#define OPAL_MODEX_RECV_VALUE_IMMEDIATE(r, s, p, d, t)                                  \
    do {                                                                                \
        pmix_proc_t _proc;                                                              \
        pmix_value_t *_kv = NULL;                                                       \
        pmix_info_t _info;                                                              \
        size_t _sz;                                                                     \
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,                               \
                            "%s[%s:%d] MODEX RECV VALUE IMMEDIATE FOR PROC %s KEY %s",  \
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),                         \
                            __FILE__, __LINE__,                                         \
                            OPAL_NAME_PRINT(*(p)), (s)));                               \
        OPAL_PMIX_CONVERT_NAME(&_proc, (p));                                            \
        PMIX_INFO_LOAD(&_info, PMIX_IMMEDIATE, NULL, PMIX_BOOL);                        \
        (r) = PMIx_Get(&(_proc), (s), &(_info), 1, &(_kv));                             \
        if (NULL == _kv) {                                                              \
            (r) = PMIX_ERR_NOT_FOUND;                                                   \
        } else if (_kv->type != (t)) {                                                  \
            (r) = PMIX_ERR_TYPE_MISMATCH;                                               \
        } else if (PMIX_SUCCESS == (r)) {                                               \
            PMIX_VALUE_UNLOAD((r), _kv, (void**)(d), &_sz);                             \
        }                                                                               \
        if (NULL != _kv) {                                                              \
            PMIX_VALUE_RELEASE(_kv);                                                    \
        }                                                                               \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the opal_process_name_t of the proc that posted
 *     the data (opal_process_name_t*)
 * d - pointer to a location wherein the data object
 *     is to be returned
 * t - the expected data type
 */
#define OPAL_MODEX_RECV_VALUE(r, s, p, d, t)                                    \
    do {                                                                        \
        pmix_proc_t _proc;                                                      \
        pmix_value_t *_kv = NULL;                                               \
        size_t _sz;                                                             \
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,                       \
                            "%s[%s:%d] MODEX RECV VALUE FOR PROC %s KEY %s",    \
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),                 \
                            __FILE__, __LINE__,                                 \
                            OPAL_NAME_PRINT(*(p)), (s)));                       \
        OPAL_PMIX_CONVERT_NAME(&_proc, (p));                                    \
        (r) = PMIx_Get(&(_proc), (s), NULL, 0, &(_kv));                         \
        if (NULL == _kv) {                                                      \
            (r) = PMIX_ERR_NOT_FOUND;                                           \
        } else if (_kv->type != (t)) {                                          \
            (r) = PMIX_ERR_TYPE_MISMATCH;                                       \
        } else if (PMIX_SUCCESS == (r)) {                                       \
            PMIX_VALUE_UNLOAD((r), _kv, (void**)(d), &_sz);                     \
        }                                                                       \
        if (NULL != _kv) {                                                      \
            PMIX_VALUE_RELEASE(_kv);                                            \
        }                                                                       \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the opal_process_name_t of the proc that posted
 *     the data (opal_process_name_t*)
 * d - pointer to a location wherein the data object
 *     it to be returned (char**)
 * sz - pointer to a location wherein the number of bytes
 *     in the data object can be returned (size_t)
 */
#define OPAL_MODEX_RECV_STRING_OPTIONAL(r, s, p, d, sz)                                \
    do {                                                                               \
        pmix_proc_t _proc;                                                             \
        pmix_value_t *_kv = NULL;                                                      \
        pmix_info_t _info;                                                             \
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,                              \
                            "%s[%s:%d] MODEX RECV STRING OPTIONAL FOR PROC %s KEY %s", \
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),                        \
                            __FILE__, __LINE__,                                        \
                            OPAL_NAME_PRINT(*(p)), (s)));                              \
        *(d) = NULL;                                                                   \
        *(sz) = 0;                                                                     \
        OPAL_PMIX_CONVERT_NAME(&_proc, (p));                                           \
        PMIX_INFO_LOAD(&_info, PMIX_OPTIONAL, NULL, PMIX_BOOL);                        \
        (r) = PMIx_Get(&(_proc), (s), &(_info), 1, &(_kv));                            \
        if (NULL == _kv) {                                                             \
            (r) = PMIX_ERR_NOT_FOUND;                                                  \
        } else if (PMIX_SUCCESS == (r)) {                                              \
            *(d) = (uint8_t*)_kv->data.bo.bytes;                                       \
            *(sz) = _kv->data.bo.size;                                                 \
            _kv->data.bo.bytes = NULL; /* protect the data */                          \
        }                                                                              \
        if (NULL != _kv) {                                                             \
            PMIX_VALUE_RELEASE(_kv);                                                   \
        }                                                                              \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the opal_process_name_t of the proc that posted
 *     the data (opal_process_name_t*)
 * d - pointer to a location wherein the data object
 *     it to be returned (char**)
 * sz - pointer to a location wherein the number of bytes
 *     in the data object can be returned (size_t)
 */
#define OPAL_MODEX_RECV_STRING(r, s, p, d, sz)                                  \
    do {                                                                        \
        pmix_proc_t _proc;                                                      \
        pmix_value_t *_kv = NULL;                                               \
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,                       \
                            "%s[%s:%d] MODEX RECV STRING FOR PROC %s KEY %s",   \
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),                 \
                            __FILE__, __LINE__,                                 \
                            OPAL_NAME_PRINT(*(p)), (s)));                       \
        *(d) = NULL;                                                            \
        *(sz) = 0;                                                              \
        OPAL_PMIX_CONVERT_NAME(&_proc, (p));                                    \
        (r) = PMIx_Get(&(_proc), (s), NULL, 0, &(_kv));                         \
        if (NULL == _kv) {                                                      \
            (r) = PMIX_ERR_NOT_FOUND;                                           \
        } else if (PMIX_SUCCESS == (r)) {                                       \
            *(d) = (uint8_t*)_kv->data.bo.bytes;                                \
            *(sz) = _kv->data.bo.size;                                          \
            _kv->data.bo.bytes = NULL; /* protect the data */                   \
        }                                                                       \
        if (NULL != _kv) {                                                      \
            PMIX_VALUE_RELEASE(_kv);                                            \
        }                                                                       \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process:
 *
 * r - the integer return status from the modex op (int)
 * s - the MCA component that posted the data (mca_base_component_t*)
 * p - pointer to the opal_process_name_t of the proc that posted
 *     the data (opal_process_name_t*)
 * d - pointer to a location wherein the data object
 *     it to be returned (char**)
 * sz - pointer to a location wherein the number of bytes
 *     in the data object can be returned (size_t)
 */
#define OPAL_MODEX_RECV(r, s, p, d, sz)                                 \
    do {                                                                \
        char *_key;                                                     \
        _key = mca_base_component_to_string((s));                       \
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,               \
                            "%s[%s:%d] MODEX RECV FOR PROC %s KEY %s",  \
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            OPAL_NAME_PRINT(*(p)), _key));              \
        if (NULL == _key) {                                             \
            OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);                   \
            (r) = OPAL_ERR_OUT_OF_RESOURCE;                             \
        } else {                                                        \
            OPAL_MODEX_RECV_STRING((r), _key, (p), (d), (sz));          \
            free(_key);                                                 \
        }                                                               \
    } while(0);

#define PMIX_ERROR_LOG(r)          \
    opal_output(0, "[%s:%d] PMIx Error: %s", __FILE__, __LINE__, PMIx_Error_string((r)))

#define OPAL_PMIX_SHOW_HELP    "opal.show.help"

/* some helper functions */
OPAL_DECLSPEC pmix_proc_state_t opal_pmix_convert_state(int state);
OPAL_DECLSPEC int opal_pmix_convert_pstate(pmix_proc_state_t);
OPAL_DECLSPEC pmix_status_t opal_pmix_convert_rc(int rc);
OPAL_DECLSPEC int opal_pmix_convert_status(pmix_status_t status);

#define OPAL_PMIX_CONVERT_JOBID(n, j) \
    (void)opal_snprintf_jobid((n), PMIX_MAX_NSLEN, (j))

#define OPAL_PMIX_CONVERT_VPID(r, v)        \
    do {                                    \
        if (OPAL_VPID_WILDCARD == (v)) {    \
            (r) = PMIX_RANK_WILDCARD;       \
        } else {                            \
            (r) = (v);                      \
        }                                   \
    } while(0)
#define OPAL_PMIX_CONVERT_NAME(p, n)                        \
    do {                                                    \
        OPAL_PMIX_CONVERT_JOBID((p)->nspace, (n)->jobid);   \
        OPAL_PMIX_CONVERT_VPID((p)->rank, (n)->vpid);       \
    } while(0)


#define OPAL_PMIX_CONVERT_NSPACE(r, j, n)       \
    (r) = opal_convert_string_to_jobid((j), (n))

#define OPAL_PMIX_CONVERT_RANK(v, r)        \
    do {                                    \
        if (PMIX_RANK_WILDCARD == (r)) {    \
            (v) = OPAL_VPID_WILDCARD;       \
        } else {                            \
            (v) = (r);                      \
        }                                   \
    } while(0)

#define OPAL_PMIX_CONVERT_PROCT(r, n, p)                            \
    do {                                                            \
        OPAL_PMIX_CONVERT_NSPACE((r), &(n)->jobid, (p)->nspace);    \
        if (OPAL_SUCCESS == (r)) {                                  \
            OPAL_PMIX_CONVERT_RANK((n)->vpid, (p)->rank);           \
        }                                                           \
    } while(0)

OPAL_DECLSPEC void opal_pmix_value_load(pmix_value_t *v,
                                        opal_value_t *kv);

OPAL_DECLSPEC int opal_pmix_value_unload(opal_value_t *kv,
                                         const pmix_value_t *v);

OPAL_DECLSPEC int opal_pmix_register_cleanup(char *path,
                                             bool directory,
                                             bool ignore,
                                             bool jobscope);

/* protect against early versions of PMIx */
#if PMIX_VERSION_MAJOR == 3
#if   PMIX_VERSION_MINOR == 0
#if     PMIX_VERSION_RELEASE == 0
#define   PMIX_NUMERIC_VERSION 0x00030000
#define   PMIX_OPERATION_SUCCEEDED (PMIX_ERR_OP_BASE - 27)
#elif   PMIX_VERSION_RELEASE == 2
#undef    PMIX_NUMERIC_VERSION
#define   PMIX_NUMERIC_VERSION 0x00030002
#endif
#define PMIX_LOAD_KEY(a, b) \
    do {                                            \
        memset((a), 0, PMIX_MAX_KEYLEN+1);          \
        pmix_strncpy((a), (b), PMIX_MAX_KEYLEN);    \
    }while(0)
#if     PMIX_VERSION_RELEASE < 2
#define PMIX_CHECK_KEY(a, b) \
    (0 == strncmp((a)->key, (b), PMIX_MAX_KEYLEN))
#endif
#elif PMIX_VERSION_MINOR == 1
#if     PMIX_VERSION_RELEASE == 1
#undef    PMIX_NUMERIC_VERSION
#define   PMIX_NUMERIC_VERSION 0x00030101
#elif   PMIX_VERSION_RELEASE == 2
#undef    PMIX_NUMERIC_VERSION
#define   PMIX_NUMERIC_VERSION 0x00030102
#endif
#endif
#endif

/**
 * Structure for pmix components.
 */
struct opal_pmix_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};

/**
 * Convenience typedef
 */
typedef struct opal_pmix_base_component_2_0_0_t opal_pmix_base_component_2_0_0_t;
typedef struct opal_pmix_base_component_2_0_0_t opal_pmix_component_t;

/**
 * Macro for use in components that are of type hwloc
 */
#define OPAL_PMIX_BASE_VERSION_2_0_0 \
    OPAL_MCA_BASE_VERSION_2_1_0("pmix", 2, 0, 0)


END_C_DECLS

#endif

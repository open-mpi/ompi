/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#include "opal/event/event-internal.h"
#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"
#include "opal/util/error.h"
#include "opal/util/proc.h"
#include "opal/hash_string.h"

#include "pmix.h"
#include "pmix_server.h"
#include "pmix_tool.h"
#include "pmix_version.h"

BEGIN_C_DECLS

extern int opal_pmix_verbose_output;
extern bool opal_pmix_collect_all_data;
extern bool opal_pmix_base_async_modex;
extern int opal_pmix_base_timeout;

/* define a caddy for pointing to pmix_info_t that
 * are to be included in an answer */
typedef struct {
    opal_list_item_t super;
    pmix_proc_t source;
    pmix_info_t *info;
    pmix_persistence_t persistence;
} opal_ds_info_t;
OBJ_CLASS_DECLARATION(opal_ds_info_t);

/* define another cady for putting statically defined
 * pmix_info_t objects on a list */
typedef struct {
    opal_list_item_t super;
    pmix_info_t info;
} opal_info_item_t;
OBJ_CLASS_DECLARATION(opal_info_item_t);


/****    PMIX LOOKUP RETURN STRUCT    ****/
typedef struct {
    opal_list_item_t super;
    opal_process_name_t proc;
    opal_value_t value;
} opal_pmix_pdata_t;

OBJ_CLASS_DECLARATION(opal_pmix_pdata_t);

/****    PMIX APP STRUCT    ****/
typedef struct {
    opal_list_item_t super;
    char *cmd;
    char **argv;
    char **env;
    char *cwd;
    int maxprocs;
    opal_list_t info;
} opal_pmix_app_t;
/* utility macros for working with pmix_app_t structs */
OBJ_CLASS_DECLARATION(opal_pmix_app_t);

typedef struct {
    opal_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    int status;
    char *msg;
} opal_pmix_lock_t;

extern int opal_pmix_base_exchange(opal_value_t *info,
                                   opal_pmix_pdata_t *pdat,
                                   int timeout);

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
        _kv.data.bo.bytes = (uint8_t*)(d);          \
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
#define OPAL_MODEX_RECV_VALUE_IMMEDIATE(r, s, p, d, t)                                   \
    do {                                                                                 \
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
        OPAL_PMIX_CONVERT_NAME(&_proc, (p));                                   \
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
            *(d) = _kv->data.bo.bytes;                                          \
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

/* define a callback for common operations that simply return
 * a status. Examples include the non-blocking versions of
 * Fence, Connect, and Disconnect */
typedef void (*opal_pmix_op_cbfunc_t)(int status, void *cbdata);

/* define a callback function by which event handlers can notify
 * us that they have completed their action, and pass along any
 * further information for subsequent handlers */
typedef void (*opal_pmix_notification_complete_fn_t)(int status, opal_list_t *results,
                                                     opal_pmix_op_cbfunc_t cbfunc, void *thiscbdata,
                                                     void *notification_cbdata);

/* define a callback function for calls to register_evhandler. The
 * status indicates if the request was successful or not, evhandler_ref is
 * a size_t reference assigned to the evhandler by PMIX, this reference
 * must be used to deregister the err handler. A ptr to the original
 * cbdata is returned. */
typedef void (*opal_pmix_evhandler_reg_cbfunc_t)(int status,
                                                 size_t evhandler_ref,
                                                 void *cbdata);

/* define a callback function for the evhandler. Upon receipt of an
 * event notification, the active module will execute the specified notification
 * callback function, providing:
 *
 * status - the error that occurred
 * source - identity of the proc that generated the event
 * info - any additional info provided regarding the error.
 * results - any info from prior event handlers
 * cbfunc - callback function to execute when the evhandler is
 *          finished with the provided data so it can be released
 * cbdata - pointer to be returned in cbfunc
 *
 * Note that different resource managers may provide differing levels
 * of support for event notification to application processes. Thus, the
 * info list may be NULL or may contain detailed information of the event.
 * It is the responsibility of the application to parse any provided info array
 * for defined key-values if it so desires.
 *
 * Possible uses of the opal_value_t list include:
 *
 * - for the RM to alert the process as to planned actions, such as
 *   to abort the session, in response to the reported event
 *
 * - provide a timeout for alternative action to occur, such as for
 *   the application to request an alternate response to the event
 *
 * For example, the RM might alert the application to the failure of
 * a node that resulted in termination of several processes, and indicate
 * that the overall session will be aborted unless the application
 * requests an alternative behavior in the next 5 seconds. The application
 * then has time to respond with a checkpoint request, or a request to
 * recover from the failure by obtaining replacement nodes and restarting
 * from some earlier checkpoint.
 *
 * Support for these options is left to the discretion of the host RM. Info
 * keys are included in the common definions above, but also may be augmented
 * on a per-RM basis.
 *
 * On the server side, the notification function is used to inform the host
 * server of a detected error in the PMIx subsystem and/or client */
typedef void (*opal_pmix_notification_fn_t)(int status,
                                            const opal_process_name_t *source,
                                            opal_list_t *info, opal_list_t *results,
                                            opal_pmix_notification_complete_fn_t cbfunc,
                                            void *cbdata);

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

OPAL_DECLSPEC int opal_pmix_fence(opal_list_t *procs,
                                  int collect_data);

OPAL_DECLSPEC int opal_pmix_fence_nb(opal_list_t *procs,
                                     int collect_data,
                                     opal_pmix_op_cbfunc_t cbfunc,
                                     void *cbdata);

OPAL_DECLSPEC int opal_pmix_lookup(opal_list_t *data,
                                   opal_list_t *info);

OPAL_DECLSPEC int opal_pmix_publish(opal_list_t *info);

OPAL_DECLSPEC int opal_pmix_unpublish(char **keys,
                                      opal_list_t *info);

OPAL_DECLSPEC int opal_pmix_initialized(void);

OPAL_DECLSPEC int opal_pmix_init(opal_list_t *ilist);

OPAL_DECLSPEC int opal_pmix_store_local(const opal_process_name_t *proc,
                                        opal_value_t *val);

OPAL_DECLSPEC int opal_pmix_finalize(void);

OPAL_DECLSPEC int opal_pmix_abort(int status,
                                  const char *msg,
                                  opal_list_t *procs);

OPAL_DECLSPEC void opal_pmix_register_evhandler(opal_list_t *event_codes,
                                                opal_list_t *info,
                                                opal_pmix_notification_fn_t evhandler,
                                                opal_pmix_evhandler_reg_cbfunc_t cbfunc,
                                                void *cbdata);

OPAL_DECLSPEC void opal_pmix_deregister_evhandler(size_t evhandler,
                                                  opal_pmix_op_cbfunc_t cbfunc,
                                                  void *cbdata);

OPAL_DECLSPEC void opal_pmix_base_set_evbase(opal_event_base_t *evbase);

OPAL_DECLSPEC char* opal_pmix_get_nspace(opal_jobid_t jobid);

OPAL_DECLSPEC void opal_pmix_register_jobid(opal_jobid_t jobid, const char *nspace);

OPAL_DECLSPEC int opal_pmix_connect(opal_list_t *procs);

OPAL_DECLSPEC int opal_pmix_spawn(opal_list_t *job_info,
                                  opal_list_t *apps,
                                  opal_jobid_t *jobid);

OPAL_DECLSPEC int opal_pmix_commit(void);

/* protect against early versions of PMIx */
#ifndef PMIX_VALUE_UNLOAD
pmix_status_t pmix_value_unload(pmix_value_t *kv, void **data, size_t *sz);
#define PMIX_VALUE_UNLOAD(r, k, d, s)      \
    (r) = pmix_value_unload((k), (d), (s))
#endif

#ifndef PMIX_DATA_BUTTER_LOAD
#define PMIX_DATA_BUFFER_LOAD(b, d, s)          \
    do {                                        \
        (b)->base_ptr = (char*)(d);             \
        (b)->pack_ptr = (b)->base_ptr + (s);    \
        (b)->unpack_ptr = (b)->base_ptr;        \
        (b)->bytes_allocated = (s);             \
        (b)->bytes_used = (s);                  \
    } while(0)
#endif

#ifndef PMIX_DATA_BUFFER_UNLOAD
#define PMIX_DATA_BUFFER_UNLOAD(b, d, s)    \
    do {                                    \
        (d) = (b)->base_ptr;                \
        (s) = (b)->bytes_used;              \
        (b)->base_ptr = NULL;               \
    } while(0)
#endif

#ifndef PMIX_OPERATION_IN_PROGRESS
#define PMIX_OPERATION_IN_PROGRESS  -156
#endif

#ifndef PMIX_LOAD_KEY
#define PMIX_LOAD_KEY(a, b) \
    do {                                            \
        memset((a), 0, PMIX_MAX_KEYLEN+1);          \
        pmix_strncpy((a), (b), PMIX_MAX_KEYLEN);    \
    }while(0)
#endif

#ifndef PMIX_CHECK_KEY
#define PMIX_CHECK_KEY(a, b) \
    (0 == strncmp((a)->key, (b), PMIX_MAX_KEYLEN))
#endif

/**
 * Provide a macro for accessing a base function that exchanges
 * data values between two procs using the PMIx Publish/Lookup
 * APIs */
 #define OPAL_PMIX_EXCHANGE(r, i, p, t)                          \
    do {                                                         \
        OPAL_OUTPUT_VERBOSE((1, opal_pmix_verbose_output,        \
                            "%s[%s:%d] EXCHANGE %s WITH %s",     \
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),  \
                            __FILE__, __LINE__,                  \
                            (i)->key, (p)->value.key));          \
        (r) = opal_pmix_base_exchange((i), (p), (t));            \
    } while(0);
END_C_DECLS

#endif

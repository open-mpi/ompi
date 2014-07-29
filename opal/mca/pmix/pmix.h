/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
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

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/dss/dss.h"
#include "opal/runtime/opal.h"
#include "opal/mca/dstore/dstore.h"
#include "opal/dss/dss.h"
#include "opal/util/error.h"

BEGIN_C_DECLS

/* define some maximum sizes */
#define PMIX_MAX_VALLEN   1024
#define PMIX_MAX_INFO_KEY  255
#define PMIX_MAX_INFO_VAL 1024

/* define an INFO object corresponding to
 * the MPI_Info structure */
typedef struct {
    opal_list_item_t super;
    char key[PMIX_MAX_INFO_KEY];
    char value[PMIX_MAX_INFO_VAL];
} pmix_info_t;
OBJ_CLASS_DECLARATION(pmix_info_t);

/* define a scope for data "put" by PMI per the following:
 *
 * PMI_LOCAL - the data is intended only for other application
 *             processes on the same node. Data marked in this way
 *             will not be included in data packages sent to remote requestors
 * PMI_REMOTE - the data is intended solely for applications processes on
 *              remote nodes. Data marked in this way will not be shared with
 *              other processes on the same node
 * PMI_GLOBAL - the data is to be shared with all other requesting processes,
 *              regardless of location
 */
typedef uint8_t opal_pmix_scope_t;
#define PMIX_INTERNAL  0x01  // data used internally only
#define PMIX_LOCAL     0x01  // share to procs also on this node
#define PMIX_REMOTE    0x02  // share with procs not on this node
#define PMIX_GLOBAL    0x03  // share with all procs (local + remote)

/* callback function for non-blocking operations */
typedef void (*opal_pmix_cbfunc_t)(int status, opal_value_t *kv, void *cbdata);

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
#define OPAL_MODEX_SEND_STRING(r, sc, s, d, sz)                 \
    do {                                                        \
        opal_value_t kv;                                        \
        OBJ_CONSTRUCT(&kv, opal_value_t);                       \
        kv.key = (s);                                           \
        kv.type = OPAL_BYTE_OBJECT;                             \
        kv.data.bo.bytes = (uint8_t*)(d);                       \
        kv.data.bo.size = (sz);                                 \
        if (OPAL_SUCCESS != ((r) = opal_pmix.put(sc, &kv))) {   \
            OPAL_ERROR_LOG((r));                                \
        }                                                       \
        kv.data.bo.bytes = NULL; /* protect the data */         \
        kv.key = NULL;  /* protect the key */                   \
        OBJ_DESTRUCT(&kv);                                      \
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
        char *key;                                              \
        key = mca_base_component_to_string((s));                \
        OPAL_MODEX_SEND_STRING((r), (sc), key, (d), (sz));      \
        free(key);                                              \
    } while(0);



/**
 * Provide a simplified macro for retrieving modex data
 * from another process:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the opal_proc_t of the proc that posted
 *     the data (opal_proc_t*)
 * d - pointer to a location wherein the data object
 *     it to be returned
 * t - the expected data type
 */
#define OPAL_MODEX_RECV_VALUE(r, s, p, d, t)                            \
    do {                                                                \
        opal_value_t *kv;                                               \
        if (OPAL_SUCCESS != ((r) = opal_pmix.get(&(p)->proc_name,       \
                                                 (s), &kv))) {          \
            OPAL_ERROR_LOG((r));                                        \
        } else {                                                        \
            (r) = opal_value_unload(kv, (void**)(d), (t));              \
            OBJ_RELEASE(kv);                                            \
        }                                                               \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process:
 *
 * r - the integer return status from the modex op (int)
 * s - string key (char*)
 * p - pointer to the opal_proc_t of the proc that posted
 *     the data (opal_proc_t*)
 * d - pointer to a location wherein the data object
 *     it to be returned (char**)
 * sz - pointer to a location wherein the number of bytes
 *     in the data object can be returned (size_t)
 */
#define OPAL_MODEX_RECV_STRING(r, s, p, d, sz)                          \
    do {                                                                \
        opal_value_t *kv;                                               \
        if (OPAL_SUCCESS != ((r) = opal_pmix.get(&(p)->proc_name,       \
                                                 (s), &kv))) {          \
            OPAL_ERROR_LOG((r));                                        \
        } else {                                                        \
            *(d) = kv->data.bo.bytes;                                   \
            *(sz) = kv->data.bo.size;                                   \
            kv->data.bo.bytes = NULL; /* protect the data */            \
            OBJ_RELEASE(kv);                                            \
        }                                                               \
    } while(0);

/**
 * Provide a simplified macro for retrieving modex data
 * from another process:
 *
 * r - the integer return status from the modex op (int)
 * s - the MCA component that posted the data (mca_base_component_t*)
 * p - pointer to the opal_proc_t of the proc that posted
 *     the data (opal_proc_t*)
 * d - pointer to a location wherein the data object
 *     it to be returned (char**)
 * sz - pointer to a location wherein the number of bytes
 *     in the data object can be returned (size_t)
 */
#define OPAL_MODEX_RECV(r, s, p, d, sz)                                 \
    do {                                                                \
        char *key;                                                      \
        key = mca_base_component_to_string((s));                        \
        if (NULL == key) {                                              \
            OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);                   \
            (r) = OPAL_ERR_OUT_OF_RESOURCE;                             \
        } else {                                                        \
            OPAL_MODEX_RECV_STRING((r), key, (p), (d), (sz));           \
            free(key);                                                  \
        }                                                               \
    } while(0);


/****    DEFINE THE PUBLIC API'S                          ****
 ****    NOTE THAT WE DO NOT HAVE A 1:1 MAPPING OF APIs   ****
 ****    HERE TO THOSE CURRENTLY DEFINED BY PMI AS WE     ****
 ****    DON'T USE SOME OF THOSE FUNCTIONS AND THIS ISN'T ****
 ****    A GENERAL LIBRARY                                ****/

/*****  APIs CURRENTLY USED IN THE OMPI/ORTE CODE BASE   ****/
/* NOTE: calls to these APIs must be thread-protected as there
 * currently is NO internal thread safety. */

/* Init */
typedef int (*opal_pmix_base_module_init_fn_t)(void);

/* Finalize */
typedef int (*opal_pmix_base_module_fini_fn_t)(void);

/* Initialized */
typedef bool (*opal_pmix_base_module_initialized_fn_t)(void);

/* Abort */
typedef int (*opal_pmix_base_module_abort_fn_t)(int flag, const char msg[]);

/* Get_Jobid */
typedef int (*opal_pmix_base_module_get_jobid_fn_t)(char jobId[], int jobIdSize);

/* Get_Rank */
typedef int (*opal_pmix_base_module_get_rank_fn_t)(int *rank);

/* Get_Size -  note that this API has been modified from the current PMI standard to
 * reflect the proposed PMIx extensions. Return the number of processes in this
 * job within the specified scope */
typedef int (*opal_pmix_base_module_get_size_fn_t)(opal_pmix_scope_t scope, int *size);

/* Get_appnum - return the app_context id for this process */
typedef int (*opal_pmix_base_module_get_appnum_fn_t)(int *appnum);

/* Fence - note that this call is required to commit any
 * data "put" to the system since the last call to "fence"
 * prior to (or as part of) executing the barrier. Serves both PMI2
 * and PMI1 "barrier" purposes */
typedef int (*opal_pmix_base_module_fence_fn_t)(void);

/* Fence_nb - not included in the current PMI standard. This is a non-blocking
 * version of the standard "fence" call. All subsequent "get" calls will block
 * pending completion of this operation. Non-blocking "get" calls will still
 * complete as data becomes available */
typedef int (*opal_pmix_base_module_fence_nb_fn_t)(opal_pmix_cbfunc_t cbfunc, void *cbdata);

/* Put - note that this API has been modified from the current PMI standard to
 * reflect the proposed PMIx extensions. */
typedef int (*opal_pmix_base_module_put_fn_t)(opal_pmix_scope_t scope,
                                              opal_value_t *kv);

/* Get - note that this API has been modified from the current PMI standard to
 * reflect the proposed PMIx extensions, and to include the process identifier so
 * we can form the PMI key within the active component instead of sprinkling that
 * code all over the code base. */
typedef int (*opal_pmix_base_module_get_fn_t)(const opal_identifier_t *id,
                                             const char *key,
                                             opal_value_t **kv);

/* Get_nb - not included in the current PMI standard. This is a non-blocking
 * version of the standard "get" call. Retrieved value will be provided as
 * opal_value_t object in the callback. We include the process identifier so
 * we can form the PMI key within the active component instead of sprinkling that
 * code all over the code base. */
typedef void (*opal_pmix_base_module_get_nb_fn_t)(const opal_identifier_t *id,
                                                 const char *key,
                                                 opal_pmix_cbfunc_t cbfunc,
                                                 void *cbdata);

/* Publish - the "info" parameter
 * consists of a list of pmix_info_t objects */
typedef int (*opal_pmix_base_module_publish_fn_t)(const char service_name[],
                                                  opal_list_t *info,
                                                  const char port[]);

/* Lookup - the "info" parameter
 * consists of a list of pmix_info_t objects */
typedef int (*opal_pmix_base_module_lookup_fn_t)(const char service_name[],
                                                 opal_list_t *info,
                                                 char port[], int portLen);

/* Unpublish - the "info" parameter
 * consists of a list of pmix_info_t objects */
typedef int (*opal_pmix_base_module_unpublish_fn_t)(const char service_name[], 
                                                    opal_list_t *info);

/* Not an official PMI API, but something we use. Unfortunately, the calls required to
 * retrieve the necessary info are not common between the different versions */
typedef int (*opal_pmix_base_module_get_local_info_fn_t)(int vpid, int **ranks_ret,
                                                        int *procs_ret, char **error);


/****   APIs NOT CURRENTLY USED IN THE OMPI/ORTE CODE BASE, BUT THAT  ****
 ****   MAY BE IMPLEMENTED IN THE NEAR FUTURE. COMPONENTS ARE FREE TO ****
 ****   JUST HAVE THEM RETURN "OPAL_ERR_NOT_IMPLEMENTED"              ****/

/* PMI2_Job_Spawn */
typedef int (*opal_pmix_base_module_spawn_fn_t)(int count, const char * cmds[],
                                               int argcs[], const char ** argvs[],
                                               const int maxprocs[],
                                               opal_list_t *info_keyval_vector,
                                               opal_list_t *preput_keyval_vector,
                                               char jobId[], int jobIdSize,
                                               int errors[]);

/* PMI2_Job_Connect */
typedef int (*opal_pmix_base_module_job_connect_fn_t)(const char jobId[]);

/* PMI2_Job_Disconnect */
typedef int (*opal_pmix_base_module_job_disconnect_fn_t)(const char jobId[]);



/*
 * the standard public API data structure
 */
typedef struct {
    /* currently used APIs */
    opal_pmix_base_module_init_fn_t                   init;
    opal_pmix_base_module_fini_fn_t                   finalize;
    opal_pmix_base_module_initialized_fn_t            initialized;
    opal_pmix_base_module_abort_fn_t                  abort;
    opal_pmix_base_module_get_jobid_fn_t              get_jobid;
    opal_pmix_base_module_get_rank_fn_t               get_rank;
    opal_pmix_base_module_get_size_fn_t               get_size;
    opal_pmix_base_module_get_appnum_fn_t             get_appnum;
    opal_pmix_base_module_fence_fn_t                  fence;
    opal_pmix_base_module_fence_nb_fn_t               fence_nb;
    opal_pmix_base_module_put_fn_t                    put;
    opal_pmix_base_module_get_fn_t                    get;
    opal_pmix_base_module_get_nb_fn_t                 get_nb;
    opal_pmix_base_module_publish_fn_t                publish;
    opal_pmix_base_module_lookup_fn_t                 lookup;
    opal_pmix_base_module_unpublish_fn_t              unpublish;
    opal_pmix_base_module_get_local_info_fn_t         get_local_info;
    /* currently unused APIs */
    opal_pmix_base_module_spawn_fn_t                  spawn;
    opal_pmix_base_module_job_connect_fn_t            job_connect;
    opal_pmix_base_module_job_disconnect_fn_t         job_disconnect;
} opal_pmix_base_module_t;

typedef struct {
    mca_base_component_t                      base_version;
    mca_base_component_data_t                 base_data;
    int priority;
} opal_pmix_base_component_t;

/*
 * Macro for use in components that are of type pmix
 */
#define OPAL_PMIX_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "pmix", 2, 0, 0

/* Global structure for accessing store functions */
OPAL_DECLSPEC extern opal_pmix_base_module_t opal_pmix;  /* holds base function pointers */

END_C_DECLS

#endif

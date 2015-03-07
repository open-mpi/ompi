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
#include "opal/util/proc.h"

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
#define PMIX_SCOPE_T OPAL_UINT8
#define PMIX_SCOPE_UNDEF  0
#define PMIX_INTERNAL     1  // data used internally only
#define PMIX_LOCAL        2  // share to procs also on this node
#define PMIX_REMOTE       3  // share with procs not on this node
#define PMIX_GLOBAL       4  // share with all procs (local + remote)

/* callback function for non-blocking operations */
typedef void (*opal_pmix_cbfunc_t)(int status, opal_value_t *kv, void *cbdata);

/* flags to indicate if the modex value being pushed into
 * the PMIx server comes from an element that is ready to
 * support async modex operations, or from one that requires
 * synchronous modex (i.e., blocking modex operation) */
#define PMIX_SYNC_REQD  true
#define PMIX_ASYNC_RDY  false

/* define a set of "standard" PMIx attributes that can
 * be queried. Implementations (and users) are free to extend as
 * desired, so the get_attr functions need to be capable
 * of handling the "not found" condition. Note that these
 * are attributes of the system and the job as opposed to
 * values the application (or underlying MPI library)
 * might choose to expose - i.e., they are values provided
 * by the resource manager as opposed to the application */
#define PMIX_ATTR_UNDEF      NULL

#define PMIX_CPUSET          "pmix.cpuset"      // (char*) hwloc bitmap applied to proc upon launch
#define PMIX_CREDENTIAL      "pmix.cred"        // (opal_byte_object*) security credential assigned to proc
#define PMIX_HOSTNAME        "pmix.hname"       // (char*) name of the host this proc is on
/* scratch directory locations for use by applications */
#define PMIX_TMPDIR          "pmix.tmpdir"      // (char*) top-level tmp dir assigned to session
/* information about relative ranks as assigned */
#define PMIX_JOBID           "pmix.jobid"       // (char*) jobid assigned by scheduler
#define PMIX_APPNUM          "pmix.appnum"      // (uint32_t) app number within the job
#define PMIX_RANK            "pmix.rank"        // (uint32_t) process rank within the job
#define PMIX_GLOBAL_RANK     "pmix.grank"       // (uint32_t) rank spanning across all jobs in this session
#define PMIX_APP_RANK        "pmix.apprank"     // (uint32_t) rank within this app
#define PMIX_NPROC_OFFSET    "pmix.offset"      // (uint32_t) starting global rank of this job
#define PMIX_LOCAL_RANK      "pmix.lrank"       // (uint16_t) rank on this node within this job
#define PMIX_NODE_RANK       "pmix.nrank"       // (uint16_t) rank on this node spanning all jobs
#define PMIX_LOCALLDR        "pmix.lldr"        // (uint64_t) opal_identifier of lowest rank on this node within this job
#define PMIX_APPLDR          "pmix.aldr"        // (uint32_t) lowest rank in this app within this job
#define PMIX_NODE_ID         "pmix.nodeid"      // (uint32_t) vpid of daemon hosting specified proc

/* proc location-related info */
#define PMIX_PROC_MAP        "pmix.map"         // (byte_object) packed map of proc locations within this job
#define PMIX_LOCAL_PEERS     "pmix.lpeers"      // (char*) comma-delimited string of ranks on this node within this job
#define PMIX_LOCAL_CPUSETS   "pmix.lcpus"       // (byte_object) packed names and cpusets of local peers
/* size info */
#define PMIX_UNIV_SIZE       "pmix.univ.size"   // (uint32_t) #procs in this namespace
#define PMIX_JOB_SIZE        "pmix.job.size"    // (uint32_t) #procs in this job
#define PMIX_LOCAL_SIZE      "pmix.local.size"  // (uint32_t) #procs in this job on this node
#define PMIX_NODE_SIZE       "pmix.node.size"   // (uint32_t) #procs across all jobs on this node
#define PMIX_MAX_PROCS       "pmix.max.size"    // (uint32_t) max #procs for this job
/* topology info */
#define PMIX_NET_TOPO        "pmix.ntopo"       // (byte_object) network topology
#define PMIX_LOCAL_TOPO      "pmix.ltopo"       // (hwloc topo) local node topology

/**
 * Provide a simplified macro for sending data via modex
 * to other processes. The macro requires four arguments:
 *
 * r - the integer return status from the modex op
 * f - whether this modex requires sync or is async ready
 * sc - the PMIX scope of the data
 * s - the key to tag the data being posted
 * d - pointer to the data object being posted
 * t - the type of the data
 */
#define OPAL_MODEX_SEND_VALUE(r, f, sc, s, d, t)                         \
    do {                                                                \
        opal_value_t kv;                                                \
        if (PMIX_SYNC_REQD == (f)) {                                    \
            opal_pmix_use_collective = true;                            \
        }                                                               \
        OBJ_CONSTRUCT(&kv, opal_value_t);                               \
        kv.key = (s);                                                   \
        if (OPAL_SUCCESS != ((r) = opal_value_load(&kv, (d), (t)))) {   \
            OPAL_ERROR_LOG((r));                                        \
        } else {                                                        \
            if (OPAL_SUCCESS != ((r) = opal_pmix.put(sc, &kv))) {       \
                OPAL_ERROR_LOG((r));                                    \
            }                                                           \
        }                                                               \
        /* do not destruct the keyval as we don't own */                \
        /* the data - the caller will take care of the */               \
        /* key and value storage, and the kv itself has none */         \
    } while(0);

/**
 * Provide a simplified macro for sending data via modex
 * to other processes. The macro requires four arguments:
 *
 * r - the integer return status from the modex op
 * f - whether this modex requires sync or is async ready
 * sc - the PMIX scope of the data
 * s - the key to tag the data being posted
 * d - the data object being posted
 * sz - the number of bytes in the data object
 */
#define OPAL_MODEX_SEND_STRING(r, f, sc, s, d, sz)              \
    do {                                                        \
        opal_value_t kv;                                        \
        if (PMIX_SYNC_REQD == (f)) {                            \
            opal_pmix_use_collective = true;                    \
        }                                                       \
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
 * f - whether this modex requires sync or is async ready
 * sc - the PMIX scope of the data
 * s - the MCA component that is posting the data
 * d - the data object being posted
 * sz - the number of bytes in the data object
 */
#define OPAL_MODEX_SEND(r, f, sc, s, d, sz)                     \
    do {                                                        \
        char *key;                                              \
        if (PMIX_SYNC_REQD == (f)) {                            \
            opal_pmix_use_collective = true;                    \
        }                                                       \
        key = mca_base_component_to_string((s));                \
        OPAL_MODEX_SEND_STRING((r), (f), (sc), key, (d), (sz)); \
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
 *     is to be returned
 * t - the expected data type
 */
#define OPAL_MODEX_RECV_VALUE(r, s, p, d, t)                            \
    do {                                                                \
        opal_value_t *kv;                                               \
        if (OPAL_SUCCESS != ((r) = opal_pmix.get(&(p)->proc_name,       \
                                                 (s), &kv))) {          \
            *(d) = NULL;                                                \
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
        if (OPAL_SUCCESS == ((r) = opal_pmix.get(&(p)->proc_name,       \
                                                 (s), &kv)) &&          \
            NULL != kv) {                                               \
            *(d) = kv->data.bo.bytes;                                   \
            *(sz) = kv->data.bo.size;                                   \
            kv->data.bo.bytes = NULL; /* protect the data */            \
            OBJ_RELEASE(kv);                                            \
        } else {                                                        \
            *(d) = NULL;                                                \
            *(sz) = 0;                                                  \
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


/**
 * Provide a simplified macro for calling the fence function
 * that takes into account directives and availability of
 * non-blocking operations
 */
#define OPAL_FENCE(p, s, cf, cd)                                        \
    do {                                                                \
        if (opal_pmix_use_collective || NULL == opal_pmix.fence_nb) {   \
            opal_pmix.fence((p), (s));                                  \
        } else {                                                        \
            opal_pmix.fence_nb((p), (s), (cf), (cd));                   \
        }                                                               \
    } while(0);

/* callback handler for errors */
typedef void (*opal_pmix_errhandler_fn_t)(int error);

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

/* Fence - note that this call is required to commit any
 * data "put" to the system since the last call to "fence"
 * prior to (or as part of) executing the barrier. Serves both PMI2
 * and PMI1 "barrier" purposes */
typedef int (*opal_pmix_base_module_fence_fn_t)(opal_process_name_t *procs, size_t nprocs);

/* Fence_nb - not included in the current PMI standard. This is a non-blocking
 * version of the standard "fence" call. All subsequent "get" calls will block
 * pending completion of this operation. Non-blocking "get" calls will still
 * complete as data becomes available */
typedef int (*opal_pmix_base_module_fence_nb_fn_t)(opal_process_name_t *procs, size_t nprocs,
                                                   opal_pmix_cbfunc_t cbfunc, void *cbdata);

/* Put - note that this API has been modified from the current PMI standard to
 * reflect the proposed PMIx extensions. */
typedef int (*opal_pmix_base_module_put_fn_t)(opal_pmix_scope_t scope,
                                              opal_value_t *kv);

/* Get - note that this API has been modified from the current PMI standard to
 * reflect the proposed PMIx extensions, and to include the process identifier so
 * we can form the PMI key within the active component instead of sprinkling that
 * code all over the code base. */
typedef int (*opal_pmix_base_module_get_fn_t)(const opal_process_name_t *id,
                                              const char *key,
                                              opal_value_t **kv);

/* Get_nb - not included in the current PMI standard. This is a non-blocking
 * version of the standard "get" call. Retrieved value will be provided as
 * opal_value_t object in the callback. We include the process identifier so
 * we can form the PMI key within the active component instead of sprinkling that
 * code all over the code base. */
typedef void (*opal_pmix_base_module_get_nb_fn_t)(const opal_process_name_t *id,
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

/* Get attribute
 * Query the server for the specified attribute, returning it in the
 * provided opal_value_t. The function will return "true" if the attribute
 * is found, and "false" if not.
 * Attributes are provided by the PMIx server, so there is no corresponding
 * "put" function. */
typedef bool (*opal_pmix_base_module_get_attr_fn_t)(const char *attr, opal_value_t **kv);

/* Get attribute (non-blocking)
 * Query the server for the specified attribute..
 * Attributes are provided by the PMIx server, so there is no corresponding "put"
 * function. The call will be executed as non-blocking, returning immediately,
 * with data resulting from the call returned in the callback function. A returned
 * NULL opal_value_t* indicates that the attribute was not found. The returned
 * pointer is "owned" by the PMIx module and must not be released by the
 * callback function */
typedef int (*opal_pmix_base_module_get_attr_nb_fn_t)(const char *attr,
                                                      opal_pmix_cbfunc_t cbfunc,
                                                      void *cbdata);


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


/* register an errhandler to report loss of connection to the server */
typedef void (*opal_pmix_base_module_register_fn_t)(opal_pmix_errhandler_fn_t errhandler);

/* deregister the errhandler */
typedef void (*opal_pmix_base_module_deregister_fn_t)(void);

/*
 * the standard public API data structure
 */
typedef struct {
    /* currently used APIs */
    opal_pmix_base_module_init_fn_t                   init;
    opal_pmix_base_module_fini_fn_t                   finalize;
    opal_pmix_base_module_initialized_fn_t            initialized;
    opal_pmix_base_module_abort_fn_t                  abort;
    opal_pmix_base_module_fence_fn_t                  fence;
    opal_pmix_base_module_fence_nb_fn_t               fence_nb;
    opal_pmix_base_module_put_fn_t                    put;
    opal_pmix_base_module_get_fn_t                    get;
    opal_pmix_base_module_get_nb_fn_t                 get_nb;
    opal_pmix_base_module_publish_fn_t                publish;
    opal_pmix_base_module_lookup_fn_t                 lookup;
    opal_pmix_base_module_unpublish_fn_t              unpublish;
    opal_pmix_base_module_get_attr_fn_t               get_attr;
    opal_pmix_base_module_get_attr_nb_fn_t            get_attr_nb;
    /* currently unused APIs */
    opal_pmix_base_module_spawn_fn_t                  spawn;
    opal_pmix_base_module_job_connect_fn_t            job_connect;
    opal_pmix_base_module_job_disconnect_fn_t         job_disconnect;
    /* register the errhandler */
    opal_pmix_base_module_register_fn_t               register_errhandler;
    opal_pmix_base_module_deregister_fn_t             deregister_errhandler;
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

/* flag to indicate collective vs direct fence operations */
OPAL_DECLSPEC extern bool opal_pmix_use_collective;

END_C_DECLS

#endif

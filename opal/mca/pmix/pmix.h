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

#if WANT_PMI_SUPPORT
#include <pmi.h>
#endif
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/dss/dss_types.h"


BEGIN_C_DECLS

/* protect a couple of definitions that may not
 * be available in some versions of PMI, but are
 * needed to ensure pmix.h can compile */
#ifndef MPID_Info
typedef struct MPID_Info {
    uint16_t foo;
} MPID_Info;
#endif
#ifndef PMI2_Connect_comm_t
typedef struct PMI2_Connect_comm {
    uint16_t foo;
} PMI2_Connect_comm_t;
#endif

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
#define PMI_LOCAL   0x01
#define PMI_REMOTE  0x02
#define PMI_GLOBAL  0x03

/* callback function for non-blocking operations */
typedef void (*opal_pmix_cbfunc_t)(int status, opal_value_t *kv, void *cbdata);


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
 * reflect the proposed PMIx extensions, and to include the process identifier so
 * we can form the PMI key within the active component instead of sprinkling that
 * code all over the code base. */
typedef int (*opal_pmix_base_module_put_fn_t)(opal_identifier_t *id,
                                             const char key[],
                                             opal_pmix_scope_t scope,
                                             opal_value_t *kv);

/* Get - note that this API has been modified from the current PMI standard to
 * reflect the proposed PMIx extensions, and to include the process identifier so
 * we can form the PMI key within the active component instead of sprinkling that
 * code all over the code base. */
typedef int (*opal_pmix_base_module_get_fn_t)(opal_identifier_t *id,
                                             const char *key,
                                             opal_value_t *kv);

/* Get_nb - not included in the current PMI standard. This is a non-blocking
 * version of the standard "get" call. Retrieved value will be provided as
 * opal_value_t object in the callback. We include the process identifier so
 * we can form the PMI key within the active component instead of sprinkling that
 * code all over the code base. */
typedef void (*opal_pmix_base_module_get_nb_fn_t)(opal_identifier_t *id,
                                                 const char *key,
                                                 opal_pmix_cbfunc_t cbfunc,
                                                 void *cbdata);

/* Nameserv_publish - note that this API has been modified from the current PMI standard
 * to include the process identifier so we can form the PMI key within the active
 * component instead of sprinkling that code all over the code base. As this is an extension
 * of the PMI-1 Publish_name function, it serves both PMI2 and PMI1 calls */
typedef int (*opal_pmix_base_module_nameserv_publish_fn_t)(opal_identifier_t *id,
                                                          const char service_name[],
                                                          const struct MPID_Info *info_ptr,
                                                          const char port[]);

/* Nameserv_lookup - note that this API has been modified from the current PMI standard
 * to include the process identifier so we can form the PMI key within the active
 * component instead of sprinkling that code all over the code base. As this is an extension
* of the PMI-1 Publish_name function, it serves both PMI2 and PMI1 calls */
typedef int (*opal_pmix_base_module_nameserv_lookup_fn_t)(opal_identifier_t *id,
                                                         const char service_name[],
                                                         const struct MPID_Info *info_ptr,
                                                         char port[], int portLen);

/* Nameserv_unpublish - note that this API has been modified from the current PMI standard
 * to include the process identifier so we can form the PMI key within the active
 * component instead of sprinkling that code all over the code base. As this is an extension
* of the PMI-1 Unpublish_name function, it serves both PMI2 and PMI1 calls */
typedef int (*opal_pmix_base_module_nameserv_unpublish_fn_t)(opal_identifier_t *id,
                                                            const char service_name[], 
                                                            const struct MPID_Info *info_ptr);

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
                                               const int info_keyval_sizes[],
                                               const struct MPID_Info *info_keyval_vectors[],
                                               int preput_keyval_size,
                                               const struct MPID_Info *preput_keyval_vector[],
                                               char jobId[], int jobIdSize,
                                               int errors[]);

/* PMI2_Job_Connect */
typedef int (*opal_pmix_base_module_job_connect_fn_t)(const char jobId[],
                                                     PMI2_Connect_comm_t *conn);

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
    opal_pmix_base_module_nameserv_publish_fn_t       publish;
    opal_pmix_base_module_nameserv_lookup_fn_t        lookup;
    opal_pmix_base_module_nameserv_unpublish_fn_t     unpublish;
    opal_pmix_base_module_get_local_info_fn_t         get_local_info;
    /* currently unused APIs */
    opal_pmix_base_module_spawn_fn_t                  spawn;
    opal_pmix_base_module_job_connect_fn_t            job_connect;
    opal_pmix_base_module_job_disconnect_fn_t         job_disconnect;
} opal_pmix_base_module_t;

typedef struct {
    mca_base_component_t                      base_version;
    mca_base_component_data_t                 base_data;
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

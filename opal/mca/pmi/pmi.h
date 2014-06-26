/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_PMI_H
#define OPAL_PMI_H

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
 * be available in some versions of PMI */
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

/****    DEFINE THE PUBLIC API'S    ****/
/*
 * PMI_Init
 *
 * NOTE: calls to these APIs must be thread-protected as there
 * is NO internal thread safety.
 */
typedef int (*opal_pmi_base_module_init_fn_t)(void);

/*
 * PMI_Finalize
 */
typedef int (*opal_pmi_base_module_fini_fn_t)(void);

/* PMI_Initialized */
typedef bool (*opal_pmi_base_module_initialized_fn_t)(void);

/* PMI_Abort */
typedef int (*opal_pmi_base_module_abort_fn_t)(int flag, const char msg[]);

/* PMI2_Job_Spawn */
typedef int (*opal_pmi_base_module_spawn_fn_t)(int count, const char * cmds[],
                                               int argcs[], const char ** argvs[],
                                               const int maxprocs[],
                                               const int info_keyval_sizes[],
                                               const struct MPID_Info *info_keyval_vectors[],
                                               int preput_keyval_size,
                                               const struct MPID_Info *preput_keyval_vector[],
                                               char jobId[], int jobIdSize,
                                               int errors[]);

/* PMI_Get_Jobid */
typedef int (*opal_pmi_base_module_get_jobid_fn_t)(char jobId[], int jobIdSize);

/* PMI_Get_Rank */
typedef int (*opal_pmi_base_module_get_rank_fn_t)(int *rank);

/* PMI GetSize */
typedef int (*opal_pmi_base_module_get_size_fn_t)(int *size);

/* PMI2_Job_Connect */
typedef int (*opal_pmi_base_module_job_connect_fn_t)(const char jobId[],
                                                     PMI2_Connect_comm_t *conn);

/* PMI2_Job_Disconnect */
typedef int (*opal_pmi_base_module_job_disconnect_fn_t)(const char jobId[]);

/* Put */
typedef int (*opal_pmi_base_module_put_fn_t)(const char key[], const char value[]);

/* Fence */
typedef int (*opal_pmi_base_module_fence_fn_t)(void);

/* Get */
typedef int (*opal_pmi_base_module_get_fn_t)(const char *jobid,
                                             int src_pmi_id,
                                             const char key[],
                                             char value [],
                                             int maxvalue,
                                             int *vallen);

/* GetNodeAttr */
typedef int (*opal_pmi_base_module_get_node_attr_fn_t)(const char name[],
                                                       char value[],
                                                       int valuelen,
                                                       int *found,
                                                       int waitfor);

/* GetNodeAttrIntArray */
typedef int (*opal_pmi_base_module_get_node_attr_array_fn_t)(const char name[],
                                                             int array[],
                                                             int arraylen,
                                                             int *outlen,
                                                             int *found);

/* PutNodeAttr */
typedef int (*opal_pmi_base_module_put_node_attr_fn_t)(const char name[], const char value[]);

/* GetJobAttr */
typedef int (*opal_pmi_base_module_get_job_attr_fn_t)(const char name[],
                                                      char value[],
                                                      int valuelen,
                                                      int *found);

/* GetJobAttrArray */
typedef int (*opal_pmi_base_module_get_job_attr_array_fn_t)(const char name[],
                                                            int array[],
                                                            int arraylen,
                                                            int *outlen,
                                                            int *found);

/* Nameserv_publish */
typedef int (*opal_pmi_base_module_nameserv_publish_fn_t)(const char service_name[],
                                                          const struct MPID_Info *info_ptr,
                                                          const char port[]);

/* Nameserv_lookup */
typedef int (*opal_pmi_base_module_nameserv_lookup_fn_t)(const char service_name[],
                                                         const struct MPID_Info *info_ptr,
                                                         char port[], int portLen);

/* Nameserv_unpublish */
typedef int (*opal_pmi_base_module_nameserv_unpublish_fn_t)(const char service_name[], 
                                                            const struct MPID_Info *info_ptr);

/*
 * the standard public API data structure
 */
typedef struct {
    opal_pmi_base_module_init_fn_t                   init;
    opal_pmi_base_module_fini_fn_t                   finalize;
    opal_pmi_base_module_initialized_fn_t            initialized;
    opal_pmi_base_module_abort_fn_t                  abort;
    opal_pmi_base_module_spawn_fn_t                  spawn;
    opal_pmi_base_module_get_jobid_fn_t              get_jobid;
    opal_pmi_base_module_get_rank_fn_t               get_rank;
    opal_pmi_base_module_get_size_fn_t               get_size;
    opal_pmi_base_module_job_connect_fn_t            job_connect;
    opal_pmi_base_module_job_disconnect_fn_t         job_disconnect;
    opal_pmi_base_module_put_fn_t                    put;
    opal_pmi_base_module_fence_fn_t                  fence;
    opal_pmi_base_module_get_fn_t                    get;
    opal_pmi_base_module_get_node_attr_fn_t          get_node_attr;
    opal_pmi_base_module_get_node_attr_array_fn_t    get_node_attr_array;
    opal_pmi_base_module_put_node_attr_fn_t          put_node_attr;
    opal_pmi_base_module_get_job_attr_fn_t           get_job_attr;
    opal_pmi_base_module_get_job_attr_array_fn_t     get_job_attr_array;
    opal_pmi_base_module_nameserv_publish_fn_t       publish;
    opal_pmi_base_module_nameserv_lookup_fn_t        lookup;
    opal_pmi_base_module_nameserv_unpublish_fn_t     unpublish;
} opal_pmi_base_module_t;

typedef struct {
    mca_base_component_t                      base_version;
    mca_base_component_data_t                 base_data;
} opal_pmi_base_component_t;

/*
 * Macro for use in components that are of type pmi
 */
#define OPAL_PMI_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "pmi", 2, 0, 0

/* Global structure for accessing store functions */
OPAL_DECLSPEC extern opal_pmi_base_module_t opal_pmi;  /* holds base function pointers */

END_C_DECLS

#endif

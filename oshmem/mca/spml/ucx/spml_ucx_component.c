/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#define _GNU_SOURCE
#include <stdio.h>

#include <sys/types.h>
#include <unistd.h>

#include "oshmem_config.h"
#include "orte/util/show_help.h"
#include "shmem.h"
#include "oshmem/runtime/params.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"
#include "spml_ucx_component.h"
#include "oshmem/mca/spml/ucx/spml_ucx.h"

#include "orte/util/show_help.h"
#include "opal/util/opal_environ.h"

static int mca_spml_ucx_component_register(void);
static int mca_spml_ucx_component_open(void);
static int mca_spml_ucx_component_close(void);
static mca_spml_base_module_t*
mca_spml_ucx_component_init(int* priority,
                              bool enable_progress_threads,
                              bool enable_mpi_threads);
static int mca_spml_ucx_component_fini(void);
mca_spml_base_component_2_0_0_t mca_spml_ucx_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      MCA_SPML_BASE_VERSION_2_0_0,
    
      "ucx",                        /* MCA component name */
      OSHMEM_MAJOR_VERSION,           /* MCA component major version */
      OSHMEM_MINOR_VERSION,           /* MCA component minor version */
      OSHMEM_RELEASE_VERSION,         /* MCA component release version */
      mca_spml_ucx_component_open,  /* component open */
      mca_spml_ucx_component_close, /* component close */
      NULL,
      mca_spml_ucx_component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    mca_spml_ucx_component_init,    /* component init */
    mca_spml_ucx_component_fini     /* component finalize */
    
};


static inline void mca_spml_ucx_param_register_int(const char* param_name,
                                                    int default_value,
                                                    const char *help_msg,
                                                    int *storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_spml_ucx_component.spmlm_version,
                                           param_name,
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           storage);
}

static inline void  mca_spml_ucx_param_register_string(const char* param_name,
                                                    char* default_value,
                                                    const char *help_msg,
                                                    char **storage)
{
    *storage = default_value;
    (void) mca_base_component_var_register(&mca_spml_ucx_component.spmlm_version,
                                           param_name,
                                           help_msg,
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           storage);
}

static int mca_spml_ucx_component_register(void)
{
    mca_spml_ucx_param_register_int("priority", 5,
                                      "[integer] ucx priority",
                                      &mca_spml_ucx.priority);

    return OSHMEM_SUCCESS;
}

int spml_ucx_progress(void)
{
    ucp_worker_progress(mca_spml_ucx.ucp_worker);
    return 1;
}

static int mca_spml_ucx_component_open(void)
{
    ucs_status_t err;
    ucp_config_t *ucp_config;
    ucp_params_t params;

    err = ucp_config_read("OSHMEM", NULL, &ucp_config);
    if (UCS_OK != err) {
        return OSHMEM_ERROR;
    }

    memset(&params, 0, sizeof(params));
    params.field_mask = UCP_PARAM_FIELD_FEATURES;
    params.features   = UCP_FEATURE_RMA|UCP_FEATURE_AMO32|UCP_FEATURE_AMO64;
    err = ucp_init(&params, ucp_config, &mca_spml_ucx.ucp_context);
    ucp_config_release(ucp_config);
    if (UCS_OK != err) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

static int mca_spml_ucx_component_close(void)
{
    ucp_cleanup(mca_spml_ucx.ucp_context);
    return OSHMEM_SUCCESS;
}

static int spml_ucx_init(void)
{
    ucp_worker_params_t params;
    ucs_status_t err;

    params.field_mask  = UCP_WORKER_PARAM_FIELD_THREAD_MODE;
    params.thread_mode = UCS_THREAD_MODE_SINGLE;

    err = ucp_worker_create(mca_spml_ucx.ucp_context, &params,
                            &mca_spml_ucx.ucp_worker);
    if (UCS_OK != err) {
        return OSHMEM_ERROR;
    }

    return OSHMEM_SUCCESS;
}

static mca_spml_base_module_t*
mca_spml_ucx_component_init(int* priority,
                              bool enable_progress_threads,
                              bool enable_mpi_threads)
{
    SPML_VERBOSE( 10, "in ucx, my priority is %d\n", mca_spml_ucx.priority);

    if ((*priority) > mca_spml_ucx.priority) {
        *priority = mca_spml_ucx.priority;
        return NULL ;
    }
    *priority = mca_spml_ucx.priority;

    if (OSHMEM_SUCCESS != spml_ucx_init())
        return NULL ;

    SPML_VERBOSE(50, "*** ucx initialized ****");
    return &mca_spml_ucx.super;
}

static int mca_spml_ucx_component_fini(void)
{
    opal_progress_unregister(spml_ucx_progress);
        
    if (mca_spml_ucx.ucp_worker) {
        ucp_worker_destroy(mca_spml_ucx.ucp_worker);
    }
    if(!mca_spml_ucx.enabled)
        return OSHMEM_SUCCESS; /* never selected.. return success.. */

    mca_spml_ucx.enabled = false;  /* not anymore */
    return OSHMEM_SUCCESS;
}


/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All Rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <dlfcn.h>

#include "opal/mca/dl/base/base.h"
#include "opal/runtime/opal_params.h"
#include "accelerator_ze.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal_config.h"
#include "opal/util/argv.h"
#include "opal/util/printf.h"
#include "opal/util/output.h"                


int opal_accelerator_ze_memcpy_async = 1;
int opal_accelerator_ze_verbose = 0;
uint32_t opal_accelerator_ze_device_count = 0;
ze_device_handle_t *opal_accelerator_ze_devices_handle = NULL;
ze_driver_handle_t opal_accelerator_ze_driver_handle = NULL;
ze_context_handle_t opal_accelerator_ze_context = NULL;
ze_event_pool_handle_t opal_accelerator_ze_event_pool = NULL;
opal_accelerator_stream_t **opal_accelerator_ze_MemcpyStream = NULL;

/* Initialization lock for lazy ze initialization */
static opal_mutex_t accelerator_ze_init_lock;
static bool accelerator_ze_init_complete = false;

/*
 * Public string showing the accelerator ze component version number
 */
const char *opal_accelerator_ze_component_version_string
    = "OPAL ze accelerator MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int accelerator_ze_open(void);
static int accelerator_ze_close(void);
static int accelerator_ze_component_register(void);
static opal_accelerator_base_module_t* accelerator_ze_init(void);
static void accelerator_ze_finalize(opal_accelerator_base_module_t* module);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_accelerator_ze_component_t mca_accelerator_ze_component = {{

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    .base_version =
        {
            /* Indicate that we are a accelerator v1.1.0 component (which also
             * implies a specific MCA version) */

            OPAL_ACCELERATOR_BASE_VERSION_1_0_0,

            /* Component name and version */

            .mca_component_name = "ze",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */

            .mca_open_component = accelerator_ze_open,
            .mca_close_component = accelerator_ze_close,
            .mca_register_component_params = accelerator_ze_component_register,

        },
    /* Next the MCA v1.0.0 component meta data */
    .base_data =
        { /* The component is checkpoint ready */
         MCA_BASE_METADATA_PARAM_CHECKPOINT},
    .accelerator_init = accelerator_ze_init,
    .accelerator_finalize = accelerator_ze_finalize,
}};

static int accelerator_ze_open(void)
{
    /* construct the component fields */

    return OPAL_SUCCESS;
}

static int accelerator_ze_close(void)
{
    return OPAL_SUCCESS;
}

static int accelerator_ze_component_register(void)
{
    /* Set verbosity in the ze related code. */
    opal_accelerator_ze_verbose = 0;
    (void) mca_base_var_register("ompi", "mpi", "accelerator_ze", "verbose",
                                 "Set level of ze verbosity", MCA_BASE_VAR_TYPE_INT, NULL,
                                 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_accelerator_ze_verbose);

    return OPAL_SUCCESS;
}

/*
 * If this method is invoked it means we already
 * initialized ZE in the accelerator_ze_init method below
 */

int opal_accelerator_ze_lazy_init(void)
{
    uint32_t i,d;
    int err = OPAL_SUCCESS;
    ze_result_t zret;
    uint32_t driver_count = 0;
    ze_driver_handle_t *all_drivers = NULL;

    /* Double checked locking to avoid having to
     * grab locks post lazy-initialization.  */

    opal_atomic_rmb();
    if (true == accelerator_ze_init_complete) {
        return OPAL_SUCCESS;
    }
    OPAL_THREAD_LOCK(&accelerator_ze_init_lock);

    /* If already initialized, just exit */
    if (true == accelerator_ze_init_complete) {
        goto fn_fail;
    }

    opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                       "ZE: starting lazy init");

    zret = zeDriverGet(&driver_count, NULL);
    if (ZE_RESULT_SUCCESS != zret) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "ZE: zeDriverGet returned %d\n", zret);
        err =  OPAL_ERR_NOT_INITIALIZED;
        goto fn_fail;
    }

    /*
     * driver count should not be zero as to get here ZE component
     * was successfully init'd.
     */
    if (0 == driver_count) {
        err = OPAL_ERR_NOT_FOUND;
    }

    all_drivers = (ze_driver_handle_t *)malloc(driver_count * sizeof(ze_driver_handle_t));
    if (all_drivers == NULL) {
        err = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_fail;
    }

    zret = zeDriverGet(&driver_count, all_drivers);
    if (ZE_RESULT_SUCCESS != zret) {
        err = OPAL_ERR_NOT_FOUND;
        goto fn_fail;
    }

    /*
     * Current design of ZE component assumes we find the first driver with a GPU device.
     * we'll create a single ZE context if we do find such a device.  This may need to
     * be revisited at some point but would impact areas of code outside of the 
     * accelerator framework.
     */

    for (i = 0; i < driver_count; ++i) {
        opal_accelerator_ze_device_count = 0;
        zret = zeDeviceGet(all_drivers[i], &opal_accelerator_ze_device_count, NULL);
        if (ZE_RESULT_SUCCESS != zret) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                               "ZE: zeDeviceGet returned %d\n", zret);
            err = OPAL_ERROR;
            goto fn_fail;
        }
        opal_accelerator_ze_devices_handle =
            malloc(opal_accelerator_ze_device_count * sizeof(ze_device_handle_t));
        if (NULL == opal_accelerator_ze_devices_handle) {
            err = OPAL_ERR_OUT_OF_RESOURCE;
            goto fn_fail;
        }
        zret = zeDeviceGet(all_drivers[i], &opal_accelerator_ze_device_count, opal_accelerator_ze_devices_handle);
        if (ZE_RESULT_SUCCESS != zret) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                               "ZE: zeDeviceGet returned %d\n", zret);
            err = OPAL_ERROR;
            goto fn_fail;
        }
        /* Check if the driver supports a gpu */
        for (d = 0; d < opal_accelerator_ze_device_count; ++d) {
            ze_device_properties_t device_properties;
            zret = zeDeviceGetProperties(opal_accelerator_ze_devices_handle[d], &device_properties);
            if (ZE_RESULT_SUCCESS != zret) {
                opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                   "ZE: zeDeviceGetProperties returned %d\n", zret);
                err = OPAL_ERROR;
                goto fn_fail;
            }

            if (ZE_DEVICE_TYPE_GPU == device_properties.type) {
                opal_accelerator_ze_driver_handle = all_drivers[i];
                break;
            }
        }

        if (NULL != opal_accelerator_ze_driver_handle) {
            break;
        } else {
            free(opal_accelerator_ze_devices_handle);
            opal_accelerator_ze_devices_handle = NULL;
        }
    }

    ze_context_desc_t contextDesc = {
        .stype = ZE_STRUCTURE_TYPE_CONTEXT_DESC,
        .pNext = NULL,
        .flags = 0,
    };
    zret = zeContextCreate(opal_accelerator_ze_driver_handle, 
                          &contextDesc, 
                          &opal_accelerator_ze_context);
    if (ZE_RESULT_SUCCESS != zret) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                           "ZE: zeContextCreate returned %d\n", zret);
        err = OPAL_ERROR;
        goto fn_fail;
    }

    /*
     * allocate synchronous memcpy stream handles, but delay creating streams till needed
     */

    opal_accelerator_ze_MemcpyStream = (opal_accelerator_stream_t **)calloc((size_t)opal_accelerator_ze_device_count,
                                                                            sizeof(opal_accelerator_stream_t *));
    if (NULL == opal_accelerator_ze_MemcpyStream) {
        err = OPAL_ERR_OUT_OF_RESOURCE;
        goto fn_fail;
    }

    /*
     * set up an event pool
     */

    ze_event_pool_desc_t eventPoolDesc = {
            .stype = ZE_STRUCTURE_TYPE_EVENT_POOL_DESC,
            .pNext = NULL,
            .flags = 0,
            .count = 1000,  /* TODO: fix this! */
    };

    /*
     * create an event pool that can be used by all devices associated with this ze context
     */
    zret = zeEventPoolCreate(opal_accelerator_ze_context,
                             &eventPoolDesc,
                             0,
                             NULL,
                             &opal_accelerator_ze_event_pool);
    if (ZE_RESULT_SUCCESS != zret) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                           "ZE: zeEventPoolCreate returned %d\n", zret);
        err = OPAL_ERROR;
        goto fn_fail;
    }

    opal_atomic_wmb();
    opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                       "ZE: found %d devices", opal_accelerator_ze_device_count);
    accelerator_ze_init_complete = true;

    return OPAL_SUCCESS;

fn_fail:
    if (NULL != opal_accelerator_ze_MemcpyStream) {
        free(opal_accelerator_ze_MemcpyStream);
        opal_accelerator_ze_MemcpyStream = NULL;
    }

    if (NULL != all_drivers) {
        free(all_drivers);
    }

    if (OPAL_SUCCESS != err)  {
        free(opal_accelerator_ze_devices_handle);
        opal_accelerator_ze_devices_handle = NULL;
    }
      
    OPAL_THREAD_UNLOCK(&accelerator_ze_init_lock);
    return err;
}

static opal_accelerator_base_module_t* accelerator_ze_init(void)
{
    uint32_t driver_count=0;
    ze_result_t zret;
    ze_init_flag_t flags = ZE_INIT_FLAG_GPU_ONLY;

    OBJ_CONSTRUCT(&accelerator_ze_init_lock, opal_mutex_t);

    if (opal_ze_runtime_initialized) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output, "ZE: runtime not initialized");
        return NULL;
    }

    /* 
     * Initialize ze, this function can be called multiple times
     */

    zret = zeInit(flags);
    if (ZE_RESULT_SUCCESS != zret) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output, "ZE: zeInit returned %d flags = %d\n", zret, flags);
        return NULL;
    }

    /*
     * zeDriverGet can return:
     * ZE_RESULT_SUCCESS
     * ZE_RESULT_ERROR_UNINITIALIZED
     * ZE_RESULT_ERROR_DEVICE_LOST
     * ZE_RESULT_ERROR_OUT_OF_HOST_MEMORY
     * ZE_RESULT_ERROR_OUT_OF_DEVICE_MEMORY
     */
    zret = zeDriverGet(&driver_count, NULL);
    if (ZE_RESULT_SUCCESS != zret || 0 == driver_count) {
        if (ZE_RESULT_SUCCESS != zret) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output, 
                               "ZE: zeDriverGet returned %d\n", zret);
        } else {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output, 
                               "ZE: no device drivers found\n");
        }
        return NULL;
    } else {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output, 
                                   "ZE: %d device drivers found\n", driver_count);
    }

    opal_atomic_mb();
    opal_ze_runtime_initialized = true;

    return &opal_accelerator_ze_module;
}

static void accelerator_ze_finalize(opal_accelerator_base_module_t* module)
{
    ze_result_t zret;

    opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "ZE: finalizing component\n");

    if (NULL != opal_accelerator_ze_event_pool) {
        zret = zeEventPoolDestroy(opal_accelerator_ze_event_pool);
        if (ZE_RESULT_SUCCESS != zret) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "zeEventPoolDestroy returned %d", zret);
        }
        opal_accelerator_ze_event_pool = NULL;
    }


    if (NULL != opal_accelerator_ze_MemcpyStream) {
        for (uint32_t i = 0; i < opal_accelerator_ze_device_count; i++) {
            if (NULL != opal_accelerator_ze_MemcpyStream[i]) {
                OBJ_RELEASE(opal_accelerator_ze_MemcpyStream[i]);
            }
        }
        free(opal_accelerator_ze_MemcpyStream);
        opal_accelerator_ze_MemcpyStream = NULL;
    }

    if (NULL != (void *)opal_accelerator_ze_context) {
        zret = zeContextDestroy(opal_accelerator_ze_context);
        if (ZE_RESULT_SUCCESS != zret) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "zeContextDestroy returned %d", zret);
        }
        opal_accelerator_ze_context = NULL;
    }

    opal_accelerator_ze_device_count = 0;

    OBJ_DESTRUCT(&accelerator_ze_init_lock);
    return;
}

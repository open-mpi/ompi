/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All Rights reserved.
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
#include "accelerator_rocm.h"

static struct opal_dl_handle_t* hip_handle = NULL;
opal_accelerator_rocm_hipFunctionTable_t opal_accelerator_hip_funcs={};

int opal_accelerator_rocm_memcpy_async = 1;
int opal_accelerator_rocm_verbose = 0;
size_t opal_accelerator_rocm_memcpyD2H_limit=1024;
size_t opal_accelerator_rocm_memcpyH2D_limit=1048576;

hipStream_t opal_accelerator_rocm_MemcpyStream = NULL;
static opal_mutex_t opal_accelerator_rocm_init_lock = OPAL_MUTEX_STATIC_INIT;


/*
 * Public string showing the accelerator rocm component version number
 */
const char *opal_accelerator_rocm_component_version_string
    = "OPAL rocm accelerator MCA component version " OPAL_VERSION;


#define HIP_CHECK(condition)                                                 \
{                                                                            \
    hipError_t error = condition;                                            \
    if (hipSuccess != error){                                                \
        const char* msg = HIP_FUNCS.hipGetErrorString(error);                \
        opal_output(0, "HIP error: %d %s file: %s line: %d\n", error, msg, __FILE__, __LINE__); \
        return error;                                                        \
    }                                                                        \
}

#define HIP_CHECK_RETNULL(condition)                                         \
{                                                                            \
    hipError_t error = condition;                                            \
    if (hipSuccess != error){                                                \
        const char* msg = HIP_FUNCS.hipGetErrorString(error);                \
        opal_output(0, "HIP error: %d %s file: %s line: %d\n", error, msg, __FILE__, __LINE__); \
        return NULL;                                                         \
    }                                                                        \
}


/*
 * Local function
 */
static int accelerator_rocm_open(void);
static int accelerator_rocm_close(void);
static int accelerator_rocm_component_register(void);
static opal_accelerator_base_module_t* accelerator_rocm_init(void);
static void accelerator_rocm_finalize(opal_accelerator_base_module_t* module);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_accelerator_rocm_component_t mca_accelerator_rocm_component = {{

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    .base_version =
        {
            /* Indicate that we are a accelerator v1.1.0 component (which also
             * implies a specific MCA version) */

            OPAL_ACCELERATOR_BASE_VERSION_1_0_0,

            /* Component name and version */

            .mca_component_name = "rocm",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */

            .mca_open_component = accelerator_rocm_open,
            .mca_close_component = accelerator_rocm_close,
            .mca_register_component_params = accelerator_rocm_component_register,

        },
    /* Next the MCA v1.0.0 component meta data */
    .base_data =
        { /* The component is checkpoint ready */
         MCA_BASE_METADATA_PARAM_CHECKPOINT},
    .accelerator_init = accelerator_rocm_init,
    .accelerator_finalize = accelerator_rocm_finalize,
}};

static int accelerator_rocm_open(void)
{
    /* construct the component fields */

    return OPAL_SUCCESS;
}

static int accelerator_rocm_close(void)
{
    return OPAL_SUCCESS;
}

static int accelerator_rocm_component_register(void)
{
    /* Set verbosity in the rocm related code. */
    opal_accelerator_rocm_verbose = 0;
    (void) mca_base_var_register("ompi", "mpi", "accelerator_rocm", "verbose",
                                 "Set level of rocm verbosity", MCA_BASE_VAR_TYPE_INT, NULL,
                                 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_accelerator_rocm_verbose);

    /* Switching point between using memcpy and hipMemcpy* functions. */
    opal_accelerator_rocm_memcpyD2H_limit = 1024;
    (void) mca_base_var_register("ompi", "mpi", "accelerator_rocm", "memcpyD2H_limit",
                                 "Max. msg. length to use memcpy instead of hip functions "
                                 "for device-to-host copy operations",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_accelerator_rocm_memcpyD2H_limit);

    /* Switching point between using memcpy and hipMemcpy* functions. */
    opal_accelerator_rocm_memcpyH2D_limit = 1048576;
    (void) mca_base_var_register("ompi", "mpi", "accelerator_rocm", "memcpyH2D_limit",
                                 "Max. msg. length to use memcpy instead of hip functions "
                                 "for host-to-device copy operations",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_accelerator_rocm_memcpyH2D_limit);

    /* Use this flag to test async vs sync copies */
    opal_accelerator_rocm_memcpy_async = 1;
    (void) mca_base_var_register("ompi", "mpi", "accelerator_rocm", "memcpy_async",
                                 "Set to 0 to force using hipMemcpy instead of hipMemcpyAsync",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &opal_accelerator_rocm_memcpy_async);

    return OPAL_SUCCESS;
}


static int hip_dl_init(void)
{
    char *str;
    void *ptr;
    int ret  = opal_dl_open("libamdhip64.so", false, false, &hip_handle, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output(0, "Unable to open libamdhip64.so\n");
        return OPAL_ERROR;
    }

    ret = opal_dl_lookup(hip_handle, "hipMalloc", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output(0, "Failed to find hipMalloc\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipMalloc = (hipMalloc_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipFree", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipFree\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipFree = (hipFree_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipMemcpy", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipMemcpy\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipMemcpy = (hipMemcpy_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipMemcpyAsync", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipMemcpyAsync\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipMemcpyAsync = (hipMemcpyAsync_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipMemcpy2D", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipMemcpy2D\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipMemcpy2D = (hipMemcpy2D_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipMemcpy2DAsync", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipMemcpy2DAsync\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipMemcpy2DAsync = (hipMemcpy2DAsync_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipMemGetAddressRange", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipMemGetAddressRange\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipMemGetAddressRange  = (hipMemGetAddressRange_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipHostRegister", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipHostRegister\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipHostRegister = (hipHostRegister_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipHostUnregister", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipHostUnregister\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipHostUnregister = (hipHostUnregister_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipStreamCreate", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipStreamCreate\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipStreamCreate = (hipStreamCreate_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipStreamDestroy", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipStreamDestroy\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipStreamDestroy = (hipStreamDestroy_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipStreamSynchronize", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipStreamSynchronize\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipStreamSynchronize = (hipStreamSynchronize_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipGetErrorString", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipGetErrorString\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipGetErrorString = (hipGetErrorString_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipPointerGetAttributes", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipPointerGetAttributes\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipPointerGetAttributes = (hipPointerGetAttributes_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipEventCreateWithFlags", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipEventCreateWithFlags\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipEventCreateWithFlags = (hipEventCreateWithFlags_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipEventDestroy", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipEventDestroy\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipEventDestroy = (hipEventDestroy_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipEventRecord", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipEventRecord\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipEventRecord = (hipEventRecord_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipEventQuery", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipEventQuery\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipEventQuery = (hipEventQuery_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipEventSynchronize", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipEventSynchronize\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipEventSynchronize = (hipEventSynchronize_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipIpcGetMemHandle", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipIpcGetMemHandle\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipIpcGetMemHandle = (hipIpcGetMemHandle_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipIpcOpenMemHandle", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipIpcOpenMemHandle\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipIpcOpenMemHandle = (hipIpcOpenMemHandle_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipIpcCloseMemHandle", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipIpcCloseMemHandle\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipIpcCloseMemHandle = (hipIpcCloseMemHandle_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipGetDevice", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipGetDevice\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipGetDevice = (hipGetDevice_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipGetDeviceCount", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipGetDeviceCount\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipGetDeviceCount = (hipGetDeviceCount_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipDeviceCanAccessPeer", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(10, 0, "Failed to find hipDeviceCanAccessPeer\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    HIP_FUNCS.hipDeviceCanAccessPeer = (hipDeviceCanAccessPeer_t)ptr;

    return OPAL_SUCCESS;
}


static opal_accelerator_base_module_t* accelerator_rocm_init(void)
{
    hipError_t err;

    if (opal_rocm_runtime_initialized) {
        return NULL;
    }

    OPAL_THREAD_LOCK(&opal_accelerator_rocm_init_lock);

    if (opal_rocm_runtime_initialized) {
        OPAL_THREAD_UNLOCK(&opal_accelerator_rocm_init_lock);
        return NULL;
    }

    if (OPAL_SUCCESS != hip_dl_init()) {
        opal_output(0, "Could not open libamdhip64.so. Please check your LD_LIBRARY_PATH\n");
        OPAL_THREAD_UNLOCK(&opal_accelerator_rocm_init_lock);
        return NULL;
    }

    int count=0;
    err = HIP_FUNCS.hipGetDeviceCount(&count);
    if (hipSuccess != err || 0 == count) {
        opal_output(0, "No HIP capabale device found. Disabling component.\n");
        OPAL_THREAD_UNLOCK(&opal_accelerator_rocm_init_lock);
        return NULL;
    }

    err = HIP_FUNCS.hipStreamCreate(&opal_accelerator_rocm_MemcpyStream);
    if (hipSuccess != err) {
        opal_output(0, "Could not create hipStream, err=%d %s\n",
                err, HIP_FUNCS.hipGetErrorString(err));
        OPAL_THREAD_UNLOCK(&opal_accelerator_rocm_init_lock);
        return NULL;
    }

    opal_atomic_mb();
    opal_rocm_runtime_initialized = true;
    OPAL_THREAD_UNLOCK(&opal_accelerator_rocm_init_lock);

    return &opal_accelerator_rocm_module;
}

static void accelerator_rocm_finalize(opal_accelerator_base_module_t* module)
{
    OPAL_THREAD_LOCK(&opal_accelerator_rocm_init_lock);
    if (NULL != (void*)opal_accelerator_rocm_MemcpyStream) {
        hipError_t err = HIP_FUNCS.hipStreamDestroy(opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err) {
            opal_output_verbose(10, 0, "hip_dl_finalize: error while destroying the hipStream\n");
        }
        opal_accelerator_rocm_MemcpyStream = NULL;
    }
    if (NULL != hip_handle) {
        opal_dl_close(hip_handle);
        hip_handle = NULL;
    }
    OPAL_THREAD_UNLOCK(&opal_accelerator_rocm_init_lock);

    return;
}

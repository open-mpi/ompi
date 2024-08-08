/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022-2024 Advanced Micro Devices, Inc. All Rights reserved.
 * Copyright (c) 2024      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>

#include "opal/mca/dl/base/base.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/runtime/opal_params.h"
#include "opal/util/proc.h"
#include "accelerator_rocm.h"

int opal_accelerator_rocm_memcpy_async = 0;
int opal_accelerator_rocm_verbose = 0;
size_t opal_accelerator_rocm_memcpyD2H_limit=1024;
size_t opal_accelerator_rocm_memcpyH2D_limit=1048576;

/* Initialization lock for lazy rocm initialization */
static opal_mutex_t accelerator_rocm_init_lock;
static bool accelerator_rocm_init_complete = false;

/* Define global variables, used in accelerator_rocm.c */
int opal_accelerator_rocm_num_devices = 0;
float *opal_accelerator_rocm_mem_bw = NULL;
hipStream_t *opal_accelerator_rocm_MemcpyStream = NULL;

/*
 * Public string showing the accelerator rocm component version number
 */
const char *opal_accelerator_rocm_component_version_string
    = "OPAL rocm accelerator MCA component version " OPAL_VERSION;


#define HIP_CHECK(condition)                                                 \
{                                                                            \
    hipError_t error = condition;                                            \
    if (hipSuccess != error){                                                \
        const char* msg = hipGetErrorString(error);                \
        opal_output(0, "HIP error: %d %s file: %s line: %d\n", error, msg, __FILE__, __LINE__); \
        return error;                                                        \
    }                                                                        \
}

#define HIP_CHECK_RETNULL(condition)                                         \
{                                                                            \
    hipError_t error = condition;                                            \
    if (hipSuccess != error){                                                \
        const char* msg = hipGetErrorString(error);                \
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
    int var_id;

    /* Set verbosity in the rocm related code. */
    opal_accelerator_rocm_verbose = 0;
    var_id = mca_base_component_var_register (&mca_accelerator_rocm_component.super.base_version,
                                              "verbose", "Set level of verbosity of rocm component",
                                              MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                              0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                              &opal_accelerator_rocm_verbose);
    (void) mca_base_var_register_synonym (var_id, "ompi", "mpi", "accelerator_rocm", "verbose",
                                          MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /* Switching point between using memcpy and hipMemcpy* functions. */
    opal_accelerator_rocm_memcpyD2H_limit = 1024;
    var_id = mca_base_component_var_register (&mca_accelerator_rocm_component.super.base_version,
                                              "memcpyD2H_limit",
                                              "Max. msg. length to use memcpy instead of hip functions "
                                              "for device-to-host copy operations",
                                              MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                              OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                              &opal_accelerator_rocm_memcpyD2H_limit);
    (void) mca_base_var_register_synonym (var_id, "ompi", "mpi", "accelerator_rocm", "memcpyD2H_limit",
                                          MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    
    /* Switching point between using memcpy and hipMemcpy* functions. */
    opal_accelerator_rocm_memcpyH2D_limit = 1048576;
    var_id = mca_base_component_var_register (&mca_accelerator_rocm_component.super.base_version,
                                              "memcpyH2D_limit",
                                              "Max. msg. length to use memcpy instead of hip functions "
                                              "for host-to-device copy operations",
                                              MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                              OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                              &opal_accelerator_rocm_memcpyH2D_limit);
    (void) mca_base_var_register_synonym (var_id, "ompi", "mpi", "accelerator_rocm", "memcpyH2D_limit",
                                          MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /* Use this flag to test async vs sync copies */
    opal_accelerator_rocm_memcpy_async = 0;
    var_id = mca_base_component_var_register (&mca_accelerator_rocm_component.super.base_version,
                                              "memcpy_async",
                                              "Set to 1 to force using hipMemcpyAsync instead of hipMemcpy",
                                              MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                              MCA_BASE_VAR_SCOPE_READONLY,
                                              &opal_accelerator_rocm_memcpy_async);
    (void) mca_base_var_register_synonym (var_id, "ompi", "mpi", "accelerator_rocm", "memcpy_async",
                                          MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    return OPAL_SUCCESS;
}

int opal_accelerator_rocm_lazy_init()
{
    int err = OPAL_SUCCESS;

    /* Double checked locking to avoid having to
     * grab locks post lazy-initialization.  */
    opal_atomic_rmb();
    if (true == accelerator_rocm_init_complete) {
        return OPAL_SUCCESS;
    }
    OPAL_THREAD_LOCK(&accelerator_rocm_init_lock);

    /* If already initialized, just exit */
    if (true == accelerator_rocm_init_complete) {
        goto out;
    }

    err = hipGetDeviceCount(&opal_accelerator_rocm_num_devices);
    if (hipSuccess != err) {
        opal_output(0, "Failed to query device count, err=%d %s\n",
                err, hipGetErrorString(err));
	    err = OPAL_ERROR;
        goto out;
    }

    hipStream_t memcpy_stream;
    err = hipStreamCreate(&memcpy_stream);
    if (hipSuccess != err) {
        opal_output(0, "Could not create hipStream, err=%d %s\n",
                err, hipGetErrorString(err));
	    err = OPAL_ERROR;  // we got hipErrorInvalidValue, pretty bad
        goto out;
    }
    opal_accelerator_rocm_MemcpyStream = malloc(sizeof(hipStream_t));
    *opal_accelerator_rocm_MemcpyStream = memcpy_stream;

    opal_accelerator_rocm_mem_bw = malloc(sizeof(float)*opal_accelerator_rocm_num_devices);
    for (int i = 0; i < opal_accelerator_rocm_num_devices; ++i) {
        int mem_clock_rate; // kHz
        err = hipDeviceGetAttribute(&mem_clock_rate,
                                    hipDeviceAttributeMemoryClockRate,
                                    i);
        int bus_width; // bit
        err = hipDeviceGetAttribute(&bus_width,
                                    hipDeviceAttributeMemoryBusWidth,
                                    i);
        /* bw = clock_rate * bus width * 2bit multiplier
         * See https://forums.developer.nvidia.com/t/memory-clock-rate/107940
         */
        float bw = ((float)mem_clock_rate*(float)bus_width*2.0) / 1024 / 1024 / 8;
        opal_accelerator_rocm_mem_bw[i] = bw;
    }
    err = OPAL_SUCCESS;
    opal_atomic_wmb();
    accelerator_rocm_init_complete = true;
out:
    OPAL_THREAD_UNLOCK(&accelerator_rocm_init_lock);
    return err;
}

static opal_accelerator_base_module_t* accelerator_rocm_init(void)
{
    OBJ_CONSTRUCT(&accelerator_rocm_init_lock, opal_mutex_t);

    hipError_t err;

    if (opal_rocm_runtime_initialized) {
        return NULL;
    }

    int count=0;
    err = hipGetDeviceCount(&count);
    if (hipSuccess != err || 0 == count) {
        opal_output(0, "No HIP capabale device found. Disabling component.\n");
        return NULL;
    }

    opal_atomic_mb();
    opal_rocm_runtime_initialized = true;

    return &opal_accelerator_rocm_module;
}

static void accelerator_rocm_finalize(opal_accelerator_base_module_t* module)
{
    if (NULL != opal_accelerator_rocm_MemcpyStream) {
        hipError_t err = hipStreamDestroy(*opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err) {
            opal_output_verbose(10, 0, "hip_dl_finalize: error while destroying the hipStream\n");
        }
        free(opal_accelerator_rocm_MemcpyStream);
        opal_accelerator_rocm_MemcpyStream = NULL;

        free(opal_accelerator_rocm_mem_bw);
        opal_accelerator_rocm_mem_bw = NULL;
    }

    OBJ_DESTRUCT(&accelerator_rocm_init_lock);
    return;
}

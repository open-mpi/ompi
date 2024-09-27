/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal_config.h"

#include <cuda.h>

#include "accelerator_cuda.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/dl/base/base.h"
#include "opal/runtime/opal_params.h"
#include "opal/util/argv.h"
#include "opal/util/printf.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/sys/atomic.h"

/* Define global variables, used in accelerator_cuda.c */
CUstream opal_accelerator_cuda_memcpy_stream = NULL;
opal_mutex_t opal_accelerator_cuda_stream_lock = {0};
int opal_accelerator_cuda_num_devices = 0;

/* Initialization lock for delayed cuda initialization */
static opal_mutex_t accelerator_cuda_init_lock;
static bool accelerator_cuda_init_complete = false;

#define STRINGIFY2(x) #x
#define STRINGIFY(x)  STRINGIFY2(x)

/* Unused variable that we register at init time and unregister at fini time.
 * This is used to detect if user has done a device reset prior to MPI_Finalize.
 * This is a workaround to avoid SEGVs.
 */
static int checkmem;
static int ctx_ok = 1;

/*
 * Public string showing the accelerator cuda component version number
 */
const char *opal_accelerator_cuda_component_version_string
    = "OPAL cuda accelerator MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int accelerator_cuda_open(void);
static int accelerator_cuda_close(void);
static int accelerator_cuda_component_register(void);
static opal_accelerator_base_module_t* accelerator_cuda_init(void);
static void accelerator_cuda_finalize(opal_accelerator_base_module_t* module);
/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_accelerator_cuda_component_t mca_accelerator_cuda_component = {{

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    .base_version =
        {
            /* Indicate that we are a accelerator v1.1.0 component (which also
             * implies a specific MCA version) */

            OPAL_ACCELERATOR_BASE_VERSION_1_0_0,

            /* Component name and version */

            .mca_component_name = "cuda",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */

            .mca_open_component = accelerator_cuda_open,
            .mca_close_component = accelerator_cuda_close,
            .mca_register_component_params = accelerator_cuda_component_register,

        },
    /* Next the MCA v1.0.0 component meta data */
    .base_data =
        { /* The component is checkpoint ready */
         MCA_BASE_METADATA_PARAM_CHECKPOINT},
    .accelerator_init = accelerator_cuda_init,
    .accelerator_finalize = accelerator_cuda_finalize,
}};

static int accelerator_cuda_open(void)
{
    /* construct the component fields */
    return OPAL_SUCCESS;
}

static int accelerator_cuda_close(void)
{
    return OPAL_SUCCESS;
}

static int accelerator_cuda_component_register(void)
{
    return OPAL_SUCCESS;
}

int opal_accelerator_cuda_delayed_init()
{
    int result = OPAL_SUCCESS;
    CUcontext cuContext;

    /* Double checked locking to avoid having to
     * grab locks post lazy-initialization.  */
    opal_atomic_rmb();
    if (true == accelerator_cuda_init_complete) {
        return OPAL_SUCCESS;
    }
    OPAL_THREAD_LOCK(&accelerator_cuda_init_lock);

    /* If already initialized, just exit */
    if (true == accelerator_cuda_init_complete) {
        goto out;
    }

    cuDeviceGetCount(&opal_accelerator_cuda_num_devices);

    /* Check to see if this process is running in a CUDA context.  If
     * so, all is good.  If not, then disable registration of memory. */
    result = cuCtxGetCurrent(&cuContext);
    if (CUDA_SUCCESS != result) {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent failed");
        goto out;
    } else if ((CUDA_SUCCESS == result) && (NULL == cuContext)) {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent returned NULL context");
        result = OPAL_ERROR;
        goto out;
    } else {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent succeeded");
    }

    /* Create stream for use in cuMemcpyAsync synchronous copies */
    result = cuStreamCreate(&opal_accelerator_cuda_memcpy_stream, 0);
    if (OPAL_UNLIKELY(result != CUDA_SUCCESS)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamCreate failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        goto out;
    }

    result = cuMemHostRegister(&checkmem, sizeof(int), 0);
    if (result != CUDA_SUCCESS) {
        /* If registering the memory fails, print a message and continue.
         * This is not a fatal error. */
        opal_show_help("help-accelerator-cuda.txt", "cuMemHostRegister during init failed", true,
                       &checkmem, sizeof(int), OPAL_PROC_MY_HOSTNAME, result, "checkmem");

    } else {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output,
                            "CUDA: cuMemHostRegister OK on test region");
    }
    result = OPAL_SUCCESS;
    opal_atomic_wmb();
    accelerator_cuda_init_complete = true;
out:
    OPAL_THREAD_UNLOCK(&accelerator_cuda_init_lock);
    return result;
}

static opal_accelerator_base_module_t* accelerator_cuda_init(void)
{
    OBJ_CONSTRUCT(&opal_accelerator_cuda_stream_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&accelerator_cuda_init_lock, opal_mutex_t);
    /* First check if the support is enabled.  In the case that the user has
     * turned it off, we do not need to continue with any CUDA specific
     * initialization.  Do this after MCA parameter registration. */
    if (!opal_cuda_support) {
        return NULL;
    }

    opal_accelerator_cuda_delayed_init();
    return &opal_accelerator_cuda_module;
}

static void accelerator_cuda_finalize(opal_accelerator_base_module_t* module)
{
    CUresult result;
    /* This call is in here to make sure the context is still valid.
     * This was the one way of checking which did not cause problems
     * while calling into the CUDA library.  This check will detect if
     * a user has called cudaDeviceReset prior to MPI_Finalize. If so,
     * then this call will fail and we skip cleaning up CUDA resources. */
    result = cuMemHostUnregister(&checkmem);
    if (CUDA_SUCCESS != result) {
        ctx_ok = 0;
    }
    if ((NULL != opal_accelerator_cuda_memcpy_stream) && ctx_ok) {
        cuStreamDestroy(opal_accelerator_cuda_memcpy_stream);
    }

    OBJ_DESTRUCT(&opal_accelerator_cuda_stream_lock);
    OBJ_DESTRUCT(&accelerator_cuda_init_lock);
    return;
}

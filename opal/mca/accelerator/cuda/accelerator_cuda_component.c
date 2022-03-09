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


/* Define global variables, used in accelerator_cuda.c */
accelerator_cuda_func_table_t opal_accelerator_cuda_func = {0};
CUstream opal_accelerator_cuda_memcpy_stream = NULL;
opal_mutex_t opal_accelerator_cuda_stream_lock = {0};

#define STRINGIFY2(x) #x
#define STRINGIFY(x)  STRINGIFY2(x)
#define OPAL_CUDA_DLSYM(libhandle, func_name)                                                           \
    do {                                                                                                \
        char *err_msg;                                                                                  \
        void *ptr;                                                                                      \
        if (OPAL_SUCCESS != opal_dl_lookup(libhandle, STRINGIFY(func_name), &ptr, &err_msg)) {          \
            opal_show_help("help-mpi-accelerator-cuda.txt", "dlsym failed", true, STRINGIFY(func_name), \
                           err_msg);                                                                    \
            return -1;                                                                                \
        } else {                                                                                        \
            *(void **) (&opal_accelerator_cuda_func.func_name) = ptr;                                        \
            opal_output_verbose(15, opal_accelerator_base_framework.framework_output, "CUDA: successful dlsym of %s",            \
                                STRINGIFY(funcName));                                                   \
        }                                                                                               \
    } while (0)

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
static int accelerator_cuda_populate_func_table(opal_dl_handle_t *libcuda_handle);
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

static opal_accelerator_base_module_t* accelerator_cuda_init(void)
{
    int retval, i, j;
    char *cudalibs[] = {"libcuda.so.1", "libcuda.dylib", NULL};
    char *searchpaths[] = {"", "/usr/lib64", NULL};
    char **errmsgs = NULL;
    char *errmsg = NULL;
    int errsize;
    bool found_libraries = false;
    opal_dl_handle_t *libcuda_handle = NULL;
    CUresult result;
    CUcontext cuContext;

    OBJ_CONSTRUCT(&opal_accelerator_cuda_stream_lock, opal_mutex_t);

    /* First check if the support is enabled.  In the case that the user has
     * turned it off, we do not need to continue with any CUDA specific
     * initialization.  Do this after MCA parameter registration. */
    if (!opal_cuda_support) {
        return NULL;
    }

    if (!OPAL_HAVE_DL_SUPPORT) {
        opal_show_help("help-accelerator-cuda.txt", "dlopen disabled", true);
        return NULL;
    }

    /* Now walk through all the potential names libcuda and find one
     * that works.  If it does, all is good.  If not, print out all
     * the messages about why things failed.  This code was careful
     * to try and save away all error messages if the loading ultimately
     * failed to help with debugging.
     *
     * NOTE: On the first loop we just utilize the default loading
     * paths from the system.  For the second loop, set /usr/lib64 to
     * the search path and try again.  This is done to handle the case
     * where we have both 32 and 64 bit libcuda.so libraries
     * installed.  Even when running in 64-bit mode, the /usr/lib
     * directory is searched first and we may find a 32-bit
     * libcuda.so.1 library.  Loading of this library will fail as the
     * OPAL DL framework does not handle having the wrong ABI in the
     * search path (unlike ld or ld.so).  Note that we only set this
     * search path after the original search.  This is so that
     * LD_LIBRARY_PATH and run path settings are respected.  Setting
     * this search path overrides them (rather then being
     * appended). */

    j = 0;
    while (searchpaths[j] != NULL) {
        i = 0;
        while (cudalibs[i] != NULL) {
            char *filename = NULL;
            char *str = NULL;

            /* If there's a non-empty search path, prepend it
             * to the library filename */
            if (strlen(searchpaths[j]) > 0) {
                opal_asprintf(&filename, "%s/%s", searchpaths[j], cudalibs[i]);
            } else {
                filename = strdup(cudalibs[i]);
            }
            if (NULL == filename) {
                opal_show_help("help-accelerator-cuda.txt", "No memory", true,
                               OPAL_PROC_MY_HOSTNAME);
                return NULL;
            }

            retval = opal_dl_open(filename, false, false, &libcuda_handle, &str);
            if (OPAL_SUCCESS != retval || NULL == libcuda_handle) {
                if (NULL != str) {
                    opal_argv_append(&errsize, &errmsgs, str);
                } else {
                    opal_argv_append(&errsize, &errmsgs, "opal_dl_open() returned NULL.");
                }
                opal_output_verbose(10, opal_accelerator_base_framework.framework_output, "CUDA: Library open error: %s",
                                    errmsgs[errsize - 1]);
            } else {
                opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                    "CUDA: Library successfully opened %s", cudalibs[i]);
                found_libraries = true;
                break;
            }
            i++;
            free(filename);
        }
        if (true == found_libraries) {
            break; /* Break out of outer loop */
        }
        j++;
    }

    if (true != found_libraries) {
        errmsg = opal_argv_join(errmsgs, '\n');
        if (opal_warn_on_missing_libcuda) {
            opal_show_help("help-accelerator-cuda.txt", "dlopen failed", true, errmsg);
        }
    }
    opal_argv_free(errmsgs);
    free(errmsg);

    if (true != found_libraries) {
        return NULL;
    }

    if (OPAL_SUCCESS != accelerator_cuda_populate_func_table(libcuda_handle)) {
        return NULL;
    }

    /* Check to see if this process is running in a CUDA context.  If
     * so, all is good.  If not, then disable registration of memory. */
    result = opal_accelerator_cuda_func.cuCtxGetCurrent(&cuContext);
    if (CUDA_SUCCESS != result) {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent failed");
        return NULL;
    } else if ((CUDA_SUCCESS == result) && (NULL == cuContext)) {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent returned NULL context");
        return NULL;
    } else {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent succeeded");
    }

    /* Create stream for use in cuMemcpyAsync synchronous copies */
    result = opal_accelerator_cuda_func.cuStreamCreate(&opal_accelerator_cuda_memcpy_stream, 0);
    if (OPAL_UNLIKELY(result != CUDA_SUCCESS)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamCreate failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        return NULL;
    }

    result = opal_accelerator_cuda_func.cuMemHostRegister(&checkmem, sizeof(int), 0);
    if (result != CUDA_SUCCESS) {
        /* If registering the memory fails, print a message and continue.
         * This is not a fatal error. */
        opal_show_help("help-accelerator-cuda.txt", "cuMemHostRegister during init failed", true,
                       &checkmem, sizeof(int), OPAL_PROC_MY_HOSTNAME, result, "checkmem");

    } else {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output,
                            "CUDA: cuMemHostRegister OK on test region");
    }

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
    result = opal_accelerator_cuda_func.cuMemHostUnregister(&checkmem);
    if (CUDA_SUCCESS != result) {
        ctx_ok = 0;
    }
    if ((NULL != opal_accelerator_cuda_memcpy_stream) && ctx_ok) {
        opal_accelerator_cuda_func.cuStreamDestroy(opal_accelerator_cuda_memcpy_stream);
    }

    OBJ_DESTRUCT(&opal_accelerator_cuda_stream_lock);
    return;
}

static int accelerator_cuda_populate_func_table(opal_dl_handle_t *libcuda_handle)
{
    /* Map in the functions that we need.  Note that if there is an error
     * the macro OPAL_CUDA_DLSYM will print an error and call return.  */
    OPAL_CUDA_DLSYM(libcuda_handle, cuStreamCreate);
    OPAL_CUDA_DLSYM(libcuda_handle, cuCtxGetCurrent);
    OPAL_CUDA_DLSYM(libcuda_handle, cuEventCreate);
    OPAL_CUDA_DLSYM(libcuda_handle, cuEventRecord);
    OPAL_CUDA_DLSYM(libcuda_handle, cuEventQuery);
    OPAL_CUDA_DLSYM(libcuda_handle, cuEventSynchronize);
    OPAL_CUDA_DLSYM(libcuda_handle, cuEventDestroy);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemHostRegister);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemHostUnregister);
    OPAL_CUDA_DLSYM(libcuda_handle, cuPointerGetAttribute);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemcpyAsync);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemcpy);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemcpy2D);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemFree);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemAlloc);
    OPAL_CUDA_DLSYM(libcuda_handle, cuMemGetAddressRange);
    OPAL_CUDA_DLSYM(libcuda_handle, cuIpcGetEventHandle);
    OPAL_CUDA_DLSYM(libcuda_handle, cuIpcOpenEventHandle);
    OPAL_CUDA_DLSYM(libcuda_handle, cuIpcOpenMemHandle);
    OPAL_CUDA_DLSYM(libcuda_handle, cuIpcCloseMemHandle);
    OPAL_CUDA_DLSYM(libcuda_handle, cuIpcGetMemHandle);
    OPAL_CUDA_DLSYM(libcuda_handle, cuCtxGetDevice);
    OPAL_CUDA_DLSYM(libcuda_handle, cuDeviceCanAccessPeer);
    OPAL_CUDA_DLSYM(libcuda_handle, cuCtxSetCurrent);
    OPAL_CUDA_DLSYM(libcuda_handle, cuStreamSynchronize);
    OPAL_CUDA_DLSYM(libcuda_handle, cuStreamDestroy);
    OPAL_CUDA_DLSYM(libcuda_handle, cuPointerSetAttribute);
#if OPAL_CUDA_GET_ATTRIBUTES
    OPAL_CUDA_DLSYM(libcuda_handle, cuPointerGetAttributes);
#endif /* OPAL_CUDA_GET_ATTRIBUTES */
    return OPAL_SUCCESS;
}

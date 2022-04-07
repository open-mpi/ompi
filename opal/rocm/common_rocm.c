/*
 * Copyright (c) 2022 Advanced Micro Devices, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <stdio.h>
#include <dlfcn.h>
#include "common_rocm.h"
#include "opal/mca/dl/base/base.h"
#include "opal/runtime/opal_params.h"

static struct opal_dl_handle_t* hip_handle = NULL;
static hipFunctionTable_t hip_funcs;

int mca_common_rocm_memcpy_async = 1;
int mca_common_rocm_verbose = 0;
size_t mca_common_rocm_memcpy_limit=1024;

static hipStream_t common_rocm_MemcpyStream;
static opal_mutex_t common_rocm_init_lock = OPAL_MUTEX_STATIC_INIT;

#define HIP_CHECK(condition)                                                 \
{                                                                            \
    hipError_t error = condition;                                            \
    if (hipSuccess != error){                                                \
        const char* msg = hip_funcs.hipGetErrorString(error);                \
        opal_output(0, "HIP error: %d %s file: %s line: %d\n", error, msg, __FILE__, __LINE__); \
        return error;                                                        \
    }                                                                        \
}

#define HIP_CHECK_RETNULL(condition)                                         \
{                                                                            \
    hipError_t error = condition;                                            \
    if (hipSuccess != error){                                                \
        const char* msg = hip_funcs.hipGetErrorString(error);                \
        opal_output(0, "HIP error: %d %s file: %s line: %d\n", error, msg, __FILE__, __LINE__); \
        return NULL;                                                         \
    }                                                                        \
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
    hip_funcs.hipMalloc = (hipMalloc_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipFree", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipFree\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipFree = (hipFree_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipMemcpy", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipMemcpy\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipMemcpy = (hipMemcpy_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipMemcpyAsync", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipMemcpyAsync\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipMemcpyAsync = (hipMemcpyAsync_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipStreamCreate", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipStreamCreate\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipStreamCreate = (hipStreamCreate_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipStreamDestroy", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipStreamDestroy\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipStreamDestroy = (hipStreamDestroy_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipStreamSynchronize", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipStreamSynchronize\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipStreamSynchronize = (hipStreamSynchronize_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipGetErrorString", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipGetErrorString\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipGetErrorString = (hipGetErrorString_t)ptr;

    ret = opal_dl_lookup(hip_handle, "hipPointerGetAttributes", &ptr, &str);
    if (OPAL_SUCCESS != ret) {
      opal_output_verbose(10, 0, "Failed to find hipPointerGetAttributes\n");
        dlclose(hip_handle);
        return OPAL_ERROR;
    }
    hip_funcs.hipPointerGetAttributes = (hipPointerGetAttributes_t)ptr;

    return OPAL_SUCCESS;
}

static void hip_dl_finalize(void)
{
    hipError_t err = hip_funcs.hipStreamDestroy(common_rocm_MemcpyStream);
    if (hipSuccess != err) {
      opal_output_verbose(10, 0, "hip_dl_finalize: error while destroying the hipStream\n");
    }
    dlclose(hip_handle);
}


static void mca_common_rocm_support_init(void)
{
    if (opal_rocm_runtime_initialized) {
        return;
    }

    OPAL_THREAD_LOCK(&common_rocm_init_lock);

    if (opal_rocm_runtime_initialized) {
        return;
    }

    if (OPAL_SUCCESS != hip_dl_init()) {
        opal_output(0, "Could not open libamdhip64.so. Please check your LD_LIBRARY_PATH\n");
        OPAL_THREAD_UNLOCK(&common_rocm_init_lock);
        return;
    }

    hipError_t err = hip_funcs.hipStreamCreate(&common_rocm_MemcpyStream);
    if (hipSuccess != err) {
        opal_output(0, "Could not create hipStream, err=%d %s\n",
                err, hip_funcs.hipGetErrorString(err));
    }

    opal_atomic_mb();
    opal_rocm_runtime_initialized = true;
    OPAL_THREAD_UNLOCK(&common_rocm_init_lock);
}

void mca_common_rocm_convertor_init(opal_convertor_t *convertor, const void *pUserBuf)
{
    /* Only do the initialization on the first GPU access */
    if (!opal_rocm_runtime_initialized) {
        mca_common_rocm_support_init();
    }

    convertor->cbmemcpy = (memcpy_fct_t) &mca_common_rocm_memcpy;
    // set convertor->stream as well ? Does not seem to be used at the moment in the datatype engine.

    hipPointerAttribute_t attr;
    hipError_t err =  hip_funcs.hipPointerGetAttributes(&attr, pUserBuf);
    if (hipSuccess != err) {
        return;
    }
    if (hipMemoryTypeDevice == attr.memoryType || hipMemoryTypeUnified == attr.memoryType) {
        convertor->flags |= CONVERTOR_ROCM;
    }
}

bool mca_common_rocm_check_bufs(char *dst, char *src)
{
    /* Only do the initialization on the first GPU access */
    if (!opal_rocm_runtime_initialized) {
        mca_common_rocm_support_init();
    }

    hipPointerAttribute_t srcAttr, dstAttr;
    bool srcFlag=false, dstFlag=false;
    hipError_t err = hip_funcs.hipPointerGetAttributes(&srcAttr, src);
    if (hipSuccess == err) {
        if (hipMemoryTypeDevice == srcAttr.memoryType || hipMemoryTypeUnified == srcAttr.memoryType) {
            srcFlag = true;
        }
    }
    err = hip_funcs.hipPointerGetAttributes(&dstAttr, dst);
    if (hipSuccess == err) {
        if (hipMemoryTypeDevice == dstAttr.memoryType || hipMemoryTypeUnified == dstAttr.memoryType ) {
            dstFlag = true;
        }
    }
    if (srcFlag || dstFlag) {
        return true;
    }
    
    return false;
}

int mca_common_rocm_memcpy_sync(void *dst, void *src, size_t nBytes)
{

    if (!opal_rocm_runtime_initialized) {
        mca_common_rocm_support_init();
    }

    if (nBytes < mca_common_rocm_memcpy_limit) {
        memcpy(dst, src, nBytes);
        return OPAL_SUCCESS;
    }

    if (mca_common_rocm_memcpy_async) {
        HIP_CHECK(hip_funcs.hipMemcpyAsync(dst, src, nBytes, hipMemcpyDefault,
                                            common_rocm_MemcpyStream));
        HIP_CHECK(hip_funcs.hipStreamSynchronize(common_rocm_MemcpyStream));
    }
    else {
        HIP_CHECK(hip_funcs.hipMemcpy(dst, src, nBytes, hipMemcpyDefault));
    }

    return OPAL_SUCCESS;
}

void *mca_common_rocm_memcpy(void *dst, const void *src, size_t nBytes, opal_convertor_t *pConvertor)
{

    if (!(pConvertor->flags & CONVERTOR_ROCM) || nBytes < mca_common_rocm_memcpy_limit) {
        //For short messages a regular memcpy is faster than hipMemcpy or hipMemcpyAsync
        return memcpy(dst, src, nBytes);
    }

    if (mca_common_rocm_memcpy_async) {
        HIP_CHECK_RETNULL(hip_funcs.hipMemcpyAsync(dst, src, nBytes, hipMemcpyDefault, common_rocm_MemcpyStream));
        HIP_CHECK_RETNULL(hip_funcs.hipStreamSynchronize(common_rocm_MemcpyStream));
    }
    else {
        HIP_CHECK_RETNULL(hip_funcs.hipMemcpy(dst, src, nBytes, hipMemcpyDefault));
    }

  return dst;
}

int mca_common_rocm_memmove(void *dst, void *src, size_t nBytes)
{
    if (!opal_rocm_runtime_initialized) {
        mca_common_rocm_support_init();
    }

    char *tmp=NULL;
    HIP_CHECK(hip_funcs.hipMalloc((void **)&tmp, nBytes));
    if (mca_common_rocm_memcpy_async) {
        HIP_CHECK(hip_funcs.hipMemcpyAsync(tmp, src, nBytes, hipMemcpyDefault, common_rocm_MemcpyStream));
        HIP_CHECK(hip_funcs.hipMemcpyAsync(dst, tmp, nBytes, hipMemcpyDefault, common_rocm_MemcpyStream));
        HIP_CHECK(hip_funcs.hipStreamSynchronize(common_rocm_MemcpyStream));
    } else {
        HIP_CHECK(hip_funcs.hipMemcpy(tmp, src, nBytes, hipMemcpyDefault));
        HIP_CHECK(hip_funcs.hipMemcpy(dst, tmp, nBytes, hipMemcpyDefault));
    }

    HIP_CHECK(hip_funcs.hipFree(tmp));
    return OPAL_SUCCESS;
}

void mca_common_rocm_register_mca_variables(void)
{

    /* Set verbosity in the rocm related code. */
    mca_common_rocm_verbose = 0;
    (void) mca_base_var_register("ompi", "mpi", "common_rocm", "verbose",
                                 "Set level of common rocm verbosity", MCA_BASE_VAR_TYPE_INT, NULL,
                                 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_common_rocm_verbose);

    /* Switching point between using memcpy and hipMemcpy* functions. */
    mca_common_rocm_memcpy_limit = 1024;
    (void) mca_base_var_register("ompi", "mpi", "common_rocm", "memcpy_limit",
                                 "Max. msg. length to use memcpy instead of hip copy functions",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_common_rocm_memcpy_limit);

    /* Use this flag to test async vs sync copies */
    mca_common_rocm_memcpy_async = 1;
    (void) mca_base_var_register("ompi", "mpi", "common_rocm", "memcpy_async",
                                 "Set to 0 to force using hipMemcpy instead of hipMemcpyAsync",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &mca_common_rocm_memcpy_async);

    return;
}

/*
 * Copyright (c) 2022 Advanced Micro Devices, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_ACCELERATOR_ROCM_H
#define OPAL_ACCELERATOR_ROCM_H

#include "opal_config.h"

#include <stdio.h>
#include <hip/hip_runtime_api.h>

#include "opal/mca/btl/base/base.h"

typedef hipError_t(*hipMalloc_t)(void **, size_t);
typedef hipError_t(*hipFree_t)(void*);
typedef hipError_t(*hipMemcpy_t)(void*, const void*, size_t, hipMemcpyKind);
typedef hipError_t(*hipMemcpyAsync_t)(void*, const void*, size_t, hipMemcpyKind, hipStream_t);
typedef hipError_t(*hipMemcpy2D_t)(void*, size_t, const void*, size_t, size_t, size_t, hipMemcpyKind);
typedef hipError_t(*hipMemcpy2DAsync_t)(void*, size_t, const void*, size_t, size_t, size_t,
                                       hipMemcpyKind, hipStream_t);
typedef hipError_t(*hipMemGetAddressRange_t)(hipDeviceptr_t*, size_t*, hipDeviceptr_t);
typedef hipError_t(*hipHostRegister_t)(void*, size_t, unsigned int);
typedef hipError_t(*hipHostUnregister_t)(void*);

typedef hipError_t(*hipStreamCreate_t)(hipStream_t*);
typedef hipError_t(*hipStreamDestroy_t)(hipStream_t);
typedef hipError_t(*hipStreamSynchronize_t)(hipStream_t);

typedef const char*(*hipGetErrorString_t)(hipError_t);
typedef hipError_t(*hipPointerGetAttributes_t)(hipPointerAttribute_t *, const void *);

typedef hipError_t(*hipEventCreateWithFlags_t)(hipEvent_t*, unsigned);
typedef hipError_t(*hipEventDestroy_t)(hipEvent_t);
typedef hipError_t(*hipEventRecord_t)(hipEvent_t, hipStream_t);
typedef hipError_t(*hipEventQuery_t)(hipEvent_t);
typedef hipError_t(*hipEventSynchronize_t)(hipEvent_t);

typedef hipError_t(*hipIpcGetMemHandle_t)(hipIpcMemHandle_t*, void*);
typedef hipError_t(*hipIpcOpenMemHandle_t)(void **, hipIpcMemHandle_t, unsigned int);
typedef hipError_t(*hipIpcCloseMemHandle_t)(void*);

typedef hipError_t(*hipGetDevice_t)(int*);
typedef hipError_t(*hipGetDeviceCount_t)(int*);
typedef hipError_t(*hipDeviceCanAccessPeer_t)(int*, int, int);

struct opal_accelerator_rocm_hipFunctionTable_s {
    hipError_t (*hipMalloc)(void** pts, size_t size);
    hipError_t (*hipFree)(void* ptr);
    hipError_t (*hipMemcpy)(void* dst, const void* src, size_t sizeBytes, hipMemcpyKind kind);
    hipError_t (*hipMemcpyAsync)(void* dst, const void* src, size_t sizeBytes, hipMemcpyKind kind,
                                 hipStream_t stream);
    hipError_t(*hipMemcpy2D)(void* dst, size_t dpitch, const void* src, size_t spitch,
                             size_t width, size_t height, hipMemcpyKind kind);
    hipError_t(*hipMemcpy2DAsync)(void* dst, size_t dpitch, const void* src, size_t spitch,
                                  size_t width, size_t height, hipMemcpyKind kind,
                                  hipStream_t stream);
    hipError_t(*hipMemGetAddressRange)(hipDeviceptr_t* pbase, size_t* psize, hipDeviceptr_t dptr);

    hipError_t(*hipHostRegister)(void* ptr, size_t size, unsigned int flags);
    hipError_t(*hipHostUnregister)(void* ptr);

    hipError_t (*hipStreamCreate)(hipStream_t* stream);
    hipError_t (*hipStreamDestroy)(hipStream_t stream);
    hipError_t (*hipStreamSynchronize)(hipStream_t stream);
    const char* (*hipGetErrorString)(hipError_t hipError);
    hipError_t (*hipPointerGetAttributes)(hipPointerAttribute_t *attributes, const void *ptr);

    hipError_t(*hipEventCreateWithFlags)(hipEvent_t* event, unsigned int flags);
    hipError_t(*hipEventDestroy)(hipEvent_t event);
    hipError_t(*hipEventRecord)(hipEvent_t event, hipStream_t stream);
    hipError_t(*hipEventQuery)(hipEvent_t event);
    hipError_t(*hipEventSynchronize)(hipEvent_t event);

    hipError_t(*hipIpcGetMemHandle)(hipIpcMemHandle_t* handle, void* devPtr);
    hipError_t(*hipIpcOpenMemHandle)(void **devPtr, hipIpcMemHandle_t handle, unsigned int flags);
    hipError_t(*hipIpcCloseMemHandle)(void* devPtr);

    hipError_t(*hipGetDevice)(int* dev_id);
    hipError_t(*hipGetDeviceCount)(int* count);
    hipError_t(*hipDeviceCanAccessPeer)(int* canAccess, int dev1, int dev2);
};
typedef struct opal_accelerator_rocm_hipFunctionTable_s opal_accelerator_rocm_hipFunctionTable_t;


typedef struct {
    opal_accelerator_base_component_t super;
} opal_accelerator_rocm_component_t;

OPAL_DECLSPEC extern opal_accelerator_rocm_component_t mca_accelerator_rocm_component;
OPAL_DECLSPEC extern opal_accelerator_base_module_t opal_accelerator_rocm_module;

OPAL_DECLSPEC extern opal_accelerator_rocm_hipFunctionTable_t opal_accelerator_hip_funcs;

struct opal_accelerator_rocm_stream_t {
    opal_accelerator_stream_t base;
};
typedef struct opal_accelerator_rocm_stream_t opal_accelerator_rocm_stream_t;
OBJ_CLASS_DECLARATION(opal_accelerator_rocm_stream_t);

struct opal_accelerator_rocm_event_t {
    opal_accelerator_event_t base;
};
typedef struct opal_accelerator_rocm_event_t opal_accelerator_rocm_event_t;
OBJ_CLASS_DECLARATION(opal_accelerator_rocm_event_t);

#define HIP_FUNCS opal_accelerator_hip_funcs

OPAL_DECLSPEC extern hipStream_t opal_accelerator_rocm_MemcpyStream;
OPAL_DECLSPEC extern int opal_accelerator_rocm_memcpy_async;
OPAL_DECLSPEC extern int opal_accelerator_rocm_verbose;
OPAL_DECLSPEC extern size_t opal_accelerator_rocm_memcpyH2D_limit;
OPAL_DECLSPEC extern size_t opal_accelerator_rocm_memcpyD2H_limit;

#endif

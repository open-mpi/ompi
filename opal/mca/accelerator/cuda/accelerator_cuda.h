/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_ACCELERATOR_CUDA_H
#define MCA_ACCELERATOR_CUDA_H

#include "opal_config.h"

#include <cuda.h>

#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/threads/mutex.h"

BEGIN_C_DECLS

typedef struct {
    opal_accelerator_base_component_t super;
} opal_accelerator_cuda_component_t;

/* Structure to hold CUDA function pointers that get dynamically loaded. */
struct  accelerator_cuda_func_table {
    int (*cuPointerGetAttribute)(void *, CUpointer_attribute, CUdeviceptr);
    int (*cuMemcpyAsync)(CUdeviceptr, CUdeviceptr, size_t, CUstream);
    int (*cuMemcpy)(CUdeviceptr, CUdeviceptr, size_t);
    int (*cuMemcpy2D)(const CUDA_MEMCPY2D* pCopy);
    int (*cuMemAlloc)(CUdeviceptr *, size_t);
    int (*cuMemFree)(CUdeviceptr buf);
    int (*cuCtxGetCurrent)(void *cuContext);
    int (*cuStreamCreate)(CUstream *, int);
    int (*cuEventCreate)(CUevent *, int);
    int (*cuEventRecord)(CUevent, CUstream);
    int (*cuEventQuery)(CUevent);
    int (*cuEventDestroy)(CUevent);
    int (*cuMemHostRegister)(void *, size_t, unsigned int);
    int (*cuMemHostUnregister)(void *);
    int (*cuMemGetAddressRange)(CUdeviceptr *, size_t *, CUdeviceptr);
    int (*cuIpcGetEventHandle)(CUipcEventHandle *, CUevent);
    int (*cuIpcOpenEventHandle)(CUevent *, CUipcEventHandle);
    int (*cuIpcOpenMemHandle)(CUdeviceptr *, CUipcMemHandle, unsigned int);
    int (*cuIpcCloseMemHandle)(CUdeviceptr);
    int (*cuIpcGetMemHandle)(CUipcMemHandle *, CUdeviceptr);
    int (*cuCtxGetDevice)(CUdevice *);
    int (*cuDeviceCanAccessPeer)(int *, CUdevice, CUdevice);
    int (*cuCtxSetCurrent)(CUcontext);
    int (*cuEventSynchronize)(CUevent);
    int (*cuStreamSynchronize)(CUstream);
    int (*cuStreamDestroy)(CUstream);
    int (*cuPointerSetAttribute)(const void *, CUpointer_attribute, CUdeviceptr);
#if OPAL_CUDA_GET_ATTRIBUTES
    int (*cuPointerGetAttributes)(unsigned int, CUpointer_attribute *, void **, CUdeviceptr);
#endif /* OPAL_CUDA_GET_ATTRIBUTES */
};
typedef struct accelerator_cuda_func_table  accelerator_cuda_func_table_t;

struct opal_accelerator_cuda_stream_t {
    opal_accelerator_stream_t base;
};
typedef struct opal_accelerator_cuda_stream_t opal_accelerator_cuda_stream_t;
OBJ_CLASS_DECLARATION(opal_accelerator_cuda_stream_t);

struct opal_accelerator_cuda_event_t {
    opal_accelerator_event_t base;
};
typedef struct opal_accelerator_cuda_event_t opal_accelerator_cuda_event_t;
OBJ_CLASS_DECLARATION(opal_accelerator_cuda_event_t);

/* Declare extern variables, defined in accelerator_cuda_component.c */
OPAL_DECLSPEC extern accelerator_cuda_func_table_t opal_accelerator_cuda_func;
OPAL_DECLSPEC extern CUstream opal_accelerator_cuda_memcpy_stream;
OPAL_DECLSPEC extern opal_mutex_t opal_accelerator_cuda_stream_lock;

OPAL_DECLSPEC extern opal_accelerator_cuda_component_t mca_accelerator_cuda_component;

OPAL_DECLSPEC extern opal_accelerator_base_module_t opal_accelerator_cuda_module;

END_C_DECLS

#endif /* MCA_ACCELERATOR_CUDA_H */

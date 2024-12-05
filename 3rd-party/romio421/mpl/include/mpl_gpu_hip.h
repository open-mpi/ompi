/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_GPU_HIP_H_INCLUDED
#define MPL_GPU_HIP_H_INCLUDED

#if !defined(__HIP_PLATFORM_AMD__) && !defined(__HIP_PLATFORM_NVIDIA__)
#define __HIP_PLATFORM_AMD__
#endif

#include "hip/hip_runtime.h"
#include "hip/hip_runtime_api.h"

typedef hipIpcMemHandle_t MPL_gpu_ipc_mem_handle_t;
typedef int MPL_gpu_device_handle_t;
typedef struct hipPointerAttribute_t MPL_gpu_device_attr;
typedef int MPL_gpu_request;
typedef hipStream_t MPL_gpu_stream_t;

typedef volatile int MPL_gpu_event_t;

#define MPL_GPU_STREAM_DEFAULT 0
#define MPL_GPU_DEVICE_INVALID -1

#define MPL_GPU_DEV_AFFINITY_ENV "HIP_VISIBLE_DEVICES"

#endif /* ifndef MPL_GPU_HIP_H_INCLUDED */

/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_GPU_CUDA_H_INCLUDED
#define MPL_GPU_CUDA_H_INCLUDED

#include "cuda.h"
#include "cuda_runtime_api.h"

typedef cudaIpcMemHandle_t MPL_gpu_ipc_mem_handle_t;
typedef int MPL_gpu_device_handle_t;
typedef struct cudaPointerAttributes MPL_gpu_device_attr;
typedef int MPL_gpu_request;
typedef cudaStream_t MPL_gpu_stream_t;

/* Note: event variable need be allocated on a gpu registered host buffer for it to work */
typedef volatile int MPL_gpu_event_t;

#define MPL_GPU_STREAM_DEFAULT 0
#define MPL_GPU_DEVICE_INVALID -1

#define MPL_GPU_DEV_AFFINITY_ENV "CUDA_VISIBLE_DEVICES"

#endif /* ifndef MPL_GPU_CUDA_H_INCLUDED */

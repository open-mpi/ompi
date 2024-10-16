/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_GPU_CUDA_H_INCLUDED
#define MPL_GPU_CUDA_H_INCLUDED

typedef int MPL_gpu_ipc_mem_handle_t;
typedef int MPL_gpu_device_handle_t;
typedef int MPL_gpu_device_attr;        /* dummy type */
typedef int MPL_gpu_request;
typedef int MPL_gpu_stream_t;

typedef volatile int MPL_gpu_event_t;

#define MPL_GPU_STREAM_DEFAULT 0
#define MPL_GPU_DEVICE_INVALID -1

#define MPL_GPU_DEV_AFFINITY_ENV NULL

#endif /* ifndef MPL_GPU_CUDA_H_INCLUDED */

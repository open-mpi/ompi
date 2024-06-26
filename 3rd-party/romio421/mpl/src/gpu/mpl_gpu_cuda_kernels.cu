/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl_gpu_cuda.h"
#include <stdio.h>

__global__ void MPL_gpu_kernel_trigger(MPL_gpu_event_t *var)
{
    *var -= 1;
    __threadfence_system();
}

__global__ void MPL_gpu_kernel_wait(MPL_gpu_event_t *var)
{
    while(*var > 0);
}

extern "C"
void MPL_gpu_enqueue_trigger(volatile int *var, cudaStream_t stream)
{
    cudaError_t cerr;
    void *args[] = {&var};
    cerr = cudaLaunchKernel((const void *) MPL_gpu_kernel_trigger, dim3(1,1,1), dim3(1,1,1),
                            args, 0, stream);
    if (cerr != cudaSuccess) {
        fprintf(stderr, "CUDA Error (%s): %s\n", __func__, cudaGetErrorString(cerr));
    }
}

extern "C"
void MPL_gpu_enqueue_wait(volatile int *var, cudaStream_t stream)
{
    cudaError_t cerr;

    void *args[] = {&var};
    cerr = cudaLaunchKernel((const void *) MPL_gpu_kernel_wait, dim3(1,1,1), dim3(1,1,1),
                            args, 0, stream);
    if (cerr != cudaSuccess) {
        fprintf(stderr, "CUDA Error (%s): %s\n", __func__, cudaGetErrorString(cerr));
    }
}

extern "C"
void MPL_gpu_event_init_count(MPL_gpu_event_t *var, int count)
{
    *var = count;
}

extern "C"
void MPL_gpu_event_complete(MPL_gpu_event_t *var)
{
    *var -= 1;
}

extern "C"
bool MPL_gpu_event_is_complete(MPL_gpu_event_t *var)
{
    return (*var) <= 0;
}

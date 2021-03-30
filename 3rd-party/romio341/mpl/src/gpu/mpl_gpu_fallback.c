/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_ipc_mem_handle_t * ipc_handle)
{
    abort();
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t ipc_handle, MPL_gpu_device_handle_t dev_handle,
                           void **ptr)
{
    abort();
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_unmap(void *ptr)
{
    abort();
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_malloc_host(void **ptr, size_t size)
{
    *ptr = MPL_malloc(size, MPL_MEM_BUFFER);
    return MPL_SUCCESS;
}

int MPL_gpu_free_host(void *ptr)
{
    MPL_free(ptr);
    return MPL_SUCCESS;
}

int MPL_gpu_register_host(const void *ptr, size_t size)
{
    return MPL_SUCCESS;
}

int MPL_gpu_unregister_host(const void *ptr)
{
    return MPL_SUCCESS;
}

int MPL_gpu_malloc(void **ptr, size_t size, MPL_gpu_device_handle_t h_device)
{
    abort();
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_free(void *ptr)
{
    abort();
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_init(int *device_count, int *max_dev_id_ptr)
{
    return MPL_SUCCESS;
}

int MPL_gpu_finalize()
{
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_id(MPL_gpu_device_handle_t dev_handle, int *dev_id)
{
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_handle(int dev_id, MPL_gpu_device_handle_t * dev_handle)
{
    return MPL_SUCCESS;
}

int MPL_gpu_get_global_dev_ids(int *global_ids, int count)
{
    return MPL_SUCCESS;
}

int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len)
{
    return MPL_SUCCESS;
}

int MPL_gpu_free_hook_register(void (*free_hook) (void *dptr))
{
    return MPL_SUCCESS;
}

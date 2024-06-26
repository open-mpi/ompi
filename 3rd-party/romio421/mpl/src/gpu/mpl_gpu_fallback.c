/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"
#include <assert.h>

int MPL_gpu_query_pointer_is_strict_dev(const void *ptr, MPL_pointer_attr_t * attr)
{
    return 0;
}

int MPL_gpu_get_dev_count(int *dev_cnt, int *dev_id, int *subdevice_id)
{
    *dev_cnt = *dev_id = *subdevice_id = -1;
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_list(int *dev_count, char ***dev_list, bool is_subdev)
{
    int ret = MPL_SUCCESS;
    *dev_list = NULL;
    return ret;
}

int MPL_gpu_dev_affinity_to_env(int dev_count, char **dev_list, char **env)
{
    int ret = MPL_SUCCESS;
    *env = NULL;
    return ret;
}

int MPL_gpu_init_device_mappings(int max_devid, int max_subdev_id)
{
    return MPL_SUCCESS;
}

int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_device_attr * ptr_attr,
                              MPL_gpu_ipc_mem_handle_t * ipc_handle)
{
    abort();
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_destroy(const void *ptr, MPL_pointer_attr_t * gpu_attr)
{
    abort();
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t * ipc_handle, int dev_id, void **ptr)
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

int MPL_gpu_init(int debug_summary)
{
    MPL_gpu_info.enable_ipc = false;
    MPL_gpu_info.specialized_cache = false;
    return MPL_SUCCESS;
}

int MPL_gpu_finalize(void)
{
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_id_from_attr(MPL_pointer_attr_t * attr)
{
    return -1;
}

int MPL_gpu_get_root_device(int dev_id)
{
    return -1;
}

int MPL_gpu_global_to_local_dev_id(int global_dev_id)
{
    return -1;
}

int MPL_gpu_local_to_global_dev_id(int local_dev_id)
{
    return -1;
}

int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len)
{
    return MPL_SUCCESS;
}

int MPL_gpu_free_hook_register(void (*free_hook) (void *dptr))
{
    return MPL_SUCCESS;
}

int MPL_gpu_fast_memcpy(void *src, MPL_pointer_attr_t * src_attr, void *dest,
                        MPL_pointer_attr_t * dest_attr, size_t size)
{
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_imemcpy(void *dest_ptr, void *src_ptr, size_t size, int dev,
                    MPL_gpu_copy_direction_t dir, MPL_gpu_engine_type_t engine_type,
                    MPL_gpu_request * req, bool commit)
{
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_test(MPL_gpu_request * req, int *completed)
{
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_launch_hostfn(int stream, MPL_gpu_hostfn fn, void *data)
{
    return -1;
}

bool MPL_gpu_stream_is_valid(MPL_gpu_stream_t stream)
{
    return false;
}

void MPL_gpu_enqueue_trigger(MPL_gpu_event_t * var, MPL_gpu_stream_t stream)
{
    assert(0);
}

void MPL_gpu_enqueue_wait(MPL_gpu_event_t * var, MPL_gpu_stream_t stream)
{
    assert(0);
}

void MPL_gpu_event_init_count(MPL_gpu_event_t * var, int count)
{
    *var = count;
}

void MPL_gpu_event_complete(MPL_gpu_event_t * var)
{
    *var -= 1;
}

bool MPL_gpu_event_is_complete(MPL_gpu_event_t * var)
{
    return (*var) <= 0;
}

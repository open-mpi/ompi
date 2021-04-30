/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_GPU_H_INCLUDED
#define MPL_GPU_H_INCLUDED

#include "mplconfig.h"

#undef MPL_HAVE_GPU
#ifdef MPL_HAVE_CUDA
#define MPL_HAVE_GPU MPL_GPU_TYPE_CUDA
#include "mpl_gpu_cuda.h"
#elif defined MPL_HAVE_ZE
#define MPL_HAVE_GPU MPL_GPU_TYPE_ZE
#include "mpl_gpu_ze.h"
#else
#include "mpl_gpu_fallback.h"
#endif

typedef enum {
    MPL_GPU_POINTER_UNREGISTERED_HOST = 0,
    MPL_GPU_POINTER_REGISTERED_HOST,
    MPL_GPU_POINTER_DEV,
    MPL_GPU_POINTER_MANAGED
} MPL_pointer_type_t;

typedef struct {
    MPL_pointer_type_t type;
    MPL_gpu_device_handle_t device;
} MPL_pointer_attr_t;

typedef enum {
    MPL_GPU_TYPE_NONE = 0,
    MPL_GPU_TYPE_CUDA,
    MPL_GPU_TYPE_ZE,
} MPL_gpu_type_t;

#ifndef MPL_HAVE_GPU
/* inline the query function in the fallback path to provide compiler optimization opportunity */
static inline int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr)
{
    attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
    attr->device = -1;

    return MPL_SUCCESS;
}

#endif /* ! MPL_HAVE_GPU */

int MPL_gpu_query_support(MPL_gpu_type_t * type);
int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr);

int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_ipc_mem_handle_t * ipc_handle);
int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t ipc_handle, MPL_gpu_device_handle_t dev_handle,
                           void **ptr);
int MPL_gpu_ipc_handle_unmap(void *ptr);

int MPL_gpu_malloc_host(void **ptr, size_t size);
int MPL_gpu_free_host(void *ptr);
int MPL_gpu_register_host(const void *ptr, size_t size);
int MPL_gpu_unregister_host(const void *ptr);

int MPL_gpu_malloc(void **ptr, size_t size, MPL_gpu_device_handle_t h_device);
int MPL_gpu_free(void *ptr);

int MPL_gpu_init(int *device_count, int *max_dev_id_ptr);
int MPL_gpu_finalize(void);

int MPL_gpu_get_dev_id(MPL_gpu_device_handle_t dev_handle, int *dev_id);
int MPL_gpu_get_dev_handle(int dev_id, MPL_gpu_device_handle_t * dev_handle);
int MPL_gpu_get_global_dev_ids(int *global_ids, int count);
int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len);

int MPL_gpu_free_hook_register(void (*free_hook) (void *dptr));

#endif /* ifndef MPL_GPU_H_INCLUDED */

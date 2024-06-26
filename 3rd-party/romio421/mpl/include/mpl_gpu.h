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
#elif defined MPL_HAVE_HIP
#define MPL_HAVE_GPU MPL_GPU_TYPE_HIP
#include "mpl_gpu_hip.h"
#else
#include "mpl_gpu_fallback.h"
#endif

typedef enum {
    MPL_GPU_POINTER_UNREGISTERED_HOST = 0,
    MPL_GPU_POINTER_REGISTERED_HOST,
    MPL_GPU_POINTER_DEV,
    MPL_GPU_POINTER_MANAGED
} MPL_pointer_type_t;

typedef enum {
    MPL_GPU_IPC_HANDLE_SHAREABLE = 0,
    MPL_GPU_IPC_HANDLE_SHAREABLE_FD
} MPL_gpu_ipc_handle_type_t;

typedef struct {
    MPL_pointer_type_t type;
    MPL_gpu_device_handle_t device;
    MPL_gpu_device_attr device_attr;
} MPL_pointer_attr_t;

typedef enum {
    MPL_GPU_TYPE_NONE = 0,
    MPL_GPU_TYPE_CUDA,
    MPL_GPU_TYPE_ZE,
    MPL_GPU_TYPE_HIP,
} MPL_gpu_type_t;

typedef enum {
    MPL_GPU_ENGINE_TYPE_COMPUTE = 0,
    MPL_GPU_ENGINE_TYPE_COPY_HIGH_BANDWIDTH,
    MPL_GPU_ENGINE_TYPE_COPY_LOW_LATENCY,
    MPL_GPU_ENGINE_TYPE_LAST,
} MPL_gpu_engine_type_t;

#define MPL_GPU_ENGINE_NUM_TYPES 3

typedef enum {
    MPL_GPU_COPY_D2H = 0,
    MPL_GPU_COPY_H2D,
    MPL_GPU_COPY_D2D_INCOMING,
    MPL_GPU_COPY_D2D_OUTGOING,
    MPL_GPU_COPY_DIRECTION_NONE,
} MPL_gpu_copy_direction_t;

#define MPL_GPU_COPY_DIRECTION_TYPES 4

typedef struct {
    /* Input */
    int debug_summary;
    bool use_immediate_cmdlist;
    bool roundrobin_cmdq;
    /* Output */
    bool enable_ipc;
    MPL_gpu_ipc_handle_type_t ipc_handle_type;
    /* Input/Output */
    bool specialized_cache;
} MPL_gpu_info_t;

extern MPL_gpu_info_t MPL_gpu_info;

#ifndef MPL_HAVE_GPU
/* inline the query function in the fallback path to provide compiler optimization opportunity */
static inline int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr)
{
    attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
    attr->device = -1;

    return MPL_SUCCESS;
}

static inline int MPL_gpu_query_pointer_is_dev(const void *ptr, MPL_pointer_attr_t * attr)
{
    return 0;
}

static inline int MPL_gpu_query_is_same_dev(int dev1, int dev2)
{
    return dev1 == dev2;
}
#endif /* ! MPL_HAVE_GPU */

int MPL_gpu_query_support(MPL_gpu_type_t * type);
int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr);
int MPL_gpu_query_pointer_is_dev(const void *ptr, MPL_pointer_attr_t * attr);
int MPL_gpu_query_pointer_is_strict_dev(const void *ptr, MPL_pointer_attr_t * attr);
int MPL_gpu_query_is_same_dev(int dev1, int dev2);

int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_device_attr * ptr_attr,
                              MPL_gpu_ipc_mem_handle_t * mpl_ipc_handle);
/* Used in ipc_handle_free_hook. Needed for fd-based ipc mechanism. */
int MPL_gpu_ipc_handle_destroy(const void *ptr, MPL_pointer_attr_t * gpu_attr);
int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t * mpl_ipc_handle, int dev_id, void **ptr);
int MPL_gpu_ipc_handle_unmap(void *ptr);

int MPL_gpu_malloc_host(void **ptr, size_t size);
int MPL_gpu_free_host(void *ptr);
int MPL_gpu_register_host(const void *ptr, size_t size);
int MPL_gpu_unregister_host(const void *ptr);

int MPL_gpu_malloc(void **ptr, size_t size, MPL_gpu_device_handle_t h_device);
int MPL_gpu_free(void *ptr);

int MPL_gpu_init(int debug_summary);
int MPL_gpu_finalize(void);

int MPL_gpu_global_to_local_dev_id(int global_dev_id);
int MPL_gpu_local_to_global_dev_id(int local_dev_id);

int MPL_gpu_get_dev_id_from_attr(MPL_pointer_attr_t * attr);
int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len);
int MPL_gpu_get_root_device(int dev_id);

int MPL_gpu_free_hook_register(void (*free_hook) (void *dptr));
int MPL_gpu_get_dev_count(int *dev_cnt, int *dev_id, int *subdevice_id);
int MPL_gpu_get_dev_list(int *dev_count, char ***dev_list, bool is_subdev);
int MPL_gpu_dev_affinity_to_env(int dev_count, char **dev_list, char **env);

int MPL_gpu_init_device_mappings(int max_devid, int max_subdev_id);

int MPL_gpu_fast_memcpy(void *src, MPL_pointer_attr_t * src_attr, void *dest,
                        MPL_pointer_attr_t * dest_attr, size_t size);

int MPL_gpu_imemcpy(void *dest_ptr, void *src_ptr, size_t size, int dev,
                    MPL_gpu_copy_direction_t dir, MPL_gpu_engine_type_t engine_type,
                    MPL_gpu_request * req, bool commit);
int MPL_gpu_test(MPL_gpu_request * req, int *completed);

typedef void (*MPL_gpu_hostfn) (void *data);
int MPL_gpu_launch_hostfn(MPL_gpu_stream_t stream, MPL_gpu_hostfn fn, void *data);
bool MPL_gpu_stream_is_valid(MPL_gpu_stream_t stream);
void MPL_gpu_enqueue_trigger(MPL_gpu_event_t * var, MPL_gpu_stream_t stream);
void MPL_gpu_enqueue_wait(MPL_gpu_event_t * var, MPL_gpu_stream_t stream);

/* the synchronization event has the similar semantics as completion counter,
 * init to a count, then each completion decrement it by 1. */
void MPL_gpu_event_init_count(MPL_gpu_event_t * var, int count);
void MPL_gpu_event_complete(MPL_gpu_event_t * var);
bool MPL_gpu_event_is_complete(MPL_gpu_event_t * var);

#endif /* ifndef MPL_GPU_H_INCLUDED */

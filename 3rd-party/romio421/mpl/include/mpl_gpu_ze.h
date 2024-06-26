/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_GPU_ZE_H_INCLUDED
#define MPL_GPU_ZE_H_INCLUDED

#include "level_zero/ze_api.h"

typedef struct {
    ze_memory_allocation_properties_t prop;
    ze_device_handle_t device;
} ze_alloc_attr_t;

typedef struct {
    int fds[2];
    uint32_t nfds;
    pid_t pid;
    int dev_id;
    uint64_t mem_id;
} fd_pid_t;

typedef struct _MPL_gpu_ipc_mem_handle_t {
    ze_ipc_mem_handle_t ipc_handles[2];
    fd_pid_t data;
} MPL_gpu_ipc_mem_handle_t;

typedef ze_device_handle_t MPL_gpu_device_handle_t;
typedef ze_alloc_attr_t MPL_gpu_device_attr;

typedef struct MPL_cmdlist_pool {
    ze_command_list_handle_t cmdList;
    int dev;
    int engine;
    struct MPL_cmdlist_pool *next, *prev;
} MPL_cmdlist_pool_t;

typedef struct MPL_ze_event {
    ze_event_handle_t event;
    struct MPL_ze_event *next, *prev;
} MPL_gpu_event;

typedef struct {
    MPL_gpu_event *gpu_event;
    MPL_cmdlist_pool_t *cmdList;
} MPL_gpu_request;

/* FIXME: implement ze stream */
typedef int MPL_gpu_stream_t;

typedef volatile int MPL_gpu_event_t;

#define MPL_GPU_STREAM_DEFAULT 0
#define MPL_GPU_DEVICE_INVALID NULL

#define MPL_GPU_DEV_AFFINITY_ENV "ZE_AFFINITY_MASK"

/* ZE specific function */
int MPL_ze_init_device_fds(int *num_fds, int *device_fds, int *bdfs);
void MPL_ze_set_fds(int num_fds, int *fds, int *bdfs);
void MPL_ze_ipc_remove_cache_handle(void *dptr);
int MPL_ze_ipc_handle_create(const void *ptr, MPL_gpu_device_attr * ptr_attr, int local_dev_id,
                             int use_shared_fd, MPL_gpu_ipc_mem_handle_t * ipc_handle);
int MPL_ze_ipc_handle_map(MPL_gpu_ipc_mem_handle_t * ipc_handle, int is_shared_handle, int dev_id,
                          int is_mmap, size_t size, void **ptr);
int MPL_ze_ipc_handle_mmap_host(MPL_gpu_ipc_mem_handle_t * ipc_handle, int shared_handle,
                                int dev_id, size_t size, void **ptr);
int MPL_ze_mmap_device_pointer(void *dptr, MPL_gpu_device_attr * attr,
                               MPL_gpu_device_handle_t device, void **mmaped_ptr);
int MPL_ze_mmap_handle_unmap(void *ptr, int dev_id);

#endif /* ifndef MPL_GPU_ZE_H_INCLUDED */

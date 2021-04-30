/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"
#include <dlfcn.h>
#include <assert.h>

#define CUDA_ERR_CHECK(ret) if (unlikely((ret) != cudaSuccess)) goto fn_fail
#define CU_ERR_CHECK(ret) if (unlikely((ret) != CUDA_SUCCESS)) goto fn_fail

typedef struct gpu_free_hook {
    void (*free_hook) (void *dptr);
    struct gpu_free_hook *next;
} gpu_free_hook_s;

static gpu_free_hook_s *free_hook_chain = NULL;

static CUresult CUDAAPI(*sys_cuMemFree) (CUdeviceptr dptr);
static cudaError_t CUDARTAPI(*sys_cudaFree) (void *dptr);

static int gpu_mem_hook_init();

int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr)
{
    cudaError_t ret;
    struct cudaPointerAttributes ptr_attr;
    ret = cudaPointerGetAttributes(&ptr_attr, ptr);
    if (ret == cudaSuccess) {
        switch (ptr_attr.type) {
            case cudaMemoryTypeUnregistered:
                attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
                attr->device = ptr_attr.device;
                break;
            case cudaMemoryTypeHost:
                attr->type = MPL_GPU_POINTER_REGISTERED_HOST;
                attr->device = ptr_attr.device;
                break;
            case cudaMemoryTypeDevice:
                attr->type = MPL_GPU_POINTER_DEV;
                attr->device = ptr_attr.device;
                break;
            case cudaMemoryTypeManaged:
                attr->type = MPL_GPU_POINTER_MANAGED;
                attr->device = ptr_attr.device;
                break;
        }
    } else if (ret == cudaErrorInvalidValue) {
        attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
        attr->device = -1;
    } else {
        goto fn_fail;
    }

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_ipc_mem_handle_t * ipc_handle)
{
    cudaError_t ret;

    ret = cudaIpcGetMemHandle(ipc_handle, (void *) ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t ipc_handle, MPL_gpu_device_handle_t dev_handle,
                           void **ptr)
{
    cudaError_t ret;
    int prev_devid;

    cudaGetDevice(&prev_devid);
    cudaSetDevice(dev_handle);
    ret = cudaIpcOpenMemHandle(ptr, ipc_handle, cudaIpcMemLazyEnablePeerAccess);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    cudaSetDevice(prev_devid);
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_unmap(void *ptr)
{
    cudaError_t ret;
    ret = cudaIpcCloseMemHandle(ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_malloc_host(void **ptr, size_t size)
{
    cudaError_t ret;
    ret = cudaMallocHost(ptr, size);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_free_host(void *ptr)
{
    cudaError_t ret;
    ret = cudaFreeHost(ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_register_host(const void *ptr, size_t size)
{
    cudaError_t ret;
    ret = cudaHostRegister((void *) ptr, size, cudaHostRegisterDefault);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_unregister_host(const void *ptr)
{
    cudaError_t ret;
    ret = cudaHostUnregister((void *) ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_malloc(void **ptr, size_t size, MPL_gpu_device_handle_t h_device)
{
    int mpl_errno = MPL_SUCCESS;
    int prev_devid;
    cudaError_t ret;
    cudaGetDevice(&prev_devid);
    cudaSetDevice(h_device);
    ret = cudaMalloc(ptr, size);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    cudaSetDevice(prev_devid);
    return mpl_errno;
  fn_fail:
    mpl_errno = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_free(void *ptr)
{
    cudaError_t ret;
    ret = cudaFree(ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_init(int *device_count, int *max_dev_id_ptr)
{
    int count, max_dev_id = -1;
    cudaError_t ret = cudaGetDeviceCount(&count);
    CUDA_ERR_CHECK(ret);

    char *visible_devices = getenv("CUDA_VISIBLE_DEVICES");
    if (visible_devices) {
        uintptr_t len = strlen(visible_devices);
        char *devices = MPL_malloc(len + 1, MPL_MEM_OTHER);
        char *free_ptr = devices;
        memcpy(devices, visible_devices, len + 1);
        for (int i = 0; i < count; i++) {
            int global_dev_id;
            char *tmp = strtok(devices, ",");
            assert(tmp);
            global_dev_id = atoi(tmp);
            if (global_dev_id > max_dev_id)
                max_dev_id = global_dev_id;
            devices = NULL;
        }
        MPL_free(free_ptr);
    } else {
        max_dev_id = count - 1;
    }

    *max_dev_id_ptr = max_dev_id;
    *device_count = count;

    gpu_mem_hook_init();

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_finalize()
{
    gpu_free_hook_s *prev;
    while (free_hook_chain) {
        prev = free_hook_chain;
        free_hook_chain = free_hook_chain->next;
        MPL_free(prev);
    }
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_id(MPL_gpu_device_handle_t dev_handle, int *dev_id)
{
    *dev_id = dev_handle;
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_handle(int dev_id, MPL_gpu_device_handle_t * dev_handle)
{
    *dev_handle = dev_id;
    return MPL_SUCCESS;
}

int MPL_gpu_get_global_dev_ids(int *global_ids, int count)
{
    char *visible_devices = getenv("CUDA_VISIBLE_DEVICES");

    if (visible_devices) {
        uintptr_t len = strlen(visible_devices);
        char *devices = MPL_malloc(len + 1, MPL_MEM_OTHER);
        char *free_ptr = devices;
        memcpy(devices, visible_devices, len + 1);
        for (int i = 0; i < count; i++) {
            char *tmp = strtok(devices, ",");
            assert(tmp);
            global_ids[i] = atoi(tmp);
            devices = NULL;
        }
        MPL_free(free_ptr);
    } else {
        for (int i = 0; i < count; i++) {
            global_ids[i] = i;
        }
    }

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len)
{
    CUresult curet;

    curet = cuMemGetAddressRange((CUdeviceptr *) pbase, (size_t *) len, (CUdeviceptr) ptr);
    CU_ERR_CHECK(curet);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

static void gpu_free_hooks_cb(void *dptr)
{
    gpu_free_hook_s *current = free_hook_chain;
    if (dptr != NULL) {
        /* we call gpu hook only when dptr != NULL */
        while (current) {
            current->free_hook(dptr);
            current = current->next;
        }
    }
    return;
}

static int gpu_mem_hook_init()
{
    void *libcuda_handle;
    void *libcudart_handle;

    libcuda_handle = dlopen("libcuda.so", RTLD_LAZY | RTLD_GLOBAL);
    assert(libcuda_handle);
    libcudart_handle = dlopen("libcudart.so", RTLD_LAZY | RTLD_GLOBAL);
    assert(libcudart_handle);

    sys_cuMemFree = (void *) dlsym(libcuda_handle, "cuMemFree");
    assert(sys_cuMemFree);
    sys_cudaFree = (void *) dlsym(libcudart_handle, "cudaFree");
    assert(sys_cudaFree);
    return MPL_SUCCESS;
}

int MPL_gpu_free_hook_register(void (*free_hook) (void *dptr))
{
    gpu_free_hook_s *hook_obj = MPL_malloc(sizeof(gpu_free_hook_s), MPL_MEM_OTHER);
    assert(hook_obj);
    hook_obj->free_hook = free_hook;
    hook_obj->next = NULL;
    if (!free_hook_chain)
        free_hook_chain = hook_obj;
    else {
        hook_obj->next = free_hook_chain;
        free_hook_chain = hook_obj;
    }

    return MPL_SUCCESS;
}

CUresult CUDAAPI cuMemFree(CUdeviceptr dptr)
{
    CUresult result;
    gpu_free_hooks_cb((void *) dptr);
    result = sys_cuMemFree(dptr);
    return (result);
}

cudaError_t CUDARTAPI cudaFree(void *dptr)
{
    cudaError_t result;
    gpu_free_hooks_cb(dptr);
    result = sys_cudaFree(dptr);
    return result;
}

/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"
#include "mpl_str.h"
#include <dlfcn.h>
#include <assert.h>

#define CUDA_ERR_CHECK(ret) if (unlikely((ret) != cudaSuccess)) goto fn_fail
#define CU_ERR_CHECK(ret) if (unlikely((ret) != CUDA_SUCCESS)) goto fn_fail

typedef struct gpu_free_hook {
    void (*free_hook) (void *dptr);
    struct gpu_free_hook *next;
} gpu_free_hook_s;

static int gpu_initialized = 0;
static int device_count = -1;
static int max_dev_id = -1;
static char **device_list = NULL;
#define MAX_GPU_STR_LEN 256
static char affinity_env[MAX_GPU_STR_LEN] = { 0 };

static int *local_to_global_map;        /* [device_count] */
static int *global_to_local_map;        /* [max_dev_id + 1]   */

static gpu_free_hook_s *free_hook_chain = NULL;

static CUresult CUDAAPI(*sys_cuMemFree) (CUdeviceptr dptr);
static cudaError_t CUDARTAPI(*sys_cudaFree) (void *dptr);

static int gpu_mem_hook_init();
static MPL_initlock_t free_hook_mutex = MPL_INITLOCK_INITIALIZER;

int MPL_gpu_get_dev_count(int *dev_cnt, int *dev_id, int *subdevice_id)
{
    int ret = MPL_SUCCESS;
    if (!gpu_initialized) {
        ret = MPL_gpu_init(0);
    }

    *dev_cnt = device_count;
    *dev_id = max_dev_id;
    *subdevice_id = 0;
    return ret;
}

int MPL_gpu_get_dev_list(int *dev_count, char ***dev_list, bool is_subdev)
{
    int ret = MPL_SUCCESS;
    if (!gpu_initialized) {
        ret = MPL_gpu_init(0);
    }

    device_list = (char **) MPL_malloc(device_count * sizeof(char *), MPL_MEM_OTHER);
    assert(device_list != NULL);

    for (int i = 0; i < device_count; ++i) {
        int str_len = snprintf(NULL, 0, "%d", i);
        device_list[i] = (char *) MPL_malloc((str_len + 1) * sizeof(char *), MPL_MEM_OTHER);
        sprintf(device_list[i], "%d", i);
    }

    *dev_count = device_count;
    *dev_list = device_list;
    return ret;
}

int MPL_gpu_dev_affinity_to_env(int dev_count, char **dev_list, char **env)
{
    int ret = MPL_SUCCESS;
    memset(affinity_env, 0, MAX_GPU_STR_LEN);
    if (dev_count == 0) {
        snprintf(affinity_env, 3, "-1");
    } else {
        int str_offset = 0;
        for (int i = 0; i < dev_count; ++i) {
            if (i) {
                MPL_strncpy(affinity_env + str_offset, ",", MAX_GPU_STR_LEN - str_offset);
                str_offset++;
            }
            MPL_strncpy(affinity_env + str_offset, dev_list[i], MAX_GPU_STR_LEN - str_offset);
            str_offset += strlen(dev_list[i]);
        }
    }
    *env = affinity_env;
    return ret;
}

int MPL_gpu_init_device_mappings(int max_devid, int max_subdev_id)
{
    return MPL_SUCCESS;
}

int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    ret = cudaPointerGetAttributes(&attr->device_attr, ptr);
    if (ret == cudaSuccess) {
        switch (attr->device_attr.type) {
            case cudaMemoryTypeUnregistered:
                attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
                attr->device = attr->device_attr.device;
                break;
            case cudaMemoryTypeHost:
                attr->type = MPL_GPU_POINTER_REGISTERED_HOST;
                attr->device = attr->device_attr.device;
                break;
            case cudaMemoryTypeDevice:
                attr->type = MPL_GPU_POINTER_DEV;
                attr->device = attr->device_attr.device;
                break;
            case cudaMemoryTypeManaged:
                attr->type = MPL_GPU_POINTER_MANAGED;
                attr->device = attr->device_attr.device;
                break;
        }
    } else if (ret == cudaErrorInvalidValue) {
        attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
        attr->device = -1;
    } else {
        goto fn_fail;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_query_pointer_is_dev(const void *ptr, MPL_pointer_attr_t * attr)
{
    MPL_pointer_attr_t a;

    if (attr == NULL) {
        MPL_gpu_query_pointer_attr(ptr, &a);
        attr = &a;
    }
    return attr->type == MPL_GPU_POINTER_DEV;
}

int MPL_gpu_query_pointer_is_strict_dev(const void *ptr, MPL_pointer_attr_t * attr)
{
    MPL_pointer_attr_t a;

    if (attr == NULL) {
        MPL_gpu_query_pointer_attr(ptr, &a);
        attr = &a;
    }
    return attr->type == MPL_GPU_POINTER_DEV;
}

int MPL_gpu_query_is_same_dev(int dev1, int dev2)
{
    return dev1 == dev2;
}

int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_device_attr * ptr_attr,
                              MPL_gpu_ipc_mem_handle_t * ipc_handle)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;

    ret = cudaIpcGetMemHandle(ipc_handle, (void *) ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_ipc_handle_destroy(const void *ptr, MPL_pointer_attr_t * gpu_attr)
{
    return MPL_SUCCESS;
}

int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t * ipc_handle, int dev_id, void **ptr)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    int prev_devid;

    cudaGetDevice(&prev_devid);
    cudaSetDevice(dev_id);
    ret = cudaIpcOpenMemHandle(ptr, *ipc_handle, cudaIpcMemLazyEnablePeerAccess);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    cudaSetDevice(prev_devid);
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_ipc_handle_unmap(void *ptr)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    ret = cudaIpcCloseMemHandle(ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_malloc_host(void **ptr, size_t size)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    ret = cudaMallocHost(ptr, size);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_free_host(void *ptr)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    ret = cudaFreeHost(ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_register_host(const void *ptr, size_t size)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    ret = cudaHostRegister((void *) ptr, size, cudaHostRegisterDefault);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_unregister_host(const void *ptr)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    ret = cudaHostUnregister((void *) ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_malloc(void **ptr, size_t size, MPL_gpu_device_handle_t h_device)
{
    int mpl_err = MPL_SUCCESS;
    int prev_devid;
    cudaError_t ret;
    cudaGetDevice(&prev_devid);
    cudaSetDevice(h_device);
    ret = cudaMalloc(ptr, size);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    cudaSetDevice(prev_devid);
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_free(void *ptr)
{
    int mpl_err = MPL_SUCCESS;
    cudaError_t ret;
    ret = cudaFree(ptr);
    CUDA_ERR_CHECK(ret);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_init(int debug_summary)
{
    int mpl_err = MPL_SUCCESS;
    if (gpu_initialized) {
        goto fn_exit;
    }

    cudaError_t ret = cudaGetDeviceCount(&device_count);
    if (ret == cudaErrorNoDevice) {
        /* call cudaGetLastError() to consume the error */
        ret = cudaGetLastError();
        assert(ret == cudaErrorNoDevice);
        device_count = 0;
    } else {
        CUDA_ERR_CHECK(ret);
    }

    if (device_count <= 0) {
        gpu_initialized = 1;
        goto fn_exit;
    }

    MPL_gpu_info.debug_summary = debug_summary;
    MPL_gpu_info.enable_ipc = true;
    MPL_gpu_info.ipc_handle_type = MPL_GPU_IPC_HANDLE_SHAREABLE;
    MPL_gpu_info.specialized_cache = false;

    char *visible_devices = getenv("CUDA_VISIBLE_DEVICES");
    if (visible_devices) {
        local_to_global_map = MPL_malloc(device_count * sizeof(int), MPL_MEM_OTHER);

        char *devices = MPL_strdup(visible_devices);
        char *free_ptr = devices;
        for (int i = 0; i < device_count; i++) {
            char *tmp = strtok(devices, ",");
            assert(tmp);
            local_to_global_map[i] = atoi(tmp);
            if (max_dev_id < local_to_global_map[i]) {
                max_dev_id = local_to_global_map[i];
            }
            devices = NULL;
        }
        MPL_free(free_ptr);

        global_to_local_map = MPL_malloc((max_dev_id + 1) * sizeof(int), MPL_MEM_OTHER);
        for (int i = 0; i < max_dev_id + 1; i++) {
            global_to_local_map[i] = -1;
        }
        for (int i = 0; i < device_count; i++) {
            global_to_local_map[local_to_global_map[i]] = i;
        }
    } else {
        local_to_global_map = MPL_malloc(device_count * sizeof(int), MPL_MEM_OTHER);
        global_to_local_map = MPL_malloc(device_count * sizeof(int), MPL_MEM_OTHER);
        for (int i = 0; i < device_count; i++) {
            local_to_global_map[i] = i;
            global_to_local_map[i] = i;
        }
        max_dev_id = device_count - 1;
    }

    /* gpu shm module would cache gpu handle to accelerate intra-node
     * communication; we must register hooks for memory-related functions
     * in cuda, such as cudaFree and cuMemFree, to track user behaviors on
     * the memory buffer and invalidate cached handle/buffer respectively
     * for result correctness. */
    MPL_initlock_lock(&free_hook_mutex);
    gpu_mem_hook_init();
    MPL_initlock_unlock(&free_hook_mutex);
    gpu_initialized = 1;

    if (MPL_gpu_info.debug_summary) {
        printf("==== GPU Init (CUDA) ====\n");
        printf("device_count: %d\n", device_count);
        if (visible_devices) {
            printf("CUDA_VISIBLE_DEVICES: %s\n", visible_devices);
        }
        printf("=========================\n");
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_finalize(void)
{
    if (device_count <= 0) {
        goto fn_exit;
    }

    MPL_free(local_to_global_map);
    MPL_free(global_to_local_map);

    gpu_free_hook_s *prev;
    MPL_initlock_lock(&free_hook_mutex);
    while (free_hook_chain) {
        prev = free_hook_chain;
        free_hook_chain = free_hook_chain->next;
        MPL_free(prev);
    }
    free_hook_chain = NULL;
    MPL_initlock_unlock(&free_hook_mutex);

    /* Reset initialization state */
    gpu_initialized = 0;

  fn_exit:
    return MPL_SUCCESS;
}

int MPL_gpu_global_to_local_dev_id(int global_dev_id)
{
    if (global_dev_id > max_dev_id) {
        return -1;
    }

    return global_to_local_map[global_dev_id];
}

int MPL_gpu_local_to_global_dev_id(int local_dev_id)
{
    assert(local_dev_id < device_count);
    return local_to_global_map[local_dev_id];
}

int MPL_gpu_get_dev_id_from_attr(MPL_pointer_attr_t * attr)
{
    return attr->device;
}

int MPL_gpu_get_root_device(int dev_id)
{
    return dev_id;
}

int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len)
{
    int mpl_err = MPL_SUCCESS;
    CUresult curet;

    /* get the device where the pointer is located */
    int device;
    struct cudaPointerAttributes attr;
    cudaPointerGetAttributes(&attr, ptr);
    device = attr.device;

    /* set the device to query the address range, otherwise the driver
     * might return CUDA_ERROR_NOT_FOUND */
    int device_save;
    cudaGetDevice(&device_save);
    cudaSetDevice(device);

    curet = cuMemGetAddressRange((CUdeviceptr *) pbase, (size_t *) len, (CUdeviceptr) ptr);
    CU_ERR_CHECK(curet);

    /* set device back to saved value */
    cudaSetDevice(device_save);

  fn_exit:
    return mpl_err;
  fn_fail:
    mpl_err = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
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

    MPL_initlock_lock(&free_hook_mutex);
    if (!free_hook_chain)
        free_hook_chain = hook_obj;
    else {
        hook_obj->next = free_hook_chain;
        free_hook_chain = hook_obj;
    }
    MPL_initlock_unlock(&free_hook_mutex);

    return MPL_SUCCESS;
}

#ifdef HAVE_VISIBILITY
__attribute__ ((visibility("default")))
#endif
CUresult CUDAAPI cuMemFree(CUdeviceptr dptr)
{
    CUresult result;
    MPL_initlock_lock(&free_hook_mutex);
    if (!sys_cuMemFree) {
        gpu_mem_hook_init();
    }

    gpu_free_hooks_cb((void *) dptr);
    result = sys_cuMemFree(dptr);

    MPL_initlock_unlock(&free_hook_mutex);
    return (result);
}

#ifdef HAVE_VISIBILITY
__attribute__ ((visibility("default")))
#endif
cudaError_t CUDARTAPI cudaFree(void *dptr)
{
    cudaError_t result;
    MPL_initlock_lock(&free_hook_mutex);
    if (!sys_cudaFree) {
        gpu_mem_hook_init();
    }

    gpu_free_hooks_cb(dptr);
    result = sys_cudaFree(dptr);

    MPL_initlock_unlock(&free_hook_mutex);
    return result;
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

int MPL_gpu_launch_hostfn(cudaStream_t stream, MPL_gpu_hostfn fn, void *data)
{
    cudaError_t result;
    result = cudaLaunchHostFunc(stream, fn, data);
    return result;
}

bool MPL_gpu_stream_is_valid(MPL_gpu_stream_t stream)
{
    cudaError_t result;
    /* CUDA may blindly dereference the stream as pointer, which may segfault
     * if the wrong value is passed in. This is still better than segfault later
     * upon using the stream. */
    result = cudaStreamQuery(stream);
    return (result != cudaErrorInvalidResourceHandle);
}

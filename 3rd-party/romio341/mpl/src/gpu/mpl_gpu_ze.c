/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#include <assert.h>

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#ifdef MPL_HAVE_ZE

ze_driver_handle_t global_ze_driver_handle;
ze_device_handle_t *global_ze_devices_handle;
int gpu_ze_init_driver();

#define ZE_ERR_CHECK(ret) \
    do { \
        if (unlikely((ret) != ZE_RESULT_SUCCESS)) \
            goto fn_fail; \
    } while (0)

int MPL_gpu_init(int *device_count_ptr, int *max_dev_id_ptr)
{
    ze_result_t ret;
    int ret_error, device_count;
    ret_error = gpu_ze_init_driver();
    if (ret_error != MPL_SUCCESS)
        goto fn_fail;

    zeDriverGet(NULL, &global_ze_driver_handle);
    ret = zeDeviceGet(global_ze_driver_handle, &device_count, NULL);
    ZE_ERR_CHECK(ret);

    *max_dev_id_ptr = *device_count_ptr = device_count;
    global_ze_devices_handle =
        (ze_device_handle_t *) MPL_malloc(sizeof(ze_device_handle_t) * device_count, MPL_MEM_OTHER);
    ret = zeDeviceGet(global_ze_driver_handle, &device_count, global_ze_devices_handle);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return ret_error;
}

/* Loads a global ze driver */
int gpu_ze_init_driver()
{
    uint32_t driver_count = 0;
    ze_result_t ret;
    int ret_error = MPL_SUCCESS;
    ze_driver_handle_t *all_drivers;

    ret = zeDriverGet(&driver_count, NULL);
    ZE_ERR_CHECK(ret);
    if (driver_count == 0) {
        goto fn_fail;
    }

    all_drivers = MPL_malloc(driver_count * sizeof(ze_driver_handle_t), MPL_MEM_OTHER);
    if (all_drivers == NULL) {
        ret_error = MPL_ERR_GPU_NOMEM;
        goto fn_fail;
    }
    ret = zeDriverGet(&driver_count, all_drivers);
    ZE_ERR_CHECK(ret);

    int i, d;
    ze_device_handle_t *all_devices = NULL;
    /* Find a driver instance with a GPU device */
    for (i = 0; i < driver_count; ++i) {
        uint32_t device_count = 0;
        ret = zeDeviceGet(all_drivers[i], &device_count, NULL);
        ZE_ERR_CHECK(ret);
        all_devices = MPL_malloc(device_count * sizeof(ze_device_handle_t), MPL_MEM_OTHER);
        if (all_devices == NULL) {
            ret_error = MPL_ERR_GPU_NOMEM;
            goto fn_fail;
        }
        ret = zeDeviceGet(all_drivers[i], &device_count, all_devices);
        ZE_ERR_CHECK(ret);
        /* Check if the driver supports a gpu */
        for (d = 0; d < device_count; ++d) {
            ze_device_properties_t device_properties;
            ret = zeDeviceGetProperties(all_devices[d], &device_properties);
            ZE_ERR_CHECK(ret);

            if (ZE_DEVICE_TYPE_GPU == device_properties.type) {
                global_ze_driver_handle = all_drivers[i];
                break;
            }
        }

        MPL_free(all_devices);
        all_devices = NULL;
        if (NULL != global_ze_driver_handle) {
            break;
        }
    }

  fn_exit:
    MPL_free(all_drivers);
    return ret_error;
  fn_fail:
    MPL_free(all_devices);
    /* If error code is already set, preserve it */
    if (ret_error == MPL_SUCCESS)
        ret_error = MPL_ERR_GPU_INTERNAL;
    goto fn_exit;
}

int MPL_gpu_finalize()
{
    MPL_free(global_ze_devices_handle);
    return MPL_SUCCESS;
}

int MPL_gpu_ipc_handle_create(const void *ptr, MPL_gpu_ipc_mem_handle_t * ipc_handle)
{
    int mpl_err;
    ze_result_t ret;
    ret = zeDriverGetMemIpcHandle(global_ze_driver_handle, ptr, ipc_handle);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_ipc_handle_map(MPL_gpu_ipc_mem_handle_t ipc_handle, MPL_gpu_device_handle_t dev_handle,
                           void **ptr)
{
    int mpl_err = MPL_SUCCESS;
    ze_result_t ret;

    ret =
        zeDriverOpenMemIpcHandle(global_ze_driver_handle,
                                 global_ze_devices_handle[ipc_handle.global_dev_id],
                                 ipc_handle.handle, ZE_IPC_MEMORY_FLAG_NONE, ptr);
    if (ret != ZE_RESULT_SUCCESS) {
        mpl_err = MPL_ERR_GPU_INTERNAL;
        goto fn_fail;
    }

  fn_exit:
    return mpl_err;
  fn_fail:
    goto fn_exit;
}

int MPL_gpu_ipc_handle_unmap(void *ptr)
{
    ze_result_t ret;
    ret = zeDriverCloseMemIpcHandle(global_ze_driver_handle, ptr);
    ZE_ERR_CHECK(ret);

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_query_pointer_attr(const void *ptr, MPL_pointer_attr_t * attr)
{
    ze_result_t ret;
    ze_memory_allocation_properties_t ptr_attr;
    ze_device_handle_t device;
    ze_device_properties_t p_device_properties;
    ret = zeDriverGetMemAllocProperties(global_ze_driver_handle, ptr, &ptr_attr, &device);
    ZE_ERR_CHECK(ret);
    attr->device = device;
    switch (ptr_attr.type) {
        case ZE_MEMORY_TYPE_UNKNOWN:
            attr->type = MPL_GPU_POINTER_UNREGISTERED_HOST;
            break;
        case ZE_MEMORY_TYPE_HOST:
            attr->type = MPL_GPU_POINTER_REGISTERED_HOST;
            break;
        case ZE_MEMORY_TYPE_DEVICE:
            attr->type = MPL_GPU_POINTER_DEV;
            break;
        case ZE_MEMORY_TYPE_SHARED:
            attr->type = MPL_GPU_POINTER_MANAGED;
            break;
        default:
            goto fn_fail;
    }

  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_malloc(void **ptr, size_t size, MPL_gpu_device_handle_t h_device)
{
    int ret;
    size_t mem_alignment;
    ze_device_mem_alloc_desc_t device_desc;
    device_desc.flags = ZE_DEVICE_MEM_ALLOC_FLAG_DEFAULT;
    device_desc.ordinal = 0;    /* We currently support a single memory type */
    device_desc.version = ZE_DEVICE_MEM_ALLOC_DESC_VERSION_CURRENT;
    /* Currently ZE ignores this augument and uses an internal alignment
     * value. However, this behavior can change in the future. */
    mem_alignment = 1;
    ret = zeDriverAllocDeviceMem(global_ze_driver_handle, &device_desc,
                                 size, mem_alignment, h_device, ptr);

    ZE_ERR_CHECK(ret);
  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_malloc_host(void **ptr, size_t size)
{
    int ret;
    size_t mem_alignment;
    ze_host_mem_alloc_desc_t host_desc;
    host_desc.flags = ZE_HOST_MEM_ALLOC_FLAG_DEFAULT;
    host_desc.version = ZE_HOST_MEM_ALLOC_DESC_VERSION_CURRENT;

    /* Currently ZE ignores this augument and uses an internal alignment
     * value. However, this behavior can change in the future. */
    mem_alignment = 1;
    ret = zeDriverAllocHostMem(global_ze_driver_handle, &host_desc, size, mem_alignment, ptr);
    ZE_ERR_CHECK(ret);
  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_free(void *ptr)
{
    int ret;
    ret = zeDriverFreeMem(global_ze_driver_handle, ptr);
    ZE_ERR_CHECK(ret);
  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_free_host(void *ptr)
{
    int ret;
    ret = zeDriverFreeMem(global_ze_driver_handle, ptr);
    ZE_ERR_CHECK(ret);
  fn_exit:
    return MPL_SUCCESS;
  fn_fail:
    return MPL_ERR_GPU_INTERNAL;
}

int MPL_gpu_register_host(const void *ptr, size_t size)
{
    return MPL_SUCCESS;
}

int MPL_gpu_unregister_host(const void *ptr)
{
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_id(MPL_gpu_device_handle_t dev_handle, int *dev_id)
{
    ze_device_properties_t devproerty;

    zeDeviceGetProperties(dev_handle, &devproerty);
    *dev_id = devproerty.deviceId;
    return MPL_SUCCESS;
}

int MPL_gpu_get_dev_handle(int dev_id, MPL_gpu_device_handle_t * dev_handle)
{
    *dev_handle = device_handles[dev_id];
    return MPL_SUCCESS;
}

int MPL_gpu_get_global_dev_ids(int *global_ids, int count)
{
    for (int i = 0; i < count; ++i)
        global_ids[i] = i;
    return MPL_SUCCESS;
}

int MPL_gpu_get_buffer_bounds(const void *ptr, void **pbase, uintptr_t * len)
{
    /* TODO: need to find oneAPI function to retrieve base addr and buffer len */
    return MPL_SUCCESS;
}

int MPL_gpu_free_hook_register(void (*free_hook) (void *dptr))
{
    return MPL_SUCCESS;
}

#endif /* MPL_HAVE_ZE */

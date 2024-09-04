/*
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2024      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <cuda.h>

#include "accelerator_cuda.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/util/show_help.h"
#include "opal/util/proc.h"
/* Accelerator API's */
static int accelerator_cuda_check_addr(const void *addr, int *dev_id, uint64_t *flags);
static int accelerator_cuda_create_stream(int dev_id, opal_accelerator_stream_t **stream);

static int accelerator_cuda_create_event(int dev_id, opal_accelerator_event_t **event, bool enable_ipc);
static int accelerator_cuda_record_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);
static int accelerator_cuda_query_event(int dev_id, opal_accelerator_event_t *event);
static int accelerator_cuda_wait_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);

static int accelerator_cuda_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                  opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type);
static int accelerator_cuda_memcpy(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                            size_t size, opal_accelerator_transfer_type_t type);
static int accelerator_cuda_memmove_async(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                                          size_t size, opal_accelerator_stream_t *stream,
                                          opal_accelerator_transfer_type_t type);
static int accelerator_cuda_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                             opal_accelerator_transfer_type_t type);
static int accelerator_cuda_mem_alloc(int dev_id, void **ptr, size_t size);
static int accelerator_cuda_mem_release(int dev_id, void *ptr);
static int accelerator_cuda_mem_alloc_stream(int dev_id, void **ptr, size_t size,
                                             opal_accelerator_stream_t *stream);
static int accelerator_cuda_mem_release_stream(int dev_id, void *ptr, opal_accelerator_stream_t *stream);
static int accelerator_cuda_get_address_range(int dev_id, const void *ptr, void **base,
                                              size_t *size);

static bool accelerator_cuda_is_ipc_enabled(void);
static int accelerator_cuda_get_ipc_handle(int dev_id, void *dev_ptr,
                                           opal_accelerator_ipc_handle_t *handle);
static int accelerator_cuda_import_ipc_handle(int dev_id, uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                              opal_accelerator_ipc_handle_t *handle);
static int accelerator_cuda_open_ipc_handle(int dev_id, opal_accelerator_ipc_handle_t *handle,
                                            void **dev_ptr);
static int accelerator_cuda_compare_ipc_handles(uint8_t handle_1[IPC_MAX_HANDLE_SIZE],
                                                uint8_t handle_2[IPC_MAX_HANDLE_SIZE]);
static int accelerator_cuda_get_ipc_event_handle(opal_accelerator_event_t *event,
                                                 opal_accelerator_ipc_event_handle_t *handle);
static int accelerator_cuda_import_ipc_event_handle(uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                    opal_accelerator_ipc_event_handle_t *handle);
static int accelerator_cuda_open_ipc_event_handle(opal_accelerator_ipc_event_handle_t *handle,
                                                  opal_accelerator_event_t *event);

static int accelerator_cuda_host_register(int dev_id, void *ptr, size_t size);
static int accelerator_cuda_host_unregister(int dev_id, void *ptr);

static int accelerator_cuda_get_device(int *dev_id);
static int accelerator_cuda_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr);
static int accelerator_cuda_device_can_access_peer( int *access, int dev1, int dev2);

static int accelerator_cuda_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id);

static int accelerator_cuda_sync_stream(opal_accelerator_stream_t *stream);
static int accelerator_cuda_get_num_devices(int *num_devices);
static int accelerator_cuda_get_mem_bw(int device, float *bw);

#define GET_STREAM(_stream) \
    ((_stream) == MCA_ACCELERATOR_STREAM_DEFAULT ? 0 : *((CUstream *) (_stream)->stream))

opal_accelerator_base_module_t opal_accelerator_cuda_module =
{
    accelerator_cuda_check_addr,

    accelerator_cuda_create_stream,
    accelerator_cuda_sync_stream,

    accelerator_cuda_create_event,
    accelerator_cuda_record_event,
    accelerator_cuda_query_event,
    accelerator_cuda_wait_event,

    accelerator_cuda_memcpy_async,
    accelerator_cuda_memcpy,
    accelerator_cuda_memmove_async,
    accelerator_cuda_memmove,
    accelerator_cuda_mem_alloc,
    accelerator_cuda_mem_release,
    accelerator_cuda_mem_alloc_stream,
    accelerator_cuda_mem_release_stream,
    accelerator_cuda_get_address_range,

    accelerator_cuda_is_ipc_enabled,
    accelerator_cuda_get_ipc_handle,
    accelerator_cuda_import_ipc_handle,
    accelerator_cuda_open_ipc_handle,
    accelerator_cuda_compare_ipc_handles,
    accelerator_cuda_get_ipc_event_handle,
    accelerator_cuda_import_ipc_event_handle,
    accelerator_cuda_open_ipc_event_handle,

    accelerator_cuda_host_register,
    accelerator_cuda_host_unregister,

    accelerator_cuda_get_device,
    accelerator_cuda_get_device_pci_attr,
    accelerator_cuda_device_can_access_peer,

    accelerator_cuda_get_buffer_id,

    accelerator_cuda_get_num_devices,
    accelerator_cuda_get_mem_bw
};

static inline int opal_accelerator_cuda_delayed_init_check(void)
{
    if (OPAL_UNLIKELY(true != mca_accelerator_cuda_init_complete)) {
        return opal_accelerator_cuda_delayed_init();
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_get_device_id(CUcontext mem_ctx) {
    /* query the device from the context */
    int dev_id = -1;
    CUdevice ptr_dev;
    cuCtxPushCurrent(mem_ctx);
    cuCtxGetDevice(&ptr_dev);
    for (int i = 0; i < opal_accelerator_cuda_num_devices; ++i) {
        CUdevice dev;
        cuDeviceGet(&dev, i);
        if (dev == ptr_dev) {
            dev_id = i;
            break;
        }
    }
    cuCtxPopCurrent(&mem_ctx);
    return dev_id;
}

static int accelerator_cuda_check_vmm(CUdeviceptr dbuf, CUmemorytype *mem_type,
                                      int *dev_id)
{
#if OPAL_CUDA_VMM_SUPPORT
    static int device_count = -1;
    CUmemAllocationProp prop;
    CUmemLocation location;
    CUresult result;
    unsigned long long flags;
    CUmemGenericAllocationHandle alloc_handle;

    if (device_count == -1) {
        result = cuDeviceGetCount(&device_count);
        if (result != CUDA_SUCCESS) {
            return 0;
        }
    }

    result = cuMemRetainAllocationHandle(&alloc_handle, (void*)dbuf);
    if (result != CUDA_SUCCESS) {
        return 0;
    }

    result = cuMemGetAllocationPropertiesFromHandle(&prop, alloc_handle);
    if (result != CUDA_SUCCESS) {
        cuMemRelease(alloc_handle);
        return 0;
    }

    if (prop.location.type == CU_MEM_LOCATION_TYPE_DEVICE) {
        *mem_type = CU_MEMORYTYPE_DEVICE;
        *dev_id  = prop.location.id;
        cuMemRelease(alloc_handle);
        return 1;
    }

    if (prop.location.type == CU_MEM_LOCATION_TYPE_HOST_NUMA) {
        /* check if device has access */
        for (int i = 0; i < device_count; i++) {
            location.type = CU_MEM_LOCATION_TYPE_DEVICE;
            location.id   = i;
            result = cuMemGetAccess(&flags, &location, dbuf);
            if ((CUDA_SUCCESS == result) &&
                (CU_MEM_ACCESS_FLAGS_PROT_READWRITE == flags)) {
                *mem_type = CU_MEMORYTYPE_DEVICE;
                *dev_id  = i;
                cuMemRelease(alloc_handle);
                return 1;
            }
        }
    }

    /* host must have access as device access possibility is exhausted */
    *mem_type = CU_MEMORYTYPE_HOST;
    *dev_id = MCA_ACCELERATOR_NO_DEVICE_ID;
    cuMemRelease(alloc_handle);
    return 1;

#endif

    return 0;
}

static int accelerator_cuda_check_addr(const void *addr, int *dev_id, uint64_t *flags)
{
    CUresult result;
    int is_vmm = 0;
    int vmm_dev_id = MCA_ACCELERATOR_NO_DEVICE_ID;
    CUmemorytype vmm_mem_type = 0;
    CUmemorytype mem_type = 0;
    CUdeviceptr dbuf = (CUdeviceptr) addr;
    CUcontext ctx = NULL, mem_ctx = NULL;
    *dev_id = MCA_ACCELERATOR_NO_DEVICE_ID;

    if (NULL == addr || NULL == flags) {
        return OPAL_ERR_BAD_PARAM;
    }

    *flags = 0;

    is_vmm = accelerator_cuda_check_vmm(dbuf, &vmm_mem_type, &vmm_dev_id);

#if OPAL_CUDA_GET_ATTRIBUTES
    uint32_t is_managed = 0;
    /* With CUDA 7.0, we can get multiple attributes with a single call */
    CUpointer_attribute attributes[3] = {CU_POINTER_ATTRIBUTE_MEMORY_TYPE,
                                         CU_POINTER_ATTRIBUTE_CONTEXT,
                                         CU_POINTER_ATTRIBUTE_IS_MANAGED};
    void *attrdata[] = {(void *) &mem_type, (void *) &mem_ctx, (void *) &is_managed};

    result = cuPointerGetAttributes(3, attributes, attrdata, dbuf);
    OPAL_OUTPUT_VERBOSE((101, opal_accelerator_base_framework.framework_output,
                         "dbuf=%p, mem_type=%d, mem_ctx=%p, is_managed=%d, result=%d", (void *) dbuf,
                         (int) mem_type, (void *) mem_ctx, is_managed, result));

    /* Mark unified memory buffers with a flag.  This will allow all unified
     * memory to be forced through host buffers.  Note that this memory can
     * be either host or device so we need to set this flag prior to that check. */
    if (1 == is_managed) {
        *flags |= MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY;
    }
    if (CUDA_SUCCESS != result) {
        /* If cuda is not initialized, assume it is a host buffer. It can also
         * return invalid value if the ptr was not allocated by, mapped by,
         * or registered with a CUcontext */
        if (CUDA_ERROR_NOT_INITIALIZED == result || CUDA_ERROR_INVALID_VALUE == result) {
            return 0;
        } else {
            return OPAL_ERROR;
        }
    } else if (CU_MEMORYTYPE_HOST == mem_type) {
        if (is_vmm && (vmm_mem_type == CU_MEMORYTYPE_DEVICE)) {
            mem_type = CU_MEMORYTYPE_DEVICE;
            *dev_id = vmm_dev_id;
        } else {
            /* Host memory, nothing to do here */
            return 0;
        }
    } else if (0 == mem_type) {
        /* This can happen when CUDA is initialized but dbuf is not valid CUDA pointer */
        return 0;
    } else {
        if (is_vmm) {
            *dev_id = vmm_dev_id;
        } else {
            /* query the device from the context */
            *dev_id = accelerator_cuda_get_device_id(mem_ctx);
        }
    }
#else /* OPAL_CUDA_GET_ATTRIBUTES */
    result = cuPointerGetAttribute(&mem_type, CU_POINTER_ATTRIBUTE_MEMORY_TYPE, dbuf);
    if (CUDA_SUCCESS != result) {
        /* If cuda is not initialized, assume it is a host buffer. */
        if (CUDA_ERROR_NOT_INITIALIZED == result) {
            return 0;
        } else {
            return OPAL_ERROR;
        }
    } else if (CU_MEMORYTYPE_HOST == mem_type) {
        if (is_vmm && (vmm_mem_type == CU_MEMORYTYPE_DEVICE)) {
            mem_type = CU_MEMORYTYPE_DEVICE;
            *dev_id = vmm_dev_id;
        } else {
            /* Host memory, nothing to do here */
            return 0;
        }
    } else {
        if (is_vmm) {
            *dev_id = vmm_dev_id;
        } else {
            result = cuPointerGetAttribute(&mem_ctx,
                                           CU_POINTER_ATTRIBUTE_CONTEXT, dbuf);
            /* query the device from the context */
            *dev_id = accelerator_cuda_get_device_id(mem_ctx);
        }
    }
#endif /* OPAL_CUDA_GET_ATTRIBUTES */

    /* Must be a device pointer */
    assert(CU_MEMORYTYPE_DEVICE == mem_type);

    /* This piece of code was added in to handle in a case involving
     * OMP threads.  The user had initialized CUDA and then spawned
     * two threads.  The first thread had the CUDA context, but the
     * second thread did not.  We therefore had no context to act upon
     * and future CUDA driver calls would fail.  Therefore, if we have
     * GPU memory, but no context, get the context from the GPU memory
     * and set the current context to that.  It is rare that we will not
     * have a context. */
    result = cuCtxGetCurrent(&ctx);
    if (OPAL_UNLIKELY(NULL == ctx)) {
        if (CUDA_SUCCESS == result) {
#if !OPAL_CUDA_GET_ATTRIBUTES
            result = cuPointerGetAttribute(&mem_ctx, CU_POINTER_ATTRIBUTE_CONTEXT, dbuf);
            if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
                opal_output(0,
                            "CUDA: error calling cuPointerGetAttribute: "
                            "result=%d, ptr=%p aborting...",
                            result, addr);
                return OPAL_ERROR;
            }
#endif /* OPAL_CUDA_GET_ATTRIBUTES */
            if (is_vmm) {
                /* This function is expected to set context if pointer is device
                 * accessible but VMM allocations have NULL context associated
                 * which cannot be set against the calling thread */
                opal_output(0,
                        "CUDA: unable to set context with the given pointer"
                        "ptr=%p aborting...", addr);
                return OPAL_ERROR;
            }

            result = cuCtxSetCurrent(mem_ctx);
            if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
                opal_output(0,
                            "CUDA: error calling cuCtxSetCurrent: "
                            "result=%d, ptr=%p aborting...",
                            result, addr);
                return OPAL_ERROR;
            } else {
                OPAL_OUTPUT_VERBOSE(
                    (10, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxSetCurrent passed: ptr=%p", addr));
            }
        } else {
            /* Print error and proceed */
            opal_output(0,
                        "CUDA: error calling cuCtxGetCurrent: "
                        "result=%d, ptr=%p aborting...",
                        result, addr);
            return OPAL_ERROR;
        }
    }

    /* WORKAROUND - There are times when the above code determines a pice of memory
     * is GPU memory, but it actually is not.  That has been seen on multi-GPU systems
     * with 6 or 8 GPUs on them. Therefore, we will do this extra check.  Note if we
     * made it this far, then the assumption at this point is we have GPU memory.
     * Unfotunately, this extra call is costing us another 100 ns almost doubling
     * the cost of this entire function. */
    if (OPAL_LIKELY(((CUDA_VERSION > 7000) ? 0 : 1))) {
        CUdeviceptr pbase;
        size_t psize;
        result = cuMemGetAddressRange(&pbase, &psize, dbuf);
        if (CUDA_SUCCESS != result) {
            opal_output_verbose(5, opal_accelerator_base_framework.framework_output,
                                "CUDA: cuMemGetAddressRange failed on this pointer: result=%d, buf=%p "
                                "Overriding check and setting to host pointer. ",
                                result, (void *) dbuf);
            /* This cannot be GPU memory if the previous call failed */
            return 0;
        }
    }
    /* First access on a device pointer finalizes CUDA support initialization. */
    (void)opal_accelerator_cuda_delayed_init_check();
    return 1;
}

static int accelerator_cuda_create_stream(int dev_id, opal_accelerator_stream_t **stream)
{
    CUresult result;
    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }
    *stream = (opal_accelerator_stream_t*)OBJ_NEW(opal_accelerator_cuda_stream_t);
    if (NULL == *stream) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (*stream)->stream = malloc(sizeof(CUstream));
    if (NULL == (*stream)->stream) {
        OBJ_RELEASE(*stream);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    result = cuStreamCreate((*stream)->stream, 0);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamCreate failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        free((*stream)->stream);
        OBJ_RELEASE(*stream);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static void opal_accelerator_cuda_stream_destruct(opal_accelerator_cuda_stream_t *stream)
{
    CUresult result;

    if (MCA_ACCELERATOR_STREAM_DEFAULT != (opal_accelerator_stream_t *)stream &&
        NULL != stream->base.stream) {
        result = cuStreamDestroy(*(CUstream *)stream->base.stream);
        if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
            opal_show_help("help-accelerator-cuda.txt", "cuStreamDestroy failed", true,
                           result);
        }
        free(stream->base.stream);
    }
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_cuda_stream_t,
    opal_accelerator_stream_t,
    NULL,
    opal_accelerator_cuda_stream_destruct);

static int accelerator_cuda_create_event(int dev_id, opal_accelerator_event_t **event, bool enable_ipc)
{
    CUresult result;
    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    *event = (opal_accelerator_event_t*)OBJ_NEW(opal_accelerator_cuda_event_t);
    if (NULL == *event) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (*event)->event = malloc(sizeof(CUevent));
    if (NULL == (*event)->event) {
        OBJ_RELEASE(*event);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    result = cuEventCreate((*event)->event, enable_ipc ? CU_EVENT_DISABLE_TIMING|CU_EVENT_INTERPROCESS :
			   CU_EVENT_DISABLE_TIMING);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuEventCreate failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        free((*event)->event);
        OBJ_RELEASE(*event);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static void opal_accelerator_cuda_event_destruct(opal_accelerator_cuda_event_t *event)
{
    CUresult result;
    if (NULL != event->base.event) {
        result = cuEventDestroy(*(CUevent *)event->base.event);
        if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
            opal_show_help("help-accelerator-cuda.txt", "cuEventDestroy failed", true,
                           result);
        }
        free(event->base.event);
    }
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_cuda_event_t,
    opal_accelerator_event_t,
    NULL,
    opal_accelerator_cuda_event_destruct);

static int accelerator_cuda_record_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream)
{
    CUresult result;

    if ((MCA_ACCELERATOR_STREAM_DEFAULT != stream &&
        (NULL == stream || NULL == stream->stream)) ||
        NULL == event) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuEventRecord(*(CUevent *)event->event, GET_STREAM(stream));
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuEventRecord failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_query_event(int dev_id, opal_accelerator_event_t *event)
{
    CUresult result;

    if (NULL == event) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuEventQuery(*(CUevent *)event->event);
    switch (result) {
        case CUDA_SUCCESS:
            {
                return OPAL_SUCCESS;
                break;
            }
        case CUDA_ERROR_NOT_READY:
            {
                return OPAL_ERR_RESOURCE_BUSY;
                break;
            }
        default:
            {
                opal_show_help("help-accelerator-cuda.txt", "cuEventQuery failed", true,
                               result);
                return OPAL_ERROR;
            }
    }
}
static int accelerator_cuda_wait_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_cuda_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                  opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if ((MCA_ACCELERATOR_STREAM_DEFAULT != stream && NULL == stream) ||
        NULL == dest || NULL == src || size < 0) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (0 == size) {
        return OPAL_SUCCESS;
    }

    result = cuMemcpyAsync((CUdeviceptr) dest, (CUdeviceptr) src, size, GET_STREAM(stream));
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemcpyAsync failed", true, dest, src,
                       size, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_memcpy(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                            size_t size, opal_accelerator_transfer_type_t type)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if (NULL == dest || NULL == src || size < 0) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (0 == size) {
        return OPAL_SUCCESS;
    }

    /* Async copy then synchronize is the default behavior as some applications
     * cannot utilize synchronous copies. In addition, host memory does not need
     * to be page-locked if an Async memory copy is done (It just makes it synchronous
     * which is what we want anyway):
     * https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#concurrent-execution-host-device
     * Additionally, cuMemcpy is not necessarily always synchronous. See:
     * https://docs.nvidia.com/cuda/cuda-driver-api/api-sync-behavior.html
     * TODO: Add optimizations for type field */
    result = cuMemcpyAsync((CUdeviceptr) dest, (CUdeviceptr) src, size,
                           *(CUstream *) opal_accelerator_cuda_memcpy_stream.base.stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemcpyAsync failed", true, dest, src,
                       size, result);
        return OPAL_ERROR;
    }
    result = cuStreamSynchronize(*(CUstream *) opal_accelerator_cuda_memcpy_stream.base.stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamSynchronize failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_memmove_async(int dest_dev_id, int src_dev_id, void *dest,
                                          const void *src, size_t size,
                                          opal_accelerator_stream_t *stream,
                                          opal_accelerator_transfer_type_t type)
{
    CUdeviceptr tmp;
    CUresult result;
    void *ptr;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if (NULL == dest || NULL == src || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = accelerator_cuda_mem_alloc_stream(src_dev_id, &ptr, size, stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        return OPAL_ERROR;
    }
    tmp = (CUdeviceptr)ptr;
    result = cuMemcpyAsync(tmp, (CUdeviceptr) src, size, *(CUstream*)stream->stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemcpyAsync failed", true, tmp, src, size,
                       result);
        return OPAL_ERROR;
    }
    result = cuMemcpyAsync((CUdeviceptr) dest, tmp, size, *(CUstream*)stream->stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemcpyAsync failed", true, dest, tmp,
                       size, result);
        return OPAL_ERROR;
    }
    return accelerator_cuda_mem_release_stream(src_dev_id, ptr, stream);
}

static int accelerator_cuda_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                    opal_accelerator_transfer_type_t type)
{
    int ret;

    ret = accelerator_cuda_memmove_async(dest_dev_id, src_dev_id, dest, src, size, &opal_accelerator_cuda_memcpy_stream.base, type);
    if (OPAL_SUCCESS != ret) {
        return OPAL_ERROR;
    }
    ret = accelerator_cuda_sync_stream(&opal_accelerator_cuda_memcpy_stream.base);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamSynchronize failed", true,
                       OPAL_PROC_MY_HOSTNAME, ret);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_mem_alloc(int dev_id, void **ptr, size_t size)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if (NULL == ptr || 0 == size) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuMemAlloc((CUdeviceptr *) ptr, size);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemAlloc failed", true,
                        OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}



static int accelerator_cuda_mem_alloc_stream(int dev_id, void **addr, size_t size,
                                             opal_accelerator_stream_t *stream)
{

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    /* fall-back to regular stream allocation */

    CUresult result = cuMemAllocAsync((CUdeviceptr*)addr, size, *(CUstream*)stream->stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemAlloc failed", true,
                        OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_mem_release(int dev_id, void *ptr)
{
    CUresult result;
    if (NULL != ptr) {
        result = cuMemFree((CUdeviceptr) ptr);
        if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
            opal_show_help("help-accelerator-cuda.txt", "cuMemFree failed", true,
                           OPAL_PROC_MY_HOSTNAME, result);
            return OPAL_ERROR;
        }
    }
    return 0;
}

static int accelerator_cuda_mem_release_stream(int dev_id, void *addr,
                                               opal_accelerator_stream_t *stream)
{
    CUresult result;

    if (NULL == stream || NULL == addr) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuMemFreeAsync((CUdeviceptr)addr, *(CUstream*)stream->stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemFree failed", true,
                        OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}


static int accelerator_cuda_sync_stream(opal_accelerator_stream_t *stream)
{
    CUresult result;
    result = cuStreamSynchronize(*(CUstream*)stream->stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamSynchronize failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}


static int accelerator_cuda_get_address_range(int dev_id, const void *ptr, void **base,
                                       size_t *size)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if (NULL == ptr || NULL == base || NULL == size) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuMemGetAddressRange((CUdeviceptr *) base, size, (CUdeviceptr) ptr);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemGetAddressRange failed 2", true,
                       OPAL_PROC_MY_HOSTNAME, result, ptr);
        return OPAL_ERROR;
    } else {
        opal_output_verbose(50, opal_accelerator_base_framework.framework_output,
                            "CUDA: cuMemGetAddressRange passed: addr=%p, pbase=%p, psize=%lu ",
                            ptr, *(char **) base, *size);
    }
    return 0;
}

static bool accelerator_cuda_is_ipc_enabled(void)
{
    return true;
}

static void mca_accelerator_cuda_ipc_handle_destruct(opal_accelerator_cuda_ipc_handle_t *handle)
{
    if (NULL != handle && NULL != handle->base.dev_ptr) {
        cuIpcCloseMemHandle((CUdeviceptr) handle->base.dev_ptr);
        handle->base.dev_ptr = NULL;
    }
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_cuda_ipc_handle_t,
    opal_accelerator_ipc_handle_t,
    NULL,
    mca_accelerator_cuda_ipc_handle_destruct);

static int accelerator_cuda_get_ipc_handle(int dev_id, void *dev_ptr,
                                           opal_accelerator_ipc_handle_t *handle)
{
    if (NULL == dev_ptr || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    CUipcMemHandle cuda_ipc_handle;
    opal_accelerator_cuda_ipc_handle_t *cuda_handle = (opal_accelerator_cuda_ipc_handle_t *) handle;

    OBJ_CONSTRUCT(cuda_handle, opal_accelerator_cuda_ipc_handle_t);
    cuda_handle->base.dev_ptr = NULL;

    CUresult err = cuIpcGetMemHandle(&cuda_ipc_handle,
				     (CUdeviceptr)dev_ptr);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != err)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "Error in cuIpcGetMemHandle dev_ptr %p", dev_ptr);
        OBJ_DESTRUCT(cuda_handle);
        return OPAL_ERROR;
    }
    memcpy(cuda_handle->base.handle, &cuda_ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int accelerator_cuda_import_ipc_handle(int dev_id, uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                              opal_accelerator_ipc_handle_t *handle)
{
    opal_accelerator_cuda_ipc_handle_t *cuda_handle = (opal_accelerator_cuda_ipc_handle_t *) handle;
    OBJ_CONSTRUCT(cuda_handle, opal_accelerator_cuda_ipc_handle_t);
    memcpy(cuda_handle->base.handle, ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int accelerator_cuda_open_ipc_handle(int dev_id, opal_accelerator_ipc_handle_t *handle,
                                            void **dev_ptr)
{
    if (NULL == dev_ptr || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    CUresult err = cuIpcOpenMemHandle((CUdeviceptr *) &handle->dev_ptr,
				      *(CUipcMemHandle*)handle->handle,
				      CU_IPC_MEM_LAZY_ENABLE_PEER_ACCESS);
    if (CUDA_ERROR_ALREADY_MAPPED == err) {
        return OPAL_ERR_WOULD_BLOCK;
    }
    else if (CUDA_SUCCESS != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in cuIpcOpenMemHandle");
        return OPAL_ERROR;
    }
    *dev_ptr = handle->dev_ptr;

    return OPAL_SUCCESS;
}

static int accelerator_cuda_compare_ipc_handles(uint8_t handle_1[IPC_MAX_HANDLE_SIZE],
                                                uint8_t handle_2[IPC_MAX_HANDLE_SIZE])
{
    return memcmp(handle_1, handle_2, IPC_MAX_HANDLE_SIZE);
}

static void mca_accelerator_cuda_ipc_event_handle_destruct(opal_accelerator_cuda_ipc_handle_t *handle)
{
    // Just a place holder, there is no cuIpcCloseEventHandle.
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_cuda_ipc_event_handle_t,
    opal_accelerator_ipc_event_handle_t,
    NULL,
    mca_accelerator_cuda_ipc_event_handle_destruct);

static int accelerator_cuda_get_ipc_event_handle(opal_accelerator_event_t *event,
                                                 opal_accelerator_ipc_event_handle_t *handle)
{
    if (NULL == event || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    CUipcEventHandle cuda_ipc_handle;
    opal_accelerator_cuda_ipc_event_handle_t *cuda_handle = (opal_accelerator_cuda_ipc_event_handle_t *) handle;
    OBJ_CONSTRUCT(cuda_handle, opal_accelerator_cuda_ipc_event_handle_t);

    memset(cuda_ipc_handle.reserved, 0, CU_IPC_HANDLE_SIZE);
    CUresult err = cuIpcGetEventHandle(&cuda_ipc_handle,
                                          *((CUevent *)event->event));
    if (OPAL_UNLIKELY(CUDA_SUCCESS != err)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in cuIpcGetEventHandle");
        OBJ_DESTRUCT(cuda_handle);
        return OPAL_ERROR;
    }
    memcpy(cuda_handle->base.handle, &cuda_ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int accelerator_cuda_import_ipc_event_handle(uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                    opal_accelerator_ipc_event_handle_t *handle)
{
    opal_accelerator_cuda_ipc_handle_t *cuda_handle = (opal_accelerator_cuda_ipc_handle_t *) handle;

    OBJ_CONSTRUCT(cuda_handle, opal_accelerator_cuda_ipc_handle_t);
    memcpy(cuda_handle->base.handle, ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int accelerator_cuda_open_ipc_event_handle(opal_accelerator_ipc_event_handle_t *handle,
                                                  opal_accelerator_event_t *event)
{
    if (NULL == event || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    opal_accelerator_cuda_ipc_event_handle_t *cuda_handle = (opal_accelerator_cuda_ipc_event_handle_t *) handle;
    opal_accelerator_cuda_event_t *cuda_event = (opal_accelerator_cuda_event_t *) event;
    OBJ_CONSTRUCT(cuda_event, opal_accelerator_cuda_event_t);
    cuda_event->base.event = malloc(sizeof(CUevent));
    if (NULL == cuda_event->base.event) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    CUresult err = cuIpcOpenEventHandle( (CUevent *)cuda_event->base.event,
					 *((CUipcEventHandle*)cuda_handle->base.handle));
    if (OPAL_UNLIKELY(CUDA_SUCCESS != err)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in cuIpcOpenEventHandle");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int accelerator_cuda_host_register(int dev_id, void *ptr, size_t size)
{
    CUresult result;
    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if (NULL == ptr && size > 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuMemHostRegister(ptr, size, 0);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemHostRegister failed", true,
                       ptr, size, OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int accelerator_cuda_host_unregister(int dev_id, void *ptr)
{
    CUresult result;
    if (NULL != ptr) {
        result = cuMemHostUnregister(ptr);
        if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
            opal_show_help("help-accelerator-cuda.txt", "cuMemHostUnregister failed", true,
                           ptr, OPAL_PROC_MY_HOSTNAME, result);
            return OPAL_ERROR;
        }
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_get_device(int *dev_id)
{
    CUdevice cuDev;
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if (NULL == dev_id) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuCtxGetDevice(&cuDev);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuCtxGetDevice failed", true,
                       result);
        return OPAL_ERROR;
    }
    *dev_id = cuDev;
    return 0;
}

static int accelerator_cuda_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr)
{
    CUresult result;
    int ret;
    static const int PCI_BUS_ID_LENGTH = 13;
    char pci_bus_id[PCI_BUS_ID_LENGTH];
    char domain_id[5] = {0}, bus_id[3] = {0}, device_id[3] = {0}, function_id[2] = {0};

    if (NULL == pci_attr) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuDeviceGetPCIBusId(pci_bus_id, PCI_BUS_ID_LENGTH, dev_id);

    if (CUDA_SUCCESS != result) {
        opal_output_verbose(5, opal_accelerator_base_framework.framework_output,
                            "CUDA: Failed to get device PCI bus id");
        return OPAL_ERROR;
    }

    ret = sscanf(pci_bus_id, "%4s:%2s:%2s.%1s", domain_id, bus_id, device_id, function_id);
    if (4 > ret) {
        opal_output_verbose(5, opal_accelerator_base_framework.framework_output,
                            "CUDA: Failed to parse device PCI bus id");
        return OPAL_ERROR;
    }

    errno = 0;
    pci_attr->domain_id = strtol(domain_id, NULL, 16);
    pci_attr->bus_id = strtol(bus_id, NULL, 16);
    pci_attr->device_id = strtol(device_id, NULL, 16);
    pci_attr->function_id = strtol(function_id, NULL, 16);
    if (0 != errno) {
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int accelerator_cuda_device_can_access_peer(int *access, int dev1, int dev2)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    if (NULL == access) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuDeviceCanAccessPeer(access, (CUdevice) dev1, (CUdevice) dev2);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuDeviceCanAccessPeer failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return 0;
}

/*
 * Get the buffer ID from the memory.
 * This is needed to ensure the cached registration is not stale.  If
 * we fail to get buffer ID, print an error and set buffer ID to 0.
 * Also set SYNC_MEMOPS on any GPU registration to ensure that
 * synchronous copies complete before the buffer is accessed.
 */
static int accelerator_cuda_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id)
{
    CUresult result;
    int enable = 1;

    int delayed_init = opal_accelerator_cuda_delayed_init_check();
    if (OPAL_UNLIKELY(OPAL_SUCCESS != delayed_init)) {
        return delayed_init;
    }

    result = cuPointerGetAttribute((unsigned long long *)buf_id, CU_POINTER_ATTRIBUTE_BUFFER_ID, (CUdeviceptr) addr);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "bufferID failed", true, OPAL_PROC_MY_HOSTNAME,
                       result);
        return OPAL_ERROR;
    }
    result = cuPointerSetAttribute(&enable, CU_POINTER_ATTRIBUTE_SYNC_MEMOPS,
                                       (CUdeviceptr) addr);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuPointerSetAttribute failed", true,
                       OPAL_PROC_MY_HOSTNAME, result, addr);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}



static int accelerator_cuda_get_num_devices(int *num_devices)
{

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    *num_devices = opal_accelerator_cuda_num_devices;
    return OPAL_SUCCESS;
}

static int accelerator_cuda_get_mem_bw(int device, float *bw)
{
    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }
    assert(opal_accelerator_cuda_mem_bw != NULL);

    *bw = opal_accelerator_cuda_mem_bw[device];
    return OPAL_SUCCESS;
}

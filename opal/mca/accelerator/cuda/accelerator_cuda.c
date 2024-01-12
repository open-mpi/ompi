/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
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

static int accelerator_cuda_create_event(int dev_id, opal_accelerator_event_t **event);
static int accelerator_cuda_record_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);
static int accelerator_cuda_query_event(int dev_id, opal_accelerator_event_t *event);

static int accelerator_cuda_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                  opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type);
static int accelerator_cuda_memcpy(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                            size_t size, opal_accelerator_transfer_type_t type);
static int accelerator_cuda_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                             opal_accelerator_transfer_type_t type);
static int accelerator_cuda_mem_alloc(int dev_id, void **ptr, size_t size);
static int accelerator_cuda_mem_release(int dev_id, void *ptr);
static int accelerator_cuda_get_address_range(int dev_id, const void *ptr, void **base,
                                              size_t *size);

static int accelerator_cuda_host_register(int dev_id, void *ptr, size_t size);
static int accelerator_cuda_host_unregister(int dev_id, void *ptr);

static int accelerator_cuda_get_device(int *dev_id);
static int accelerator_cuda_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr);
static int accelerator_cuda_device_can_access_peer( int *access, int dev1, int dev2);

static int accelerator_cuda_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id);

opal_accelerator_base_module_t opal_accelerator_cuda_module =
{
    accelerator_cuda_check_addr,

    accelerator_cuda_create_stream,

    accelerator_cuda_create_event,
    accelerator_cuda_record_event,
    accelerator_cuda_query_event,

    accelerator_cuda_memcpy_async,
    accelerator_cuda_memcpy,
    accelerator_cuda_memmove,
    accelerator_cuda_mem_alloc,
    accelerator_cuda_mem_release,
    accelerator_cuda_get_address_range,

    accelerator_cuda_host_register,
    accelerator_cuda_host_unregister,

    accelerator_cuda_get_device,
    accelerator_cuda_get_device_pci_attr,
    accelerator_cuda_device_can_access_peer,

    accelerator_cuda_get_buffer_id
};

static int accelerator_cuda_check_addr(const void *addr, int *dev_id, uint64_t *flags)
{
    CUresult result;
    CUmemorytype mem_type = 0;
    CUdeviceptr dbuf = (CUdeviceptr) addr;
    CUcontext ctx = NULL, mem_ctx = NULL;
    *dev_id = MCA_ACCELERATOR_NO_DEVICE_ID;

    if (NULL == addr || NULL == flags) {
        return OPAL_ERR_BAD_PARAM;
    }

    *flags = 0;

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
        /* Host memory, nothing to do here */
        return 0;
    } else if (0 == mem_type) {
        /* This can happen when CUDA is initialized but dbuf is not valid CUDA pointer */
        return 0;
    }
    /* Must be a device pointer */
    assert(CU_MEMORYTYPE_DEVICE == mem_type);
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
        /* Host memory, nothing to do here */
        return 0;
    }
    /* Must be a device pointer */
    assert(CU_MEMORYTYPE_DEVICE == mem_type);
#endif /* OPAL_CUDA_GET_ATTRIBUTES */

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

    /* WORKAROUND - They are times when the above code determines a pice of memory
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
    opal_accelerator_cuda_delayed_init();
    return 1;
}

static int accelerator_cuda_create_stream(int dev_id, opal_accelerator_stream_t **stream)
{
    CUresult result;
    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
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
    if (OPAL_UNLIKELY(result != CUDA_SUCCESS)) {
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

    if (NULL != stream->base.stream) {
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

static int accelerator_cuda_create_event(int dev_id, opal_accelerator_event_t **event)
{
    CUresult result;
    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
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
    result = cuEventCreate((*event)->event, CU_EVENT_DISABLE_TIMING);
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

    if (NULL == stream || NULL == event) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuEventRecord(*(CUevent *)event->event, *(CUstream *)stream->stream);
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

static int accelerator_cuda_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                  opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    if (NULL == stream || NULL == dest || NULL == src || size < 0) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (0 == size) {
        return OPAL_SUCCESS;
    }

    result = cuMemcpyAsync((CUdeviceptr) dest, (CUdeviceptr) src, size, *(CUstream *)stream->stream);
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

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
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
    result = cuMemcpyAsync((CUdeviceptr) dest, (CUdeviceptr) src, size, opal_accelerator_cuda_memcpy_stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemcpyAsync failed", true, dest, src,
                       size, result);
        return OPAL_ERROR;
    }
    result = cuStreamSynchronize(opal_accelerator_cuda_memcpy_stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamSynchronize failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int accelerator_cuda_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                             opal_accelerator_transfer_type_t type)
{
    CUdeviceptr tmp;
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    if (NULL == dest || NULL == src || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = cuMemAlloc(&tmp, size);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        return OPAL_ERROR;
    }
    result = cuMemcpyAsync(tmp, (CUdeviceptr) src, size, opal_accelerator_cuda_memcpy_stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemcpyAsync failed", true, tmp, src, size,
                       result);
        return OPAL_ERROR;
    }
    result = cuMemcpyAsync((CUdeviceptr) dest, tmp, size, opal_accelerator_cuda_memcpy_stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuMemcpyAsync failed", true, dest, tmp,
                       size, result);
        return OPAL_ERROR;
    }
    result = cuStreamSynchronize(opal_accelerator_cuda_memcpy_stream);
    if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamSynchronize failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        return OPAL_ERROR;
    }
    cuMemFree(tmp);
    return OPAL_SUCCESS;
}

static int accelerator_cuda_mem_alloc(int dev_id, void **ptr, size_t size)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    if (NULL == ptr || 0 == size) {
        return OPAL_ERR_BAD_PARAM;
    }

    if (size > 0) {
        result = cuMemAlloc((CUdeviceptr *) ptr, size);
        if (OPAL_UNLIKELY(CUDA_SUCCESS != result)) {
            opal_show_help("help-accelerator-cuda.txt", "cuMemAlloc failed", true,
                           OPAL_PROC_MY_HOSTNAME, result);
            return OPAL_ERROR;
        }
    }
    return 0;
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

static int accelerator_cuda_get_address_range(int dev_id, const void *ptr, void **base,
                                       size_t *size)
{
    CUresult result;

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
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

static int accelerator_cuda_host_register(int dev_id, void *ptr, size_t size)
{
    CUresult result;
    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
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

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
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

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
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

    int delayed_init = opal_accelerator_cuda_delayed_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    result = cuPointerGetAttribute((unsigned long long *)buf_id, CU_POINTER_ATTRIBUTE_BUFFER_ID, (CUdeviceptr) addr);
    if (OPAL_UNLIKELY(result != CUDA_SUCCESS)) {
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

/*
 * Copyright (c) 2022-2025 Advanced Micro Devices, Inc. All Rights reserved.
 * Copyright (c) 2024      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "accelerator_rocm.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/constants.h"
#include "opal/util/output.h"
#include "ompi/info/info_memkind.h"

/* Accelerator API's */
static int mca_accelerator_rocm_check_addr(const void *addr, int *dev_id, uint64_t *flags);
static int mca_accelerator_rocm_create_stream(int dev_id, opal_accelerator_stream_t **stream);

static int mca_accelerator_rocm_create_event(int dev_id, opal_accelerator_event_t **event, bool enable_ipc);
static int mca_accelerator_rocm_record_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);
static int mca_accelerator_rocm_query_event(int dev_id, opal_accelerator_event_t *event);
static int mca_accelerator_rocm_wait_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);

static int mca_accelerator_rocm_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                  opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type);
static int mca_accelerator_rocm_memcpy(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                            size_t size, opal_accelerator_transfer_type_t type);
static int mca_accelerator_rocm_memmove_async(int dest_dev_id, int src_dev_id, void *dest,
                                              const void *src, size_t size,
                                              opal_accelerator_stream_t *stream,
                                              opal_accelerator_transfer_type_t type);
static int mca_accelerator_rocm_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                        opal_accelerator_transfer_type_t type);
static int mca_accelerator_rocm_mem_alloc(int dev_id, void **ptr, size_t size);
static int mca_accelerator_rocm_mem_release(int dev_id, void *ptr);
static int mca_accelerator_rocm_mem_alloc_stream(int dev_id, void **ptr, size_t size,
                                                 opal_accelerator_stream_t *stream);
static int mca_accelerator_rocm_mem_release_stream(int dev_id, void *ptr, opal_accelerator_stream_t *stream);
static int mca_accelerator_rocm_get_address_range(int dev_id, const void *ptr, void **base,
                                                  size_t *size);

static bool mca_accelerator_rocm_is_ipc_enabled(void);
static int mca_accelerator_rocm_get_ipc_handle(int dev_id, void *dev_ptr,
                                               opal_accelerator_ipc_handle_t *handle);
static int mca_accelerator_rocm_import_ipc_handle(int dev_id, uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                  opal_accelerator_ipc_handle_t *handle);
static int mca_accelerator_rocm_open_ipc_handle(int dev_id, opal_accelerator_ipc_handle_t *handle,
                                                void **dev_ptr);
static int mca_accelerator_rocm_compare_ipc_handles(uint8_t handle_1[IPC_MAX_HANDLE_SIZE],
                                                    uint8_t handle_2[IPC_MAX_HANDLE_SIZE]);
static int mca_accelerator_rocm_get_ipc_event_handle(opal_accelerator_event_t *event,
                                                     opal_accelerator_ipc_event_handle_t *handle);
static int mca_accelerator_rocm_import_ipc_event_handle(uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                        opal_accelerator_ipc_event_handle_t *handle);
static int mca_accelerator_rocm_open_ipc_event_handle(opal_accelerator_ipc_event_handle_t *handle,
                                                      opal_accelerator_event_t *event);

static int mca_accelerator_rocm_host_register(int dev_id, void *ptr, size_t size);
static int mca_accelerator_rocm_host_unregister(int dev_id, void *ptr);

static int mca_accelerator_rocm_get_device(int *dev_id);
static int mca_accelerator_rocm_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr);
static int mca_accelerator_rocm_device_can_access_peer( int *access, int dev1, int dev2);

static int mca_accelerator_rocm_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id);

static int mca_accelerator_rocm_sync_stream(opal_accelerator_stream_t *stream);

static int mca_accelerator_rocm_get_num_devices(int *num_devices);

static int mca_accelerator_rocm_get_mem_bw(int device, float *bw);
static void mca_accelerator_rocm_get_memkind(ompi_memkind_t *memkind);

#define GET_STREAM(_stream) (_stream == MCA_ACCELERATOR_STREAM_DEFAULT ? 0 : *((hipStream_t *)_stream->stream))

opal_accelerator_base_module_t opal_accelerator_rocm_module =
{
    mca_accelerator_rocm_check_addr,

    mca_accelerator_rocm_create_stream,
    mca_accelerator_rocm_sync_stream,

    mca_accelerator_rocm_create_event,
    mca_accelerator_rocm_record_event,
    mca_accelerator_rocm_query_event,
    mca_accelerator_rocm_wait_event,

    mca_accelerator_rocm_memcpy_async,
    mca_accelerator_rocm_memcpy,
    mca_accelerator_rocm_memmove_async,
    mca_accelerator_rocm_memmove,
    mca_accelerator_rocm_mem_alloc,
    mca_accelerator_rocm_mem_release,
    mca_accelerator_rocm_mem_alloc_stream,
    mca_accelerator_rocm_mem_release_stream,
    mca_accelerator_rocm_get_address_range,

    mca_accelerator_rocm_is_ipc_enabled,
    mca_accelerator_rocm_get_ipc_handle,
    mca_accelerator_rocm_import_ipc_handle,
    mca_accelerator_rocm_open_ipc_handle,
    mca_accelerator_rocm_compare_ipc_handles,
    mca_accelerator_rocm_get_ipc_event_handle,
    mca_accelerator_rocm_import_ipc_event_handle,
    mca_accelerator_rocm_open_ipc_event_handle,

    mca_accelerator_rocm_host_register,
    mca_accelerator_rocm_host_unregister,

    mca_accelerator_rocm_get_device,
    mca_accelerator_rocm_get_device_pci_attr,
    mca_accelerator_rocm_device_can_access_peer,

    mca_accelerator_rocm_get_buffer_id,

    mca_accelerator_rocm_get_num_devices,
    mca_accelerator_rocm_get_mem_bw,
    mca_accelerator_rocm_get_memkind
};


static int mca_accelerator_rocm_check_addr (const void *addr, int *dev_id, uint64_t *flags)
{
    int ret = 0;
    hipPointerAttribute_t srcAttr;
    hipError_t err;

    *dev_id = MCA_ACCELERATOR_NO_DEVICE_ID;

    if (NULL == addr || NULL == flags) {
        return OPAL_ERR_BAD_PARAM;
    }

    *flags = 0;
    err = hipPointerGetAttributes(&srcAttr, addr);
    if (hipSuccess == err) {
#if HIP_VERSION >= 50731921
        if (hipMemoryTypeDevice == srcAttr.type) {
#else
        if (hipMemoryTypeDevice == srcAttr.memoryType) {
#endif
            opal_accelerator_rocm_lazy_init();
            *dev_id = srcAttr.device;
            ret = 1;
#if HIP_VERSION >= 50731921
        } else if (hipMemoryTypeUnified == srcAttr.type) {
#else
        } else if (hipMemoryTypeUnified == srcAttr.memoryType) {
#endif
            *flags |= MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY;
            opal_accelerator_rocm_lazy_init();
            *dev_id = srcAttr.device;
            ret = 1;
        }
    }

    return ret;
}

static int mca_accelerator_rocm_create_stream(int dev_id, opal_accelerator_stream_t **stream)
{
    if (NULL == stream) {
        return OPAL_ERR_BAD_PARAM;
    }
    *stream = (opal_accelerator_stream_t*)OBJ_NEW(opal_accelerator_rocm_stream_t);
    if (NULL == *stream) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (*stream)->stream = (hipStream_t *)malloc(sizeof(hipStream_t));
    if (NULL == (*stream)->stream) {
        OBJ_RELEASE(*stream);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    hipError_t err = hipStreamCreate((hipStream_t *)(*stream)->stream);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "Could not create hipStream, err=%d %s\n",
                            err, hipGetErrorString(err));
        free((*stream)->stream);
        OBJ_RELEASE(*stream);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static void mca_accelerator_rocm_stream_destruct(opal_accelerator_rocm_stream_t *stream)
{
    if (MCA_ACCELERATOR_STREAM_DEFAULT != (opal_accelerator_stream_t *)stream &&
	NULL != stream->base.stream) {
        hipError_t err = hipStreamDestroy(*(hipStream_t *)stream->base.stream);
        if (hipSuccess != err) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error while destroying the hipStream\n");
        }
        free(stream->base.stream);
    }
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_rocm_stream_t,
    opal_accelerator_stream_t,
    NULL,
    mca_accelerator_rocm_stream_destruct);

 static int mca_accelerator_rocm_create_event(int dev_id, opal_accelerator_event_t **event, bool enable_ipc)
{
    if (NULL == event) {
        return OPAL_ERR_BAD_PARAM;
    }

    *event = (opal_accelerator_event_t*)OBJ_NEW(opal_accelerator_rocm_event_t);
    if (NULL == *event) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (*event)->event = malloc(sizeof(hipEvent_t));
    if (NULL == (*event)->event) {
        OBJ_RELEASE(*event);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    hipError_t err = hipEventCreateWithFlags((hipEvent_t*)(*event)->event,
                                             enable_ipc ? hipEventDisableTiming|hipEventInterprocess :
                                             hipEventDisableTiming);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error creating event\n");
        free((*event)->event);
        OBJ_RELEASE(*event);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static void mca_accelerator_rocm_event_destruct(opal_accelerator_rocm_event_t *event)
{
    if (NULL != event->base.event) {
        hipError_t err = hipEventDestroy(*(hipEvent_t*)event->base.event);
        if (hipSuccess != err) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error destroying event\n");
        }
        free(event->base.event);
    }
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_rocm_event_t,
    opal_accelerator_event_t,
    NULL,
    mca_accelerator_rocm_event_destruct);

static int mca_accelerator_rocm_record_event(int dev_id, opal_accelerator_event_t *event,
                                             opal_accelerator_stream_t *stream)
{
    if (NULL == event || NULL == event->event){
        return OPAL_ERR_BAD_PARAM;
    }
    if (MCA_ACCELERATOR_STREAM_DEFAULT != stream  &&
	(NULL == stream || NULL == stream->stream)){
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipEventRecord(*((hipEvent_t *)event->event), GET_STREAM(stream));
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error recording event\n");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_query_event(int dev_id, opal_accelerator_event_t *event)
{
    if (NULL == event || NULL == event->event) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipEventQuery(*((hipEvent_t *)event->event));
    switch (err) {
        case hipSuccess:
            return OPAL_SUCCESS;
            break;
        case hipErrorNotReady:
            return OPAL_ERR_RESOURCE_BUSY;
            break;
        default:
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error while querying event\n");
            return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_wait_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream)
{
    if (NULL == event || NULL == event->event) {
        return OPAL_ERR_BAD_PARAM;
    }

    if (MCA_ACCELERATOR_STREAM_DEFAULT != stream &&
	(NULL == stream || NULL == stream->stream)){
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipStreamWaitEvent(GET_STREAM(stream), *((hipEvent_t *)event->event), 0);
    if (hipSuccess != err) {
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                                             size_t size, opal_accelerator_stream_t *stream,
                                             opal_accelerator_transfer_type_t type)
{
    if ((MCA_ACCELERATOR_STREAM_DEFAULT != stream &&
        (NULL == stream || NULL == stream->stream)) ||
        NULL == src || NULL == dest || size < 0) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (0 == size) {
        return OPAL_SUCCESS;
    }

    hipError_t err = hipMemcpyAsync(dest, src, size, hipMemcpyDefault,
				    GET_STREAM(stream));
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error while starting asynchronous copy\n");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_memcpy(int dest_dev_id, int src_dev_id, void *dest,
                                       const void *src, size_t size,
                                       opal_accelerator_transfer_type_t type)
{
    hipError_t err;

    if (NULL == src || NULL == dest || size < 0) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (0 == size) {
        return OPAL_SUCCESS;
    }

    if ((type == MCA_ACCELERATOR_TRANSFER_DTOH ||
	 type == MCA_ACCELERATOR_TRANSFER_UNSPEC) &&
	size <= opal_accelerator_rocm_memcpyD2H_limit) {
        memcpy(dest, src, size);
        return OPAL_SUCCESS;
    }

    if (type == MCA_ACCELERATOR_TRANSFER_HTOD && size <= opal_accelerator_rocm_memcpyH2D_limit) {
        memcpy(dest, src, size);
        return OPAL_SUCCESS;
    }

    if (opal_accelerator_rocm_memcpy_async) {
        err = hipMemcpyAsync(dest, src, size, hipMemcpyDefault,
                                       *opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error starting async copy\n");
            return OPAL_ERROR;
        }

        err = hipStreamSynchronize(*opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error synchronizing stream after async copy\n");
            return OPAL_ERROR;
        }
    } else {
        err = hipMemcpy(dest, src, size, hipMemcpyDefault);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error during synchronous copy\n");
            return OPAL_ERROR;
        }
        err = hipStreamSynchronize(0);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error synchronizing default stream after hipMemcpy\n");
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_memmove_async(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                                              size_t size, opal_accelerator_stream_t *stream,
                                              opal_accelerator_transfer_type_t type)
{
    hipDeviceptr_t tmp;
    hipError_t result;
    int ret;
    void *ptr;

    int delayed_init = opal_accelerator_rocm_lazy_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    if (NULL == dest || NULL == src || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    ret = mca_accelerator_rocm_mem_alloc_stream(src_dev_id, &ptr, size, stream);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != ret)) {
        return OPAL_ERROR;
    }
    tmp = (hipDeviceptr_t)ptr;
    result = hipMemcpyAsync(tmp, (hipDeviceptr_t) src, size, hipMemcpyDefault, *(hipStream_t*)stream->stream);
    if (OPAL_UNLIKELY(hipSuccess != result)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error during synchronous copy\n");
        return OPAL_ERROR;
    }
    result = hipMemcpyAsync((hipDeviceptr_t) dest, tmp, size, hipMemcpyDefault, *(hipStream_t*)stream->stream);
    if (OPAL_UNLIKELY(hipSuccess != result)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error during synchronous copy\n");
        return OPAL_ERROR;
    }
    return mca_accelerator_rocm_mem_release_stream(src_dev_id, ptr, stream);
}

static int mca_accelerator_rocm_memmove(int dest_dev_id, int src_dev_id, void *dest,
                                        const void *src, size_t size,
                                        opal_accelerator_transfer_type_t type)
{
    char *tmp = NULL;
    hipError_t err;

    if (NULL == src || NULL == dest || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    err = hipMalloc((void **)&tmp, size);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error allocating memory for memmove\n");
        return OPAL_ERROR;
    }

    if (opal_accelerator_rocm_memcpy_async) {
        err = hipMemcpyAsync(tmp, src, size, hipMemcpyDefault,
                                       *opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in async memcpy for memmove\n");
            return OPAL_ERROR;
        }

        err = hipMemcpyAsync(dest, tmp, size, hipMemcpyDefault,
                                       *opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in async memcpy for memmove\n");
            return OPAL_ERROR;
        }

        err = hipStreamSynchronize(*opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error synchronizing stream for memmove\n");
            return OPAL_ERROR;
        }
    } else {
        err = hipMemcpy(tmp, src, size, hipMemcpyDefault);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in memcpy for memmove\n");
            return OPAL_ERROR;
        }
        err = hipMemcpy(dest, tmp, size, hipMemcpyDefault);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in memcpy for memmove\n");
            return OPAL_ERROR;
        }
    }

    err = hipFree(tmp);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipFree for memmove\n");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_mem_alloc(int dev_id, void **ptr, size_t size)
{
    if (NULL == ptr || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipMalloc(ptr, size);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error allocating memory\n");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_mem_release(int dev_id, void *ptr)
{
    if (NULL == ptr) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipFree(ptr);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error freeing memory\n");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_get_address_range(int dev_id, const void *ptr, void **base,
                                                  size_t *size)
{
    hipError_t err;
    hipDeviceptr_t tBase;
    size_t tSize;

    if (NULL == ptr || NULL == base || NULL == size) {
        return OPAL_ERR_BAD_PARAM;
    }

    err = hipMemGetAddressRange(&tBase, &tSize, (hipDeviceptr_t) ptr);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "couldn't get address range for pointer %p/%lu", ptr, *size);
        return OPAL_ERROR;
    }

    *size = tSize;
    *base = (char *) tBase;

    return OPAL_SUCCESS;
}

static bool mca_accelerator_rocm_is_ipc_enabled(void)
{
    return true;
}

static void mca_accelerator_rocm_ipc_handle_destruct(opal_accelerator_rocm_ipc_handle_t *handle)
{
    if (NULL != handle && NULL != handle->base.dev_ptr) {
        hipIpcCloseMemHandle((hipDeviceptr_t) handle->base.dev_ptr);
	handle->base.dev_ptr = NULL;
    }
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_rocm_ipc_handle_t,
    opal_accelerator_ipc_handle_t,
    NULL,
    mca_accelerator_rocm_ipc_handle_destruct);

static int mca_accelerator_rocm_get_ipc_handle(int dev_id, void *dev_ptr,
                                               opal_accelerator_ipc_handle_t *handle)
{
    if (NULL == dev_ptr || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipIpcMemHandle_t rocm_ipc_handle;
    opal_accelerator_rocm_ipc_handle_t *rocm_handle = (opal_accelerator_rocm_ipc_handle_t *) handle;

    OBJ_CONSTRUCT(rocm_handle, opal_accelerator_rocm_ipc_handle_t);
    rocm_handle->base.dev_ptr = NULL;

    memset(rocm_ipc_handle.reserved, 0, HIP_IPC_HANDLE_SIZE);
    hipError_t err = hipIpcGetMemHandle(&rocm_ipc_handle,
                                        (hipDeviceptr_t)dev_ptr);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "Error in hipIpcGetMemHandle dev_ptr %p", dev_ptr);
        OBJ_DESTRUCT(rocm_handle);
        return OPAL_ERROR;
    }
    memcpy(rocm_handle->base.handle, &rocm_ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_import_ipc_handle(int dev_id, uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                  opal_accelerator_ipc_handle_t *handle)
{
    opal_accelerator_rocm_ipc_handle_t *rocm_handle = (opal_accelerator_rocm_ipc_handle_t *) handle;
    OBJ_CONSTRUCT(rocm_handle, opal_accelerator_rocm_ipc_handle_t);
    memcpy(rocm_handle->base.handle, ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_open_ipc_handle(int dev_id, opal_accelerator_ipc_handle_t *handle,
                                                void **dev_ptr)
{
    if (NULL == dev_ptr || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipIpcOpenMemHandle((hipDeviceptr_t *) &handle->dev_ptr,
                                         *(hipIpcMemHandle_t*)handle->handle,
                                         hipIpcMemLazyEnablePeerAccess);
    if (hipErrorMapFailed == err) {
        return OPAL_ERR_WOULD_BLOCK;
    }
    else if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipIpcOpenMemHandle");
        return OPAL_ERROR;
    }
    *dev_ptr = handle->dev_ptr;

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_compare_ipc_handles(uint8_t handle_1[IPC_MAX_HANDLE_SIZE],
                                                    uint8_t handle_2[IPC_MAX_HANDLE_SIZE])
{
    /*
     * The HIP IPC handles consists of multiple elements.
     * We will only use the ROCr IPC handle (32 bytes, starting at pos 0)
     * and the process ID for comparison.
     * We definitily need to exclude the offset component in the comparison.
     */
    static const int rocr_ipc_handle_size = 32;
    static const int pos = rocr_ipc_handle_size + 2*sizeof(size_t);
    int *pid_1 = (int *)&handle_1[pos];
    int *pid_2 = (int *)&handle_2[pos];

    if (*pid_1 != *pid_2) {
        return 1;
    }

    return memcmp(handle_1, handle_2, rocr_ipc_handle_size);
}

static void mca_accelerator_rocm_ipc_event_handle_destruct(opal_accelerator_rocm_ipc_handle_t *handle)
{
    // Just a place holder, there is no hipIpcCloseEventHandle.
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_rocm_ipc_event_handle_t,
    opal_accelerator_ipc_event_handle_t,
    NULL,
    mca_accelerator_rocm_ipc_event_handle_destruct);


static int mca_accelerator_rocm_get_ipc_event_handle(opal_accelerator_event_t *event,
                                                     opal_accelerator_ipc_event_handle_t *handle)
{
    if (NULL == event || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipIpcEventHandle_t rocm_ipc_handle;
    opal_accelerator_rocm_ipc_event_handle_t *rocm_handle = (opal_accelerator_rocm_ipc_event_handle_t *) handle;
    OBJ_CONSTRUCT(rocm_handle, opal_accelerator_rocm_ipc_event_handle_t);

    memset(rocm_ipc_handle.reserved, 0, HIP_IPC_HANDLE_SIZE);
    hipError_t err = hipIpcGetEventHandle(&rocm_ipc_handle,
                                          *((hipEvent_t *)event->event));
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipIpcGetEventHandle");
        OBJ_DESTRUCT(rocm_handle);
        return OPAL_ERROR;
    }
    memcpy(rocm_handle->base.handle, &rocm_ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_import_ipc_event_handle(uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                        opal_accelerator_ipc_event_handle_t *handle)
{
    opal_accelerator_rocm_ipc_handle_t *rocm_handle = (opal_accelerator_rocm_ipc_handle_t *) handle;

    OBJ_CONSTRUCT(rocm_handle, opal_accelerator_rocm_ipc_handle_t);
    memcpy(rocm_handle->base.handle, ipc_handle, IPC_MAX_HANDLE_SIZE);

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_open_ipc_event_handle(opal_accelerator_ipc_event_handle_t *handle,
                                                      opal_accelerator_event_t *event)
{
    if (NULL == event || NULL == handle) {
        return OPAL_ERR_BAD_PARAM;
    }

    opal_accelerator_rocm_ipc_event_handle_t *rocm_handle = (opal_accelerator_rocm_ipc_event_handle_t *) handle;
    opal_accelerator_rocm_event_t *rocm_event = (opal_accelerator_rocm_event_t *) event;
    OBJ_CONSTRUCT(rocm_event, opal_accelerator_rocm_event_t);
    rocm_event->base.event = malloc(sizeof(hipEvent_t));
    if (NULL == rocm_event->base.event) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    hipError_t err = hipIpcOpenEventHandle( (hipEvent_t *)rocm_event->base.event,
                                            *((hipIpcEventHandle_t*)rocm_handle->base.handle));
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipIpcOpenEventHandle");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_host_register(int dev_id, void *ptr, size_t size)
{
    if (NULL == ptr && size > 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipHostRegister(ptr, size, 0);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error registering address %p", ptr);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_host_unregister(int dev_id, void *ptr)
{
    if (NULL == ptr) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipHostUnregister(ptr);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error unregistering address %p", ptr);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_get_device(int *dev_id)
{
    if (NULL == dev_id) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipGetDevice(dev_id);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error retrieviung current device");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr)
{
    hipError_t err;
    int ret;
    static const int PCI_BUS_ID_LENGTH = 13;
    char pci_bus_id[PCI_BUS_ID_LENGTH];
    char domain_id[5] = {0}, bus_id[3] = {0}, device_id[3] = {0}, function_id[2] = {0};

    if (NULL == pci_attr) {
        return OPAL_ERR_BAD_PARAM;
    }

    err = hipDeviceGetPCIBusId(pci_bus_id, PCI_BUS_ID_LENGTH, dev_id);
    if(hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error retrieving device PCI attributes");
        return OPAL_ERROR;
    }

    ret = sscanf(pci_bus_id, "%4s:%2s:%2s.%1s", domain_id, bus_id, device_id, function_id);
    if (4 > ret) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error parsing device PCI attributes");
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

static int mca_accelerator_rocm_device_can_access_peer(int *access, int dev1, int dev2)
{
    if (NULL == access || dev1 < 0 || dev2 < 0){
        return OPAL_ERR_BAD_PARAM;
    }

    if (dev1 == dev2) {
        *access = 1;
        return OPAL_SUCCESS;
    }

    hipError_t err = hipDeviceCanAccessPeer(access, dev1, dev2);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipDeviceCanAccessPerr dev1 %d dev2 %d", dev1, dev2);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id)
{
    *buf_id = 0;

#if HIP_VERSION >= 50120531
    hipError_t result = hipPointerGetAttribute((unsigned long long *)buf_id, HIP_POINTER_ATTRIBUTE_BUFFER_ID,
                                               (hipDeviceptr_t)addr);
    if (hipSuccess != result) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipPointerGetAttribute, could not retrieve buffer_id");
        return OPAL_ERROR;
    }
#endif

#if HIP_VERSION >= 50530201
    int enable = 1;
    hipError_t err = hipPointerSetAttribute(&enable, HIP_POINTER_ATTRIBUTE_SYNC_MEMOPS,
                                            (hipDeviceptr_t)addr);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipPointerSetAttribute, could not set SYNC_MEMOPS");
        return OPAL_ERROR;
    }
#endif
    return OPAL_SUCCESS;
}


static int mca_accelerator_rocm_mem_alloc_stream(
    int dev_id,
    void **addr,
    size_t size,
    opal_accelerator_stream_t *stream)
{
    hipError_t result;

    int delayed_init = opal_accelerator_rocm_lazy_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }

    if (NULL == stream || NULL == addr || 0 == size) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = hipMallocAsync(addr, size, *(hipStream_t*)stream->stream);
    if (OPAL_UNLIKELY(hipSuccess != result)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error allocating memory\n");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_mem_release_stream(
    int dev_id,
    void *addr,
    opal_accelerator_stream_t *stream)
{
    hipError_t result;

    if (NULL == stream || NULL == addr) {
        return OPAL_ERR_BAD_PARAM;
    }

    result = hipFreeAsync(addr, *(hipStream_t*)stream->stream);
    if (OPAL_UNLIKELY(hipSuccess != result)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error freeing memory\n");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_sync_stream(opal_accelerator_stream_t *stream)
{
    hipError_t result;
    result = hipStreamSynchronize(*(hipStream_t*)stream->stream);
    if (OPAL_UNLIKELY(hipSuccess != result)) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error synchronizing stream\n");
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}


static int mca_accelerator_rocm_get_num_devices(int *num_devices)
{
    *num_devices = opal_accelerator_rocm_num_devices;
    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_get_mem_bw(int device, float *bw)
{
    int delayed_init = opal_accelerator_rocm_lazy_init();
    if (OPAL_UNLIKELY(0 != delayed_init)) {
        return delayed_init;
    }
    assert(opal_accelerator_rocm_mem_bw != NULL);

    *bw = opal_accelerator_rocm_mem_bw[device];
    return OPAL_SUCCESS;
}

static void mca_accelerator_rocm_get_memkind (ompi_memkind_t *memkind)
{
  memkind->im_name = strdup("rocm");
  memkind->im_no_restrictors = false;
  memkind->im_num_restrictors = 3;
  memkind->im_restrictors[0] = strdup("host");
  memkind->im_restrictors[1] = strdup("device");
  memkind->im_restrictors[2] = strdup("managed");

  return;
}

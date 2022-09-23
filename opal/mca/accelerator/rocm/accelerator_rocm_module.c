/*
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "accelerator_rocm.h"
#include "opal/mca/accelerator/base/base.h"

/* Accelerator API's */
static int mca_accelerator_rocm_check_addr(const void *addr, int *dev_id, uint64_t *flags);
static int mca_accelerator_rocm_create_stream(int dev_id, opal_accelerator_stream_t **stream);

static int mca_accelerator_rocm_create_event(int dev_id, opal_accelerator_event_t **event);
static int mca_accelerator_rocm_record_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);
static int mca_accelerator_rocm_query_event(int dev_id, opal_accelerator_event_t *event);

static int mca_accelerator_rocm_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                  opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type);
static int mca_accelerator_rocm_memcpy(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                            size_t size, opal_accelerator_transfer_type_t type);
static int mca_accelerator_rocm_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                        opal_accelerator_transfer_type_t type);
static int mca_accelerator_rocm_malloc(int dev_id, void **ptr, size_t size);
static int mca_accelerator_rocm_free(int dev_id, void *ptr);
static int mca_accelerator_rocm_get_address_range(int dev_id, const void *ptr, void **base,
                                                  size_t *size);

static int mca_accelerator_rocm_host_register(int dev_id, void *ptr, size_t size);
static int mca_accelerator_rocm_host_unregister(int dev_id, void *ptr);

static int mca_accelerator_rocm_get_device(int *dev_id);
static int mca_accelerator_rocm_device_can_access_peer( int *access, int dev1, int dev2);


opal_accelerator_base_module_t opal_accelerator_rocm_module =
{
    mca_accelerator_rocm_check_addr,

    mca_accelerator_rocm_create_stream,

    mca_accelerator_rocm_create_event,
    mca_accelerator_rocm_record_event,
    mca_accelerator_rocm_query_event,

    mca_accelerator_rocm_memcpy_async,
    mca_accelerator_rocm_memcpy,
    mca_accelerator_rocm_memmove,
    mca_accelerator_rocm_malloc,
    mca_accelerator_rocm_free,
    mca_accelerator_rocm_get_address_range,

    mca_accelerator_rocm_host_register,
    mca_accelerator_rocm_host_unregister,

    mca_accelerator_rocm_get_device,
    mca_accelerator_rocm_device_can_access_peer
};


static int mca_accelerator_rocm_check_addr (const void *addr, int *dev_id, uint64_t *flags)
{
    int ret = -1;
    hipPointerAttribute_t srcAttr;
    hipError_t err;

    if (NULL == addr || NULL == flags) {
        return OPAL_ERR_BAD_PARAM;
    }

    *flags = 0;
    err = HIP_FUNCS.hipPointerGetAttributes(&srcAttr, addr);
    if (hipSuccess == err) {
        ret = 0;
        if (hipMemoryTypeDevice == srcAttr.memoryType) {
            //We might want to set additional flags in a later iteration.
            //*flags |= MCA_ACCELERATOR_FLAGS_HOST_LDSTR;
            //*flags |= MCA_ACCELERATOR_FLAGS_HOST_ATOMICS;
            ret = 1;
        } else if (hipMemoryTypeUnified == srcAttr.memoryType) {
            *flags |= MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY;
            //*flags |= MCA_ACCELERATOR_FLAGS_HOST_LDSTR;
            //*flags |= MCA_ACCELERATOR_FLAGS_HOST_ATOMICS;
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

    hipError_t err = HIP_FUNCS.hipStreamCreate((hipStream_t *)(*stream)->stream);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "Could not create hipStream, err=%d %s\n",
                            err, HIP_FUNCS.hipGetErrorString(err));
        free((*stream)->stream);
        OBJ_RELEASE(*stream);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static void mca_accelerator_rocm_stream_destruct(opal_accelerator_rocm_stream_t *stream)
{
    if (NULL != stream->base.stream) {
        hipError_t err = HIP_FUNCS.hipStreamDestroy(*(hipStream_t *)stream->base.stream);
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

static int mca_accelerator_rocm_create_event(int dev_id, opal_accelerator_event_t **event)
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
    hipError_t err = HIP_FUNCS.hipEventCreateWithFlags((hipEvent_t*)(*event)->event,
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
        hipError_t err = HIP_FUNCS.hipEventDestroy(*(hipEvent_t*)event->base.event);
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
    if (NULL == stream || NULL == stream->stream){
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = HIP_FUNCS.hipEventRecord((hipEvent_t)event->event,
                                              *((hipStream_t *)stream->stream));
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

    hipError_t err = HIP_FUNCS.hipEventQuery(*((hipEvent_t *)event->event));
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

static int mca_accelerator_rocm_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                                             size_t size, opal_accelerator_stream_t *stream,
                                             opal_accelerator_transfer_type_t type)
{
    if (NULL == stream || NULL == src ||
        NULL == dest   || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = HIP_FUNCS.hipMemcpyAsync(dest, src, size, hipMemcpyDefault,
                                              *((hipStream_t *)stream->stream));
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

    if (NULL == src || NULL == dest || size <=0) {
        return OPAL_ERR_BAD_PARAM;
    }

    if (type == MCA_ACCELERATOR_TRANSFER_DTOH && size <= opal_accelerator_rocm_memcpyD2H_limit) {
        memcpy(dest, src, size);
        return OPAL_SUCCESS;
    }

    if (type == MCA_ACCELERATOR_TRANSFER_HTOD && size <= opal_accelerator_rocm_memcpyH2D_limit) {
        memcpy(dest, src, size);
        return OPAL_SUCCESS;
    }

    if (opal_accelerator_rocm_memcpy_async) {
        err = HIP_FUNCS.hipMemcpyAsync(dest, src, size, hipMemcpyDefault,
                                       opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error starting async copy\n");
            return OPAL_ERROR;
        }

        err = HIP_FUNCS.hipStreamSynchronize(opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error synchronizing stream after async copy\n");
            return OPAL_ERROR;
        }
    } else {
        err = HIP_FUNCS.hipMemcpy(dest, src, size, hipMemcpyDefault);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error during synchronous copy\n");
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
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

    err = HIP_FUNCS.hipMalloc((void **)&tmp, size);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error allocating memory for memmove\n");
        return OPAL_ERROR;
    }

    if (opal_accelerator_rocm_memcpy_async) {
        err = HIP_FUNCS.hipMemcpyAsync(tmp, src, size, hipMemcpyDefault,
                                       opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in async memcpy for memmove\n");
            return OPAL_ERROR;
        }

        err = HIP_FUNCS.hipMemcpyAsync(dest, tmp, size, hipMemcpyDefault,
                                       opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in async memcpy for memmove\n");
            return OPAL_ERROR;
        }

        err = HIP_FUNCS.hipStreamSynchronize(opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error synchronizing stream for memmove\n");
            return OPAL_ERROR;
        }
    } else {
        err = HIP_FUNCS.hipMemcpy(tmp, src, size, hipMemcpyDefault);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in memcpy for memmove\n");
            return OPAL_ERROR;
        }
        err = HIP_FUNCS.hipMemcpy(dest, tmp, size, hipMemcpyDefault);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in memcpy for memmove\n");
            return OPAL_ERROR;
        }
    }

    err = HIP_FUNCS.hipFree(tmp);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipFree for memmove\n");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_malloc(int dev_id, void **ptr, size_t size)
{
    if (NULL == ptr || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = HIP_FUNCS.hipMalloc(ptr, size);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error allocating memory\n");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_free(int dev_id, void *ptr)
{
    if (NULL == ptr) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = HIP_FUNCS.hipFree(ptr);
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

    err = HIP_FUNCS.hipMemGetAddressRange(&tBase, &tSize, (hipDeviceptr_t) ptr);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "couldn't get address range for pointer %p/%lu", ptr, *size);
        return OPAL_ERROR;
    }

    *size = tSize;
    *base = (char *) tBase;

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_host_register(int dev_id, void *ptr, size_t size)
{
    if (NULL == ptr && size > 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = HIP_FUNCS.hipHostRegister(ptr, size, 0);
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

    hipError_t err = HIP_FUNCS.hipHostUnregister(ptr);
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

    hipError_t err = HIP_FUNCS.hipGetDevice(dev_id);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error retrieviung current device");
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int mca_accelerator_rocm_device_can_access_peer(int *access, int dev1, int dev2)
{
    if (NULL == access || dev1 < 0 || dev2 < 0){
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = HIP_FUNCS.hipDeviceCanAccessPeer(access, dev1, dev2);
    if (hipSuccess != err) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error in hipDeviceCanAccessPerr dev1 %d dev2 %d", dev1, dev2);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

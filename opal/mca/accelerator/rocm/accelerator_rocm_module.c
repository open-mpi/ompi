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
#include "opal/constants.h"
#include "opal/util/output.h"

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
static int mca_accelerator_rocm_mem_alloc(int dev_id, void **ptr, size_t size);
static int mca_accelerator_rocm_mem_release(int dev_id, void *ptr);
static int mca_accelerator_rocm_get_address_range(int dev_id, const void *ptr, void **base,
                                                  size_t *size);

static int mca_accelerator_rocm_host_register(int dev_id, void *ptr, size_t size);
static int mca_accelerator_rocm_host_unregister(int dev_id, void *ptr);

static int mca_accelerator_rocm_get_device(int *dev_id);
static int mca_accelerator_rocm_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr);
static int mca_accelerator_rocm_device_can_access_peer( int *access, int dev1, int dev2);

static int mca_accelerator_rocm_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id);

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
    mca_accelerator_rocm_mem_alloc,
    mca_accelerator_rocm_mem_release,
    mca_accelerator_rocm_get_address_range,

    mca_accelerator_rocm_host_register,
    mca_accelerator_rocm_host_unregister,

    mca_accelerator_rocm_get_device,
    mca_accelerator_rocm_get_device_pci_attr,
    mca_accelerator_rocm_device_can_access_peer,

    mca_accelerator_rocm_get_buffer_id
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
            //We might want to set additional flags in a later iteration.
            //*flags |= MCA_ACCELERATOR_FLAGS_HOST_LDSTR;
            //*flags |= MCA_ACCELERATOR_FLAGS_HOST_ATOMICS;
            /* First access on a device pointer triggers ROCM support lazy initialization. */
            opal_accelerator_rocm_lazy_init();
            ret = 1;
#if HIP_VERSION >= 50731921
        } else if (hipMemoryTypeUnified == srcAttr.type) {
#else
        } else if (hipMemoryTypeUnified == srcAttr.memoryType) {
#endif
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
    if (NULL != stream->base.stream) {
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
    hipError_t err = hipEventCreateWithFlags((hipEvent_t*)(*event)->event,
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
    if (NULL == stream || NULL == stream->stream){
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipEventRecord(*((hipEvent_t *)event->event),
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

static int mca_accelerator_rocm_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                                             size_t size, opal_accelerator_stream_t *stream,
                                             opal_accelerator_transfer_type_t type)
{
    if (NULL == stream || NULL == src ||
        NULL == dest   || size <= 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    hipError_t err = hipMemcpyAsync(dest, src, size, hipMemcpyDefault,
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
        err = hipMemcpyAsync(dest, src, size, hipMemcpyDefault,
                                       opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error starting async copy\n");
            return OPAL_ERROR;
        }

        err = hipStreamSynchronize(opal_accelerator_rocm_MemcpyStream);
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

    err = hipMalloc((void **)&tmp, size);
    if (hipSuccess != err ) {
        opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                            "error allocating memory for memmove\n");
        return OPAL_ERROR;
    }

    if (opal_accelerator_rocm_memcpy_async) {
        err = hipMemcpyAsync(tmp, src, size, hipMemcpyDefault,
                                       opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in async memcpy for memmove\n");
            return OPAL_ERROR;
        }

        err = hipMemcpyAsync(dest, tmp, size, hipMemcpyDefault,
                                       opal_accelerator_rocm_MemcpyStream);
        if (hipSuccess != err ) {
            opal_output_verbose(10, opal_accelerator_base_framework.framework_output,
                                "error in async memcpy for memmove\n");
            return OPAL_ERROR;
        }

        err = hipStreamSynchronize(opal_accelerator_rocm_MemcpyStream);
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
    hipError_t result = hipPointerGetAttribute((unsigned long long *)&buf_id, HIP_POINTER_ATTRIBUTE_BUFFER_ID,
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

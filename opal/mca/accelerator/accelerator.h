/*
 * Copyright (c) 2014-2021 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 * Accelerator Framework
 *
 * Accelerator initialization and selection:
 *
 * During opal initialization, all available accelerator components
 * will be attempted to be initialized. Only the null component and
 * at most, one other component are expected to succeed initialization
 * or we will abort the accelerator initialization process.
 *
 * The framework selection function will set the global opal_accelerator
 * module to allow usage of the accelerator API.
 *
 *
 * Transfer types:
 *
 * There are five transfer types that can be passed to memory copy functions.
 * They are defined in the enum, opal_accelerator_transfer_type_t:
 *
 * UNSPEC - Not specified
 * HTOH - Host to Host
 * HTOD - Host to Device
 * DTOH - Device to Host
 * DTOD - Device to Device
 *
 * The UNSPEC transfer type should be used when the detection of buffer types
 * will be deferred to the implementation. For certain optimizations, the caller
 * may pass the transfer type to the memory copying functions.
 *
 *
 * Streams and Events:
 *
 * A stream is a sequence of operations that execute on the device in the order
 * in which they are issued by the host code. A single stream guarantees execution
 * in order, but multiple streams may have interleaved or concurrent execution.
 *
 * An event is a synchronization marker that can be used to monitor the device's
 * progress. They can be appended to a stream through the record_event api.
 * The event can then be queried for completion through the query_event api.
 *
 * For a more brute force method of synchronization, the synchronize_stream api
 * was added which will wait until all operations on the stream are completed
 * before progressing.
 *
 *
 * Asynchronous copies:
 *
 * Asynchronous copies are appended to a stream using the memcpy_async api.
 * A subsequent event can be appended to the stream for querying.
 * There is currently no callback functionality for asynchronous copy completion,
 * and thus asynchronous copy completions must be progressed manually.
 *
 *
 * Host register/unregister:
 *
 * Host register page-locks host register and informs the device. This allows
 * the device to perform fast operations using DMA (direct memory access) which
 * can increase the performance of certain memory copies.
 *
 */
#ifndef OPAL_ACCELERATOR_H
#define OPAL_ACCELERATOR_H

#include "opal/class/opal_object.h"
#include "opal/mca/mca.h"

BEGIN_C_DECLS

#define MCA_ACCELERATOR_NO_DEVICE_ID -1
/**
 * Accelerator flags
 */
/* Unified memory buffers */
#define MCA_ACCELERATOR_FLAGS_UNIFIED_MEMORY 0x00000001

/**
 * Transfer types.
 * UNSPEC - Not specified
 * HTOH - Host to Host
 * HTOD - Host to Device
 * DTOH - Device to Host
 * DTOD - Device to Device
 */
typedef enum {
    MCA_ACCELERATOR_TRANSFER_UNSPEC = 0,
    MCA_ACCELERATOR_TRANSFER_HTOH,
    MCA_ACCELERATOR_TRANSFER_HTOD,
    MCA_ACCELERATOR_TRANSFER_DTOH,
    MCA_ACCELERATOR_TRANSFER_DTOD,
} opal_accelerator_transfer_type_t;

typedef uint64_t opal_accelerator_buffer_id_t;

struct opal_accelerator_stream_t {
    opal_object_t super;
    /* Stream object */
    void *stream;
};
typedef struct opal_accelerator_stream_t opal_accelerator_stream_t;

struct opal_accelerator_pci_attr_t {
    uint16_t domain_id;
    uint8_t bus_id;
    uint8_t device_id;
    uint8_t function_id;
};
typedef struct opal_accelerator_pci_attr_t opal_accelerator_pci_attr_t;

OBJ_CLASS_DECLARATION(opal_accelerator_stream_t);

struct opal_accelerator_event_t {
    opal_object_t super;
    /* Event object */
    void *event;
};
typedef struct opal_accelerator_event_t opal_accelerator_event_t;
OBJ_CLASS_DECLARATION(opal_accelerator_event_t);

/**
 * Check whether a pointer belongs to an accelerator or not.
 * interfaces
 *
 * @param[IN] addr           Pointer to check
 * @param[OUT] dev_id        Returns the device id against which the memory was allocated
 *                           or MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[OUT] flags         Set to 0 or an Accelerator flag to indicate additional
 *                           information about the corresponding pointer
 *
 * @retval <0                An error has occurred.
 * @retval 0                 The buffer does not belong to a managed buffer
 *                           in device memory.
 * @retval >0                The buffer belongs to a managed buffer in
 *                           device memory.
 */
typedef int (*opal_accelerator_base_module_check_addr_fn_t)(
    const void *addr, int *dev_id, uint64_t *flags);

/**
 * Creates a stream for asynchonous operations. This function will allocate
 * memory for the object.
 * To release the memory and associated resources,
 * call opal_accelerator_base_module_create_stream_fn_t
 *
 * @param[IN] dev_id         Associated device for the stream or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] stream         Stream to create
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_create_stream_fn_t)(
    int dev_id, opal_accelerator_stream_t **stream);

/**
 * Destroys a stream and release the object memory.
 * This function should return immediately, but resources associated with
 * the stream may be released later.
 *
 * @param[IN] stream         Stream to destroy
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_destroy_stream_fn_t)(
    opal_accelerator_stream_t *stream);

/**
 * Creates an event. An event is a synchronization marker that can be
 * appended to a stream to monitor device progress or synchronize the
 * corresponding stream. This function will allocate memory for the object.
 * To release the memory, call OBJ_RELEASE(*event);
 *
 * @param[IN] dev_id         Associated device for the event or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] event          Event to create
 *
 * @return                   OPAL_SUCCESS or error status on failure.
 */
typedef int (*opal_accelerator_base_module_create_event_fn_t)(
    int dev_id, opal_accelerator_event_t **event);

/**
 * Records an event on a stream. An event recorded on the stream is
 * a synchronization marker that can be used to monitor the device's
 * progress or to synchronize the corresponding stream. This API appends
 * the given event onto the given stream.
 *
 * @param[IN] dev_id         Associated device for the event or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] event          Event to record
 * @param[IN] stream         Stream to record event for
 *
 * @return                   OPAL_SUCCESS or error status on failure.
 */
typedef int (*opal_accelerator_base_module_record_event_fn_t)(
    int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);

/**
 * Queries an event's status. This can be used to monitor the device
 * progress, as events placed on the stream execute in order.
 *
 * @param[IN] dev_id         Associated device for the event or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] event          Event to query
 *
 * @return                   OPAL_SUCCESS on event completion, OPAL_ERROR on error,
 *                           or OPAL_ERR_RESOURCE_BUSY if any work is incomplete.
 */
typedef int (*opal_accelerator_base_module_query_event_fn_t)(
    int dev_id, opal_accelerator_event_t *event);

/**
 * Copies memory asynchronously from src to dest. Memory of dest and src
 * may not overlap. Optionally can specify the transfer type to
 * avoid pointer detection for performance.
 *
 * @param[IN] dest_dev_id    Associated device to copy to or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] src_dev_id     Associated device to copy from or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] dest           Destination to copy memory to
 * @param[IN] src            Source to copy memory from
 * @param[IN] size           Size of memory to copy
 * @param[IN] stream         Stream to perform asynchronous copy on
 * @param[IN] type           Transfer type field for performance
 *                           Can be set to MCA_ACCELERATOR_TRANSFER_UNSPEC
 *                           if caller is unsure of the transfer direction.
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_memcpy_async_fn_t)(
    int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
    opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type);

/**
 * Copies memory synchronously from src to dest. Memory of dest and src
 * may not overlap. Optionally can specify the transfer type to
 * avoid pointer detection for performance.
 *
 * @param[IN] dest_dev_id    Associated device to copy to or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] src_dev_id     Associated device to copy from or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] dest           Destination to copy memory to
 * @param[IN] src            Source to copy memory from
 * @param[IN] size           Size of memory to copy
 * @param[IN] type           Transfer type field for performance
 *                           Can be set to MCA_ACCELERATOR_TRANSFER_UNSPEC
 *                           if caller is unsure of the transfer direction.
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_memcpy_fn_t)(
    int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
    opal_accelerator_transfer_type_t type);

/**
 * Copies memory synchronously from src to dest. Memory of dest and src
 * may overlap. Optionally can specify the transfer type to
 * avoid pointer detection for performance.
 *
 * @param[IN] dest_dev_id    Associated device to copy to or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] src_dev_id     Associated device to copy from or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] dest           Destination to copy memory to
 * @param[IN] src            Source to copy memory from
 * @param[IN] size           Size of memory to copy
 * @param[IN] type           Transfer type field for performance
 *                           Can be set to MCA_ACCELERATOR_TRANSFER_UNSPEC
 *                           if caller is unsure of the transfer direction.
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_memmove_fn_t)(
    int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
    opal_accelerator_transfer_type_t type);

/**
 * Allocates size bytes memory from the device and sets ptr to the
 * pointer of the allocated memory. The memory is not initialized.
 *
 * @param[IN] dev_id         Associated device for the allocation or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[OUT] ptr           Returns pointer to allocated memory
 * @param[IN] size           Size of memory to allocate
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_mem_alloc_fn_t)(
    int dev_id, void **ptr, size_t size);

/**
 * Frees the memory space pointed to by ptr which has been returned by
 * a previous call to an opal_accelerator_base_module_mem_alloc_fn_t().
 * If the function is called on a ptr that has already been freed,
 * undefined behavior occurs. If ptr is NULL, no operation is performed,
 * and the function returns OPAL_SUCCESS.
 *
 * @param[IN] dev_id         Associated device for the allocation or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] ptr            Pointer to free
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_mem_release_fn_t)(
    int dev_id, void *ptr);

/**
 * Retrieves the base address and/or size of a memory allocation of the
 * device.
 *
 * @param[IN] dev_id         Associated device for the allocation or
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] ptr            Pointer to device memory to get base/size from
 * @param[OUT] base          Base address of the memory allocation
 * @param[OUT] size          Size of the memory allocation
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_get_address_range_fn_t)(
    int dev_id, const void *ptr, void **base, size_t *size);

/**
 * Page-locks the memory range specified by ptr and size
 *
 * @param[IN] dev_id         Associated device to register host memory with
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] ptr            Host pointer to memory to page-lock
 * @param[IN] size           Size in bytes of the address range to page-lock in bytes
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_host_register_fn_t)(
    int dev_id, void *ptr, size_t size);

/**
 * Unregisters a memory range that was registered with
 * opal_accelerator_base_module_host_register_fn_t.
 *
 * @param[IN] dev_id         Associated device to deregister host memory with
 *                           MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] ptr            Host pointer to memory to unregister
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_host_unregister_fn_t)(
    int dev_id, void *ptr);

/**
 * Retrieves current device id for a device associated with the local process.
 * If MCA_ACCELERATOR_NO_DEVICE_ID is provided, there is no device/process pairing.
 *
 * @param[OUT] dev_id        ID of the device or MCA_ACCELERATOR_NO_DEVICE_ID
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_get_device_fn_t)(
    int *dev_id);

/**
 * Retrieves PCI attributes of an accelerator device.
 *
 * @param[int] dev_id        Accelerator device id
 * @param[out] pci_attr      PCI attributes of the requested device
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_get_device_pci_attr_fn_t)(
    int dev_id, opal_accelerator_pci_attr_t *pci_attr);

/**
 * Queries if a device may directly access a peer device's memory.
 *
 * @param[OUT] access        Returns 1 if dev1 can directly access memory on dev2
 *                           Returns 0 if dev1 can not directly access memory on dev2
 * @param[IN] dev1           ID of device checking if peer device memory can be accessed
 * @param[IN] dev2           ID of peer device on which the memory allocations
 *                           reside.
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_device_can_access_peer_fn_t)(
    int *access, int dev1, int dev2);

/**
 * Retrieves current device id for a device associated with the local process.
 * If MCA_ACCELERATOR_NO_DEVICE_ID is provided, there is no device/process pairing.
 *
 * @param[IN] dev_id         ID of the device or MCA_ACCELERATOR_NO_DEVICE_ID
 * @param[IN] addr           Buffer pointer to check
 * @param[OUT] buf_id        ID of the given buffer
 *
 *
 * @return                   OPAL_SUCCESS or error status on failure
 */
typedef int (*opal_accelerator_base_module_get_buffer_id_fn_t)(
    int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id);

/*
 * the standard public API data structure
 */
typedef struct {
    /* accelerator function table */
    opal_accelerator_base_module_check_addr_fn_t check_addr;

    opal_accelerator_base_module_create_stream_fn_t create_stream;
    opal_accelerator_base_module_destroy_stream_fn_t destroy_stream;
    opal_accelerator_base_module_create_event_fn_t create_event;
    opal_accelerator_base_module_record_event_fn_t record_event;
    opal_accelerator_base_module_query_event_fn_t query_event;

    opal_accelerator_base_module_memcpy_async_fn_t mem_copy_async;
    opal_accelerator_base_module_memcpy_fn_t mem_copy;
    opal_accelerator_base_module_memmove_fn_t mem_move;

    opal_accelerator_base_module_mem_alloc_fn_t mem_alloc;
    opal_accelerator_base_module_mem_release_fn_t mem_release;
    opal_accelerator_base_module_get_address_range_fn_t get_address_range;

    opal_accelerator_base_module_host_register_fn_t host_register;
    opal_accelerator_base_module_host_unregister_fn_t host_unregister;

    opal_accelerator_base_module_get_device_fn_t get_device;
    opal_accelerator_base_module_get_device_pci_attr_fn_t get_device_pci_attr;
    opal_accelerator_base_module_device_can_access_peer_fn_t device_can_access_peer;

    opal_accelerator_base_module_get_buffer_id_fn_t get_buffer_id;
} opal_accelerator_base_module_t;

/**
 * Accelerator component initialization.
 * Called by MCA framework to initialize the component.
 *
 * This should initialize any component level data.
 *
 * This should discover accelerators that are available.
 * We assume that only one accelerator will be present
 * on any given node.
 *
 * @return                   Initialized module or NULL if init failed.
 */
typedef opal_accelerator_base_module_t * (*mca_accelerator_base_component_init_fn_t)(void);

/**
 * Accelerator component finalization
 * Called by MCA framework to finalize the component.
 *
 * This should finalize the given accelerator component.
 * Any component level data should be cleaned up, including
 * any allocated during component_init() and data created
 * during the lifetime of the component, including outstanding
 * modules.
 *
 * @param[IN] module    If the component performed allocation within
 *                      the module, allow the component the to perform
 *                      the required cleanup
 *
 * No return since error will likely be ignored anyway.
 */
typedef void (*mca_accelerator_base_component_fini_fn_t)(opal_accelerator_base_module_t* module);

typedef struct {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
    mca_accelerator_base_component_init_fn_t accelerator_init;
    mca_accelerator_base_component_fini_fn_t accelerator_finalize;
} opal_accelerator_base_component_t;

/*
 * Macro for use in components that are of type accelerator
 */
#define OPAL_ACCELERATOR_BASE_VERSION_1_0_0 OPAL_MCA_BASE_VERSION_2_1_0("accelerator", 1, 0, 0)

/* Global structure for accessing accelerator functions */
OPAL_DECLSPEC extern opal_accelerator_base_module_t opal_accelerator;

END_C_DECLS

#endif

/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c)           Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
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

#include "accelerator_null_component.h"
#include "opal/constants.h"
#include "ompi/info/info_memkind.h"
#include <string.h>

/*
 * Public string showing the accelerator null component version number
 */
const char *opal_accelerator_null_component_version_string
    = "OPAL null accelerator MCA component version " OPAL_VERSION;

/*
 * Component API functions
 */
static int accelerator_null_open(void);
static int accelerator_null_close(void);
static int accelerator_null_component_register(void);
static opal_accelerator_base_module_t* accelerator_null_init(void);
static void accelerator_null_finalize(opal_accelerator_base_module_t* module);

/* Accelerator API's */
static int accelerator_null_check_addr(const void *addr, int *dev_id, uint64_t *flags);

static int accelerator_null_create_stream(int dev_id, opal_accelerator_stream_t **stream);
static int accelerator_null_create_event(int dev_id, opal_accelerator_event_t **event, bool enable_ipc);
static int accelerator_null_record_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);
static int accelerator_null_query_event(int dev_id, opal_accelerator_event_t *event);
static int accelerator_null_wait_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream);

static int accelerator_null_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                         opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type);
static int accelerator_null_memcpy(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                                   size_t size, opal_accelerator_transfer_type_t type);
static int accelerator_null_memmove_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                          opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type);
static int accelerator_null_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                    opal_accelerator_transfer_type_t type);

static int accelerator_null_mem_alloc(int dev_id, void **ptr, size_t size);
static int accelerator_null_mem_release(int dev_id, void *ptr);
static int accelerator_null_mem_alloc_stream(int dev_id, void **ptr, size_t size, opal_accelerator_stream_t* stream);
static int accelerator_null_mem_release_stream(int dev_id, void *ptr, opal_accelerator_stream_t *stream);
static int accelerator_null_get_address_range(int dev_id, const void *ptr, void **base, size_t *size);

static bool accelerator_null_is_ipc_enabled(void);
static int accelerator_null_get_ipc_handle(int dev_id, void *dev_ptr,
                                           opal_accelerator_ipc_handle_t *handle);
static int accelerator_null_import_ipc_handle(int dev_id, uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                              opal_accelerator_ipc_handle_t *handle);
static int accelerator_null_open_ipc_handle(int dev_id, opal_accelerator_ipc_handle_t *handle,
                                            void **dev_ptr);
static int accelerator_null_compare_ipc_handles(uint8_t handle_1[IPC_MAX_HANDLE_SIZE],
                                                uint8_t handle_2[IPC_MAX_HANDLE_SIZE]);
static int accelerator_null_get_ipc_event_handle(opal_accelerator_event_t *event,
                                                 opal_accelerator_ipc_event_handle_t *handle);
static int accelerator_null_import_ipc_event_handle(uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                    opal_accelerator_ipc_event_handle_t *handle);
static int accelerator_null_open_ipc_event_handle(opal_accelerator_ipc_event_handle_t *handle,
                                                  opal_accelerator_event_t *event);

static int accelerator_null_host_register(int dev_id, void *ptr, size_t size);
static int accelerator_null_host_unregister(int dev_id, void *ptr);

static int accelerator_null_get_device(int *dev_id);
static int accelerator_null_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr);
static int accelerator_null_device_can_access_peer(int *access, int dev1, int dev2);

static int accelerator_null_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id);

static int accelerator_null_sync_stream(opal_accelerator_stream_t *stream);

static int accelerator_null_get_num_devices(int *num_devices);

static int accelerator_null_get_mem_bw(int device, float *bw);
static void accelerator_null_get_memkind(ompi_memkind_t *memkind);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_accelerator_null_component_t mca_accelerator_null_component = {{

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    .base_version =
        {
            /* Indicate that we are a accelerator v1.1.0 component (which also
             * implies a specific MCA version) */

            OPAL_ACCELERATOR_BASE_VERSION_1_0_0,

            /* Component name and version */

            .mca_component_name = "null",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */

            .mca_open_component = accelerator_null_open,
            .mca_close_component = accelerator_null_close,
            .mca_register_component_params = accelerator_null_component_register,

        },
    /* Next the MCA v1.0.0 component meta data */
    .base_data =
        { /* The component is checkpoint ready */
         MCA_BASE_METADATA_PARAM_CHECKPOINT},
    .accelerator_init = accelerator_null_init,
    .accelerator_finalize = accelerator_null_finalize,
}};

opal_accelerator_base_module_t opal_accelerator_null_module =
{
    accelerator_null_check_addr,

    accelerator_null_create_stream,
    accelerator_null_sync_stream,

    accelerator_null_create_event,
    accelerator_null_record_event,
    accelerator_null_query_event,
    accelerator_null_wait_event,

    accelerator_null_memcpy_async,
    accelerator_null_memcpy,
    accelerator_null_memmove_async,
    accelerator_null_memmove,
    accelerator_null_mem_alloc,
    accelerator_null_mem_release,
    accelerator_null_mem_alloc_stream,
    accelerator_null_mem_release_stream,
    accelerator_null_get_address_range,

    accelerator_null_is_ipc_enabled,
    accelerator_null_get_ipc_handle,
    accelerator_null_import_ipc_handle,
    accelerator_null_open_ipc_handle,
    accelerator_null_compare_ipc_handles,
    accelerator_null_get_ipc_event_handle,
    accelerator_null_import_ipc_event_handle,
    accelerator_null_open_ipc_event_handle,

    accelerator_null_host_register,
    accelerator_null_host_unregister,

    accelerator_null_get_device,
    accelerator_null_get_device_pci_attr,
    accelerator_null_device_can_access_peer,

    accelerator_null_get_buffer_id,

    accelerator_null_get_num_devices,
    accelerator_null_get_mem_bw,
    accelerator_null_get_memkind
};

static int accelerator_null_open(void)
{
    return OPAL_SUCCESS;
}

static int accelerator_null_close(void)
{
    return OPAL_SUCCESS;
}

static int accelerator_null_component_register(void)
{
    return OPAL_SUCCESS;
}

static opal_accelerator_base_module_t* accelerator_null_init(void)
{
    return &opal_accelerator_null_module;
}

static void accelerator_null_finalize(opal_accelerator_base_module_t* module)
{
    return;
}

/* Accelerator API's Implementation */
static int accelerator_null_check_addr(const void *addr, int *dev_id, uint64_t *flags)
{
    /* Always return that the pointer belongs to the host */
    return 0;
}

static int accelerator_null_create_stream(int dev_id, opal_accelerator_stream_t **stream)
{
    *stream = OBJ_NEW(opal_accelerator_stream_t);
    return OPAL_SUCCESS;
}

static int accelerator_null_create_event(int dev_id, opal_accelerator_event_t **event, bool enable_ipc)
{
    *event = OBJ_NEW(opal_accelerator_event_t);
    return OPAL_SUCCESS;
}

static int accelerator_null_record_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream)
{
    return OPAL_SUCCESS;
}

static int accelerator_null_query_event(int dev_id, opal_accelerator_event_t *event)
{
    return OPAL_SUCCESS;
}

static int accelerator_null_wait_event(int dev_id, opal_accelerator_event_t *event, opal_accelerator_stream_t *stream)
{
    return OPAL_SUCCESS;
}

static int accelerator_null_memcpy_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                  opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type)
{
    memcpy(dest, src, size);
    return OPAL_SUCCESS;
}

static int accelerator_null_memcpy(int dest_dev_id, int src_dev_id, void *dest, const void *src,
                            size_t size, opal_accelerator_transfer_type_t type)
{
    memcpy(dest, src, size);
    return OPAL_SUCCESS;
}

static int accelerator_null_memmove(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                             opal_accelerator_transfer_type_t type)
{
    memmove(dest, src, size);
    return OPAL_SUCCESS;
}

static int accelerator_null_memmove_async(int dest_dev_id, int src_dev_id, void *dest, const void *src, size_t size,
                                          opal_accelerator_stream_t *stream, opal_accelerator_transfer_type_t type)
{
    memmove(dest, src, size);
    return OPAL_SUCCESS;
}

static int accelerator_null_mem_alloc(int dev_id, void **ptr, size_t size)
{
    *ptr = malloc(size);
    return OPAL_SUCCESS;
}

static int accelerator_null_mem_release(int dev_id, void *ptr)
{
    free(ptr);
    return OPAL_SUCCESS;
}


static int accelerator_null_mem_alloc_stream(int dev_id, void **ptr, size_t size,
                                             opal_accelerator_stream_t *stream)
{
    (void)stream;
    *ptr = malloc(size);
    return OPAL_SUCCESS;
}

static int accelerator_null_mem_release_stream(int dev_id, void *ptr,
                                               opal_accelerator_stream_t *stream)
{
    (void)stream;
    free(ptr);
    return OPAL_SUCCESS;
}

static int accelerator_null_get_address_range(int dev_id, const void *ptr, void **base,
                                              size_t *size)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static bool accelerator_null_is_ipc_enabled(void)
{
    return false;
}

static int accelerator_null_get_ipc_handle(int dev_id, void *dev_ptr,
                                           opal_accelerator_ipc_handle_t *handle)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_import_ipc_handle(int dev_id, uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                              opal_accelerator_ipc_handle_t *handle)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_open_ipc_handle(int dev_id, opal_accelerator_ipc_handle_t *handle,
                                            void **dev_ptr)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_compare_ipc_handles(uint8_t handle_1[IPC_MAX_HANDLE_SIZE],
						uint8_t handle_2[IPC_MAX_HANDLE_SIZE])
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_get_ipc_event_handle(opal_accelerator_event_t *event,
                                                 opal_accelerator_ipc_event_handle_t *handle)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_import_ipc_event_handle(uint8_t ipc_handle[IPC_MAX_HANDLE_SIZE],
                                                    opal_accelerator_ipc_event_handle_t *handle)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_open_ipc_event_handle(opal_accelerator_ipc_event_handle_t *handle,
                                                  opal_accelerator_event_t *event)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_host_register(int dev_id, void *ptr, size_t size)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_host_unregister(int dev_id, void *ptr)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_get_device(int *dev_id)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_get_device_pci_attr(int dev_id, opal_accelerator_pci_attr_t *pci_attr)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_device_can_access_peer( int *access, int dev1, int dev2)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_get_buffer_id(int dev_id, const void *addr, opal_accelerator_buffer_id_t *buf_id)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int accelerator_null_sync_stream(opal_accelerator_stream_t *stream)
{
    return OPAL_SUCCESS;
}

static int accelerator_null_get_num_devices(int *num_devices)
{
    *num_devices = 0;
    return OPAL_SUCCESS;
}


static int accelerator_null_get_mem_bw(int device, float *bw)
{
    *bw = 1.0; // return something that is not 0
    return OPAL_SUCCESS;
}

static void accelerator_null_get_memkind (ompi_memkind_t *memkind)
{
  memkind->im_name = NULL;
  memkind->im_no_restrictors = false;
  memkind->im_num_restrictors = 0;

  return;
}

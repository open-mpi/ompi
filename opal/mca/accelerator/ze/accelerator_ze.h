/*
 * Copyright (c) 2022-2023  Advanced Micro Devices, Inc. All rights reserved.
 * Copyright (c) 2023       Triad National Security, LLC. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_ACCELERATOR_ZE_H
#define OPAL_ACCELERATOR_ZE_H

#include "opal_config.h"

#include "level_zero/ze_api.h"

#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/threads/mutex.h"

typedef struct {
    opal_accelerator_base_component_t super;
} opal_accelerator_ze_component_t;

OPAL_DECLSPEC extern opal_accelerator_ze_component_t mca_accelerator_ze_component;
OPAL_DECLSPEC extern opal_accelerator_base_module_t opal_accelerator_ze_module;

struct opal_accelerator_ze_stream_t {
    opal_accelerator_stream_t base;
    ze_command_queue_handle_t hCommandQueue;
    ze_command_list_handle_t hCommandList;
    int dev_id;
};
typedef struct opal_accelerator_ze_stream_t opal_accelerator_ze_stream_t;
OBJ_CLASS_DECLARATION(opal_accelerator_ze_stream_t);

struct opal_accelerator_ze_event_t {
    opal_accelerator_event_t base;
};
typedef struct opal_accelerator_ze_event_t opal_accelerator_ze_event_t;
OBJ_CLASS_DECLARATION(opal_accelerator_ze_event_t);

OPAL_DECLSPEC extern uint32_t opal_accelerator_ze_device_count;
OPAL_DECLSPEC extern ze_device_handle_t *opal_accelerator_ze_devices_handle;
OPAL_DECLSPEC extern ze_driver_handle_t opal_accelerator_ze_driver_handle;
OPAL_DECLSPEC extern ze_context_handle_t opal_accelerator_ze_context;
OPAL_DECLSPEC extern ze_event_pool_handle_t opal_accelerator_ze_event_pool;
OPAL_DECLSPEC extern opal_accelerator_stream_t **opal_accelerator_ze_MemcpyStream;

OPAL_DECLSPEC extern int opal_accelerator_ze_memcpy_async;
OPAL_DECLSPEC extern int opal_accelerator_ze_verbose;

OPAL_DECLSPEC extern int opal_accelerator_ze_lazy_init(void);

#endif

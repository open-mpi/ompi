/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2024      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_ACCELERATOR_CUDA_H
#define MCA_ACCELERATOR_CUDA_H

#include "opal_config.h"

#include <cuda.h>

#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/threads/mutex.h"

BEGIN_C_DECLS

typedef struct {
    opal_accelerator_base_component_t super;
} opal_accelerator_cuda_component_t;

struct opal_accelerator_cuda_stream_t {
    opal_accelerator_stream_t base;
};
typedef struct opal_accelerator_cuda_stream_t opal_accelerator_cuda_stream_t;
OBJ_CLASS_DECLARATION(opal_accelerator_cuda_stream_t);

struct opal_accelerator_cuda_event_t {
    opal_accelerator_event_t base;
};
typedef struct opal_accelerator_cuda_event_t opal_accelerator_cuda_event_t;
OBJ_CLASS_DECLARATION(opal_accelerator_cuda_event_t);

struct opal_accelerator_cuda_ipc_handle_t {
    opal_accelerator_ipc_handle_t base;
};
typedef struct opal_accelerator_cuda_ipc_handle_t opal_accelerator_cuda_ipc_handle_t;
OBJ_CLASS_DECLARATION(opal_accelerator_cuda_ipc_handle_t);

struct opal_accelerator_cuda_ipc_event_handle_t {
    opal_accelerator_ipc_event_handle_t base;
};
typedef struct opal_accelerator_cuda_ipc_event_handle_t opal_accelerator_cuda_ipc_event_handle_t;
OBJ_CLASS_DECLARATION(opal_accelerator_cuda_ipc_event_handle_t);

/* Declare extern variables, defined in accelerator_cuda_component.c */
extern opal_accelerator_cuda_stream_t opal_accelerator_cuda_memcpy_stream;
extern opal_mutex_t opal_accelerator_cuda_stream_lock;
extern bool mca_accelerator_cuda_init_complete;

OPAL_DECLSPEC extern opal_accelerator_cuda_component_t mca_accelerator_cuda_component;

extern opal_accelerator_base_module_t opal_accelerator_cuda_module;

extern int opal_accelerator_cuda_delayed_init(void);

OPAL_DECLSPEC extern int opal_accelerator_cuda_num_devices;

OPAL_DECLSPEC extern float *opal_accelerator_cuda_mem_bw;

OPAL_DECLSPEC extern int opal_accelerator_cuda_delayed_init(void);

END_C_DECLS

#endif /* MCA_ACCELERATOR_CUDA_H */

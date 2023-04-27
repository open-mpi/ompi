/*
 * Copyright (c) 2022-2023  Advanced Micro Devices, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_ACCELERATOR_ROCM_H
#define OPAL_ACCELERATOR_ROCM_H

#include "opal_config.h"

#include <stdio.h>

/* Not interested in warnings generated in hip_runtime_api.h */
#pragma GCC diagnostic push
/* Clang won't quietly accept "-pedantic", but GCC versions older than ~4.8
 * won't quietly accept "-Wpedanic".  The whole "#pragma GCC diagnostic ..."
 * facility only was added to GCC as of version 4.6. */
#if defined(__clang__) || (defined(__GNUC__) && __GNUC__ >= 6)
#    pragma GCC diagnostic ignored "-Wpedantic"
#    pragma GCC diagnostic ignored "-Wundef"
#    pragma GCC diagnostic ignored "-Wstrict-prototypes"
#else
#    pragma GCC diagnostic ignored "-pedantic"
#endif
#include <hip/hip_runtime_api.h>
#include <hip/hip_version.h>
/* Restore warnings to original state */
#pragma GCC diagnostic pop


#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/threads/mutex.h"

typedef struct {
    opal_accelerator_base_component_t super;
} opal_accelerator_rocm_component_t;

OPAL_DECLSPEC extern opal_accelerator_rocm_component_t mca_accelerator_rocm_component;
OPAL_DECLSPEC extern opal_accelerator_base_module_t opal_accelerator_rocm_module;

struct opal_accelerator_rocm_stream_t {
    opal_accelerator_stream_t base;
};
typedef struct opal_accelerator_rocm_stream_t opal_accelerator_rocm_stream_t;
OBJ_CLASS_DECLARATION(opal_accelerator_rocm_stream_t);

struct opal_accelerator_rocm_event_t {
    opal_accelerator_event_t base;
};
typedef struct opal_accelerator_rocm_event_t opal_accelerator_rocm_event_t;
OBJ_CLASS_DECLARATION(opal_accelerator_rocm_event_t);

OPAL_DECLSPEC extern hipStream_t opal_accelerator_rocm_MemcpyStream;
OPAL_DECLSPEC extern int opal_accelerator_rocm_memcpy_async;
OPAL_DECLSPEC extern int opal_accelerator_rocm_verbose;
OPAL_DECLSPEC extern size_t opal_accelerator_rocm_memcpyH2D_limit;
OPAL_DECLSPEC extern size_t opal_accelerator_rocm_memcpyD2H_limit;

OPAL_DECLSPEC extern int opal_accelerator_rocm_lazy_init(void);

#endif

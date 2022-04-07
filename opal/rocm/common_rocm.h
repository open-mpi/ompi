/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2022 Advanced Micro Devices, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_COMMON_ROCM_H
#define OPAL_MCA_COMMON_ROCM_H

#include <stdio.h>
#include <hip/hip_runtime_api.h>

#include "opal_config.h"

#include "common_rocm_prototypes.h"
#include "opal/mca/btl/base/base.h"

struct hipFunctionTable_s {
    hipError_t (*hipMalloc)(void** pts, size_t size);
    hipError_t (*hipFree)(void* ptr);
    hipError_t (*hipMemcpy)(void* dst, const void* src, size_t sizeBytes, hipMemcpyKind kind);
    hipError_t (*hipMemcpyAsync)(void* dst, const void* src, size_t sizeBytes, hipMemcpyKind kind, hipStream_t stream);
    hipError_t (*hipStreamCreate)(hipStream_t* stream);
    hipError_t (*hipStreamDestroy)(hipStream_t stream);
    hipError_t (*hipStreamSynchronize)(hipStream_t stream);
    const char* (*hipGetErrorString)(hipError_t hipError);
    hipError_t (*hipPointerGetAttributes)(hipPointerAttribute_t *attributes, const void *ptr);
};
typedef struct hipFunctionTable_s hipFunctionTable_t;

OPAL_DECLSPEC void mca_common_rocm_register_mca_variables(void);

#endif

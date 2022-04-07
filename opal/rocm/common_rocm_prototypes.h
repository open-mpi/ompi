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

#ifndef OPAL_MCA_COMMON_ROCM_PROTOTYPES_H
#define OPAL_MCA_COMMON_ROCM_PROTOTYPES_H

#include "opal/datatype/opal_convertor.h"

OPAL_DECLSPEC void mca_common_rocm_convertor_init(opal_convertor_t *convertor, const void *pUserBuf);
OPAL_DECLSPEC bool mca_common_rocm_check_bufs(char *destination_base, char *source_base);
OPAL_DECLSPEC int mca_common_rocm_memcpy_sync(void *dst, void *src, size_t nBytes);
OPAL_DECLSPEC int mca_common_rocm_memmove(void *dst, void *src, size_t nBytes);
OPAL_DECLSPEC void *mca_common_rocm_memcpy(void *dest, const void *src, size_t n, opal_convertor_t *pConvertor);

#endif

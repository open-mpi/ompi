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

/* These five routines represent at the moment the functionality required to support a device/GPU
 * in the Open MPI datatype engine. The interface are kept on purpose independent of the library
 * supporting the GPU/device, since it avoids compile problems in cases where the library is not 
 * available.
 */

/* Function invoked by the datatype engine to initialize support for the GPU device. */
OPAL_DECLSPEC void mca_common_rocm_convertor_init(opal_convertor_t *convertor, const void *pUserBuf);

/* Function verifying the buffer types provided as input argument. Returns true if either 
 * one of the two functions is located in the GPU device memory, indicating the necessity 
 * to utilize GPU specific data movement functions.
 */
OPAL_DECLSPEC bool mca_common_rocm_check_bufs(char *destination_base, char *source_base);

/* Function performing a copy from the src to the dst buffer if either one (or both)
 * are GPU buffers. Buffers must be non-overlapping.
 */
OPAL_DECLSPEC int mca_common_rocm_memcpy_sync(void *dst, void *src, size_t nBytes);


/* Function performing a copy from the src to the dst buffer if either one (or both)
 * are GPU buffers. Used when src and dst buffer are overlapping.
 */
OPAL_DECLSPEC int mca_common_rocm_memmove(void *dst, void *src, size_t nBytes);


/* Function performing a copy from the src to the dst buffer if either one 
 * are GPU buffers. Buffers must be non-overlapping. The additional convertor argument
 * is at the moment only used to verify that the appropriate flags for device buffers
 * has been set.
 */
OPAL_DECLSPEC void *mca_common_rocm_memcpy(void *dest, const void *src, size_t n, opal_convertor_t *pConvertor);

/* Registration function for rocm_mca_parameters. */
OPAL_DECLSPEC void mca_common_rocm_register_mca_variables(void);

#endif

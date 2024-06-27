/*
 *  Copyright (C) by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

MPL_gpu_info_t MPL_gpu_info;

int MPL_gpu_query_support(MPL_gpu_type_t * type)
{
#ifdef MPL_HAVE_CUDA
    *type = MPL_GPU_TYPE_CUDA;
#elif defined MPL_HAVE_ZE
    *type = MPL_GPU_TYPE_ZE;
#elif defined MPL_HAVE_HIP
    *type = MPL_GPU_TYPE_HIP;
#else
    *type = MPL_GPU_TYPE_NONE;
#endif

    return MPL_SUCCESS;
}

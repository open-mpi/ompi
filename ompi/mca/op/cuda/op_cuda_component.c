/*
 * Copyright (c) 2019-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file
 *
 * This is the "cuda" op component source code.
 *
 */

#include "ompi_config.h"

#include "opal/util/printf.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/cuda/op_cuda.h"

#include <stdlib.h>

static int cuda_component_open(void);
static int cuda_component_close(void);
static int cuda_component_init_query(bool enable_progress_threads,
                                    bool enable_mpi_thread_multiple);
static struct ompi_op_base_module_1_0_0_t *
    cuda_component_op_query(struct ompi_op_t *op, int *priority);
static int cuda_component_register(void);

ompi_op_cuda_component_t mca_op_cuda_component = {
    {
        .opc_version = {
            OMPI_OP_BASE_VERSION_1_0_0,

            .mca_component_name = "cuda",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = cuda_component_open,
            .mca_close_component = cuda_component_close,
            .mca_register_component_params = cuda_component_register,
        },
        .opc_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .opc_init_query = cuda_component_init_query,
        .opc_op_query = cuda_component_op_query,
    },
    .cu_max_threads_per_block = NULL,
    .cu_max_blocks = NULL,
    .cu_devices = NULL,
    .cu_num_devices  = 0,
};

/*
 * Component open
 */
static int cuda_component_open(void)
{
    /* We checked the flags during register, so if they are set to
     * zero either the architecture is not suitable or the user disabled
     * AVX support.
     *
     * A first level check to see what level of AVX is available on the
     * hardware.
     *
     * Note that if this function returns non-OMPI_SUCCESS, then this
     * component won't even be shown in ompi_info output (which is
     * probably not what you want).
     */
    printf("op cuda_component_open\n");
    return OMPI_SUCCESS;
}

/*
 * Component close
 */
static int cuda_component_close(void)
{
    if (mca_op_cuda_component.cu_num_devices > 0) {
        //cuStreamDestroy(mca_op_cuda_component.cu_stream);
        free(mca_op_cuda_component.cu_max_threads_per_block);
        mca_op_cuda_component.cu_max_threads_per_block = NULL;
        free(mca_op_cuda_component.cu_max_blocks);
        mca_op_cuda_component.cu_max_blocks = NULL;
        free(mca_op_cuda_component.cu_devices);
        mca_op_cuda_component.cu_devices = NULL;
        mca_op_cuda_component.cu_num_devices = 0;
    }

    return OMPI_SUCCESS;
}

/*
 * Register MCA params.
 */
static int
cuda_component_register(void)
{
    /* TODO: add mca paramters */

    return OMPI_SUCCESS;
}


/*
 * Query whether this component wants to be used in this process.
 */
static int
cuda_component_init_query(bool enable_progress_threads,
                         bool enable_mpi_thread_multiple)
{
    int num_devices;
    int rc;
    int prio_lo, prio_hi;
    //memset(&mca_op_cuda_component, 0, sizeof(mca_op_cuda_component));
    cuInit(0);
    CHECK(cuDeviceGetCount, (&num_devices));
    mca_op_cuda_component.cu_num_devices = num_devices;
    mca_op_cuda_component.cu_devices = (CUdevice*)malloc(num_devices*sizeof(CUdevice));
    mca_op_cuda_component.cu_max_threads_per_block = (int*)malloc(num_devices*sizeof(int));
    mca_op_cuda_component.cu_max_blocks = (int*)malloc(num_devices*sizeof(int));
    for (int i = 0; i < num_devices; ++i) {
        CHECK(cuDeviceGet, (&mca_op_cuda_component.cu_devices[i], i));
        rc = cuDeviceGetAttribute(&mca_op_cuda_component.cu_max_threads_per_block[i],
                                  CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X,
                                  mca_op_cuda_component.cu_devices[i]);
        if (CUDA_SUCCESS != rc) {
            /* fall-back to value that should work on every device */
            mca_op_cuda_component.cu_max_threads_per_block[i] = 512;
        }
        rc = cuDeviceGetAttribute(&mca_op_cuda_component.cu_max_blocks[i],
                                  CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X,
                                  mca_op_cuda_component.cu_devices[i]);
        if (CUDA_SUCCESS != rc) {
            /* fall-back to value that should work on every device */
            mca_op_cuda_component.cu_max_blocks[i] = 512;
        }
    }

#if 0
    /* try to create a high-priority stream */
    rc = cuCtxGetStreamPriorityRange(&prio_lo, &prio_hi);
    if (CUDA_SUCCESS != rc) {
        cuStreamCreateWithPriority(&mca_op_cuda_component.cu_stream, CU_STREAM_NON_BLOCKING, prio_hi);
    } else {
        mca_op_cuda_component.cu_stream = 0;
    }
#endif // 0
    printf("op cuda_component_init_query\n");
    return OMPI_SUCCESS;
}

/*
 * Query whether this component can be used for a specific op
 */
static struct ompi_op_base_module_1_0_0_t*
cuda_component_op_query(struct ompi_op_t *op, int *priority)
{
    ompi_op_base_module_t *module = NULL;

    module = OBJ_NEW(ompi_op_base_module_t);
    module->opm_device_enabled = true;
    for (int i = 0; i < OMPI_OP_BASE_TYPE_MAX; ++i) {
        module->opm_fns[i] = ompi_op_cuda_functions[op->o_f_to_c_index][i];
        module->opm_3buff_fns[i] = ompi_op_cuda_3buff_functions[op->o_f_to_c_index][i];

        if( NULL != module->opm_fns[i] ) {
            OBJ_RETAIN(module);
        }
        if( NULL != module->opm_3buff_fns[i] ) {
            OBJ_RETAIN(module);
        }
    }
    *priority = 50;
    return (ompi_op_base_module_1_0_0_t *) module;
}

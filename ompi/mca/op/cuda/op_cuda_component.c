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

static opal_mutex_t init_lock = OPAL_MUTEX_STATIC_INIT;
static bool init_complete = false;

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
    .cu_max_num_blocks = -1,
    .cu_max_num_threads = -1,
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
    return OMPI_SUCCESS;
}

/*
 * Component close
 */
static int cuda_component_close(void)
{
    if (mca_op_cuda_component.cu_num_devices > 0) {
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
    mca_base_var_enum_flag_t *new_enum_flag = NULL;
    (void) mca_base_component_var_register(&mca_op_cuda_component.super.opc_version,
                                           "max_num_blocks",
                                           "Maximum number of thread blocks in kernels (-1: device limit)",
                                           MCA_BASE_VAR_TYPE_INT,
                                           &(new_enum_flag->super), 0, 0,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_op_cuda_component.cu_max_num_blocks);

    (void) mca_base_component_var_register(&mca_op_cuda_component.super.opc_version,
                                           "max_num_threads",
                                           "Maximum number of threads per block in kernels (-1: device limit)",
                                           MCA_BASE_VAR_TYPE_INT,
                                           &(new_enum_flag->super), 0, 0,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_op_cuda_component.cu_max_num_threads);

    return OMPI_SUCCESS;
}


/*
 * Query whether this component wants to be used in this process.
 */
static int
cuda_component_init_query(bool enable_progress_threads,
                         bool enable_mpi_thread_multiple)
{
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
        module->opm_stream_fns[i] = ompi_op_cuda_functions[op->o_f_to_c_index][i];
        module->opm_3buff_stream_fns[i] = ompi_op_cuda_3buff_functions[op->o_f_to_c_index][i];

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

void ompi_op_cuda_lazy_init()
{
    /* Double checked locking to avoid having to
     * grab locks post lazy-initialization.  */
    opal_atomic_rmb();
    if (init_complete) return;

    OPAL_THREAD_LOCK(&init_lock);

    if (!init_complete) {
        int num_devices;
        int rc;
        // TODO: is this init needed here?
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
            if (-1 < mca_op_cuda_component.cu_max_num_threads) {
                if (mca_op_cuda_component.cu_max_threads_per_block[i] >= mca_op_cuda_component.cu_max_num_threads) {
                    mca_op_cuda_component.cu_max_threads_per_block[i] = mca_op_cuda_component.cu_max_num_threads;
                }
            }

            rc = cuDeviceGetAttribute(&mca_op_cuda_component.cu_max_blocks[i],
                                    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X,
                                    mca_op_cuda_component.cu_devices[i]);
            if (CUDA_SUCCESS != rc) {
                /* fall-back to value that should work on every device */
                mca_op_cuda_component.cu_max_blocks[i] = 512;
            }
            if (-1 < mca_op_cuda_component.cu_max_num_blocks) {
                if (mca_op_cuda_component.cu_max_blocks[i] >= mca_op_cuda_component.cu_max_num_blocks) {
                    mca_op_cuda_component.cu_max_blocks[i] = mca_op_cuda_component.cu_max_num_blocks;
                }
            }
        }
        opal_atomic_wmb();
        init_complete = true;
    }
    OPAL_THREAD_UNLOCK(&init_lock);
}
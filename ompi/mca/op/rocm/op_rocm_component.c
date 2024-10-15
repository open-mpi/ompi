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
 * This is the "rocm" op component source code.
 *
 */

#include "ompi_config.h"

#include "opal/util/printf.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/rocm/op_rocm.h"

#include <stdlib.h>

static int rocm_component_open(void);
static int rocm_component_close(void);
static int rocm_component_init_query(bool enable_progress_threads,
                                    bool enable_mpi_thread_multiple);
static struct ompi_op_base_module_1_0_0_t *
    rocm_component_op_query(struct ompi_op_t *op, int *priority);
static int rocm_component_register(void);

ompi_op_rocm_component_t mca_op_rocm_component = {
    {
        .opc_version = {
            OMPI_OP_BASE_VERSION_1_0_0,

            .mca_component_name = "rocm",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = rocm_component_open,
            .mca_close_component = rocm_component_close,
            .mca_register_component_params = rocm_component_register,
        },
        .opc_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .opc_init_query = rocm_component_init_query,
        .opc_op_query = rocm_component_op_query,
    },
    .ro_max_num_blocks = -1,
    .ro_max_num_threads = -1,
    .ro_max_threads_per_block = NULL,
    .ro_max_blocks = NULL,
    .ro_devices = NULL,
    .ro_num_devices  = 0,
};

/*
 * Component open
 */
static int rocm_component_open(void)
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
    return OMPI_SUCCESS;
}

/*
 * Component close
 */
static int rocm_component_close(void)
{
    if (mca_op_rocm_component.ro_num_devices > 0) {
        //hipStreamDestroy(mca_op_rocm_component.ro_stream);
        free(mca_op_rocm_component.ro_max_threads_per_block);
        mca_op_rocm_component.ro_max_threads_per_block = NULL;
        free(mca_op_rocm_component.ro_max_blocks);
        mca_op_rocm_component.ro_max_blocks = NULL;
        free(mca_op_rocm_component.ro_devices);
        mca_op_rocm_component.ro_devices = NULL;
        mca_op_rocm_component.ro_num_devices = 0;
    }

    return OMPI_SUCCESS;
}

/*
 * Register MCA params.
 */
static int
rocm_component_register(void)
{
    /* TODO: add mca paramters */

    mca_base_var_enum_flag_t *new_enum_flag = NULL;
    (void) mca_base_component_var_register(&mca_op_rocm_component.super.opc_version,
                                           "max_num_blocks",
                                           "Maximum number of thread blocks in kernels (-1: device limit)",
                                           MCA_BASE_VAR_TYPE_INT,
                                           &(new_enum_flag->super), 0, 0,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_op_rocm_component.ro_max_num_blocks);

    (void) mca_base_component_var_register(&mca_op_rocm_component.super.opc_version,
                                           "max_num_threads",
                                           "Maximum number of threads per block in kernels (-1: device limit)",
                                           MCA_BASE_VAR_TYPE_INT,
                                           &(new_enum_flag->super), 0, 0,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_op_rocm_component.ro_max_num_threads);

    return OMPI_SUCCESS;
}


/*
 * Query whether this component wants to be used in this process.
 */
static int
rocm_component_init_query(bool enable_progress_threads,
                         bool enable_mpi_thread_multiple)
{
    int num_devices;
    int rc;
    CHECK(hipGetDeviceCount, (&num_devices));
    mca_op_rocm_component.ro_num_devices = num_devices;
    mca_op_rocm_component.ro_devices = (hipDevice_t*)malloc(num_devices*sizeof(hipDevice_t));
    mca_op_rocm_component.ro_max_threads_per_block = (int*)malloc(num_devices*sizeof(int));
    mca_op_rocm_component.ro_max_blocks = (int*)malloc(num_devices*sizeof(int));
    for (int i = 0; i < num_devices; ++i) {
        CHECK(hipDeviceGet, (&mca_op_rocm_component.ro_devices[i], i));
        rc = hipDeviceGetAttribute(&mca_op_rocm_component.ro_max_threads_per_block[i],
                                  hipDeviceAttributeMaxBlockDimX,
                                  mca_op_rocm_component.ro_devices[i]);
        if (hipSuccess != rc) {
            /* fall-back to value that should work on every device */
            mca_op_rocm_component.ro_max_threads_per_block[i] = 512;
        }
        if (-1 < mca_op_rocm_component.ro_max_num_threads) {
            if (mca_op_rocm_component.ro_max_threads_per_block[i] > mca_op_rocm_component.ro_max_num_threads) {
                mca_op_rocm_component.ro_max_threads_per_block[i] = mca_op_rocm_component.ro_max_num_threads;
            }
        }

        rc = hipDeviceGetAttribute(&mca_op_rocm_component.ro_max_blocks[i],
                                  hipDeviceAttributeMaxGridDimX,
                                  mca_op_rocm_component.ro_devices[i]);
        if (hipSuccess != rc) {
            /* we'll try to max out the blocks */
            mca_op_rocm_component.ro_max_blocks[i] = 512;
        }
        if (-1 < mca_op_rocm_component.ro_max_num_blocks) {
            if (mca_op_rocm_component.ro_max_blocks[i] > mca_op_rocm_component.ro_max_num_blocks) {
                mca_op_rocm_component.ro_max_blocks[i] = mca_op_rocm_component.ro_max_num_blocks;
            }
        }
    }

    return OMPI_SUCCESS;
}

/*
 * Query whether this component can be used for a specific op
 */
static struct ompi_op_base_module_1_0_0_t*
rocm_component_op_query(struct ompi_op_t *op, int *priority)
{
    ompi_op_base_module_t *module = NULL;

    module = OBJ_NEW(ompi_op_base_module_t);
    module->opm_device_enabled = true;
    for (int i = 0; i < OMPI_OP_BASE_TYPE_MAX; ++i) {
        module->opm_stream_fns[i] = ompi_op_rocm_functions[op->o_f_to_c_index][i];
        module->opm_3buff_stream_fns[i] = ompi_op_rocm_3buff_functions[op->o_f_to_c_index][i];

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

/*
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      ARM Ltd.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file
 *
 * This is the "AARCH64" component source code.
 *
 */

#include "ompi_config.h"

#include "opal/util/printf.h"

#include "ompi/constants.h"
#include "ompi/mca/op/aarch64/op_aarch64.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/op.h"
#include "ompi/op/op.h"

static int mca_op_aarch64_component_open(void);
static int mca_op_aarch64_component_close(void);
static int mca_op_aarch64_component_init_query(bool enable_progress_threads,
                                     bool enable_mpi_thread_multiple);
static struct ompi_op_base_module_1_0_0_t *
    mca_op_aarch64_component_op_query(struct ompi_op_t *op, int *priority);
static int mca_op_aarch64_component_register(void);

ompi_op_aarch64_component_t mca_op_aarch64_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */
    {
        .opc_version = {
            OMPI_OP_BASE_VERSION_1_0_0,

            .mca_component_name = "aarch64",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = mca_op_aarch64_component_open,
            .mca_close_component = mca_op_aarch64_component_close,
            .mca_register_component_params = mca_op_aarch64_component_register,
        },
        .opc_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .opc_init_query = mca_op_aarch64_component_init_query,
        .opc_op_query = mca_op_aarch64_component_op_query,
    },
};

/*
 * Component open
 */
static int mca_op_aarch64_component_open(void)
{

    /* A first level check to see if NEON or SVE ISA is even available in this
       process.  E.g., you may want to do a first-order check to see
       if hardware is available.  If so, return OMPI_SUCCESS.  If not,
       return anything other than OMPI_SUCCESS and the component will
       silently be ignored.

       Note that if this function returns non-OMPI_SUCCESS, then this
       component won't even be shown in ompi_info output (which is
       probably not what you want).
    */

    return OMPI_SUCCESS;
}

/*
 * Component close
 */
static int mca_op_aarch64_component_close(void)
{

    /* If the aarch64 was opened successfully, close it (i.e., release any
       resources that may have been allocated on this component).
       Note that _component_close() will always be called at the end
       of the process, so it may have been after any/all of the other
       component functions have been invoked (and possibly even after
       modules have been created and/or destroyed). */

    return OMPI_SUCCESS;
}

/*
 * Register MCA params.
 */
static int mca_op_aarch64_component_register(void)
{

    mca_op_aarch64_component.hardware_available = 1;  /* Check for Neon */
#if defined(OMPI_MCA_OP_HAVE_SVE)
    uint64_t id_aa64pfr0_el1 = (1UL << 32);
    __asm__("mrs %0, ID_AA64PFR0_EL1" : "=r"(id_aa64pfr0_el1) : :);
    /* Check for SVE support */
    mca_op_aarch64_component.hardware_available |= ((id_aa64pfr0_el1 & (1UL << 32)) ? 2 : 0);
#endif  /* defined(OMPI_MCA_OP_HAVE_SVE) */
    (void) mca_base_component_var_register(&mca_op_aarch64_component.super.opc_version,
                                           "hardware_available",
                                           "Whether the Neon (1) or SVE (2) hardware is available",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_op_aarch64_component.hardware_available);
    uint64_t id_aa64zfr0_el1 = 0;
#if defined(OMPI_MCA_OP_HAVE_SVE)
    __asm__("mrs %0, ID_AA64ZFR0_EL1" : "=r"(id_aa64zfr0_el1) : :);
#endif  /* defined(OMPI_MCA_OP_HAVE_SVE) */
    mca_op_aarch64_component.double_supported = id_aa64zfr0_el1 & (1UL << 56);
    /* Bit 1: mandatory SVE2 instructions */
    /* Bit 2: mandatory SVE2.1 instructions */
    (void) mca_base_component_var_register(&mca_op_aarch64_component.super.opc_version,
                                           "double_supported",
                                           "Whether the double precision data types are supported or not",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_op_aarch64_component.double_supported);

    return OMPI_SUCCESS;
}

/*
 * Query whether this component wants to be used in this process.
 */
static int mca_op_aarch64_component_init_query(bool enable_progress_threads,
                                               bool enable_mpi_thread_multiple)
{
    if (mca_op_aarch64_component.hardware_available) {
        return OMPI_SUCCESS;
    }
    return OMPI_ERR_NOT_SUPPORTED;
}

#if defined(OMPI_MCA_OP_HAVE_NEON)
extern ompi_op_base_handler_fn_t
ompi_op_aarch64_functions_neon[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
extern ompi_op_base_3buff_handler_fn_t
ompi_op_aarch64_3buff_functions_neon[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
#endif  /* defined(OMPI_MCA_OP_HAVE_NEON) */
#if defined(OMPI_MCA_OP_HAVE_SVE)
extern ompi_op_base_handler_fn_t
ompi_op_aarch64_functions_sve[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
extern ompi_op_base_3buff_handler_fn_t
ompi_op_aarch64_3buff_functions_sve[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
#endif  /* defined(OMPI_MCA_OP_HAVE_SVE) */

/*
 * Query whether this component can be used for a specific op
 */
static struct ompi_op_base_module_1_0_0_t *
    mca_op_aarch64_component_op_query(struct ompi_op_t *op, int *priority)
{
    ompi_op_base_module_t *module = OBJ_NEW(ompi_op_base_module_t);
    /* Sanity check -- although the framework should never invoke the
       _component_op_query() on non-intrinsic MPI_Op's, we'll put a
       check here just to be sure. */
    if (0 == (OMPI_OP_FLAGS_INTRINSIC & op->o_flags)) {
        return NULL;
    }

    switch (op->o_f_to_c_index) {
    case OMPI_OP_BASE_FORTRAN_MAX:
    case OMPI_OP_BASE_FORTRAN_MIN:
    case OMPI_OP_BASE_FORTRAN_SUM:
    case OMPI_OP_BASE_FORTRAN_PROD:
    case OMPI_OP_BASE_FORTRAN_BOR:
    case OMPI_OP_BASE_FORTRAN_BAND:
    case OMPI_OP_BASE_FORTRAN_BXOR:
        for (int i = 0; i < OMPI_OP_BASE_TYPE_MAX; ++i) {
            module->opm_fns[i] = NULL;
            module->opm_3buff_fns[i] = NULL;
#if defined(OMPI_MCA_OP_HAVE_SVE)
            if( mca_op_aarch64_component.hardware_available & 2 ) {
                module->opm_fns[i] = ompi_op_aarch64_functions_sve[op->o_f_to_c_index][i];
                module->opm_3buff_fns[i] = ompi_op_aarch64_3buff_functions_sve[op->o_f_to_c_index][i];
            }
#endif  /* defined(OMPI_MCA_OP_HAVE_SVE) */
#if defined(OMPI_MCA_OP_HAVE_NEON)
            if( mca_op_aarch64_component.hardware_available & 1 ) {
                if( NULL == module->opm_fns[i] ) {
                    module->opm_fns[i] = ompi_op_aarch64_functions_neon[op->o_f_to_c_index][i];
                }
                if( NULL == module->opm_3buff_fns[i] ) {
                    module->opm_3buff_fns[i] = ompi_op_aarch64_3buff_functions_neon[op->o_f_to_c_index][i];
                }
            }
#endif  /* defined(OMPI_MCA_OP_HAVE_NEON) */
            if( NULL != module->opm_fns[i] ) {
                OBJ_RETAIN(module);
            }
            if( NULL != module->opm_3buff_fns[i] ) {
                OBJ_RETAIN(module);
            }
        }
        break;
    case OMPI_OP_BASE_FORTRAN_LAND:
        module = NULL;
        break;
    case OMPI_OP_BASE_FORTRAN_LOR:
        module = NULL;
        break;
    case OMPI_OP_BASE_FORTRAN_LXOR:
        module = NULL;
        break;
    case OMPI_OP_BASE_FORTRAN_MAXLOC:
        module = NULL;
        break;
    case OMPI_OP_BASE_FORTRAN_MINLOC:
        module= NULL;
        break;
    default:
        module= NULL;
    }
    /* If we got a module from above, we'll return it.  Otherwise,
       we'll return NULL, indicating that this component does not want
       to be considered for selection for this MPI_Op.  Note that the
       functions each returned a *aarch64* component pointer
       (vs. a *base* component pointer -- where an *aarch64* component
       is a base component plus some other module-specific cached
       information), so we have to cast it to the right pointer type
       before returning. */
    if (NULL != module) {
        *priority = 50;
    }
    return (ompi_op_base_module_1_0_0_t *) module;
}

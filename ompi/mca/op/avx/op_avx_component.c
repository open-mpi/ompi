/*
 * Copyright (c) 2019-2020 The University of Tennessee and The University
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
 * This is the "avx" component source code.
 *
 */

#include "ompi_config.h"

#include "opal/util/printf.h"
#include "ompi/include/mpi_portable_platform.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/avx/op_avx.h"

static int avx_component_open(void);
static int avx_component_close(void);
static int avx_component_init_query(bool enable_progress_threads,
                                    bool enable_mpi_thread_multiple);
static struct ompi_op_base_module_1_0_0_t *
    avx_component_op_query(struct ompi_op_t *op, int *priority);
static int avx_component_register(void);

static mca_base_var_enum_value_flag_t avx_support_flags[] = {
    { .flag = 0x001, .string = "SSE" },
    { .flag = 0x002, .string = "SSE2" },
    { .flag = 0x004, .string = "SSE3" },
    { .flag = 0x008, .string = "SSE4.1" },
    { .flag = 0x010, .string = "AVX" },
    { .flag = 0x020, .string = "AVX2" },
    { .flag = 0x100, .string = "AVX512F" },
    { .flag = 0x200, .string = "AVX512BW" },
    { .flag = 0,     .string = NULL },
};

/**
 * A slightly modified code from
 * https://software.intel.com/en-us/articles/how-to-detect-new-instruction-support-in-the-4th-generation-intel-core-processor-family
 */
#if defined(__INTEL_COMPILER) && (__INTEL_COMPILER >= 1300)

#include <immintrin.h>

static uint32_t has_intel_AVX_features(void)
{
    uint32_t flags = 0;

    flags |= _may_i_use_cpu_feature(_FEATURE_AVX512F)  ? OMPI_OP_AVX_HAS_AVX512F_FLAG   : 0;
    flags |= _may_i_use_cpu_feature(_FEATURE_AVX512BW) ? OMPI_OP_AVX_HAS_AVX512BW_FLAG : 0;
    flags |= _may_i_use_cpu_feature(_FEATURE_AVX2)     ? OMPI_OP_AVX_HAS_AVX2_FLAG      : 0;
    flags |= _may_i_use_cpu_feature(_FEATURE_AVX)      ? OMPI_OP_AVX_HAS_AVX_FLAG       : 0;
    flags |= _may_i_use_cpu_feature(_FEATURE_SSE4_1)   ? OMPI_OP_AVX_HAS_SSE4_1_FLAG    : 0;
    flags |= _may_i_use_cpu_feature(_FEATURE_SSE3)     ? OMPI_OP_AVX_HAS_SSE3_FLAG      : 0;
    flags |= _may_i_use_cpu_feature(_FEATURE_SSE2)     ? OMPI_OP_AVX_HAS_SSE2_FLAG      : 0;
    flags |= _may_i_use_cpu_feature(_FEATURE_SSE)      ? OMPI_OP_AVX_HAS_SSE_FLAG       : 0;
    return flags;
}
#else /* non-Intel compiler */
#include <stdint.h>

#if defined(_MSC_VER)
#include <intrin.h>
#endif

static void run_cpuid(uint32_t eax, uint32_t ecx, uint32_t* abcd)
{
#if defined(_MSC_VER)
    __cpuidex(abcd, eax, ecx);
#else
    uint32_t ebx = 0, edx = 0;
#if defined( __i386__ ) && defined ( __PIC__ )
    /* in case of PIC under 32-bit EBX cannot be clobbered */
    __asm__ ( "movl %%ebx, %%edi \n\t cpuid \n\t xchgl %%ebx, %%edi" : "=D" (ebx),
#else
    __asm__ ( "cpuid" : "+b" (ebx),
#endif  /* defined( __i386__ ) && defined ( __PIC__ ) */
              "+a" (eax), "+c" (ecx), "=d" (edx) );
    abcd[0] = eax; abcd[1] = ebx; abcd[2] = ecx; abcd[3] = edx;
#endif
}

static uint32_t has_intel_AVX_features(void)
{
    /* From https://en.wikipedia.org/wiki/CPUID#EAX=1:_Processor_Info_and_Feature_Bits */
    const uint32_t avx512f_mask   = (1U << 16);  // AVX512F   (EAX = 7, ECX = 0) : EBX
    const uint32_t avx512_bw_mask = (1U << 30);  // AVX512BW  (EAX = 7, ECX = 0) : EBX
    const uint32_t avx2_mask      = (1U << 5);   // AVX2      (EAX = 7, ECX = 0) : EBX
    const uint32_t avx_mask       = (1U << 28);  // AVX       (EAX = 1, ECX = 0) : ECX
    const uint32_t sse4_1_mask    = (1U << 19);  // SSE4.1    (EAX = 1, ECX = 0) : ECX
    const uint32_t sse3_mask      = (1U << 0);   // SSE3      (EAX = 1, ECX = 0) : ECX
    const uint32_t sse2_mask      = (1U << 26);  // SSE2      (EAX = 1, ECX = 0) : EDX
    const uint32_t sse_mask       = (1U << 15);  // SSE       (EAX = 1, ECX = 0) : EDX
    uint32_t flags = 0, abcd[4];

    run_cpuid( 1, 0, abcd );
    flags |= (abcd[2] & avx_mask)       ? OMPI_OP_AVX_HAS_AVX_FLAG      : 0;
    flags |= (abcd[2] & sse4_1_mask)    ? OMPI_OP_AVX_HAS_SSE4_1_FLAG   : 0;
    flags |= (abcd[2] & sse3_mask)      ? OMPI_OP_AVX_HAS_SSE3_FLAG     : 0;
    flags |= (abcd[3] & sse2_mask)      ? OMPI_OP_AVX_HAS_SSE2_FLAG     : 0;
    flags |= (abcd[3] & sse_mask)       ? OMPI_OP_AVX_HAS_SSE_FLAG      : 0;
#if defined(__APPLE__)
    uint32_t fma_movbe_osxsave_mask = ((1U << 12) | (1U << 22) | (1U << 27));  /* FMA(12) + MOVBE (22) OSXSAVE (27) */
    // OS supports extended processor state management ?
    if ( (abcd[2] & fma_movbe_osxsave_mask) != fma_movbe_osxsave_mask )
        return 0;
#endif  /* defined(__APPLE__) */

    run_cpuid( 7, 0, abcd );
    flags |= (abcd[1] & avx512f_mask)   ? OMPI_OP_AVX_HAS_AVX512F_FLAG  : 0;
    flags |= (abcd[1] & avx512_bw_mask) ? OMPI_OP_AVX_HAS_AVX512BW_FLAG : 0;
    flags |= (abcd[1] & avx2_mask)      ? OMPI_OP_AVX_HAS_AVX2_FLAG     : 0;
    return flags;
}
#endif /* non-Intel compiler */

ompi_op_avx_component_t mca_op_avx_component = {
    {
        .opc_version = {
            OMPI_OP_BASE_VERSION_1_0_0,

            .mca_component_name = "avx",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = avx_component_open,
            .mca_close_component = avx_component_close,
            .mca_register_component_params = avx_component_register,
        },
        .opc_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .opc_init_query = avx_component_init_query,
        .opc_op_query = avx_component_op_query,
    },
};

/*
 * Component open
 */
static int avx_component_open(void)
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
static int avx_component_close(void)
{
    /* If avx was opened successfully, close it (i.e., release any
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
static int
avx_component_register(void)
{
    mca_op_avx_component.supported =
        mca_op_avx_component.flags = has_intel_AVX_features();

    // MCA var enum flag for conveniently seeing SSE/MMX/AVX support
    // values
    mca_base_var_enum_flag_t *new_enum_flag = NULL;
    (void) mca_base_var_enum_create_flag("op_avx_support_flags",
                                         avx_support_flags, &new_enum_flag);

    (void) mca_base_component_var_register(&mca_op_avx_component.super.opc_version,
                                           "capabilities",
                                           "Level of SSE/MMX/AVX support available in the current environment",
                                           MCA_BASE_VAR_TYPE_INT,
                                           &(new_enum_flag->super), 0, 0,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_CONSTANT,
                                           &mca_op_avx_component.supported);

    (void) mca_base_component_var_register(&mca_op_avx_component.super.opc_version,
                                           "support",
                                           "Level of SSE/MMX/AVX support to be used, capped by the local architecture capabilities",
                                           MCA_BASE_VAR_TYPE_INT,
                                           &(new_enum_flag->super), 0, 0,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_op_avx_component.flags);
    OBJ_RELEASE(new_enum_flag);

    mca_op_avx_component.flags &= mca_op_avx_component.supported;

    return OMPI_SUCCESS;
}

/*
 * Query whether this component wants to be used in this process.
 */
static int
avx_component_init_query(bool enable_progress_threads,
                         bool enable_mpi_thread_multiple)
{
    if( 0 == mca_op_avx_component.flags )
        return OMPI_ERR_NOT_SUPPORTED;
    return OMPI_SUCCESS;
}

#if OMPI_MCA_OP_HAVE_AVX512
 extern ompi_op_base_handler_fn_t ompi_op_avx_functions_avx512[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
 extern ompi_op_base_3buff_handler_fn_t ompi_op_avx_3buff_functions_avx512[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
#endif
#if OMPI_MCA_OP_HAVE_AVX2
 extern ompi_op_base_handler_fn_t ompi_op_avx_functions_avx2[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
 extern ompi_op_base_3buff_handler_fn_t ompi_op_avx_3buff_functions_avx2[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
#endif
#if OMPI_MCA_OP_HAVE_AVX
 extern ompi_op_base_handler_fn_t ompi_op_avx_functions_avx[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
 extern ompi_op_base_3buff_handler_fn_t ompi_op_avx_3buff_functions_avx[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];
#endif
/*
 * Query whether this component can be used for a specific op
 */
static struct ompi_op_base_module_1_0_0_t*
avx_component_op_query(struct ompi_op_t *op, int *priority)
{
    ompi_op_base_module_t *module = NULL;
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
        module = OBJ_NEW(ompi_op_base_module_t);
        for (int i = 0; i < OMPI_OP_BASE_TYPE_MAX; ++i) {
#if OMPI_MCA_OP_HAVE_AVX512
            if( mca_op_avx_component.flags & OMPI_OP_AVX_HAS_AVX512F_FLAG ) {
                module->opm_fns[i] = ompi_op_avx_functions_avx512[op->o_f_to_c_index][i];
                module->opm_3buff_fns[i] = ompi_op_avx_3buff_functions_avx512[op->o_f_to_c_index][i];
            }
#endif
#if OMPI_MCA_OP_HAVE_AVX2
            if( mca_op_avx_component.flags & OMPI_OP_AVX_HAS_AVX2_FLAG ) {
                if( NULL == module->opm_fns[i] ) {
                    module->opm_fns[i] = ompi_op_avx_functions_avx2[op->o_f_to_c_index][i];
                }
                if( NULL == module->opm_3buff_fns[i] ) {
                    module->opm_3buff_fns[i] = ompi_op_avx_3buff_functions_avx2[op->o_f_to_c_index][i];
                }
            }
#endif
#if OMPI_MCA_OP_HAVE_AVX
            if( mca_op_avx_component.flags & OMPI_OP_AVX_HAS_AVX_FLAG ) {
                if( NULL == module->opm_fns[i] ) {
                    module->opm_fns[i] = ompi_op_avx_functions_avx[op->o_f_to_c_index][i];
                }
                if( NULL == module->opm_3buff_fns[i] ) {
                    module->opm_3buff_fns[i] = ompi_op_avx_3buff_functions_avx[op->o_f_to_c_index][i];
                }
            }
#endif
            if( NULL != module->opm_fns[i] ) {
                OBJ_RETAIN(module);
            }
            if( NULL != module->opm_3buff_fns[i] ) {
                OBJ_RETAIN(module);
            }
        }
        break;
    case OMPI_OP_BASE_FORTRAN_LAND:
    case OMPI_OP_BASE_FORTRAN_LOR:
    case OMPI_OP_BASE_FORTRAN_LXOR:
    case OMPI_OP_BASE_FORTRAN_MAXLOC:
    case OMPI_OP_BASE_FORTRAN_MINLOC:
    case OMPI_OP_BASE_FORTRAN_REPLACE:
    default:
        break;
    }
    /* If we got a module from above, we'll return it.  Otherwise,
       we'll return NULL, indicating that this component does not want
       to be considered for selection for this MPI_Op.  Note that the
       functions each returned a *avx* component pointer
       (vs. a *base* component pointer -- where an *avx* component
       is a base component plus some other module-specific cached
       information), so we have to cast it to the right pointer type
       before returning. */
    if (NULL != module) {
        *priority = 50;
    }
    return (ompi_op_base_module_1_0_0_t *) module;
}

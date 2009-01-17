/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file
 *
 * This is the "x86" component source code.  It contains the
 * well-known struct that OMPI will dlsym() (or equivalent) for to
 * find how to access the rest of the component and any modules that
 * are created.
 */

#include "ompi_config.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/x86/op_x86.h"

static int x86_component_open(void);
static int x86_component_close(void);
static int x86_component_init_query(bool enable_progress_threads,
                                     bool enable_mpi_threads);
static struct ompi_op_base_module_1_0_0_t *
    x86_component_op_query(struct ompi_op_t *op, int *priority);
static int x86_component_register(void);

ompi_op_x86_component_t mca_op_x86_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */
    {
        {
            OMPI_OP_BASE_VERSION_1_0_0,
            
            "x86",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,
            x86_component_open,
            x86_component_close,
            NULL,
            x86_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        
        x86_component_init_query,
        x86_component_op_query,
    },

    /* Now comes the x86-component-specific data.  In this case,
       we'll just leave it blank, defaulting all the values to
       0/false/whatever.  We'll fill them in with meaningful values
       during _component_init_query(). */
};

/*
 * Component open
 */
static int x86_component_open(void)
{
    opal_output(ompi_op_base_output, "x86 component open");

    /* A first level check to see if x86 is even available in this
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
static int x86_component_close(void)
{
    opal_output(ompi_op_base_output, "x86 component close");

    /* If x86 was opened successfully, close it (i.e., release any
       resources that may have been allocated on this component).
       Note that _component_close() will always be called at the end
       of the process, so it may have been after any/all of the other
       component functions have been invoked (and possibly even after
       modules have been created and/or destroyed). */

    return OMPI_SUCCESS;
}


/*
 * Probe the hardware and see what we have
 */
static void hardware_probe(void)
{
    /* ... JMS fill in here ... */
}

/*
 * Register MCA params.
 */
static int x86_component_register(void)
{
    int val;

    opal_output(ompi_op_base_output, "x86 component register");

    /* Probe the hardware and see what we have */
    hardware_probe();

    val = (0 != (mca_op_x86_component.oxc_hw_flags & OP_X86_HW_FLAGS_MMX));
    mca_base_param_reg_int(&mca_op_x86_component.super.opc_version,
                           "mmx_available", 
                           "Whether the hardware supports MMX or not",
                           false, false, val, NULL);

    val = (0 != (mca_op_x86_component.oxc_hw_flags & OP_X86_HW_FLAGS_MMX2));
    mca_base_param_reg_int(&mca_op_x86_component.super.opc_version,
                           "mmx2_available", 
                           "Whether the hardware supports MMX2 or not",
                           false, false, val, NULL);
    
    val = (0 != (mca_op_x86_component.oxc_hw_flags & OP_X86_HW_FLAGS_SSE));
    mca_base_param_reg_int(&mca_op_x86_component.super.opc_version,
                           "sse_available", 
                           "Whether the hardware supports SSE or not",
                           false, false, val, NULL);
    
    val = (0 != (mca_op_x86_component.oxc_hw_flags & OP_X86_HW_FLAGS_SSE2));
    mca_base_param_reg_int(&mca_op_x86_component.super.opc_version,
                           "sse2_available", 
                           "Whether the hardware supports SSE2 or not",
                           false, false, val, NULL);
    
    val = (0 != (mca_op_x86_component.oxc_hw_flags & OP_X86_HW_FLAGS_SSE3));
    mca_base_param_reg_int(&mca_op_x86_component.super.opc_version,
                           "sse3_available", 
                           "Whether the hardware supports SSE3 or not",
                           false, false, val, NULL);
    

    return OMPI_SUCCESS;
}


/*
 * Query whether this component wants to be used in this process.
 */
static int x86_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads)
{
    opal_output(ompi_op_base_output, "x86 component init query");

    /* If we have any hardware and we're not threaded, success */
    if (0 != mca_op_x86_component.oxc_hw_flags && !enable_mpi_threads) {
        return OMPI_SUCCESS;
    }
    return OMPI_ERR_NOT_SUPPORTED;
}


/*
 * Query whether this component can be used for a specific op
 */
static struct ompi_op_base_module_1_0_0_t *
    x86_component_op_query(struct ompi_op_t *op, int *priority)
{
    ompi_op_base_module_t *module = NULL;

    opal_output(ompi_op_base_output, "x86 component op query");

    /* Sanity check -- although the framework should never invoke the
       _component_op_query() on non-intrinsic MPI_Op's, we'll put a
       check here just to be sure. */
    if (0 == (OMPI_OP_FLAGS_INTRINSIC & op->o_flags)) {
        opal_output(0, "x86 component op query: not an intrinsic MPI_Op -- skipping");
        return NULL;
    }

    /* What follows is an x86 of how to determine whether your
       component supports the queried MPI_Op.  You can do this lots of
       different ways; this is but one x86. */

    /* Note that we *do* have the hardware; _component_init_query()
       would not have returned OMPI_SUCCESS if we didn't have the
       hardware (and therefore this function would never have been
       called).  So we don't need to check for the hardware again.
       Instead, we need to do finer-grained checks (e.g., do we
       support this op, and if so, what datatypes are supported?).

       So check to see whether this MPI_Op operation is supported on
       the hardware that this component supports (which may involve
       querying the hardware to see what it is capable of).

       You can see what operation is being requested by checking the
       "op->o_f_to_c_index" value against the OMPI_OP_BASE_FORTRAN_*
       enums.  See ompi/mca/op/op.h for a full list of the
       OMPI_OP_BASE_FORTRAN_* enums.

       In this x86 component, we support MAX and BXOR. */
    switch (op->o_f_to_c_index) {
    case OMPI_OP_BASE_FORTRAN_SUM:
        /* Corresponds to MPI_SUM */
        module = ompi_op_x86_setup_sum(op);
        break;
    }

    /* If we got a module from above, we'll return it.  Otherwise,
       we'll return NULL, indicating that this component does not want
       to be considered for selection for this MPI_Op.  Note that the
       "setup" functions each returned a *x86* component pointer
       (vs. a *base* component pointer -- where an *x86* component
       is a base component plus some other module-specific cached
       information), so we have to cast it to the right pointer type
       before returning. */
    if (NULL != module) {
        *priority = 25;
    }
    return (ompi_op_base_module_1_0_0_t *) module;
}

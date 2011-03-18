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
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file
 *
 * This is the "example" component source code.  It contains the
 * well-known struct that OMPI will dlsym() (or equivalent) for to
 * find how to access the rest of the component and any modules that
 * are created.
 */

#include "ompi_config.h"

#include "opal/mca/base/mca_base_param.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/example/op_example.h"

static int example_component_open(void);
static int example_component_close(void);
static int example_component_init_query(bool enable_progress_threads,
                                     bool enable_mpi_thread_multiple);
static struct ompi_op_base_module_1_0_0_t *
    example_component_op_query(struct ompi_op_t *op, int *priority);
static int example_component_register(void);

ompi_op_example_component_t mca_op_example_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */
    {
        {
            OMPI_OP_BASE_VERSION_1_0_0,
            
            "example",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,
            example_component_open,
            example_component_close,
            NULL,
            example_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        
        example_component_init_query,
        example_component_op_query,
    },

    /* Now comes the example-component-specific data.  In this case,
       we'll just leave it blank, defaulting all the values to
       0/false/whatever.  We'll fill them in with meaningful values
       during _component_init_query(). */
};

/*
 * Component open
 */
static int example_component_open(void)
{
    opal_output(ompi_op_base_output, "example component open");

    /* A first level check to see if example is even available in this
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
static int example_component_close(void)
{
    opal_output(ompi_op_base_output, "example component close");

    /* If example was opened successfully, close it (i.e., release any
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
static int example_component_register(void)
{
    int val;
    char *str;

    opal_output(ompi_op_base_output, "example component register");

    /* Register any relevant MCA params.  At a minimum, perhaps some
       information MCA params that return version and capability
       information.  */

    /* For example, let's make a string MCA information parameter
       containing the major.minor.release version number from the
       libfoo support library (see configure.m4 for how we got these C
       macros). */
    asprintf(&str, "%s.%s.%s", 
             OP_EXAMPLE_LIBFOO_VERSION_MAJOR,
             OP_EXAMPLE_LIBFOO_VERSION_MINOR,
             OP_EXAMPLE_LIBFOO_VERSION_RELEASE);
    mca_base_param_reg_string(&mca_op_example_component.super.opc_version,
                              "libfoo_version", 
                              "Version of the libfoo support library that this component was built against.",
                              false, true, str, NULL);
    free(str);

    /* Additionally, since this component is simulating hardware,
       let's make MCA params that determine whethere a) the hardware
       is available, and b) whether double precision floating point
       types are supported.  This allows you to change the behavior of
       this component at run-time (by setting these MCA params at
       run-time), simulating different kinds of hardware. */
    mca_base_param_reg_int(&mca_op_example_component.super.opc_version,
                           "hardware_available", 
                           "Whether the hardware is available or not",
                           false, false, 1, &val);
    mca_op_example_component.hardware_available = OPAL_INT_TO_BOOL(val);

    mca_base_param_reg_int(&mca_op_example_component.super.opc_version,
                           "double_supported", 
                           "Whether the double precision data types are supported or not",
                           false, false, 1, &val);
    mca_op_example_component.double_supported = OPAL_INT_TO_BOOL(val);

    return OMPI_SUCCESS;
}


/*
 * Query whether this component wants to be used in this process.
 */
static int example_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_thread_multiple)
{
    opal_output(ompi_op_base_output, "example component init query");

    /* Query to see if we have the desired hardware / resources to be
       able to perform reduction operations.  This is a much more
       comprehensive check than _component_open().

       If this component can be used in this process, return
       OMPI_SUCCESS, meaning that we'll be queried later via during
       the MPI_Op component selection process via
       _component_op_query().  Otherwise, return anything other than
       OMPI_SUCCESS and this component will be silently ignored for
       the MPI_Op component selection process.

       The input parameters enable_progress_threads and
       enable_mpi_thread_multiple also tell the component the following:

       - If enable_progress_threads==true, then the component is
         allowed to have a progress thread in the background that is
         supported by the OMPI infrastructure (i.e., all of OMPI's
         locks and whatnot are active in this build).  Note that the
         component can *always* have a progress thread in the
         background regardless of the value of this parameter as lone
         as the HAVE_THREADS macro is true and the component uses its
         own locking schemes (i.e., does not rely on external
         OPAL/ORTE/OMPI data structures to be thread safe).  This flag
         simply indicates whether OPAL/ORTE/OMPI data structures are
         multi-threaded safe and whether multi-threading sync/IPC
         mechanisms in the OMPI code base are active.

       - If enable_mpi_thread_multiple==true, then MPI_THREAD_MULTIPLE is
         active.

       Note that a component can uses these values to deactivate
       themselves if multi-threading is not supported (keep in mind
       that in MPI_THREAD_MULTIPLE scenarios, the same MPI_Op can be
       used in multiple, concurrent operations in different threads).
       Let's assume that this component does not support
       MPI_THREAD_MULTIPLE, and will therefore deactivate itself if
       MPI_THREAD_MULTIPLE is used.
    */

    /* Note that we used MCA parameters to fill in the
       _component.hardware_available and _component.double_supported
       values.  Typically, you'd probe the hardware here and fill in
       those values instead of using MCA parameters (the MCA params
       are only used in this example to allow simulating different
       types of hardware). */

    /* If we have the hardware and are not using MPI_THREAD_MULITPLE,
       return OMPI_SUCCESS (indicating that _component_op_query() will
       be called in the future for each intrinsic MPI_Op).  Otherwise,
       return OMPI_ERR_NOT_SUPPORTED (indicating that this component
       will be closed and discarded). */
    if (mca_op_example_component.hardware_available && !enable_mpi_thread_multiple) {
        return OMPI_SUCCESS;
    }
    return OMPI_ERR_NOT_SUPPORTED;
}


/*
 * Query whether this component can be used for a specific op
 */
static struct ompi_op_base_module_1_0_0_t *
    example_component_op_query(struct ompi_op_t *op, int *priority)
{
    ompi_op_base_module_t *module = NULL;

    opal_output(ompi_op_base_output, "example component op query");

    /* Sanity check -- although the framework should never invoke the
       _component_op_query() on non-intrinsic MPI_Op's, we'll put a
       check here just to be sure. */
    if (0 == (OMPI_OP_FLAGS_INTRINSIC & op->o_flags)) {
        opal_output(0, "example component op query: not an intrinsic MPI_Op -- skipping");
        return NULL;
    }

    /* What follows is an example of how to determine whether your
       component supports the queried MPI_Op.  You can do this lots of
       different ways; this is but one example. */

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

       In this example component, we support MAX and BXOR. */
    switch (op->o_f_to_c_index) {
    case OMPI_OP_BASE_FORTRAN_MAX:
        /* Corresponds to MPI_MAX */
        module = ompi_op_example_setup_max(op);
        break;

    case OMPI_OP_BASE_FORTRAN_BXOR:
        /* Corresponds to MPI_BXOR */
        module = ompi_op_example_setup_bxor(op);
        break;
    }

    /* If we got a module from above, we'll return it.  Otherwise,
       we'll return NULL, indicating that this component does not want
       to be considered for selection for this MPI_Op.  Note that the
       "setup" functions each returned a *example* component pointer
       (vs. a *base* component pointer -- where an *example* component
       is a base component plus some other module-specific cached
       information), so we have to cast it to the right pointer type
       before returning. */
    if (NULL != module) {
        *priority = 50;
    }
    return (ompi_op_base_module_1_0_0_t *) module;
}

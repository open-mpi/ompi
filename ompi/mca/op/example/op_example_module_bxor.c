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
 * This is the bxor module source code.  It contains the "setup"
 * functions that will create a module for the MPI_BXOR MPI_Op.
 */

#include "ompi_config.h"

#include "opal/class/opal_object.h"
#include "opal/util/output.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/example/op_example.h"

/**
 * Derive a struct from the base op module struct, allowing us to
 * cache some module-specific information for BXOR.  Note that
 * information that should be shared across all modules should be put
 * on the example component.
 */
typedef struct {
    ompi_op_base_module_1_0_0_t super;

    /* Just like the ompi_op_example_component_t, this struct is meant to
       cache information on a per-module basis.  What follows are
       examples; replace them with whatever is relevant for your
       component/module.  Keep in mind that there will be one distinct
       module for each MPI_Op; you may want to have different data
       cached on the module, depending on the MPI_Op that it is
       supporting.

       In this example, we'll keep the fallback function pointers for
       several integer types. */
    ompi_op_base_handler_fn_t fallback_int;
    ompi_op_base_module_t *fallback_int_module;
    ompi_op_base_handler_fn_t fallback_long;
    ompi_op_base_module_t *fallback_long_module;
    ompi_op_base_handler_fn_t fallback_integer;
    ompi_op_base_module_t *fallback_integer_module;
} module_bxor_t;

/**
 * "Constructor" for the bxor module class
 */
static void module_bxor_constructor(module_bxor_t *m)
{
    /* Use this function to initialize any data in the class that is
       specific to this class (i.e. do *not* initialize the parent
       data members!). */
    m->fallback_int = NULL;
    m->fallback_int_module = NULL;
    m->fallback_long = NULL;
    m->fallback_long_module = NULL;
    m->fallback_integer = NULL;
    m->fallback_integer_module = NULL;
}

/**
 * "Destructor" for the bxor module class
 */
static void module_bxor_destructor(module_bxor_t *m)
{
    /* Use this function to clean up any data members that may be
       necessary.  This may include freeing resources and/or setting
       members to sentinel values to know that the object has been
       destructed. */
    m->fallback_int = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_int_module = (ompi_op_base_module_t*) 0xdeadbeef;
    m->fallback_long = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_long_module = (ompi_op_base_module_t*) 0xdeadbeef;
    m->fallback_integer = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_integer_module = (ompi_op_base_module_t*) 0xdeadbeef;
}

/**
 * Setup the class for the bxor module, listing:
 * - the name of the class
 * - the "parent" of the class
 * - function pointer for the constructor (or NULL)
 * - function pointer for the destructor (or NULL)
 */
static OBJ_CLASS_INSTANCE(module_bxor_t,
                          ompi_op_base_module_t,
                          module_bxor_constructor,
                          module_bxor_destructor);

/**
 * Bxor function for C int
 */
static void bxor_int(void *in, void *out, int *count, 
                    ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_bxor_t *m = (module_bxor_t*) module;

    /* Be chatty to the output, just so that we can see that this
       function was called */
    opal_output(0, "In example bxor int function");

    /* This is where you can decide at run-time whether to use the
       hardware or the fallback function.  For example, you could have
       logic something like this:

       extent = *count * size(int);
       if (memory_accessible_on_hw(in, extent) &&
           memory_accessible_on_hw(out, extent)) {
          ...do the function on hardware...
       } else if (extent >= large_enough) {
          ...copy host memory -> hardware memory...
          ...do the function on hardware...
          ...copy hardware memory -> host memory...
       } else {
          m->fallback_int(in, out, count, type, m->fallback_int_module);
       }
     */

    /* But for this example, we'll just call the fallback function to
       actually do the work */
    m->fallback_int(in, out, count, type, m->fallback_int_module);
}

/**
 * Bxor function for C long
 */
static void bxor_long(void *in, void *out, int *count, 
                     ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_bxor_t *m = (module_bxor_t*) module;
    opal_output(0, "In example bxor long function");

    /* Just another example function -- similar to bxor_int() */

    m->fallback_long(in, out, count, type, m->fallback_long_module);
}

/**
 * Bxor function for Fortran INTEGER
 */
static void bxor_integer(void *in, void *out, int *count, 
                        ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_bxor_t *m = (module_bxor_t*) module;
    opal_output(0, "In example bxor integer function");

    /* Just another example function -- similar to bxor_int() */

    m->fallback_integer(in, out, count, type, m->fallback_integer_module);
}

/**
 * Setup function for MPI_BXOR.  If we get here, we can assume that a)
 * the hardware is present, b) the MPI thread scenario is what we
 * want, and c) the BXOR operation is supported.  So this function's
 * job is to create a module and fill in function pointers for the
 * functions that this hardware supports.
 *
 * This function is *not* allowed to changed the op; it can only read
 * it to save functions/modules that were already set.  The op base
 * will analyze what was returned in the module and re-set values on
 * the op if necessary.
 */
ompi_op_base_module_t *ompi_op_example_setup_bxor(ompi_op_t *op)
{
    module_bxor_t *module = OBJ_NEW(module_bxor_t);

    /* Remember that we created an *example* module (vs. a *base*
       module), so we can cache extra information on there that is
       specific for the BXOR operation.  Let's cache the original
       fallback function pointers, that were passed to us in this call
       (i.e., they're already assigned on the op). */

    /* C int */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_INT] = bxor_int;
    module->fallback_int = op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_INT];
    module->fallback_int_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_INT];
    /* If you cache a fallback function, you *must* RETAIN (i.e.,
       increase the refcount) its module so that the module knows that
       it is being used and won't be freed/destructed. */
    OBJ_RETAIN(module->fallback_int_module);

    /* C long */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_LONG] = bxor_long;
    module->fallback_long = op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_LONG];
    module->fallback_long_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_LONG];
    OBJ_RETAIN(module->fallback_long_module);

    /* Fortran INTEGER */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_INTEGER] = bxor_integer;
    module->fallback_integer = 
        op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_INTEGER];
    module->fallback_integer_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_INTEGER];
    OBJ_RETAIN(module->fallback_integer_module);

    /* ...not listing the rest of the integer-typed functions in this
       example... */

    return (ompi_op_base_module_t*) module;
}

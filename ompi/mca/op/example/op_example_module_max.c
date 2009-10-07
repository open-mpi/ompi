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
 * This is the max module source code.  It contains the "setup"
 * functions that will create a module for the MPI_MAX MPI_Op.
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
 * cache some module-specific information for MAX.  Note that
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
    ompi_op_base_handler_fn_t fallback_float;
    ompi_op_base_module_t *fallback_float_module;
    ompi_op_base_handler_fn_t fallback_real;
    ompi_op_base_module_t *fallback_real_module;

    ompi_op_base_handler_fn_t fallback_double;
    ompi_op_base_module_t *fallback_double_module;
    ompi_op_base_handler_fn_t fallback_double_precision;
    ompi_op_base_module_t *fallback_double_precision_module;
} module_max_t;

/**
 * "Constructor" for the max module class
 */
static void module_max_constructor(module_max_t *m)
{
    /* Use this function to initialize any data in the class that is
       specific to this class (i.e. do *not* initialize the parent
       data members!). */
    m->fallback_float = NULL;
    m->fallback_float_module = NULL;
    m->fallback_real = NULL;
    m->fallback_real_module = NULL;

    m->fallback_double = NULL;
    m->fallback_double_module = NULL;
    m->fallback_double_precision = NULL;
    m->fallback_double_precision_module = NULL;
}

/**
 * "Destructor" for the max module class
 */
static void module_max_destructor(module_max_t *m)
{
    /* Use this function to clean up any data members that may be
       necessary.  This may include freeing resources and/or setting
       members to sentinel values to know that the object has been
       destructed. */
    m->fallback_float = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_float_module = (ompi_op_base_module_t*) 0xdeadbeef;
    m->fallback_real = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_real_module = (ompi_op_base_module_t*) 0xdeadbeef;

    m->fallback_double = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_double_module = (ompi_op_base_module_t*) 0xdeadbeef;
    m->fallback_double_precision = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_double_precision_module = (ompi_op_base_module_t*) 0xdeadbeef;
}

/**
 * Setup the class for the max module, listing:
 * - the name of the class
 * - the "parent" of the class
 * - function pointer for the constructor (or NULL)
 * - function pointer for the destructor (or NULL)
 */
static OBJ_CLASS_INSTANCE(module_max_t,
                          ompi_op_base_module_t,
                          module_max_constructor,
                          module_max_destructor);

/**
 * Max function for C float
 */
static void max_float(void *in, void *out, int *count, 
                      ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_max_t *m = (module_max_t*) module;

    /* Be chatty to the output, just so that we can see that this
       function was called */
    opal_output(0, "In example max float function");

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
          m->fallback_float(in, out, count, type, m->fallback_int_module);
       }
     */

    /* But for this example, we'll just call the fallback function to
       actually do the work */
    m->fallback_float(in, out, count, type, m->fallback_float_module);
}

/**
 * Max function for C double
 */
static void max_double(void *in, void *out, int *count, 
                       ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_max_t *m = (module_max_t*) module;
    opal_output(0, "In example max double function");

    /* Just another example function -- similar to max_int() */

    m->fallback_double(in, out, count, type, m->fallback_double_module);
}

/**
 * Max function for Fortran REAL
 */
static void max_real(void *in, void *out, int *count, 
                     ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_max_t *m = (module_max_t*) module;
    opal_output(0, "In example max real function");

    /* Just another example function -- similar to max_int() */

    m->fallback_real(in, out, count, type, m->fallback_real_module);
}

/**
 * Max function for Fortran DOUBLE PRECISION
 */
static void max_double_precision(void *in, void *out, int *count, 
                                 ompi_datatype_t **type, 
                                 ompi_op_base_module_t *module)
{
    module_max_t *m = (module_max_t*) module;
    opal_output(0, "In example max double precision function");

    /* Just another example function -- similar to max_int() */

    m->fallback_double_precision(in, out, count, type, 
                                 m->fallback_double_precision_module);
}

/**
 * Setup function for MPI_MAX.  If we get here, we can assume that a)
 * the hardware is present, b) the MPI thread scenario is what we
 * want, and c) the MAX operation is supported.  So this function's
 * job is to create a module and fill in function pointers for the
 * functions that this hardware supports.
 */
ompi_op_base_module_t *ompi_op_example_setup_max(ompi_op_t *op)
{
    module_max_t *module = OBJ_NEW(module_max_t);

    /* We defintely support the single precision floating point types */

    /* Remember that we created an *example* module (vs. a *base*
       module), so we can cache extra information on there that is
       specific for the MAX operation.  Let's cache the original
       fallback function pointers, that were passed to us in this call
       (i.e., they're already assigned on the op). */

    /* C float */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_FLOAT] = max_float;
    module->fallback_float = op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_FLOAT];
    module->fallback_float_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_FLOAT];
    /* If you cache a fallback function, you *must* RETAIN (i.e.,
       increase the refcount) its module so that the module knows that
       it is being used and won't be freed/destructed. */
    OBJ_RETAIN(module->fallback_float_module);

    /* Fortran REAL */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_REAL] = max_real;
    module->fallback_real = 
        op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_REAL];
    module->fallback_real_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_REAL];
    OBJ_RETAIN(module->fallback_real_module);

    /* Does our hardware support double precision? */

    if (mca_op_example_component.double_supported) {
        /* C double */
        module->super.opm_fns[OMPI_OP_BASE_TYPE_DOUBLE] = max_double;
        module->fallback_double = 
            op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_DOUBLE];
        module->fallback_double_module = 
            op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_DOUBLE];
        OBJ_RETAIN(module->fallback_double_module);
        
        /* Fortran DOUBLE PRECISION */
        module->super.opm_fns[OMPI_OP_BASE_TYPE_DOUBLE_PRECISION] = 
            max_double_precision;
        module->fallback_double_precision = 
            op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_DOUBLE_PRECISION];
        module->fallback_double_precision_module = 
            op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_DOUBLE_PRECISION];
        OBJ_RETAIN(module->fallback_double_precision_module);
    }

    /* ...not listing the rest of the floating point-typed functions
       in this example... */

    return (ompi_op_base_module_t*) module;
}

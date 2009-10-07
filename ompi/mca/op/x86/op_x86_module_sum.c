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
 * This is the sum module source code.  It contains the "setup"
 * functions that will create a module for the MPI_SUM MPI_Op.
 */

#include "ompi_config.h"

#include "opal/class/opal_object.h"
#include "opal/util/output.h"

#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"
#include "ompi/mca/op/x86/op_x86.h"

/**
 * SUM module struct, including local cached info
 */
typedef struct {
    ompi_op_base_module_1_0_0_t super;

    /* Fallback function pointers and modules.  Only doing a few types
       to begin with... will fill in others once we have figured out
       the basics of the assembly stuff. */
    ompi_op_base_handler_fn_t fallback_float;
    ompi_op_base_module_t *fallback_float_module;

    ompi_op_base_handler_fn_t fallback_short;
    ompi_op_base_module_t *fallback_short_module;
    ompi_op_base_handler_fn_t fallback_int;
    ompi_op_base_module_t *fallback_int_module;
    ompi_op_base_handler_fn_t fallback_long;
    ompi_op_base_module_t *fallback_long_module;
} module_sum_t;

/**
 * Sum module constructor
 */
static void module_sum_constructor(module_sum_t *m)
{
    m->fallback_float = NULL;
    m->fallback_float_module = NULL;

    m->fallback_short = NULL;
    m->fallback_short_module = NULL;
    m->fallback_int = NULL;
    m->fallback_int_module = NULL;
    m->fallback_long = NULL;
    m->fallback_long_module = NULL;
}

/**
 * Sum module destructor
 */
static void module_sum_destructor(module_sum_t *m)
{
    m->fallback_float = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_float_module = (ompi_op_base_module_t*) 0xdeadbeef;

    m->fallback_short = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_short_module = (ompi_op_base_module_t*) 0xdeadbeef;
    m->fallback_int = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_int_module = (ompi_op_base_module_t*) 0xdeadbeef;
    m->fallback_long = (ompi_op_base_handler_fn_t) 0xdeadbeef;
    m->fallback_long_module = (ompi_op_base_module_t*) 0xdeadbeef;
}

/**
 * Setup the class for the sum module, listing:
 * - the name of the class
 * - the "parent" of the class
 * - function pointer for the constructor (or NULL)
 * - function pointer for the destructor (or NULL)
 */
static OBJ_CLASS_INSTANCE(module_sum_t,
                          ompi_op_base_module_t,
                          module_sum_constructor,
                          module_sum_destructor);

/**
 * Sum function for C float
 */
static void sum_float(void *in, void *out, int *count, 
                      ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_sum_t *m = (module_sum_t*) module;

    /* Be chatty to the output, just so that we can see that this
       function was called */
    opal_output(0, "In x86 sum float function");
}

/**
 * Sum function for C short
 */
static void sum_short(void *in, void *out, int *count, 
                      ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_sum_t *m = (module_sum_t*) module;
    opal_output(0, "In x86 sum short function");
}

/**
 * Sum function for C int
 */
static void sum_int(void *in, void *out, int *count, 
                    ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_sum_t *m = (module_sum_t*) module;
    opal_output(0, "In x86 sum int function");
}

/**
 * Sum function for C long
 */
static void sum_long(void *in, void *out, int *count, 
                     ompi_datatype_t **type, ompi_op_base_module_t *module)
{
    module_sum_t *m = (module_sum_t*) module;
    opal_output(0, "In x86 sum int function");
}

/**
 * Setup function for MPI_SUM.  If we get here, we can assume that a)
 * the hardware is present, b) the MPI thread scenario is what we
 * want, and c) the SUM operation is supported.  So this function's
 * job is to create a module and fill in function pointers for the
 * functions that this hardware supports.
 */
ompi_op_base_module_t *ompi_op_x86_setup_sum(ompi_op_t *op)
{
    module_sum_t *module = OBJ_NEW(module_sum_t);

    /* JMS It might be better to set function pointers here based on
       the hardware (MMX*, SSE@) -- i.e., make first layer decision of
       which will be used.  I don't know if that's Right, though,
       because we might want to dispatch to different hardware based
       on the size of the operation...?  Just recording the idea
       here... */

    /* Commenting out everything for the moment, just so that we can
       focus on the hardware detection piece first. */
#if 0
    /* C float */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_FLOAT] = sum_float;
    module->fallback_float = op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_FLOAT];
    module->fallback_float_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_FLOAT];
    /* If you cache a fallback function, you *must* RETAIN (i.e.,
       increase the refcount) its module so that the module knows that
       it is being used and won't be freed/destructed. */
    OBJ_RETAIN(module->fallback_float_module);

    /* C short */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_SHORT] = sum_short;
    module->fallback_short = op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_SHORT];
    module->fallback_short_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_SHORT];
    /* If you cache a fallback function, you *must* RETAIN (i.e.,
       increase the refcount) its module so that the module knows that
       it is being used and won't be freed/destructed. */
    OBJ_RETAIN(module->fallback_short_module);

    /* C int */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_INT] = sum_int;
    module->fallback_int = op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_INT];
    module->fallback_int_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_INT];
    /* If you cache a fallback function, you *must* RETAIN (i.e.,
       increase the refcount) its module so that the module knows that
       it is being used and won't be freed/destructed. */
    OBJ_RETAIN(module->fallback_int_module);

    /* C long */
    module->super.opm_fns[OMPI_OP_BASE_TYPE_LONG] = sum_long;
    module->fallback_long = op->o_func.intrinsic.fns[OMPI_OP_BASE_TYPE_LONG];
    module->fallback_long_module = 
        op->o_func.intrinsic.modules[OMPI_OP_BASE_TYPE_LONG];
    /* If you cache a fallback function, you *must* RETAIN (i.e.,
       increase the refcount) its module so that the module knows that
       it is being used and won't be freed/destructed. */
    OBJ_RETAIN(module->fallback_long_module);
#endif

    return (ompi_op_base_module_t*) module;
}

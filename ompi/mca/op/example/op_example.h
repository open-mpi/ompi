/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#ifndef MCA_OP_EXAMPLE_EXPORT_H
#define MCA_OP_EXAMPLE_EXPORT_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_object.h"

#include "ompi/mca/op/op.h"

BEGIN_C_DECLS

/**
 * Derive a struct from the base op component struct, allowing us to
 * cache some component-specific information on our well-known
 * component struct.
 */
typedef struct {
    /** The base op component struct */
    ompi_op_base_component_1_0_0_t super;

    /* What follows is example-component-specific cached information.  We
       tend to use this scheme (caching information on the example
       component itself) instead of lots of individual global
       variables for the component.  The following data fields are
       examples; replace them with whatever is relevant for your
       component. */

    /** A simple boolean indicating that the hardware is available. */
    bool hardware_available;

    /** A simple boolean indicating whether double precision is
        supported. */
    bool double_supported;
} ompi_op_example_component_t;

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
       supporting.  */
    double some_bxor_data;
} ompi_op_example_module_bxor_t;

/**
 * To use OMPI's OBJ system, you have to declare each "class".
 */
OBJ_CLASS_DECLARATION(ompi_op_example_module_bxor_t);

/**
 * Globally exported variable.  Note that it is a *example* component
 * (defined above), which has the ompi_op_base_component_t as its
 * first member.  Hence, the MCA/op framework will find the data that
 * it expects in the first memory locations, but then the component
 * itself can cache additional information after that that can be used
 * by both the component and modules.
 */
OMPI_DECLSPEC extern ompi_op_example_component_t 
    mca_op_example_component;

/**
 * Setup for MPI_MAX and return a module.
 */
OMPI_DECLSPEC ompi_op_base_module_t *
    ompi_op_example_setup_max(ompi_op_t *op);

/**
 * Setup for MPI_BXOR and return a module.
 */
OMPI_DECLSPEC ompi_op_base_module_t *
    ompi_op_example_setup_bxor(ompi_op_t *op);

END_C_DECLS

#endif /* MCA_OP_EXAMPLE_EXPORT_H */

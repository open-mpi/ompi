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

#ifndef MCA_OP_X86_EXPORT_H
#define MCA_OP_X86_EXPORT_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_object.h"

#include "ompi/mca/op/op.h"

BEGIN_C_DECLS

/**
 * Flags for each hardware type
 */
typedef enum {
    OP_X86_HW_FLAGS_MMX = 1,
    OP_X86_HW_FLAGS_MMX2 = 2,
    OP_X86_HW_FLAGS_SSE = 4,
    OP_X86_HW_FLAGS_SSE2 = 8,
    OP_X86_HW_FLAGS_SSE3 = 16
} op_x86_hw_flags_t;

/**
 * Derive a struct from the base op component struct, allowing us to
 * cache some component-specific information on our well-known
 * component struct.
 */
typedef struct {
    /** The base op component struct */
    ompi_op_base_component_1_0_0_t super;

    /* What hardware do we have? */
    op_x86_hw_flags_t oxc_hw_flags;
} ompi_op_x86_component_t;

/**
 * Derive a struct from the base op module struct, allowing us to
 * cache some module-specific information for SUM. 
 */
typedef struct {
    ompi_op_base_module_1_0_0_t super;

    /* JMS need anything here? */
} ompi_op_x86_module_sum_t;

OBJ_CLASS_DECLARATION(ompi_op_x86_module_sum_t);

/**
 * Well-known component instance
 */
OMPI_DECLSPEC extern ompi_op_x86_component_t mca_op_x86_component;

/**
 * Setup for MPI_MAX and return a module.
 */
OMPI_DECLSPEC ompi_op_base_module_t *ompi_op_x86_setup_sum(ompi_op_t *op);

END_C_DECLS

#endif /* MCA_OP_X86_EXPORT_H */

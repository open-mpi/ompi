/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_PART_BASE_H
#define MCA_PART_BASE_H

#include "ompi_config.h"

#include "ompi/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "ompi/mca/part/part.h"

/*
 * Global functions for PART
 */

BEGIN_C_DECLS

/*
 * This is the base priority for a PART wrapper component
 * If there exists more than one then it is undefined
 * which one is picked.
 */
#define PART_SELECT_WRAPPER_PRIORITY -128

/*
 * MCA framework
 */
OMPI_DECLSPEC extern mca_base_framework_t ompi_part_base_framework;

/*
 * Select an available component.
 */
OMPI_DECLSPEC  int mca_part_base_select(bool enable_progress_threads,
                                        bool enable_mpi_threads);

OMPI_DECLSPEC  int mca_part_base_progress(void);

OMPI_DECLSPEC int mca_part_base_finalize(void);

/*
 * Globals
 */
OMPI_DECLSPEC extern mca_part_base_component_t mca_part_base_selected_component;
OMPI_DECLSPEC extern mca_part_base_module_t mca_part;
OMPI_DECLSPEC extern opal_pointer_array_t mca_part_base_part;

END_C_DECLS

#endif /* MCA_PART_BASE_H */

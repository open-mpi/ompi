/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 University of Houston. All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/include/ompi/constants.h"
#include "ompi/include/ompi/frameworks.h"

#include "ompi/runtime/params.h"

#include "opal/runtime/opal_info_support.h"
#include "ompi/runtime/ompi_info_support.h"
#include "opal/util/show_help.h"

#if OMPI_RTE_ORTE
#include "orte/runtime/orte_info_support.h"
#endif

static bool ompi_info_registered = false;

int ompi_info_register_framework_params(opal_pointer_array_t *component_map)
{
    int rc;

    if (ompi_info_registered) {
        return OMPI_SUCCESS;
    }

    ompi_info_registered = true;

    rc = opal_info_register_framework_params(component_map);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* Register the MPI layer's MCA parameters */
    if (OMPI_SUCCESS != (rc = ompi_mpi_register_params())) {
        fprintf(stderr, "ompi_info_register: ompi_mpi_register_params failed\n");
        return rc;
    }

#if OMPI_RTE_ORTE
    rc = orte_info_register_framework_params(component_map);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }
#endif

    return opal_info_register_project_frameworks("ompi", ompi_frameworks, component_map);
}

/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "opal/runtime/opal_cr.h"
#include "ompi/mpiext/cr/c/mpiext_cr_c.h"

#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

static const char FUNC_NAME[] = "OMPI_CR_self_register_checkpoint_callback";

int OMPI_CR_self_register_checkpoint_callback(OMPI_CR_self_checkpoint_fn function)
{
    int rc;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME); 
    }

    OPAL_CR_ENTER_LIBRARY();

    rc = opal_crs_base_self_register_checkpoint_callback(function);

    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}

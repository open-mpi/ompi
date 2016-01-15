/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Status_set_elements = PMPI_Status_set_elements
#endif
#define MPI_Status_set_elements PMPI_Status_set_elements
#endif

static const char FUNC_NAME[] = "MPI_Status_set_elements";

int MPI_Status_set_elements(MPI_Status *status, MPI_Datatype datatype, int count)
{
    int rc = MPI_SUCCESS;
    size_t size;

    MEMCHECKER(
        if(status != MPI_STATUSES_IGNORE) {
            /*
             * Before checking the complete status, we need to reset the definedness
             * of the MPI_ERROR-field (single-completion calls wait/test).
             */
            opal_memchecker_base_mem_defined(&status->MPI_ERROR, sizeof(int));
            memchecker_status (status);
            memchecker_datatype(datatype);
        }
    );

    OPAL_CR_NOOP_PROGRESS();

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == datatype || MPI_DATATYPE_NULL == datatype) {
            rc = MPI_ERR_TYPE;
        } else if (count < 0) {
            rc = MPI_ERR_COUNT;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    /* ROMIO calls MPI_STATUS_SET_ELEMENTS with IGNORE values, so we
       need to allow it.  Blah! */
    if (MPI_STATUS_IGNORE == status || MPI_STATUSES_IGNORE == status) {
        return MPI_SUCCESS;
    }

    if( ompi_datatype_is_predefined(datatype) ) {
        ompi_datatype_type_size( datatype, &size );
        status->_ucount = count * size;
    } else {
        ompi_datatype_set_element_count( datatype, count, &size );
        status->_ucount = size;
    }
    return MPI_SUCCESS;
}

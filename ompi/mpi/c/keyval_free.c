/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Keyval_free = PMPI_Keyval_free
#endif
#define MPI_Keyval_free PMPI_Keyval_free
#else
/*
 * Emit the public MPI_* symbol as a *weak* definition.  Where weak aliases
 * are available the alias above is already weak; where they are not, the
 * bindings are compiled a second time to produce MPI_*, and this is that
 * copy -- so mark it weak here.
 *
 * Weak MPI_* is what lets a profiling library provide a strong MPI_* that
 * overrides ours, and it is what the MPI Forum ABI requires of libmpi_abi.
 */
#pragma weak MPI_Keyval_free
#endif
int MPI_Keyval_free(int *keyval)
{
    int ret;

    /* Check for valid key pointer */
    if (MPI_PARAM_CHECK) {
        if (NULL == keyval) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_KEYVAL,
                                          "MPI_Keyval_free");
        }
    }

    ret = ompi_attr_free_keyval(COMM_ATTR, keyval, 0);
    OMPI_ERRHANDLER_NOHANDLE_RETURN(ret, MPI_ERR_OTHER, "MPI_Keyval_free");
}

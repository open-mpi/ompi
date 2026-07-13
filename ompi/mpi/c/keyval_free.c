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

#if OMPI_BUILD_MPI_PROFILING && !OPAL_HAVE_WEAK_ALIASES
/*
 * Mach-O cannot express a weak *alias* -- there is no way to mark a ".set"
 * alias as a weak definition -- so where weak aliases are unavailable the
 * public MPI_* symbol is defined here as a weak function that forwards to the
 * strong PMPI_* one.  That is what lets these bindings be compiled exactly
 * once: this translation unit provides both the strong PMPI_* symbol
 * (above) and the weak MPI_* symbol (here).
 */
#undef MPI_Keyval_free
__opal_attribute_weak__ int MPI_Keyval_free(int *keyval)
{
    return PMPI_Keyval_free(keyval);
}
#endif

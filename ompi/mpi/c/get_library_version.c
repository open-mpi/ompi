/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_library_version = PMPI_Get_library_version
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Get_library_version";


int MPI_Get_library_version(char *version, int *resultlen) 
{
    int len_left;
    MPI_Comm null = MPI_COMM_NULL;
    char *ptr, tmp[MPI_MAX_LIBRARY_VERSION_STRING];

    OPAL_CR_NOOP_PROGRESS();

    if (MPI_PARAM_CHECK) {
        /* Per MPI-3, this function can be invoked before
           MPI_INIT, so we don't invoke the normal
           MPI_ERR_INIT_FINALIZE() macro here */
        
        if (NULL == version || NULL == resultlen) {
            /* Note that we have to check and see if we have
               previously called MPI_INIT or not.  If so, use the
               normal OMPI_ERRHANDLER_INVOKE, because the user may
               have changed the default errhandler on MPI_COMM_WORLD.
               If we have not invoked MPI_INIT, then just abort
               (i.e., use a NULL communicator, which will end up at the
               default errhandler, which is abort). */

            if (ompi_mpi_initialized && !ompi_mpi_finalized) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                              FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_INVOKE(null, MPI_ERR_ARG,
                                              FUNC_NAME);
            }
        }
    }

    /* First write to a tmp variable so that we can write to *all* the
       chars (MPI-3 says that we can only write resultlen chars to the
       output string) */
    ptr = tmp;
    len_left = sizeof(tmp);
    memset(tmp, 0, MPI_MAX_LIBRARY_VERSION_STRING);

    snprintf(tmp, MPI_MAX_LIBRARY_VERSION_STRING, "Open MPI v%d.%d", 
             OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION);
    ptr += strlen(tmp);
    len_left -= strlen(tmp);

    if (OMPI_RELEASE_VERSION > 0) {
        snprintf(ptr, len_left, ".%d", OMPI_RELEASE_VERSION);
        ptr = tmp + strlen(tmp);
        len_left = MPI_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }
    if (NULL != OMPI_GREEK_VERSION && strlen(OMPI_GREEK_VERSION) > 0) {
        snprintf(ptr, len_left, "%s", OMPI_GREEK_VERSION);
        ptr = tmp + strlen(tmp);
        len_left = MPI_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Package name */
    if (NULL != OPAL_PACKAGE_STRING && strlen(OPAL_PACKAGE_STRING) > 0) {
        snprintf(ptr, len_left, ", package: %s", OPAL_PACKAGE_STRING);
        ptr = tmp + strlen(tmp);
        len_left = MPI_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Ident string */
    if (NULL != OMPI_IDENT_STRING && strlen(OMPI_IDENT_STRING) > 0) {
        snprintf(ptr, len_left, ", ident: %s", OMPI_IDENT_STRING);
        ptr = tmp + strlen(tmp);
        len_left = MPI_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Repository revision */
    if (NULL != OMPI_REPO_REV && strlen(OMPI_REPO_REV) > 0) {
        snprintf(ptr, len_left, ", repo rev: %s", OMPI_REPO_REV);
        ptr = tmp + strlen(tmp);
        len_left = MPI_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Release date */
    if (NULL != OMPI_RELEASE_DATE && strlen(OMPI_RELEASE_DATE) > 0) {
        snprintf(ptr, len_left, ", %s", OMPI_RELEASE_DATE);
        ptr = tmp + strlen(tmp);
        len_left = MPI_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }    

    memcpy(version, tmp, strlen(tmp) + 1);
    *resultlen = strlen(tmp) + 1;

    return MPI_SUCCESS;
}

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#if OPAL_CC_USE_PRAGMA_IDENT
#pragma ident OMPI_IDENT_STRING
#elif OPAL_CC_USE_IDENT
#ident OMPI_IDENT_STRING
#else
static const char ident[] = OMPI_IDENT_STRING;
#endif

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_INIT = ompi_init_f
#pragma weak pmpi_init = ompi_init_f
#pragma weak pmpi_init_ = ompi_init_f
#pragma weak pmpi_init__ = ompi_init_f

#pragma weak PMPI_Init_f = ompi_init_f
#pragma weak PMPI_Init_f08 = ompi_init_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INIT,
                           pmpi_init,
                           pmpi_init_,
                           pmpi_init__,
                           pompi_init_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT = ompi_init_f
#pragma weak mpi_init = ompi_init_f
#pragma weak mpi_init_ = ompi_init_f
#pragma weak mpi_init__ = ompi_init_f

#pragma weak MPI_Init_f = ompi_init_f
#pragma weak MPI_Init_f08 = ompi_init_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INIT,
                           mpi_init,
                           mpi_init_,
                           mpi_init__,
                           ompi_init_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_init_f( MPI_Fint *ierr )
{
    int c_ierr;
    int argc = 0;
    char **argv = NULL;

    c_ierr = MPI_Init( &argc, &argv );
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

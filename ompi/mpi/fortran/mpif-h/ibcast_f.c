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
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_IBCAST = ompi_ibcast_f
#pragma weak pmpi_ibcast = ompi_ibcast_f
#pragma weak pmpi_ibcast_ = ompi_ibcast_f
#pragma weak pmpi_ibcast__ = ompi_ibcast_f

#pragma weak PMPI_Ibcast_f = ompi_ibcast_f
#pragma weak PMPI_Ibcast_f08 = ompi_ibcast_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IBCAST,
                            pmpi_ibcast,
                            pmpi_ibcast_,
                            pmpi_ibcast__,
                            pompi_ibcast_f,
                            (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (buffer, count, datatype, root, comm, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IBCAST = ompi_ibcast_f
#pragma weak mpi_ibcast = ompi_ibcast_f
#pragma weak mpi_ibcast_ = ompi_ibcast_f
#pragma weak mpi_ibcast__ = ompi_ibcast_f

#pragma weak MPI_Ibcast_f = ompi_ibcast_f
#pragma weak MPI_Ibcast_f08 = ompi_ibcast_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IBCAST,
                            mpi_ibcast,
                            mpi_ibcast_,
                            mpi_ibcast__,
                            ompi_ibcast_f,
                            (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (buffer, count, datatype, root, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_ibcast_f(char *buffer, MPI_Fint *count, MPI_Fint *datatype, 
                   MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request,
                   MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Request c_req;
    MPI_Datatype c_type;

    c_comm = MPI_Comm_f2c(*comm);
    c_type = MPI_Type_f2c(*datatype);

    c_ierr = MPI_Ibcast(OMPI_F2C_BOTTOM(buffer), 
                        OMPI_FINT_2_INT(*count), 
                        c_type,
                        OMPI_FINT_2_INT(*root),
                        c_comm,
                        &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = MPI_Request_c2f(c_req);
}

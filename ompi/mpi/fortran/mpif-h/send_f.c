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
#pragma weak PMPI_SEND = ompi_send_f
#pragma weak pmpi_send = ompi_send_f
#pragma weak pmpi_send_ = ompi_send_f
#pragma weak pmpi_send__ = ompi_send_f

#pragma weak PMPI_Send_f = ompi_send_f
#pragma weak PMPI_Send_f08 = ompi_send_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SEND,
                           pmpi_send,
                           pmpi_send_,
                           pmpi_send__,
                           pompi_send_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SEND = ompi_send_f
#pragma weak mpi_send = ompi_send_f
#pragma weak mpi_send_ = ompi_send_f
#pragma weak mpi_send__ = ompi_send_f

#pragma weak MPI_Send_f = ompi_send_f
#pragma weak MPI_Send_f08 = ompi_send_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SEND,
                           mpi_send,
                           mpi_send_,
                           mpi_send__,
                           ompi_send_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_send_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, 
                MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;

    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);

    c_ierr = MPI_Send(OMPI_F2C_BOTTOM(buf), OMPI_FINT_2_INT(*count), 
                      c_type, OMPI_FINT_2_INT(*dest), 
                      OMPI_FINT_2_INT(*tag), c_comm);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

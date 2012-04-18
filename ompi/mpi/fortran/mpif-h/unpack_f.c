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
#pragma weak PMPI_UNPACK = ompi_unpack_f
#pragma weak pmpi_unpack = ompi_unpack_f
#pragma weak pmpi_unpack_ = ompi_unpack_f
#pragma weak pmpi_unpack__ = ompi_unpack_f

#pragma weak PMPI_Unpack_f = ompi_unpack_f
#pragma weak PMPI_Unpack_f08 = ompi_unpack_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_UNPACK,
                           pmpi_unpack,
                           pmpi_unpack_,
                           pmpi_unpack__,
                           pompi_unpack_f,
                           (char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, insize, position, outbuf, outcount, datatype, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_UNPACK = ompi_unpack_f
#pragma weak mpi_unpack = ompi_unpack_f
#pragma weak mpi_unpack_ = ompi_unpack_f
#pragma weak mpi_unpack__ = ompi_unpack_f

#pragma weak MPI_Unpack_f = ompi_unpack_f
#pragma weak MPI_Unpack_f08 = ompi_unpack_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_UNPACK,
                           mpi_unpack,
                           mpi_unpack_,
                           mpi_unpack__,
                           ompi_unpack_f,
                           (char *inbuf, MPI_Fint *insize, MPI_Fint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, insize, position, outbuf, outcount, datatype, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_unpack_f(char *inbuf, MPI_Fint *insize, MPI_Fint *position,
		  char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype,
		  MPI_Fint *comm, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Comm c_comm;
   MPI_Datatype c_type;
   OMPI_SINGLE_NAME_DECL(position);

   c_comm = MPI_Comm_f2c(*comm);
   c_type = MPI_Type_f2c(*datatype);
   OMPI_SINGLE_FINT_2_INT(position);

   c_ierr = MPI_Unpack(inbuf, OMPI_FINT_2_INT(*insize),
                       OMPI_SINGLE_NAME_CONVERT(position),
                       OMPI_F2C_BOTTOM(outbuf), OMPI_FINT_2_INT(*outcount),
                       c_type, c_comm);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(position);
    }
}

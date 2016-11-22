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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_PACK = ompi_pack_f
#pragma weak pmpi_pack = ompi_pack_f
#pragma weak pmpi_pack_ = ompi_pack_f
#pragma weak pmpi_pack__ = ompi_pack_f

#pragma weak PMPI_Pack_f = ompi_pack_f
#pragma weak PMPI_Pack_f08 = ompi_pack_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_PACK,
                           pmpi_pack,
                           pmpi_pack_,
                           pmpi_pack__,
                           pompi_pack_f,
                           (char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, incount, datatype, outbuf, outsize, position, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK = ompi_pack_f
#pragma weak mpi_pack = ompi_pack_f
#pragma weak mpi_pack_ = ompi_pack_f
#pragma weak mpi_pack__ = ompi_pack_f

#pragma weak MPI_Pack_f = ompi_pack_f
#pragma weak MPI_Pack_f08 = ompi_pack_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_PACK,
                           mpi_pack,
                           mpi_pack_,
                           mpi_pack__,
                           ompi_pack_f,
                           (char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Fint *outsize, MPI_Fint *position, MPI_Fint *comm, MPI_Fint *ierr),
                           (inbuf, incount, datatype, outbuf, outsize, position, comm, ierr) )
#else
#define ompi_pack_f pompi_pack_f
#endif
#endif


void ompi_pack_f(char *inbuf, MPI_Fint *incount, MPI_Fint *datatype,
		char *outbuf, MPI_Fint *outsize, MPI_Fint *position,
		MPI_Fint *comm, MPI_Fint *ierr)
{
   int c_ierr;
   MPI_Comm c_comm;
   MPI_Datatype c_type;
   OMPI_SINGLE_NAME_DECL(position);

   c_comm = PMPI_Comm_f2c(*comm);
   c_type = PMPI_Type_f2c(*datatype);
   OMPI_SINGLE_FINT_2_INT(position);

   c_ierr = PMPI_Pack(OMPI_F2C_BOTTOM(inbuf), OMPI_FINT_2_INT(*incount),
                     c_type, outbuf,
                     OMPI_FINT_2_INT(*outsize),
                     OMPI_SINGLE_NAME_CONVERT(position),
                     c_comm);
   if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

   if (MPI_SUCCESS == c_ierr) {
       OMPI_SINGLE_INT_2_FINT(position);
   }
}

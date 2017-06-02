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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpi/fortran/base/fortran_base_strings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_PACK_EXTERNAL = ompi_pack_external_f
#pragma weak pmpi_pack_external = ompi_pack_external_f
#pragma weak pmpi_pack_external_ = ompi_pack_external_f
#pragma weak pmpi_pack_external__ = ompi_pack_external_f

#pragma weak PMPI_Pack_external_f = ompi_pack_external_f
#pragma weak PMPI_Pack_external_f08 = ompi_pack_external_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_PACK_EXTERNAL,
                           pmpi_pack_external,
                           pmpi_pack_external_,
                           pmpi_pack_external__,
                           pompi_pack_external_f,
                            (char *datarep, char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Aint *outsize, MPI_Aint *position, MPI_Fint *ierr, int datarep_len),
                            (datarep, inbuf, incount, datatype, outbuf, outsize, position, ierr, datarep_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PACK_EXTERNAL = ompi_pack_external_f
#pragma weak mpi_pack_external = ompi_pack_external_f
#pragma weak mpi_pack_external_ = ompi_pack_external_f
#pragma weak mpi_pack_external__ = ompi_pack_external_f

#pragma weak MPI_Pack_external_f = ompi_pack_external_f
#pragma weak MPI_Pack_external_f08 = ompi_pack_external_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_PACK_EXTERNAL,
                           mpi_pack_external,
                           mpi_pack_external_,
                           mpi_pack_external__,
                           ompi_pack_external_f,
                            (char *datarep, char *inbuf, MPI_Fint *incount, MPI_Fint *datatype, char *outbuf, MPI_Aint *outsize, MPI_Aint *position, MPI_Fint *ierr, int datarep_len),
                            (datarep, inbuf, incount, datatype, outbuf, outsize, position, ierr, datarep_len) )
#else
#define ompi_pack_external_f pompi_pack_external_f
#endif
#endif


void ompi_pack_external_f(char *datarep, char *inbuf, MPI_Fint *incount,
			 MPI_Fint *datatype, char *outbuf,
			 MPI_Aint *outsize, MPI_Aint *position,
			 MPI_Fint *ierr, int datarep_len)
{
    int ret, c_ierr;
    char *c_datarep;
    MPI_Datatype type = PMPI_Type_f2c(*datatype);

    /* Convert the fortran string */

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(datarep, datarep_len,
                                                       &c_datarep))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret,
                                        "MPI_PACK_EXTERNAL");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = PMPI_Pack_external(c_datarep, OMPI_F2C_BOTTOM(inbuf),
                               OMPI_FINT_2_INT(*incount),
                               type, outbuf,
                               *outsize,
                               position);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free(c_datarep);
}

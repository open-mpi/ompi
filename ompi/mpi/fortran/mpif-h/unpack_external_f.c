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
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpi/fortran/base/strings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_UNPACK_EXTERNAL = ompi_unpack_external_f
#pragma weak pmpi_unpack_external = ompi_unpack_external_f
#pragma weak pmpi_unpack_external_ = ompi_unpack_external_f
#pragma weak pmpi_unpack_external__ = ompi_unpack_external_f

#pragma weak PMPI_Unpack_external_f = ompi_unpack_external_f
#pragma weak PMPI_Unpack_external_f08 = ompi_unpack_external_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_UNPACK_EXTERNAL,
                           pmpi_unpack_external,
                           pmpi_unpack_external_,
                           pmpi_unpack_external__,
                           pompi_unpack_external_f,
                            (char *datarep, char *inbuf, MPI_Aint *insize, MPI_Aint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *ierr, int datarep_len),
                            (datarep, inbuf, insize, position, outbuf, outcount, datatype, ierr, datarep_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_UNPACK_EXTERNAL = ompi_unpack_external_f
#pragma weak mpi_unpack_external = ompi_unpack_external_f
#pragma weak mpi_unpack_external_ = ompi_unpack_external_f
#pragma weak mpi_unpack_external__ = ompi_unpack_external_f

#pragma weak MPI_Unpack_external_f = ompi_unpack_external_f
#pragma weak MPI_Unpack_external_f08 = ompi_unpack_external_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_UNPACK_EXTERNAL,
                           mpi_unpack_external,
                           mpi_unpack_external_,
                           mpi_unpack_external__,
                           ompi_unpack_external_f,
                            (char *datarep, char *inbuf, MPI_Aint *insize, MPI_Aint *position, char *outbuf, MPI_Fint *outcount, MPI_Fint *datatype, MPI_Fint *ierr, int datarep_len),
                            (datarep, inbuf, insize, position, outbuf, outcount, datatype, ierr, datarep_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_unpack_external_f (char *datarep, char *inbuf, MPI_Aint *insize,
			    MPI_Aint *position, char *outbuf, 
			    MPI_Fint *outcount, MPI_Fint *datatype,
			    MPI_Fint *ierr, int datarep_len)
{
    int ret, c_ierr;
    char *c_datarep;
    MPI_Datatype c_type;

    c_type = MPI_Type_f2c(*datatype);

    /* Convert the fortran string */
    
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(datarep, datarep_len,
                                                       &c_datarep))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret,
                                        "MPI_PACK_EXTERNAL");
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    c_ierr = MPI_Unpack_external(c_datarep, inbuf, 
                                 *insize, 
                                 position,
                                 OMPI_F2C_BOTTOM(outbuf),
                                 OMPI_FINT_2_INT(*outcount),
                                 c_type);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free(c_datarep);
}

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
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/mpif-h/status-conversion.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_IMPROBE = ompi_improbe_f
#pragma weak pmpi_improbe = ompi_improbe_f
#pragma weak pmpi_improbe_ = ompi_improbe_f
#pragma weak pmpi_improbe__ = ompi_improbe_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IMPROBE,
                            pmpi_improbe,
                            pmpi_improbe_,
                            pmpi_improbe__,
                            pompi_improbe_f,
                            (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, ompi_fortran_logical_t *flag, 
                             MPI_Fint *message, MPI_Fint *status, MPI_Fint *ierr),
                            (source, tag, comm, flag, message, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IMPROBE = ompi_improbe_f
#pragma weak mpi_improbe = ompi_improbe_f
#pragma weak mpi_improbe_ = ompi_improbe_f
#pragma weak mpi_improbe__ = ompi_improbe_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IMPROBE,
                            mpi_improbe,
                            mpi_improbe_,
                            mpi_improbe__,
                            ompi_improbe_f,
                            (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, ompi_fortran_logical_t *flag, 
                             MPI_Fint *message, MPI_Fint *status, MPI_Fint *ierr),
                            (source, tag, comm, flag, message, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_improbe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm,
                    ompi_fortran_logical_t *flag, MPI_Fint *message,
                    MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Message c_message;
    MPI_Comm c_comm;
    OMPI_FORTRAN_STATUS_DECLARATION(c_status,c_status2)
    int c_ierr;
    OMPI_LOGICAL_NAME_DECL(flag);

    c_comm = MPI_Comm_f2c (*comm);

    OMPI_FORTRAN_STATUS_SET_POINTER(c_status,c_status2,status)

    c_ierr = OMPI_INT_2_FINT(MPI_Improbe(OMPI_FINT_2_INT(*source),
                                         OMPI_FINT_2_INT(*tag),
                                         c_comm, OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                                         &c_message, c_status));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
        if (OMPI_FORTRAN_VALUE_TRUE == *flag) {
            OMPI_FORTRAN_STATUS_RETURN(c_status,c_status2,status,c_ierr)
            *message = MPI_Message_c2f(c_message);
        } 
    }
}

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
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
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
#pragma weak PMPI_MPROBE = ompi_mprobe_f
#pragma weak pmpi_mprobe = ompi_mprobe_f
#pragma weak pmpi_mprobe_ = ompi_mprobe_f
#pragma weak pmpi_mprobe__ = ompi_mprobe_f

#pragma weak PMPI_Mprobe_f = ompi_mprobe_f
#pragma weak PMPI_Mprobe_f08 = ompi_mprobe_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_MPROBE,
                            pmpi_mprobe,
                            pmpi_mprobe_,
                            pmpi_mprobe__,
                            pompi_mprobe_f,
                            (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *message, 
                             MPI_Fint *status, MPI_Fint *ierr),
                            (source, tag, comm, message, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_MPROBE = ompi_mprobe_f
#pragma weak mpi_mprobe = ompi_mprobe_f
#pragma weak mpi_mprobe_ = ompi_mprobe_f
#pragma weak mpi_mprobe__ = ompi_mprobe_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_MPROBE,
                            mpi_mprobe,
                            mpi_mprobe_,
                            mpi_mprobe__,
                            ompi_mprobe_f,
                            (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *message, 
                             MPI_Fint *status, MPI_Fint *ierr),
                            (source, tag, comm, message, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_mprobe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, 
                   MPI_Fint *message, MPI_Fint *status, MPI_Fint *ierr)
{   
    MPI_Comm c_comm;
    MPI_Message c_message;
    OMPI_FORTRAN_STATUS_DECLARATION(c_status,c_status2)
    int c_ierr;

    c_comm = MPI_Comm_f2c (*comm);

    OMPI_FORTRAN_STATUS_SET_POINTER(c_status,c_status2,status)

    c_ierr = OMPI_INT_2_FINT(MPI_Mprobe(OMPI_FINT_2_INT(*source),
                                        OMPI_FINT_2_INT(*tag),
                                        c_comm, &c_message,
                                        c_status));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_FORTRAN_STATUS_RETURN(c_status,c_status2,status,c_ierr)
        *message = MPI_Message_c2f(c_message);
    }
}

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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_MPROBE = mpi_mprobe_f
#pragma weak pmpi_mprobe = mpi_mprobe_f
#pragma weak pmpi_mprobe_ = mpi_mprobe_f
#pragma weak pmpi_mprobe__ = mpi_mprobe_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_MPROBE,
                            pmpi_mprobe,
                            pmpi_mprobe_,
                            pmpi_mprobe__,
                            pmpi_mprobe_f,
                            (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *message, 
                             MPI_Fint *status, MPI_Fint *ierr),
                            (source, tag, comm, message, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_MPROBE = mpi_mprobe_f
#pragma weak mpi_mprobe = mpi_mprobe_f
#pragma weak mpi_mprobe_ = mpi_mprobe_f
#pragma weak mpi_mprobe__ = mpi_mprobe_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_MPROBE,
                            mpi_mprobe,
                            mpi_mprobe_,
                            mpi_mprobe__,
                            mpi_mprobe_f,
                            (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *message, 
                             MPI_Fint *status, MPI_Fint *ierr),
                            (source, tag, comm, message, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_mprobe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *message, MPI_Fint *status, MPI_Fint *ierr)
{   
    MPI_Status *c_status;
    MPI_Comm c_comm;
    MPI_Message c_message;
#if OMPI_SIZEOF_FORTRAN_INTEGER != SIZEOF_INT
    MPI_Status c_status2;
#endif

    c_comm = MPI_Comm_f2c (*comm);

    /* See if we got MPI_STATUS_IGNORE */
    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        c_status = MPI_STATUS_IGNORE;
    } else {

        /* If sizeof(int) == sizeof(INTEGER), then there's no
           translation necessary -- let the underlying functions write
           directly into the Fortran status */

#if OMPI_SIZEOF_FORTRAN_INTEGER == SIZEOF_INT
        c_status = (MPI_Status *) status;
#else
        c_status = &c_status2;
#endif
    }

    *ierr = OMPI_INT_2_FINT(MPI_Mprobe(OMPI_FINT_2_INT(*source),
                                       OMPI_FINT_2_INT(*tag),
                                       c_comm, &c_message,
                                       c_status));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
#if OMPI_SIZEOF_FORTRAN_INTEGER != SIZEOF_INT
        if (MPI_STATUS_IGNORE != c_status) {
            MPI_Status_c2f(c_status, status);
        }
#endif
        *message = MPI_Message_c2f(c_message);
    }
}

/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_SEND = mpi_send_f
#pragma weak pmpi_send = mpi_send_f
#pragma weak pmpi_send_ = mpi_send_f
#pragma weak pmpi_send__ = mpi_send_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SEND,
                           pmpi_send,
                           pmpi_send_,
                           pmpi_send__,
                           pmpi_send_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SEND = mpi_send_f
#pragma weak mpi_send = mpi_send_f
#pragma weak mpi_send_ = mpi_send_f
#pragma weak mpi_send__ = mpi_send_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SEND,
                           mpi_send,
                           mpi_send_,
                           mpi_send__,
                           mpi_send_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_send_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);

    c_comm = MPI_Comm_f2c (*comm);

    *ierr = MPI_Send(buf, *count, c_type, *dest, *tag, c_comm);
}

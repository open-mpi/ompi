/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_RECV = mpi_recv_f
#pragma weak pmpi_recv = mpi_recv_f
#pragma weak pmpi_recv_ = mpi_recv_f
#pragma weak pmpi_recv__ = mpi_recv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_RECV,
                           pmpi_recv,
                           pmpi_recv_,
                           pmpi_recv__,
                           pmpi_recv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_RECV = mpi_recv_f
#pragma weak mpi_recv = mpi_recv_f
#pragma weak mpi_recv_ = mpi_recv_f
#pragma weak mpi_recv__ = mpi_recv_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_RECV,
                           mpi_recv,
                           mpi_recv_,
                           mpi_recv__,
                           mpi_recv_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, source, tag, comm, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_recv_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, 
                MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, 
                MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Comm c_comm = MPI_Comm_f2c(OMPI_FINT_2_INT(*comm));
    MPI_Datatype c_type = MPI_Type_f2c(OMPI_FINT_2_INT(*datatype));

    *ierr = OMPI_INT_2_FINT(MPI_Recv( buf, OMPI_FINT_2_INT(*count), c_type, 
                                      OMPI_FINT_2_INT(*source), 
                                      OMPI_FINT_2_INT(*tag), c_comm,
                                      (MPI_Status*) status));
}

/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_SENDRECV_REPLACE = mpi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace = mpi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace_ = mpi_sendrecv_replace_f
#pragma weak pmpi_sendrecv_replace__ = mpi_sendrecv_replace_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SENDRECV_REPLACE,
                           pmpi_sendrecv_replace,
                           pmpi_sendrecv_replace_,
                           pmpi_sendrecv_replace__,
                           pmpi_sendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SENDRECV_REPLACE = mpi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace = mpi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace_ = mpi_sendrecv_replace_f
#pragma weak mpi_sendrecv_replace__ = mpi_sendrecv_replace_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SENDRECV_REPLACE,
                           mpi_sendrecv_replace,
                           mpi_sendrecv_replace_,
                           mpi_sendrecv_replace__,
                           mpi_sendrecv_replace_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_sendrecv_replace_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *sendtag, MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c (*comm);

    *ierr = MPI_Sendrecv_replace(buf, *count, c_type,  *dest, 
                                 *sendtag, *source, *recvtag, c_comm, 
                                 (MPI_Status*)*status);
}

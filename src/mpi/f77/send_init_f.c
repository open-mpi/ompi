/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_SEND_INIT = mpi_send_init_f
#pragma weak pmpi_send_init = mpi_send_init_f
#pragma weak pmpi_send_init_ = mpi_send_init_f
#pragma weak pmpi_send_init__ = mpi_send_init_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SEND_INIT,
                           pmpi_send_init,
                           pmpi_send_init_,
                           pmpi_send_init__,
                           pmpi_send_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SEND_INIT = mpi_send_init_f
#pragma weak mpi_send_init = mpi_send_init_f
#pragma weak mpi_send_init_ = mpi_send_init_f
#pragma weak mpi_send_init__ = mpi_send_init_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SEND_INIT,
                           mpi_send_init,
                           mpi_send_init_,
                           mpi_send_init__,
                           mpi_send_init_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_send_init_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);
    MPI_Request c_req;
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c (*comm);

    *ierr = MPI_Send_init(buf, *count, c_type, *dest, *tag, c_comm, &c_req);

    if (*ierr == MPI_SUCCESS) {
        *request = MPI_Request_c2f(c_req);
    }
}

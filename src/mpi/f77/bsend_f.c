/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_BSEND = mpi_bsend_f
#pragma weak pmpi_bsend = mpi_bsend_f
#pragma weak pmpi_bsend_ = mpi_bsend_f
#pragma weak pmpi_bsend__ = mpi_bsend_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_BSEND,
                           pmpi_bsend,
                           pmpi_bsend_,
                           pmpi_bsend__,
                           pmpi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BSEND = mpi_bsend_f
#pragma weak mpi_bsend = mpi_bsend_f
#pragma weak mpi_bsend_ = mpi_bsend_f
#pragma weak mpi_bsend__ = mpi_bsend_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_BSEND,
                           mpi_bsend,
                           mpi_bsend_,
                           mpi_bsend__,
                           mpi_bsend_f,
                           (char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr),
                           (buf, count, datatype, dest, tag, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_bsend_f(char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *dest, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);

    c_comm = MPI_Comm_f2c (*comm);
  
    *ierr = OMPI_INT_2_FINT(MPI_Bsend(buf, OMPI_FINT_2_INT(*count),
				      c_type, OMPI_FINT_2_INT(*dest),
				      OMPI_FINT_2_INT(*tag), c_comm));
}

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_SCATTERV = mpi_scatterv_f
#pragma weak pmpi_scatterv = mpi_scatterv_f
#pragma weak pmpi_scatterv_ = mpi_scatterv_f
#pragma weak pmpi_scatterv__ = mpi_scatterv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_SCATTERV,
                           pmpi_scatterv,
                           pmpi_scatterv_,
                           pmpi_scatterv__,
                           pmpi_scatterv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SCATTERV = mpi_scatterv_f
#pragma weak mpi_scatterv = mpi_scatterv_f
#pragma weak mpi_scatterv_ = mpi_scatterv_f
#pragma weak mpi_scatterv__ = mpi_scatterv_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_SCATTERV,
                           mpi_scatterv,
                           mpi_scatterv_,
                           mpi_scatterv__,
                           mpi_scatterv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_scatterv_f(char *sendbuf, MPI_Fint *sendcounts,
		    MPI_Fint *displs, MPI_Fint *sendtype,
		    char *recvbuf, MPI_Fint *recvcount, 
		    MPI_Fint *recvtype, MPI_Fint *root,
		    MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    int size;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    *ierr = OMPI_INT_2_FINT(MPI_Scatterv(sendbuf, 
					 OMPI_ARRAY_NAME_CONVERT(sendcounts),
					 OMPI_ARRAY_NAME_CONVERT(displs),
					 c_sendtype, recvbuf,
					 OMPI_FINT_2_INT(*recvcount),
					 c_recvtype, 
					 OMPI_FINT_2_INT(*root), c_comm));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}

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
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mpiext/pcollreq/mpif-h/mpiext_pcollreq_prototypes.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPIX_SCATTER_INIT = ompix_scatter_init_f
#pragma weak pmpix_scatter_init = ompix_scatter_init_f
#pragma weak pmpix_scatter_init_ = ompix_scatter_init_f
#pragma weak pmpix_scatter_init__ = ompix_scatter_init_f

#pragma weak PMPIX_Scatter_init_f = ompix_scatter_init_f
#pragma weak PMPIX_Scatter_init_f08 = ompix_scatter_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPIX_SCATTER_INIT,
                            pmpix_scatter_init,
                            pmpix_scatter_init_,
                            pmpix_scatter_init__,
                            pompix_scatter_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_SCATTER_INIT = ompix_scatter_init_f
#pragma weak mpix_scatter_init = ompix_scatter_init_f
#pragma weak mpix_scatter_init_ = ompix_scatter_init_f
#pragma weak mpix_scatter_init__ = ompix_scatter_init_f

#pragma weak MPIX_Scatter_init_f = ompix_scatter_init_f
#pragma weak MPIX_Scatter_init_f08 = ompix_scatter_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPIX_SCATTER_INIT,
                            mpix_scatter_init,
                            mpix_scatter_init_,
                            mpix_scatter_init__,
                            ompix_scatter_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr) )
#else
#define ompix_scatter_init_f pompix_scatter_init_f
#endif
#endif


void ompix_scatter_init_f(char *sendbuf, MPI_Fint *sendcount,
                          MPI_Fint *sendtype, char *recvbuf,
                          MPI_Fint *recvcount, MPI_Fint *recvtype,
                          MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request,
                          MPI_Fint *ierr)
{
    int c_root, c_ierr;
    MPI_Datatype c_sendtype = NULL, c_recvtype = NULL;
    MPI_Info c_info;
    MPI_Request c_request;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);

    c_root = OMPI_FINT_2_INT(*root);
    c_info = PMPI_Info_f2c(*info);
    if (OMPI_COMM_IS_INTER(c_comm)) {
        if (MPI_ROOT == c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
        } else if (MPI_PROC_NULL != c_root) {
            c_recvtype = PMPI_Type_f2c(*recvtype);
        }
    } else {
        if (OMPI_IS_FORTRAN_IN_PLACE(recvbuf)) {
            recvbuf = MPI_IN_PLACE;
        } else {
            c_recvtype = PMPI_Type_f2c(*recvtype);
        }
        if (ompi_comm_rank(c_comm) == c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
        }
    }

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPIX_Scatter_init(sendbuf,OMPI_FINT_2_INT(*sendcount),
                                c_sendtype, recvbuf,
                                OMPI_FINT_2_INT(*recvcount),
                                c_recvtype,
                                OMPI_FINT_2_INT(*root), c_comm, c_info, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_request);
}

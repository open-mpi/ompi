/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2021 Research Organization for Information Science
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
#include "ompi/mca/coll/base/coll_base_util.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_REDUCE_SCATTER_INIT = ompi_reduce_scatter_init_f
#pragma weak pmpi_reduce_scatter_init = ompi_reduce_scatter_init_f
#pragma weak pmpi_reduce_scatter_init_ = ompi_reduce_scatter_init_f
#pragma weak pmpi_reduce_scatter_init__ = ompi_reduce_scatter_init_f

#pragma weak PMPI_Reduce_scatter_init_f = ompi_reduce_scatter_init_f
#pragma weak PMPI_Reduce_scatter_init_f08 = ompi_reduce_scatter_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_REDUCE_SCATTER_INIT,
                            pmpi_reduce_scatter_init,
                            pmpi_reduce_scatter_init_,
                            pmpi_reduce_scatter_init__,
                            pompi_reduce_scatter_init_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, recvcounts, datatype, op, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REDUCE_SCATTER_INIT = ompi_reduce_scatter_init_f
#pragma weak mpi_reduce_scatter_init = ompi_reduce_scatter_init_f
#pragma weak mpi_reduce_scatter_init_ = ompi_reduce_scatter_init_f
#pragma weak mpi_reduce_scatter_init__ = ompi_reduce_scatter_init_f

#pragma weak MPI_Reduce_scatter_init_f = ompi_reduce_scatter_init_f
#pragma weak MPI_Reduce_scatter_init_f08 = ompi_reduce_scatter_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_REDUCE_SCATTER_INIT,
                            mpi_reduce_scatter_init,
                            mpi_reduce_scatter_init_,
                            mpi_reduce_scatter_init__,
                            ompi_reduce_scatter_init_f,
                            (char *sendbuf, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, recvbuf, recvcounts, datatype, op, comm, info, request, ierr) )
#else
#define ompi_reduce_scatter_init_f pompi_reduce_scatter_init_f
#endif
#endif


void ompi_reduce_scatter_init_f(char *sendbuf, char *recvbuf,
                                MPI_Fint *recvcounts, MPI_Fint *datatype,
                                MPI_Fint *op, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request,
                                MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_type;
    MPI_Info c_info;
    MPI_Request c_request;
    MPI_Op c_op;
    int size;
    OMPI_ARRAY_NAME_DECL(recvcounts);

    c_comm = PMPI_Comm_f2c(*comm);
    c_type = PMPI_Type_f2c(*datatype);
    c_op = PMPI_Op_f2c(*op);
    c_info = PMPI_Info_f2c(*info);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Reduce_scatter_init(sendbuf, recvbuf,
                                      OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                      c_type, c_op, c_comm, c_info, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) {
        *request = PMPI_Request_c2f(c_request);
        ompi_coll_base_nbc_request_t* nb_request = (ompi_coll_base_nbc_request_t*)c_request;
        nb_request->data.release_arrays[0] = OMPI_ARRAY_NAME_CONVERT(recvcounts);
        nb_request->data.release_arrays[1] = NULL;
    } else {
        OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    }
}

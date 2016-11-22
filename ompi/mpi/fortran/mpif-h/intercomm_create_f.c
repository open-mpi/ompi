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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_INTERCOMM_CREATE = ompi_intercomm_create_f
#pragma weak pmpi_intercomm_create = ompi_intercomm_create_f
#pragma weak pmpi_intercomm_create_ = ompi_intercomm_create_f
#pragma weak pmpi_intercomm_create__ = ompi_intercomm_create_f

#pragma weak PMPI_Intercomm_create_f = ompi_intercomm_create_f
#pragma weak PMPI_Intercomm_create_f08 = ompi_intercomm_create_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INTERCOMM_CREATE,
                           pmpi_intercomm_create,
                           pmpi_intercomm_create_,
                           pmpi_intercomm_create__,
                           pompi_intercomm_create_f,
                           (MPI_Fint *local_comm, MPI_Fint *local_leader, MPI_Fint *bridge_comm, MPI_Fint *remote_leader, MPI_Fint *tag, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (local_comm, local_leader, bridge_comm, remote_leader, tag, newintercomm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INTERCOMM_CREATE = ompi_intercomm_create_f
#pragma weak mpi_intercomm_create = ompi_intercomm_create_f
#pragma weak mpi_intercomm_create_ = ompi_intercomm_create_f
#pragma weak mpi_intercomm_create__ = ompi_intercomm_create_f

#pragma weak MPI_Intercomm_create_f = ompi_intercomm_create_f
#pragma weak MPI_Intercomm_create_f08 = ompi_intercomm_create_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INTERCOMM_CREATE,
                           mpi_intercomm_create,
                           mpi_intercomm_create_,
                           mpi_intercomm_create__,
                           ompi_intercomm_create_f,
                           (MPI_Fint *local_comm, MPI_Fint *local_leader, MPI_Fint *bridge_comm, MPI_Fint *remote_leader, MPI_Fint *tag, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (local_comm, local_leader, bridge_comm, remote_leader, tag, newintercomm, ierr) )
#else
#define ompi_intercomm_create_f pompi_intercomm_create_f
#endif
#endif


void ompi_intercomm_create_f(MPI_Fint *local_comm, MPI_Fint *local_leader,
			    MPI_Fint *bridge_comm,
                            MPI_Fint *remote_leader, MPI_Fint *tag,
			    MPI_Fint *newintercomm,
                            MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_newcomm;
    MPI_Comm c_local_comm = PMPI_Comm_f2c (*local_comm );
    MPI_Comm c_bridge_comm = PMPI_Comm_f2c (*bridge_comm);

    c_ierr = PMPI_Intercomm_create(c_local_comm,
                                  OMPI_FINT_2_INT(*local_leader),
                                  c_bridge_comm,
                                  OMPI_FINT_2_INT(*remote_leader),
                                  OMPI_FINT_2_INT(*tag),
                                  &c_newcomm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newintercomm = PMPI_Comm_c2f (c_newcomm);
    }
}

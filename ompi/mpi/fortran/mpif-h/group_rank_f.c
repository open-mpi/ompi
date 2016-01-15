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
#include "ompi/group/group.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_GROUP_RANK = ompi_group_rank_f
#pragma weak pmpi_group_rank = ompi_group_rank_f
#pragma weak pmpi_group_rank_ = ompi_group_rank_f
#pragma weak pmpi_group_rank__ = ompi_group_rank_f

#pragma weak PMPI_Group_rank_f = ompi_group_rank_f
#pragma weak PMPI_Group_rank_f08 = ompi_group_rank_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_RANK,
                           pmpi_group_rank,
                           pmpi_group_rank_,
                           pmpi_group_rank__,
                           pompi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_RANK = ompi_group_rank_f
#pragma weak mpi_group_rank = ompi_group_rank_f
#pragma weak mpi_group_rank_ = ompi_group_rank_f
#pragma weak mpi_group_rank__ = ompi_group_rank_f

#pragma weak MPI_Group_rank_f = ompi_group_rank_f
#pragma weak MPI_Group_rank_f08 = ompi_group_rank_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_RANK,
                           mpi_group_rank,
                           mpi_group_rank_,
                           mpi_group_rank__,
                           ompi_group_rank_f,
                           (MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr),
                           (group, rank, ierr) )
#else
#define ompi_group_rank_f pompi_group_rank_f
#endif
#endif


void ompi_group_rank_f(MPI_Fint *group, MPI_Fint *rank, MPI_Fint *ierr)
{
  int c_ierr;
  ompi_group_t *c_group;
  OMPI_SINGLE_NAME_DECL(rank);

  /* Make the fortran to c representation conversion */
  c_group = PMPI_Group_f2c(*group);

  c_ierr = PMPI_Group_rank(c_group, OMPI_SINGLE_NAME_CONVERT(rank));
  if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

  if (MPI_SUCCESS == c_ierr) {
      OMPI_SINGLE_INT_2_FINT(rank);
  }
}

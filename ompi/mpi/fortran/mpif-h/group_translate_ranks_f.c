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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/group/group.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GROUP_TRANSLATE_RANKS = ompi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks = ompi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks_ = ompi_group_translate_ranks_f
#pragma weak pmpi_group_translate_ranks__ = ompi_group_translate_ranks_f

#pragma weak PMPI_Group_translate_ranks_f = ompi_group_translate_ranks_f
#pragma weak PMPI_Group_translate_ranks_f08 = ompi_group_translate_ranks_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GROUP_TRANSLATE_RANKS,
                           pmpi_group_translate_ranks,
                           pmpi_group_translate_ranks_,
                           pmpi_group_translate_ranks__,
                           pompi_group_translate_ranks_f,
                           (MPI_Fint *group1, MPI_Fint *n, MPI_Fint *ranks1, MPI_Fint *group2, MPI_Fint *ranks2, MPI_Fint *ierr),
                           (group1, n, ranks1, group2, ranks2, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GROUP_TRANSLATE_RANKS = ompi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks = ompi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks_ = ompi_group_translate_ranks_f
#pragma weak mpi_group_translate_ranks__ = ompi_group_translate_ranks_f

#pragma weak MPI_Group_translate_ranks_f = ompi_group_translate_ranks_f
#pragma weak MPI_Group_translate_ranks_f08 = ompi_group_translate_ranks_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GROUP_TRANSLATE_RANKS,
                           mpi_group_translate_ranks,
                           mpi_group_translate_ranks_,
                           mpi_group_translate_ranks__,
                           ompi_group_translate_ranks_f,
                           (MPI_Fint *group1, MPI_Fint *n, MPI_Fint *ranks1, MPI_Fint *group2, MPI_Fint *ranks2, MPI_Fint *ierr),
                           (group1, n, ranks1, group2, ranks2, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_group_translate_ranks_f(MPI_Fint *group1, MPI_Fint *n, 
				  MPI_Fint *ranks1, MPI_Fint *group2,
				  MPI_Fint *ranks2, MPI_Fint *ierr)
{
  int c_ierr;
  ompi_group_t *c_group1, *c_group2;
  OMPI_ARRAY_NAME_DECL(ranks1);
  OMPI_ARRAY_NAME_DECL(ranks2);

  /* Make the fortran to c representation conversion */
  c_group1 = MPI_Group_f2c(*group1);
  c_group2 = MPI_Group_f2c(*group2);

  OMPI_ARRAY_FINT_2_INT(ranks1, *n);
  OMPI_ARRAY_FINT_2_INT_ALLOC(ranks2, *n);

  c_ierr = MPI_Group_translate_ranks(c_group1, 
                                     OMPI_FINT_2_INT(*n),
                                     OMPI_ARRAY_NAME_CONVERT(ranks1), 
                                     c_group2, 
                                     OMPI_ARRAY_NAME_CONVERT(ranks2)
                                     );
  if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

  if (MPI_SUCCESS == c_ierr) {
      OMPI_ARRAY_INT_2_FINT(ranks2, *n);
  } else {
      OMPI_ARRAY_FINT_2_INT_CLEANUP(ranks2);
  }
  OMPI_ARRAY_FINT_2_INT_CLEANUP(ranks1);
}

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

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_REGISTER_DATAREP = mpi_register_datarep_f
#pragma weak pmpi_register_datarep = mpi_register_datarep_f
#pragma weak pmpi_register_datarep_ = mpi_register_datarep_f
#pragma weak pmpi_register_datarep__ = mpi_register_datarep_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REGISTER_DATAREP,
                           pmpi_register_datarep,
                           pmpi_register_datarep_,
                           pmpi_register_datarep__,
                           pmpi_register_datarep_f,
                           (char *datarep, void *read_conversion_fn, void *write_conversion_fn, void *dtype_file_extent_fn, char *extra_state, MPI_Fint *ierr),
                           (datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REGISTER_DATAREP = mpi_register_datarep_f
#pragma weak mpi_register_datarep = mpi_register_datarep_f
#pragma weak mpi_register_datarep_ = mpi_register_datarep_f
#pragma weak mpi_register_datarep__ = mpi_register_datarep_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REGISTER_DATAREP,
                           mpi_register_datarep,
                           mpi_register_datarep_,
                           mpi_register_datarep__,
                           mpi_register_datarep_f,
                           (char *datarep, void *read_conversion_fn, void *write_conversion_fn, void *dtype_file_extent_fn, char *extra_state, MPI_Fint *ierr),
                           (datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_register_datarep_f(char *datarep, void *read_conversion_fn,
			    void *write_conversion_fn,
			    void *dtype_file_extent_fn, char *extra_state,
			    MPI_Fint *ierr)
{
    *ierr = OMPI_INT_2_FINT(MPI_Register_datarep(datarep,
		 (MPI_Datarep_conversion_function *)*read_conversion_fn,
		 (MPI_Datarep_conversion_function *)*write_conversion_fn, 
		 (MPI_Datarep_extent_function *)*dtype_file_extent_fn, 
		 extra_state));
}

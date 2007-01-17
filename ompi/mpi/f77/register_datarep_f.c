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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/strings.h"
#include "ompi/file/file.h"

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
                           (char *datarep, void *read_conversion_fn, void *write_conversion_fn, void *dtype_file_extent_fn, MPI_Aint *extra_state, MPI_Fint *ierr, int datarep_len),
                           (datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierr, datarep_len) )
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
                           (char *datarep, void *read_conversion_fn, void *write_conversion_fn, void *dtype_file_extent_fn, MPI_Aint *extra_state, MPI_Fint *ierr, int datarep_len),
                           (datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierr, datarep_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

union local_type_convert {
    void *voidp;
    MPI_Datarep_conversion_function *convertp;
    MPI_Datarep_extent_function *extentp;
};

void mpi_register_datarep_f(char *datarep, void *read_conversion_fn,
			    void *write_conversion_fn,
			    void *dtype_file_extent_fn, MPI_Aint *extra_state,
			    MPI_Fint *ierr, int datarep_len)
{
   char *c_datarep;
   int c_err, ret;
   union local_type_convert a, b, c;

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(datarep, datarep_len,
                                                       &c_datarep))) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, ret, "MPI_FILE_SET_VIEW");
        *ierr = OMPI_INT_2_FINT(c_err);
        return;
    }

    /* Do a little type shifting so that we don't get compiler
       warnings */
    a.voidp = read_conversion_fn;
    b.voidp = write_conversion_fn;
    c.voidp = dtype_file_extent_fn;

    *ierr = OMPI_INT_2_FINT(MPI_Register_datarep(c_datarep, a.convertp, 
                                                 b.convertp, c.extentp,
                                                 extra_state));

    free(c_datarep);
}

/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpi/fortran/base/strings.h"
#include "opal/util/argv.h"


#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_COMM_SPAWN_MULTIPLE = ompi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple = ompi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple_ = ompi_comm_spawn_multiple_f
#pragma weak pmpi_comm_spawn_multiple__ = ompi_comm_spawn_multiple_f

#pragma weak PMPI_Comm_spawn_multiple_f = ompi_comm_spawn_multiple_f
#pragma weak PMPI_Comm_spawn_multiple_f08 = ompi_comm_spawn_multiple_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SPAWN_MULTIPLE,
                           pmpi_comm_spawn_multiple,
                           pmpi_comm_spawn_multiple_,
                           pmpi_comm_spawn_multiple__,
                           pompi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr, int cmd_string_len, int argv_string_len),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr, cmd_string_len, argv_string_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SPAWN_MULTIPLE = ompi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple = ompi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple_ = ompi_comm_spawn_multiple_f
#pragma weak mpi_comm_spawn_multiple__ = ompi_comm_spawn_multiple_f

#pragma weak MPI_Comm_spawn_multiple_f = ompi_comm_spawn_multiple_f
#pragma weak MPI_Comm_spawn_multiple_f08 = ompi_comm_spawn_multiple_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SPAWN_MULTIPLE,
                           mpi_comm_spawn_multiple,
                           mpi_comm_spawn_multiple_,
                           mpi_comm_spawn_multiple__,
                           ompi_comm_spawn_multiple_f,
                           (MPI_Fint *count, char *array_of_commands, char *array_of_argv, MPI_Fint *array_of_maxprocs, MPI_Fint *array_of_info, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *intercomm, MPI_Fint *array_of_errcodes, MPI_Fint *ierr, int cmd_string_len, int argv_string_len),
                           (count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, root, comm, intercomm, array_of_errcodes, ierr, cmd_string_len, argv_string_len) )
#else
#define ompi_comm_spawn_multiple_f pompi_comm_spawn_multiple_f
#endif
#endif


void ompi_comm_spawn_multiple_f(MPI_Fint *count, char *array_commands,
			       char *array_argv,
			       MPI_Fint *array_maxprocs,
			       MPI_Fint *array_info, MPI_Fint *root,
			       MPI_Fint *comm, MPI_Fint *intercomm,
			       MPI_Fint *array_errcds, MPI_Fint *ierr,
			       int cmd_string_len, int argv_string_len)
{
    MPI_Comm c_comm, c_new_comm;
    MPI_Info *c_info;
    int size, array_size, i, c_ierr;
    int *c_errs;
    char **c_array_commands;
    char ***c_array_argv;
    OMPI_ARRAY_NAME_DECL(array_maxprocs);
    OMPI_ARRAY_NAME_DECL(array_errcds);

    c_comm = PMPI_Comm_f2c(*comm);

    PMPI_Comm_size(c_comm, &size);

    array_size = OMPI_FINT_2_INT(*count);

    /* It's allowed to ignore the errcodes */

    if (OMPI_IS_FORTRAN_ERRCODES_IGNORE(array_errcds)) {
        c_errs = MPI_ERRCODES_IGNORE;
    } else {
        OMPI_ARRAY_FINT_2_INT_ALLOC(array_errcds, size);
        c_errs = OMPI_ARRAY_NAME_CONVERT(array_errcds);
    }

    /* It's allowed to have no argv */

    if (OMPI_IS_FORTRAN_ARGVS_NULL(array_argv)) {
        c_array_argv = MPI_ARGVS_NULL;
    } else {
	ompi_fortran_multiple_argvs_f2c(OMPI_FINT_2_INT(*count), array_argv,
					argv_string_len, &c_array_argv);
    }

    OMPI_ARRAY_FINT_2_INT(array_maxprocs, array_size);

    ompi_fortran_argv_f2c(array_commands, cmd_string_len,
                          cmd_string_len, &c_array_commands);

    c_info = (MPI_Info *) malloc (array_size * sizeof(MPI_Info));
    for (i = 0; i < array_size; ++i) {
	c_info[i] = PMPI_Info_f2c(array_info[i]);
    }

    c_ierr = PMPI_Comm_spawn_multiple(OMPI_FINT_2_INT(*count),
                                     c_array_commands,
                                     c_array_argv,
                                     OMPI_ARRAY_NAME_CONVERT(array_maxprocs),
                                     c_info,
                                     OMPI_FINT_2_INT(*root),
                                     c_comm, &c_new_comm,
                                     c_errs);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *intercomm = PMPI_Comm_c2f(c_new_comm);
    }

    if (!OMPI_IS_FORTRAN_ERRCODES_IGNORE(array_errcds)) {
	OMPI_ARRAY_INT_2_FINT(array_errcds, size);
    } else {
	OMPI_ARRAY_FINT_2_INT_CLEANUP(array_errcds);
    }
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_maxprocs);

    opal_argv_free(c_array_commands);

    if (MPI_ARGVS_NULL != c_array_argv && NULL != c_array_argv) {
	for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
	    opal_argv_free(c_array_argv[i]);
	}
    }
    free(c_array_argv);
    free(c_info);
}

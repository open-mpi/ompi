/*
 * $HEADER$
 */

#ifndef LAM_F77_DEFINES_H
#define LAM_F77_DEFINES_H

/*
 * This file makes a mapping from mpi_foo_f to the proper fortran
 * external symbol convention via 4 sets of #define's.
 *
 * This file is never included at the same time as
 * mpi/f77/profile/prototypes.h (some of the #define's conflict).
 */

/*
 * First, all caps.
 */
#if LAM_F77_CAPS
#define mpi_comm_set_name_f MPI_COMM_SET_NAME
#define mpi_init_f MPI_INIT
#define mpi_finalize_f MPI_FINALIZE

/*
 * Second, all lower case.
 */
#elif LAM_F77_PLAIN
#define mpi_comm_set_name_f mpi_comm_set_name
#define mpi_init_f mpi_init
#define mpi_finalize_f mpi_finalize

/*
 * Third, one trailing underscore.
 */
#elif LAM_F77_SINGLE_UNDERSCORE
#define mpi_comm_set_name_f mpi_comm_set_name_
#define mpi_init_f mpi_init_
#define mpi_finalize_f mpi_finalize_

/*
 * Fourth, two trailing underscores.
 */
#elif LAM_F77_DOUBLE_UNDERSCORE
#define mpi_comm_set_name_f mpi_comm_set_name__
#define mpi_init_f mpi_init__
#define mpi_finalize_f mpi_finalize__

#endif

#endif /* LAM_F77_DEFINES_H */

/*
 * $HEADER$
 */

#ifndef LAM_F77_PROTOTYPES_PROFILING_H
#define LAM_F77_PROTOTYPES_PROFILING_H

/*
 * This file prototypes all profiled MPI fortran functions in all four
 * fortran symbol conventions.  It is only included when we need the
 * profiling prototypes.
 *
 * Save some coding (i.e., don't re-specify all the return types and
 * function arguments) -- make #define's to make from mpi->pmpi and
 * then include mpi/f77/prototypes.h.
 *
 * This file is never included at the same time as
 * mpi/f77/defines.h (some of the #define's conflict).
 */

/*
 * First, all caps.
 */
#define MPI_COMM_SET_NAME PMPI_COMM_SET_NAME
#define MPI_INIT PMPI_INIT
#define MPI_FINALIZE PMPI_FINALIZE

/*
 * Second, all lower case.
 */
#define mpi_comm_set_name pmpi_comm_set_name
#define mpi_init pmpi_init
#define mpi_finalize pmpi_finalize

/*
 * Third, one trailing underscore.
 */
#define mpi_comm_set_name_ pmpi_comm_set_name_
#define mpi_init_ pmpi_init_
#define mpi_finalize_ pmpi_finalize_

/*
 * Fourth, two trailing underscores.
 */
#define mpi_comm_set_name__ pmpi_comm_set_name__
#define mpi_init__ pmpi_init__
#define mpi_finalize__ pmpi_finalize__

/*
 * Now include the real prototypes.  Magic.  :-)
 */
#include "mpi/f77/prototypes.h"

#endif /* LAM_F77_PROTOTYPES_PROFILING_H */

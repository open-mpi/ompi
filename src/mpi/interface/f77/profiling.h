/*
 * $HEADER$
 */

#ifndef LAM_MPI_F77_PROFILING_H
#define LAM_MPI_F77_PROFILING_H

#include "lam_config.h"

/* Unless explicitly asked for (i.e., when we're compiling in
   src/mpi/<lang>/profile, don't automatically convert MPI_Foo ->
   PMPI_Foo via #defines. */

#ifndef LAM_PROFILING_DEFINES
#define LAM_PROFILING_DEFINES 0
#endif

/* If requested, add a #define for each MPI function to convert it to
   it's PMPI equivalent. */

#if LAM_PROFILING_DEFINES
#if LAM_FORTRAN_CAPS
#define mpi_comm_set_name_f MPI_COMM_SET_NAME
#elif LAM_FORTRAN_NO_UNDERSCORE
#define mpi_comm_set_name_f mpi_comm_set_name
#elif LAM_FORTRAN_SINGLE_UNDERSCORE
#define mpi_comm_set_name_f mpi_comm_set_name_
#elif LAM_FORTRAN_DOUBLE_UNDERSCORE
#define mpi_comm_set_name_f mpi_comm_set_name__
#else
#error I do not know how your fortran compiler exports symbols.  :-(
#endif

#endif /* LAM_MPI_F77_PROFILING_H */

/*
 * $HEADER$
 */

#ifndef LAM_MPI_C_PROFILING_H
#define LAM_MPI_C_PROFILING_H

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
#define MPI_Comm_set_name PMPI_Comm_set_name
#endif

#endif /* LAM_MPI_C_PROFILING_H */

/*
 * $HEADER$
 */

#ifndef LAM_C_PROFILE_DEFINES_H
#define LAM_C_PROFILE_DEFINES_H

/*
 * This file contains the #defines for the profiled versions of all
 * the MPI C bindings.  It is only included if we do not have weak
 * symbols, or weak symbol support was explicitly disabled.
 */

#if LAM_PROFILING_DEFINES
#define MPI_Comm_set_name PMPI_Comm_set_name
#define MPI_Init PMPI_Init
#define MPI_Finalize PMPI_Finalize
#endif

#endif /* LAM_C_PROFILE_DEFINES_H */

/*
 * $HEADER$
 */

#ifndef LAM_C_PROFILE_DEFINES_H
#define LAM_C_PROFILE_DEFINES_H
/*
 * This file is included in the top directory only if 
 * profiling is required. Once profiling is required,
 * this file will replace all MPI_* symbols with 
 * PMPI_* symbols
 */

#define MPI_Alloc_mem PMPI_Alloc_mem
#define MPI_Comm_get_name PMPI_Comm_get_name
#define MPI_Comm_set_name PMPI_Comm_set_name
#define MPI_Finalize PMPI_Finalize
#define MPI_Free_mem PMPI_Free_mem
#define MPI_Init PMPI_Init

#endif /* LAM_C_PROFILE_DEFINES_H */

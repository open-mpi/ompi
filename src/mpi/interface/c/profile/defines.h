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

#endif /* LAM_C_PROFILE_DEFINES_H */

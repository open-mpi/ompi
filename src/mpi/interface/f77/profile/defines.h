/*
 * $HEADER$
 */

#ifndef LAM_F77_PROFILE_DEFINES_H
#define LAM_F77_PROFILE_DEFINES_H

/*
 * This file makes a mapping from mpi_foo_f to the profiled version of
 * the proper fortran external symbol convention via 4 sets of
 * #define's.
 */

/*
 * First, all caps.
 */
#if LAM_F77_CAPS
#define mpi_comm_set_name_f PMPI_COMM_SET_NAME
#define mpi_comm_get_name_f PMPI_COMM_GET_NAME
#define mpi_init_f PMPI_INIT
#define mpi_finalize_f PMPI_FINALIZE
#define mpi_alloc_mem_f PMPI_ALLOC_MEM
#define mpi_free_mem_f PMPI_FREE_MEM

/*
 * Second, all lower case.
 */
#elif LAM_F77_PLAIN
#define mpi_comm_set_name_f pmpi_comm_set_name
#define mpi_comm_get_name_f pmpi_comm_get_name
#define mpi_init_f pmpi_init
#define mpi_finalize_f pmpi_finalize
#define mpi_alloc_mem_f pmpi_alloc_mem
#define mpi_free_mem_f pmpi_free_mem

/*
 * Third, one trailing underscore.
 */
#elif LAM_F77_SINGLE_UNDERSCORE
#define mpi_comm_set_name_f pmpi_comm_set_name_
#define mpi_comm_get_name_f pmpi_comm_get_name_
#define mpi_init_f pmpi_init_
#define mpi_finalize_f pmpi_finalize_
#define mpi_alloc_mem_f pmpi_alloc_mem_
#define mpi_free_mem_f pmpi_free_mem_

/*
 * Fourth, two trailing underscores.
 */
#elif LAM_F77_DOUBLE_UNDERSCORE
#define mpi_comm_set_name_f pmpi_comm_set_name__
#define mpi_comm_get_name_f pmpi_comm_get_name__
#define mpi_init_f pmpi_init__
#define mpi_finalize_f pmpi_finalize__
#define mpi_alloc_mem_f pmpi_alloc_mem__
#define mpi_free_mem_f pmpi_free_mem__

#endif

#endif /* LAM_F77_PROFILE_DEFINES_H */

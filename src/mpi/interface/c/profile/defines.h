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
#define MPI_Abort PMPI_Abort
#define MPI_Accumulate PMPI_Accumulate
#define MPI_Add_error_class PMPI_Add_error_class
#define MPI_Add_error_code PMPI_Add_error_code
#define MPI_Add_error_string PMPI_Add_error_string
#define MPI_Address PMPI_Address
#define MPI_Allgather PMPI_Allgather
#define MPI_Allgatherv PMPI_Allgatherv
#define MPI_Allgatherw PMPI_Allgatherw
#define MPI_Alloc_mem PMPI_Alloc_mem
#define MPI_Allreduce PMPI_Allreduce
#define MPI_Alltoall PMPI_Alltoall
#define MPI_Alltoallv PMPI_Alltoallv
#define MPI_Alltoallw PMPI_Alltoallw
#define MPI_Attr_delete PMPI_Attr_delete
#define MPI_Attr_get PMPI_Attr_get
#define MPI_Attr_put PMPI_Attr_put
#define MPI_Barrier PMPI_Barrier
#define MPI_Bcast PMPI_Bcast
#define MPI_Bsend_init PMPI_Bsend_init
#define MPI_Bsend PMPI_Bsend
#define MPI_Buffer_attach PMPI_Buffer_attach
#define MPI_Buffer_detach PMPI_Buffer_detach
#define MPI_Comm_get_name PMPI_Comm_get_name
#define MPI_Comm_set_name PMPI_Comm_set_name
#define MPI_Finalize PMPI_Finalize
#define MPI_Free_mem PMPI_Free_mem
#define MPI_Init PMPI_Init

#endif /* LAM_C_PROFILE_DEFINES_H */

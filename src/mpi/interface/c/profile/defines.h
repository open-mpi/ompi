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
#define MPI_Cancel PMPI_Cancel
#define MPI_Cart_coords PMPI_Cart_coords
#define MPI_Cart_create PMPI_Cart_create
#define MPI_Cart_get PMPI_Cart_get
#define MPI_Cart_map PMPI_Cart_map
#define MPI_Cart_rank PMPI_Cart_rank
#define MPI_Cart_shift PMPI_Cart_shift
#define MPI_Cart_sub PMPI_Cart_sub
#define MPI_Cartdim_get PMPI_Cartdim_get
#define MPI_Close_port PMPI_Close_port
#define MPI_Comm_accept PMPI_Comm_accept
#define MPI_Comm_c2f PMPI_Comm_c2f
#define MPI_Comm_call_errhandler PMPI_Comm_call_errhandler
#define MPI_Comm_compare PMPI_Comm_compare
#define MPI_Comm_connect PMPI_Comm_connect
#define MPI_Comm_create_errhandler PMPI_Comm_create_errhandler
#define MPI_Comm_create_keyval PMPI_Comm_create_keyval
#define MPI_Comm_create PMPI_Comm_create
#define MPI_Comm_delete_attr PMPI_Comm_delete_attr
#define MPI_Comm_disconnect PMPI_Comm_disconnect
#define MPI_Comm_dup PMPI_Comm_dup
#define MPI_Comm_f2c PMPI_Comm_f2c
#define MPI_Comm_free_keyval PMPI_Comm_free_keyval
#define MPI_Comm_free PMPI_Comm_free
#define MPI_Comm_get_attr PMPI_Comm_get_attr
#define MPI_Comm_get_errhandler PMPI_Comm_get_errhandler
#define MPI_Comm_get_name PMPI_Comm_get_name
#define MPI_Comm_get_parent PMPI_Comm_get_parent
#define MPI_Comm_group PMPI_Comm_group
#define MPI_Comm_join PMPI_Comm_join
#define MPI_Comm_rank PMPI_Comm_rank
#define MPI_Comm_remote_group PMPI_Comm_remote_group
#define MPI_Comm_remote_size PMPI_Comm_remote_size
#define MPI_Comm_set_attr PMPI_Comm_set_attr
#define MPI_Comm_set_errhandler PMPI_Comm_set_errhandler
#define MPI_Comm_set_name PMPI_Comm_set_name
#define MPI_Comm_size PMPI_Comm_size
#define MPI_Comm_spawn PMPI_Comm_spawn
#define MPI_Comm_spawn_multiple PMPI_Comm_spawn_multiple
#define MPI_Comm_split PMPI_Comm_split
#define MPI_Comm_test_inter PMPI_Comm_test_inter
#define MPI_Finalize PMPI_Finalize
#define MPI_Free_mem PMPI_Free_mem
#define MPI_Init PMPI_Init
#define MPI_Test_cancelled PMPI_Test_cancelled
#define MPI_Test PMPI_Test
#define MPI_Testall PMPI_Testall
#define MPI_Testany PMPI_Testany
#define MPI_Testsome PMPI_Testsome
#define MPI_Topo_test PMPI_Topo_test
#define MPI_Type_c2f PMPI_Type_c2f
#define MPI_Type_commit PMPI_Type_commit
#define MPI_Type_contiguous PMPI_Type_contiguous
#define MPI_Type_create_darray PMPI_Type_create_darray
#define MPI_Type_create_f90_complex PMPI_Type_create_complex
#define MPI_Type_create_f90_integer PMPI_Type_create_integer
#define MPI_Type_create_f90_real PMPI_Type_create_f90_real
#define MPI_Type_create_hindexed PMPI_Type_create_hindexed
#define MPI_Type_create_hvector PMPI_Type_create_hvector
#define MPI_Type_create_indexed_block PMPI_Type_create_indexed_block
#define MPI_Type_create_keyval PMPI_Type_create_keyval
#define MPI_Type_create_resized PMPI_create_resized
#define MPI_Type_create_struct PMPI_Type_create_struct
#define MPI_Type_create_subarray PMPI_Type_create_subarray
#define MPI_Type_delete_attr PMPI_Type_delete_attr
#define MPI_Type_dup PMPI_Type_dup
#define MPI_Type_extent PMPI_Type_extent
#define MPI_Type_f2c PMPI_Type_f2c
#define MPI_Type_free_keyval PMPI_Type_free_keyval
#define MPI_Type_free PMPI_Type_free
#define MPI_Type_get_attr PMPI_Type_get_attr
#define MPI_Type_get_contents PMPI_Type_get_contents
#define MPI_Type_get_envelope PMPI_Type_get_envelope
#define MPI_Type_get_extent PMPI_Type_get_extent
#define MPI_Type_get_name PMPI_Type_get_name
#define MPI_Type_get_true_extent PMPI_Type_get_true_extent
#define MPI_Type_hindexed PMPI_Type_hindexed
#define MPI_Type_hvector PMPI_Type_hvector
#define MPI_Type_indexed PMPI_Type_indexed
#define MPI_Type_lb PMPI_Type_lb
#define MPI_Type_match_size PMPI_Type_match_size
#define MPI_Type_set_attr PMPI_Type_set_attr
#define MPI_Type_set_name PMPI_Type_set_name
#define MPI_Type_size PMPI_Type_size
#define MPI_Type_struct PMPI_Type_struct
#define MPI_Type_ub PMPI_Type_ub
#define MPI_Type_vector PMPI_Type_vector
// defines for type_* functions to be added

#endif /* LAM_C_PROFILE_DEFINES_H */

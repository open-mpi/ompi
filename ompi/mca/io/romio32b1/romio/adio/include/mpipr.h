/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
/* This file replaces all MPI function names with their PMPI equivalents.
   PMPI versions are used by default so that the user can profile 
   application code without interference from MPI functions used by 
   MPI-IO. */

#ifndef USE_MPI_VERSIONS

#undef MPI_Abort
#define MPI_Abort PMPI_Abort
#undef MPI_Address
#define MPI_Address PMPI_Address
#undef MPI_Allgather
#define MPI_Allgather PMPI_Allgather
#undef MPI_Allgatherv
#define MPI_Allgatherv PMPI_Allgatherv
#undef MPI_Allreduce
#define MPI_Allreduce PMPI_Allreduce
#undef MPI_Alltoall
#define MPI_Alltoall PMPI_Alltoall
#undef MPI_Alltoallv
#define MPI_Alltoallv PMPI_Alltoallv
#undef MPI_Alltoallw
#define MPI_Alltoallw PMPI_Alltoallw
#undef MPI_Attr_delete
#define MPI_Attr_delete PMPI_Attr_delete
#undef MPI_Attr_get
#define MPI_Attr_get PMPI_Attr_get
#undef MPI_Attr_put
#define MPI_Attr_put PMPI_Attr_put
#undef MPI_Barrier
#define MPI_Barrier PMPI_Barrier
#undef MPI_Bcast
#define MPI_Bcast PMPI_Bcast
#undef MPI_Bsend
#define MPI_Bsend PMPI_Bsend
#undef MPI_Bsend_init
#define MPI_Bsend_init PMPI_Bsend_init
#undef MPI_Buffer_attach
#define MPI_Buffer_attach PMPI_Buffer_attach
#undef MPI_Buffer_detach
#define MPI_Buffer_detach PMPI_Buffer_detach
#undef MPI_Cancel
#define MPI_Cancel PMPI_Cancel
#undef MPI_Cart_coords
#define MPI_Cart_coords PMPI_Cart_coords
#undef MPI_Cart_create
#define MPI_Cart_create PMPI_Cart_create
#undef MPI_Cart_get
#define MPI_Cart_get PMPI_Cart_get
#undef MPI_Cart_map
#define MPI_Cart_map PMPI_Cart_map
#undef MPI_Cart_rank
#define MPI_Cart_rank PMPI_Cart_rank
#undef MPI_Cart_shift
#define MPI_Cart_shift PMPI_Cart_shift
#undef MPI_Cart_sub
#define MPI_Cart_sub PMPI_Cart_sub
#undef MPI_Cartdim_get
#define MPI_Cartdim_get PMPI_Cartdim_get
#undef MPI_Comm_call_errhandler
#define MPI_Comm_call_errhandler PMPI_Comm_call_errhandler
#undef MPI_Comm_compare
#define MPI_Comm_compare PMPI_Comm_compare
#undef MPI_Comm_create
#define MPI_Comm_create PMPI_Comm_create
#undef MPI_Comm_dup
#define MPI_Comm_dup PMPI_Comm_dup
#undef MPI_Comm_free
#define MPI_Comm_free PMPI_Comm_free
#undef MPI_Comm_group
#define MPI_Comm_group PMPI_Comm_group
#undef MPI_Comm_rank
#define MPI_Comm_rank PMPI_Comm_rank
#undef MPI_Comm_remote_group
#define MPI_Comm_remote_group PMPI_Comm_remote_group
#undef MPI_Comm_remote_size
#define MPI_Comm_remote_size PMPI_Comm_remote_size
#undef MPI_Comm_size
#define MPI_Comm_size PMPI_Comm_size
#undef MPI_Comm_split
#define MPI_Comm_split PMPI_Comm_split
#undef MPI_Comm_test_inter
#define MPI_Comm_test_inter PMPI_Comm_test_inter
#undef MPI_Dims_create
#define MPI_Dims_create PMPI_Dims_create
#undef MPI_Errhandler_create
#define MPI_Errhandler_create PMPI_Errhandler_create
#undef MPI_Errhandler_free
#define MPI_Errhandler_free PMPI_Errhandler_free
#undef MPI_Errhandler_get
#define MPI_Errhandler_get PMPI_Errhandler_get
#undef MPI_Errhandler_set
#define MPI_Errhandler_set PMPI_Errhandler_set
#undef MPI_Error_class
#define MPI_Error_class PMPI_Error_class
#undef MPI_Error_string
#define MPI_Error_string PMPI_Error_string
#undef MPI_Finalize
#define MPI_Finalize PMPI_Finalize
#undef MPI_Gather
#define MPI_Gather PMPI_Gather
#undef MPI_Gatherv
#define MPI_Gatherv PMPI_Gatherv
#undef MPI_Get_count
#define MPI_Get_count PMPI_Get_count
#undef MPI_Get_elements
#define MPI_Get_elements PMPI_Get_elements
#undef MPI_Get_processor_name
#define MPI_Get_processor_name PMPI_Get_processor_name
#undef MPI_Get_version
#define MPI_Get_version PMPI_Get_version
#undef MPI_Graph_create
#define MPI_Graph_create PMPI_Graph_create
#undef MPI_Graph_get
#define MPI_Graph_get PMPI_Graph_get
#undef MPI_Graph_map
#define MPI_Graph_map PMPI_Graph_map
#undef MPI_Graph_neighbors
#define MPI_Graph_neighbors PMPI_Graph_neighbors
#undef MPI_Graph_neighbors_count
#define MPI_Graph_neighbors_count PMPI_Graph_neighbors_count
#undef MPI_Graphdims_get
#define MPI_Graphdims_get PMPI_Graphdims_get
#undef MPI_Group_compare
#define MPI_Group_compare PMPI_Group_compare
#undef MPI_Group_difference
#define MPI_Group_difference PMPI_Group_difference
#undef MPI_Group_excl
#define MPI_Group_excl PMPI_Group_excl
#undef MPI_Group_free
#define MPI_Group_free PMPI_Group_free
#undef MPI_Group_incl
#define MPI_Group_incl PMPI_Group_incl
#undef MPI_Group_intersection
#define MPI_Group_intersection PMPI_Group_intersection
#undef MPI_Group_range_excl
#define MPI_Group_range_excl PMPI_Group_range_excl
#undef MPI_Group_range_incl
#define MPI_Group_range_incl PMPI_Group_range_incl
#undef MPI_Group_rank
#define MPI_Group_rank PMPI_Group_rank
#undef MPI_Group_size
#define MPI_Group_size PMPI_Group_size
#undef MPI_Group_translate_ranks
#define MPI_Group_translate_ranks PMPI_Group_translate_ranks
#undef MPI_Group_union
#define MPI_Group_union PMPI_Group_union
#undef MPI_Ibsend
#define MPI_Ibsend PMPI_Ibsend
#undef MPI_Init
#define MPI_Init PMPI_Init
#undef MPI_Initialized
#define MPI_Initialized PMPI_Initialized
#undef MPI_Intercomm_create
#define MPI_Intercomm_create PMPI_Intercomm_create
#undef MPI_Intercomm_merge
#define MPI_Intercomm_merge PMPI_Intercomm_merge
#undef MPI_Iprobe
#define MPI_Iprobe PMPI_Iprobe
#undef MPI_Irecv
#define MPI_Irecv PMPI_Irecv
#undef MPI_Irsend
#define MPI_Irsend PMPI_Irsend
#undef MPI_Isend
#define MPI_Isend PMPI_Isend
#undef MPI_Issend
#define MPI_Issend PMPI_Issend
#undef MPI_Keyval_create
#define MPI_Keyval_create PMPI_Keyval_create
#undef MPI_Keyval_free
#define MPI_Keyval_free PMPI_Keyval_free
#undef MPI_Name_get
#define MPI_Name_get PMPI_Name_get
#undef MPI_Name_put
#define MPI_Name_put PMPI_Name_put
#undef MPI_Op_create
#define MPI_Op_create PMPI_Op_create
#undef MPI_Op_free
#define MPI_Op_free PMPI_Op_free
#undef MPI_Pack
#define MPI_Pack PMPI_Pack
#undef MPI_Pack_size
#define MPI_Pack_size PMPI_Pack_size
#undef MPI_Pcontrol
#define MPI_Pcontrol PMPI_Pcontrol
#undef MPI_Probe
#define MPI_Probe PMPI_Probe
#undef MPI_Recv
#define MPI_Recv PMPI_Recv
#undef MPI_Recv_init
#define MPI_Recv_init PMPI_Recv_init
#undef MPI_Reduce
#define MPI_Reduce PMPI_Reduce
#undef MPI_Reduce_scatter
#define MPI_Reduce_scatter PMPI_Reduce_scatter
#undef MPI_Request_c2f
#define MPI_Request_c2f PMPI_Request_c2f
#undef MPI_Request_free
#define MPI_Request_free PMPI_Request_free
#undef MPI_Rsend
#define MPI_Rsend PMPI_Rsend
#undef MPI_Rsend_init
#define MPI_Rsend_init PMPI_Rsend_init
#undef MPI_Scan
#define MPI_Scan PMPI_Scan
#undef MPI_Scatter
#define MPI_Scatter PMPI_Scatter
#undef MPI_Scatterv
#define MPI_Scatterv PMPI_Scatterv
#undef MPI_Send
#define MPI_Send PMPI_Send
#undef MPI_Send_init
#define MPI_Send_init PMPI_Send_init
#undef MPI_Sendrecv
#define MPI_Sendrecv PMPI_Sendrecv
#undef MPI_Sendrecv_replace
#define MPI_Sendrecv_replace PMPI_Sendrecv_replace
#undef MPI_Ssend
#define MPI_Ssend PMPI_Ssend
#undef MPI_Ssend_init
#define MPI_Ssend_init PMPI_Ssend_init
#undef MPI_Start
#define MPI_Start PMPI_Start
#undef MPI_Startall
#define MPI_Startall PMPI_Startall
#undef MPI_Test
#define MPI_Test PMPI_Test
#undef MPI_Test_cancelled
#define MPI_Test_cancelled PMPI_Test_cancelled
#undef MPI_Testall
#define MPI_Testall PMPI_Testall
#undef MPI_Testany
#define MPI_Testany PMPI_Testany
#undef MPI_Testsome
#define MPI_Testsome PMPI_Testsome
#undef MPI_Topo_status
#define MPI_Topo_status PMPI_Topo_status
#undef MPI_Topo_test
#define MPI_Topo_test PMPI_Topo_test
#undef MPI_Type_commit
#define MPI_Type_commit PMPI_Type_commit
#undef MPI_Type_contiguous
#define MPI_Type_contiguous PMPI_Type_contiguous
#undef MPI_Type_count
#define MPI_Type_count PMPI_Type_count
/* #define MPI_Type_create_darray PMPI_Type_create_darray */
#undef MPI_Type_create_indexed_block
#define MPI_Type_create_indexed_block PMPI_Type_create_indexed_block
#undef MPI_Type_create_hindexed_block
#define MPI_Type_create_hindexed_block PMPI_Type_create_hindexed_block
/* #define MPI_Type_create_subarray PMPI_Type_create_subarray */
#undef MPI_Type_extent
#define MPI_Type_extent PMPI_Type_extent
#undef MPI_Type_free
#define MPI_Type_free PMPI_Type_free
#undef MPI_Type_get_contents
#define MPI_Type_get_contents PMPI_Type_get_contents
#undef MPI_Type_get_envelope
#define MPI_Type_get_envelope PMPI_Type_get_envelope
#undef MPI_Type_get_true_extent
#define MPI_Type_get_true_extent PMPI_Type_get_true_extent
#undef MPI_Type_hindexed
#define MPI_Type_hindexed PMPI_Type_hindexed
#undef MPI_Type_hvector
#define MPI_Type_hvector PMPI_Type_hvector
#undef MPI_Type_indexed
#define MPI_Type_indexed PMPI_Type_indexed
#undef MPI_Type_lb
#define MPI_Type_lb PMPI_Type_lb
#undef MPI_Type_size
#define MPI_Type_size PMPI_Type_size
#undef MPI_Type_struct
#define MPI_Type_struct PMPI_Type_struct
#undef MPI_Type_ub
#define MPI_Type_ub PMPI_Type_ub
#undef MPI_Type_vector
#define MPI_Type_vector PMPI_Type_vector
#undef MPI_Unpack
#define MPI_Unpack PMPI_Unpack
#undef MPI_Wait
#define MPI_Wait PMPI_Wait
#undef MPI_Waitall
#define MPI_Waitall PMPI_Waitall
#undef MPI_Waitany
#define MPI_Waitany PMPI_Waitany
#undef MPI_Waitsome
#define MPI_Waitsome PMPI_Waitsome
#undef MPI_Wtick
#define MPI_Wtick PMPI_Wtick
#undef MPI_Wtime
#define MPI_Wtime PMPI_Wtime

/* commented out because these could be macros themselves, as in MPICH 
#undef MPI_Type_c2f
#define MPI_Type_c2f PMPI_Type_c2f
#undef MPI_Type_f2c
#define MPI_Type_f2c PMPI_Type_f2c
#undef MPI_Comm_c2f
#define MPI_Comm_c2f PMPI_Comm_c2f
#undef MPI_Comm_f2c
#define MPI_Comm_f2c PMPI_Comm_f2c
#undef MPI_Group_c2f
#define MPI_Group_c2f PMPI_Group_c2f
#undef MPI_Group_f2c
#define MPI_Group_f2c PMPI_Group_f2c
#undef MPI_Op_c2f
#define MPI_Op_c2f PMPI_Op_c2f
#undef MPI_Op_f2c
#define MPI_Op_f2c PMPI_Op_f2c
#undef MPI_Request_c2f
#define MPI_Request_c2f PMPI_Request_c2f
#undef MPI_Request_f2c
#define MPI_Request_f2c PMPI_Request_f2c
#undef MPI_Win_c2f
#define MPI_Win_c2f PMPI_Win_c2f
#undef MPI_Win_f2c
#define MPI_Win_f2c PMPI_Win_f2c
*/
#undef MPI_Status_c2f
#define MPI_Status_c2f PMPI_Status_c2f
#undef MPI_Status_f2c
#define MPI_Status_f2c PMPI_Status_f2c

#undef MPI_Status_set_elements
#define MPI_Status_set_elements PMPI_Status_set_elements

#ifndef HAVE_MPI_INFO_SRC  /* everywhere except in info source directory */
#undef MPI_Info_create
#define MPI_Info_create PMPI_Info_create
#undef MPI_Info_set
#define MPI_Info_set PMPI_Info_set
#undef MPI_Info_delete
#define MPI_Info_delete PMPI_Info_delete
#undef MPI_Info_get
#define MPI_Info_get PMPI_Info_get
#undef MPI_Info_get_valuelen
#define MPI_Info_get_valuelen PMPI_Info_get_valuelen
#undef MPI_Info_get_nkeys
#define MPI_Info_get_nkeys PMPI_Info_get_nkeys
#undef MPI_Info_get_nthkey
#define MPI_Info_get_nthkey PMPI_Info_get_nthkey
#undef MPI_Info_dup
#define MPI_Info_dup PMPI_Info_dup
#undef MPI_Info_free
#define MPI_Info_free PMPI_Info_free
/* #undef MPI_Info_c2f
#define MPI_Info_c2f PMPI_Info_c2f
#undef MPI_Info_f2c
#define MPI_Info_f2c PMPI_Info_f2c
*/
#endif
#undef MPI_Grequest_start
#define MPI_Grequest_start PMPI_Grequest_start
#undef MPI_Grequest_complete
#define MPI_Grequest_complete PMPI_Grequest_complete
#undef MPI_Status_set_cancelled
#define MPI_Status_set_cancelled PMPI_Status_set_cancelled

#undef MPIX_Grequest_class_create
#define MPIX_Grequest_class_create PMPIX_Grequest_class_create
#undef MPIX_Grequest_class_allocate
#define MPIX_Grequest_class_allocate PMPIX_Grequest_class_allocate

#ifdef MPIO_FORTRAN_SRC   /* only in MPI-IO Fortran source directory */
#undef MPI_File_c2f
#define MPI_File_c2f PMPI_File_c2f
#undef MPI_File_f2c
#define MPI_File_f2c PMPI_File_f2c
#endif

#undef MPI_Type_get_attr
#define MPI_Type_get_attr PMPI_Type_get_attr
#undef MPI_Type_set_attr
#define MPI_Type_set_attr PMPI_Type_set_attr
#undef MPI_Comm_set_attr
#define MPI_Comm_set_attr PMPI_Comm_set_attr
#undef MPI_Type_create_keyval
#define MPI_Type_create_keyval PMPI_Type_create_keyval

#endif

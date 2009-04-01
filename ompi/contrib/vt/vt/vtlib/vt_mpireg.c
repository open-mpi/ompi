/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_mpireg.h"

#include "vt_trc.h"

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

int vt_mpi_regid[VT__MPI_REGID_NUM];

void vt_mpi_register()
{
  uint32_t fid;

  fid = vt_def_file("MPI");
  
  vt_mpi_regid[VT__MPI_ABORT] = 
    vt_def_region("MPI_Abort", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_ADDRESS] =
    vt_def_region("MPI_Address", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_ALLGATHER] =
    vt_def_region("MPI_Allgather", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);
  vt_mpi_regid[VT__MPI_ALLGATHERV] =
    vt_def_region("MPI_Allgatherv", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);              
  vt_mpi_regid[VT__MPI_ALLREDUCE] =
    vt_def_region("MPI_Allreduce", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);                
  vt_mpi_regid[VT__MPI_ALLTOALL] =
    vt_def_region("MPI_Alltoall", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);               
  vt_mpi_regid[VT__MPI_ALLTOALLV] =
    vt_def_region("MPI_Alltoallv", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);            
  vt_mpi_regid[VT__MPI_ATTR_DELETE] =
    vt_def_region("MPI_Attr_delete", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);      
  vt_mpi_regid[VT__MPI_ATTR_GET] =
    vt_def_region("MPI_Attr_get", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_ATTR_PUT] =
    vt_def_region("MPI_Attr_put", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);         
  vt_mpi_regid[VT__MPI_BARRIER] =
    vt_def_region("MPI_Barrier", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_BARRIER);              
  vt_mpi_regid[VT__MPI_BCAST] =
    vt_def_region("MPI_Bcast", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ONE2ALL);                 
  vt_mpi_regid[VT__MPI_BSEND] =
    vt_def_region("MPI_Bsend", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                  
  vt_mpi_regid[VT__MPI_BSEND_INIT] =
    vt_def_region("MPI_Bsend_init", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_BUFFER_ATTACH] =
    vt_def_region("MPI_Buffer_attach", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);    
  vt_mpi_regid[VT__MPI_BUFFER_DETACH] =
    vt_def_region("MPI_Buffer_detach", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);      
  vt_mpi_regid[VT__MPI_CANCEL] =
    vt_def_region("MPI_Cancel", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_CART_COORDS] =
    vt_def_region("MPI_Cart_coords", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_CART_CREATE] =
    vt_def_region("MPI_Cart_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);       
  vt_mpi_regid[VT__MPI_CART_GET] =
    vt_def_region("MPI_Cart_get", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_CART_MAP] =
    vt_def_region("MPI_Cart_map", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                  
  vt_mpi_regid[VT__MPI_CART_RANK] =
    vt_def_region("MPI_Cart_rank", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_CART_SHIFT] =
    vt_def_region("MPI_Cart_shift", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);         
  vt_mpi_regid[VT__MPI_CART_SUB] =
    vt_def_region("MPI_Cart_sub", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_CARTDIM_GET] =
    vt_def_region("MPI_Cartdim_get", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);       
  vt_mpi_regid[VT__MPI_COMM_COMPARE] =
    vt_def_region("MPI_Comm_compare", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);      
  vt_mpi_regid[VT__MPI_COMM_CREATE] =
    vt_def_region("MPI_Comm_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);          
  vt_mpi_regid[VT__MPI_COMM_DUP] =
    vt_def_region("MPI_Comm_dup", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_COMM_FREE] =
    vt_def_region("MPI_Comm_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_COMM_GROUP] =
    vt_def_region("MPI_Comm_group", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);              
  vt_mpi_regid[VT__MPI_COMM_RANK] =
    vt_def_region("MPI_Comm_rank", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_COMM_REMOTE_GROUP] =
    vt_def_region("MPI_Comm_remote_group", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_COMM_REMOTE_SIZE] =
    vt_def_region("MPI_Comm_remote_size", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);  
  vt_mpi_regid[VT__MPI_COMM_SIZE] =
    vt_def_region("MPI_Comm_size", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);              
  vt_mpi_regid[VT__MPI_COMM_SPLIT] =
    vt_def_region("MPI_Comm_split", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);             
  vt_mpi_regid[VT__MPI_COMM_TEST_INTER] =
    vt_def_region("MPI_Comm_test_inter", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);    
  vt_mpi_regid[VT__MPI_DIMS_CREATE] =
    vt_def_region("MPI_Dims_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_ERRHANDLER_CREATE] =
    vt_def_region("MPI_Errhandler_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_ERRHANDLER_FREE] =
    vt_def_region("MPI_Errhandler_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);    
  vt_mpi_regid[VT__MPI_ERRHANDLER_GET] =
    vt_def_region("MPI_Errhandler_get", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);      
  vt_mpi_regid[VT__MPI_ERRHANDLER_SET] =
    vt_def_region("MPI_Errhandler_set", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);      
  vt_mpi_regid[VT__MPI_ERROR_CLASS] =
    vt_def_region("MPI_Error_class", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_ERROR_STRING] =
    vt_def_region("MPI_Error_string", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);          
  vt_mpi_regid[VT__MPI_FILE_CLOSE] =
    vt_def_region("MPI_File_close", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_FILE_IREAD] =
    vt_def_region("MPI_File_iread", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_FILE_IREAD_AT] =
    vt_def_region("MPI_File_iread_at", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);    
  vt_mpi_regid[VT__MPI_FILE_IREAD_SHARED] =
    vt_def_region("MPI_File_iread_shared", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_IWRITE] =
    vt_def_region("MPI_File_iwrite", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_IWRITE_AT] =
    vt_def_region("MPI_File_iwrite_at", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_IWRITE_SHARED] =
    vt_def_region("MPI_File_iwrite_shared", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_OPEN] =
    vt_def_region("MPI_File_open", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ] =
    vt_def_region("MPI_File_read", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_ALL] =
    vt_def_region("MPI_File_read_all", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_ALL_BEGIN] =
    vt_def_region("MPI_File_read_all_begin", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_ALL_END] =
    vt_def_region("MPI_File_read_all_end", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_AT] =
    vt_def_region("MPI_File_read_at", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_AT_ALL] =
    vt_def_region("MPI_File_read_at_all", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_AT_ALL_BEGIN] =
    vt_def_region("MPI_File_read_at_all_begin", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_AT_ALL_END] =
    vt_def_region("MPI_File_read_at_all_end", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_ORDERED] =
    vt_def_region("MPI_File_read_ordered", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_FILE_READ_ORDERED_BEGIN] =
    vt_def_region("MPI_File_read_ordered_begin", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_READ_ORDERED_END] =
    vt_def_region("MPI_File_read_ordered_end", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_READ_SHARED] =
    vt_def_region("MPI_File_read_shared", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_SEEK] =
    vt_def_region("MPI_File_seek", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_SEEK_SHARED] =
    vt_def_region("MPI_File_seek_shared", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE] =
    vt_def_region("MPI_File_write", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_ALL] =
    vt_def_region("MPI_File_write_all", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_ALL_BEGIN] =
    vt_def_region("MPI_File_write_all_begin", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_ALL_END] =
    vt_def_region("MPI_File_write_all_end", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_AT] =
    vt_def_region("MPI_File_write_at", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_AT_ALL] =
    vt_def_region("MPI_File_write_at_all", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_AT_ALL_BEGIN] =
    vt_def_region("MPI_File_write_at_all_begin", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_AT_ALL_END] =
    vt_def_region("MPI_File_write_at_all_end", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_ORDERED] =
    vt_def_region("MPI_File_write_ordered", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_ORDERED_BEGIN] =
    vt_def_region("MPI_File_write_ordered_begin", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_ORDERED_END] =
    vt_def_region("MPI_File_write_ordered_end", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FILE_WRITE_SHARED] =
    vt_def_region("MPI_File_write_shared", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_FINALIZE] =
    vt_def_region("MPI_Finalize", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                  
  vt_mpi_regid[VT__MPI_GATHER] =
    vt_def_region("MPI_Gather", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ONE);                      
  vt_mpi_regid[VT__MPI_GATHERV] =
    vt_def_region("MPI_Gatherv", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ONE);                    
  vt_mpi_regid[VT__MPI_GET_COUNT] =
    vt_def_region("MPI_Get_count", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_GET_ELEMENTS] =
    vt_def_region("MPI_Get_elements", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);          
  vt_mpi_regid[VT__MPI_GET_PROCESSOR_NAME] =
    vt_def_region("MPI_Get_processor_name", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_GET_VERSION] =
    vt_def_region("MPI_Get_version", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);          
  vt_mpi_regid[VT__MPI_GRAPH_CREATE] =
    vt_def_region("MPI_Graph_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);         
  vt_mpi_regid[VT__MPI_GRAPH_GET] =
    vt_def_region("MPI_Graph_get", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_GRAPH_MAP] =
    vt_def_region("MPI_Graph_map", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);               
  vt_mpi_regid[VT__MPI_GRAPH_NEIGHBORS] =
    vt_def_region("MPI_Graph_neighbors", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);   
  vt_mpi_regid[VT__MPI_GRAPH_NEIGHBORS_COUNT] =
    vt_def_region("MPI_Graph_neighbors_count", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_GRAPHDIMS_GET] =
    vt_def_region("MPI_Graphdims_get", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);       
  vt_mpi_regid[VT__MPI_GROUP_COMPARE] =
    vt_def_region("MPI_Group_compare", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);       
  vt_mpi_regid[VT__MPI_GROUP_DIFFERENCE] =
    vt_def_region("MPI_Group_difference", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_GROUP_EXCL] =
    vt_def_region("MPI_Group_excl", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);             
  vt_mpi_regid[VT__MPI_GROUP_FREE] =
    vt_def_region("MPI_Group_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_GROUP_INCL] =
    vt_def_region("MPI_Group_incl", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);             
  vt_mpi_regid[VT__MPI_GROUP_INTERSECTION] =
    vt_def_region("MPI_Group_intersection", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION); 
  vt_mpi_regid[VT__MPI_GROUP_RANGE_EXCL] =
    vt_def_region("MPI_Group_range_excl", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);  
  vt_mpi_regid[VT__MPI_GROUP_RANGE_INCL] =
    vt_def_region("MPI_Group_range_incl", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);  
  vt_mpi_regid[VT__MPI_GROUP_RANK] =
    vt_def_region("MPI_Group_rank", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_GROUP_SIZE] =
    vt_def_region("MPI_Group_size", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);              
  vt_mpi_regid[VT__MPI_GROUP_TRANSLATE_RANKS] =
    vt_def_region("MPI_Group_translate_ranks", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_GROUP_UNION] =
    vt_def_region("MPI_Group_union", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_IBSEND] =
    vt_def_region("MPI_Ibsend", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_INIT] =
    vt_def_region("MPI_Init", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                       
  vt_mpi_regid[VT__MPI_INIT_THREAD] =
    vt_def_region("MPI_Init_thread", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);
  vt_mpi_regid[VT__MPI_INITIALIZED] =
    vt_def_region("MPI_Initialized", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_INTERCOMM_CREATE] =
    vt_def_region("MPI_Intercomm_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);  
  vt_mpi_regid[VT__MPI_INTERCOMM_MERGE] =
    vt_def_region("MPI_Intercomm_merge", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);    
  vt_mpi_regid[VT__MPI_IPROBE] =
    vt_def_region("MPI_Iprobe", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_IRECV] =
    vt_def_region("MPI_Irecv", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                    
  vt_mpi_regid[VT__MPI_IRSEND] =
    vt_def_region("MPI_Irsend", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_ISEND] =
    vt_def_region("MPI_Isend", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                        
  vt_mpi_regid[VT__MPI_ISSEND] =
    vt_def_region("MPI_Issend", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_KEYVAL_CREATE] =
    vt_def_region("MPI_Keyval_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);        
  vt_mpi_regid[VT__MPI_KEYVAL_FREE] =
    vt_def_region("MPI_Keyval_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_OP_CREATE] =
    vt_def_region("MPI_Op_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_OP_FREE] =
    vt_def_region("MPI_Op_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_PACK] =
    vt_def_region("MPI_Packpack", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                    
  vt_mpi_regid[VT__MPI_PACK_SIZE] =
    vt_def_region("MPI_Pack_size", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);            
  vt_mpi_regid[VT__MPI_PCONTROL] =
    vt_def_region("MPI_Pcontrol", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);               
  vt_mpi_regid[VT__MPI_PROBE] =
    vt_def_region("MPI_Probe", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                        
  vt_mpi_regid[VT__MPI_RECV] =
    vt_def_region("MPI_Recv", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                     
  vt_mpi_regid[VT__MPI_RECV_INIT] =
    vt_def_region("MPI_Recv_init", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                
  vt_mpi_regid[VT__MPI_REDUCE] =
    vt_def_region("MPI_Reduce", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ONE);                      
  vt_mpi_regid[VT__MPI_REDUCE_SCATTER] =
    vt_def_region("MPI_Reduce_scatter", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);     
  vt_mpi_regid[VT__MPI_REQUEST_FREE] =
    vt_def_region("MPI_Request_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);         
  vt_mpi_regid[VT__MPI_RSEND] =
    vt_def_region("MPI_Rsend", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                        
  vt_mpi_regid[VT__MPI_RSEND_INIT] =
    vt_def_region("MPI_Rsend_init", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);             
  vt_mpi_regid[VT__MPI_SCAN] =
    vt_def_region("MPI_Scan", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_OTHER);                         
  vt_mpi_regid[VT__MPI_SCATTER] =
    vt_def_region("MPI_Scatter", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ONE2ALL);                    
  vt_mpi_regid[VT__MPI_SCATTERV] =
    vt_def_region("MPI_Scatterv", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ONE2ALL);                  
  vt_mpi_regid[VT__MPI_SEND] =
    vt_def_region("MPI_Send", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                          
  vt_mpi_regid[VT__MPI_SEND_INIT] =
    vt_def_region("MPI_Send_init", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);               
  vt_mpi_regid[VT__MPI_SENDRECV] =
    vt_def_region("MPI_Sendrecv", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);               
  vt_mpi_regid[VT__MPI_SENDRECV_REPLACE] =
    vt_def_region("MPI_Sendrecv_replace", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);  
  vt_mpi_regid[VT__MPI_SSEND] =
    vt_def_region("MPI_Ssend", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                       
  vt_mpi_regid[VT__MPI_SSEND_INIT] =
    vt_def_region("MPI_Ssend_init", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);             
  vt_mpi_regid[VT__MPI_START] =
    vt_def_region("MPI_Start", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                        
  vt_mpi_regid[VT__MPI_STARTALL] =
    vt_def_region("MPI_Startall", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_TEST] =
    vt_def_region("MPI_Test", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                         
  vt_mpi_regid[VT__MPI_TEST_CANCELLED] =
    vt_def_region("MPI_Test_cancelled", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);     
  vt_mpi_regid[VT__MPI_TESTALL] =
    vt_def_region("MPI_Testall", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                   
  vt_mpi_regid[VT__MPI_TESTANY] =
    vt_def_region("MPI_Testany", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                    
  vt_mpi_regid[VT__MPI_TESTSOME] =
    vt_def_region("MPI_Testsome", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_TOPO_TEST] =
    vt_def_region("MPI_Topo_test", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                   
  vt_mpi_regid[VT__MPI_TYPE_COMMIT] =
    vt_def_region("MPI_Type_commit", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_TYPE_CONTIGUOUS] =
    vt_def_region("MPI_Type_contiguous", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);   
  vt_mpi_regid[VT__MPI_TYPE_EXTENT] =
    vt_def_region("MPI_Type_extent", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_TYPE_FREE] =
    vt_def_region("MPI_Type_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);               
  vt_mpi_regid[VT__MPI_TYPE_HINDEXED] =
    vt_def_region("MPI_Type_hindexed", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);       
  vt_mpi_regid[VT__MPI_TYPE_HVECTOR] =
    vt_def_region("MPI_Type_hvector", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);         
  vt_mpi_regid[VT__MPI_TYPE_INDEXED] =
    vt_def_region("MPI_Type_indexed", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);         
  vt_mpi_regid[VT__MPI_TYPE_LB] =
    vt_def_region("MPI_Type_lb", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                    
  vt_mpi_regid[VT__MPI_TYPE_SIZE] =
    vt_def_region("MPI_Type_size", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);               
  vt_mpi_regid[VT__MPI_TYPE_STRUCT] =
    vt_def_region("MPI_Type_struct", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);           
  vt_mpi_regid[VT__MPI_TYPE_UB] =
    vt_def_region("MPI_Type_ub", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                   
  vt_mpi_regid[VT__MPI_TYPE_VECTOR] =
    vt_def_region("MPI_Type_vector", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);         
  vt_mpi_regid[VT__MPI_UNPACK] =
    vt_def_region("MPI_Unpack", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                     
  vt_mpi_regid[VT__MPI_WAIT] =
    vt_def_region("MPI_Wait", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                          
  vt_mpi_regid[VT__MPI_WAITALL] =
    vt_def_region("MPI_Waitall", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                   
  vt_mpi_regid[VT__MPI_WAITANY] =
    vt_def_region("MPI_Waitany", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                   
  vt_mpi_regid[VT__MPI_WAITSOME] =
    vt_def_region("MPI_Waitsome", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                 
  vt_mpi_regid[VT__MPI_WTICK] =
    vt_def_region("MPI_Wtick", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                     
  vt_mpi_regid[VT__MPI_WTIME] =
    vt_def_region("MPI_Wtime", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_ACCUMULATE] =
    vt_def_region("MPI_Accumulate", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_GET] =
    vt_def_region("MPI_Get", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_PUT] =
    vt_def_region("MPI_Put", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_COMPLETE] =
    vt_def_region("MPI_Win_complete", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_CREATE] =
    vt_def_region("MPI_Win_create", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_BARRIER);                      
  vt_mpi_regid[VT__MPI_WIN_FENCE] =
    vt_def_region("MPI_Win_fence", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_BARRIER);                      
  vt_mpi_regid[VT__MPI_WIN_FREE] =
    vt_def_region("MPI_Win_free", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_BARRIER);                      
  vt_mpi_regid[VT__MPI_WIN_GET_GROUP] =
    vt_def_region("MPI_Win_get_group", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_LOCK] =
    vt_def_region("MPI_Win_lock", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_POST] =
    vt_def_region("MPI_Win_post", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_START] =
    vt_def_region("MPI_Win_start", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_TEST] =
    vt_def_region("MPI_Win_test", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_UNLOCK] =
    vt_def_region("MPI_Win_unlock", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_WIN_WAIT] =
    vt_def_region("MPI_Win_wait", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION);                      
  vt_mpi_regid[VT__MPI_ALLTOALLW] =
    vt_def_region("MPI_Alltoallw", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);                      
  vt_mpi_regid[VT__MPI_EXSCAN] =
    vt_def_region("MPI_Exscan", fid, VT_NO_LNO, VT_NO_LNO, "MPI", VT_FUNCTION_COLL_ALL2ALL);                      
}

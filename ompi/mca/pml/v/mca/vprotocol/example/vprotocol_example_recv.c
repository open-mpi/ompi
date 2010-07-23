/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2010      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "../pml_v.h"
#include "vprotocol_example.h"

int mca_vprotocol_example_irecv_init(void *addr,                           
                      size_t count,                         
                      struct ompi_datatype_t *datatype,              
                      int src,
                      int tag,                                
                      struct ompi_communicator_t* comm,
                      struct ompi_request_t **request )
{
  int ret;
  
  ret = mca_pml_v.host_pml.pml_irecv_init(addr, count, datatype, src, tag, comm, request);
  V_OUTPUT_VERBOSE(50, "posted\tirecv_init %ld\tcomm %d\tfrom %d\ttag %d\tsize %ld", ((mca_pml_base_request_t *)*request)->req_sequence, comm->c_contextid, src, tag, (long) count);
  return ret;
}

int mca_vprotocol_example_irecv(void *addr, 
                     size_t count,
                     ompi_datatype_t * datatype,
                     int src,
                     int tag,
                     struct ompi_communicator_t *comm,
                     struct ompi_request_t **request)
{
  int ret;
  
  ret = mca_pml_v.host_pml.pml_irecv(addr, count, datatype, src, tag, comm, request);
  V_OUTPUT_VERBOSE(50, "posted\tirecv %ld\tcomm %d\tfrom %d\ttag %d\tsize %ld", ((mca_pml_base_request_t *)*request)->req_sequence, comm->c_contextid, src, tag, (long) count);
  return ret;
}

int mca_vprotocol_example_recv(void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status )
{
  int ret;
  V_OUTPUT_VERBOSE(50, "posted\trecv \tcomm %d\tfrom %d\ttag %d\tsize %ld", comm->c_contextid, src, tag, (long) count);  
  ret = mca_pml_v.host_pml.pml_recv(addr, count, datatype, src, tag, comm, status);
# ifdef OPAL_ENABLE_DEBUG
    if(status)
      V_OUTPUT_VERBOSE(75, "deliver\trecv \tcomm %d\tfrom %d(%d)\ttag %d(%d)\tsize %ld(%ld)\tstatus %d", comm->c_contextid, src, status->MPI_SOURCE, tag, status->MPI_TAG, (long) count, (long) status->_ucount, status->MPI_ERROR);
    else 
      V_OUTPUT_VERBOSE(75, "deliver\trecv \tcomm %d\tfrom %d\ttag %d\tsize %ld", comm->c_contextid, src, tag, (long) count);
# endif
  return ret;
}

/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "../pml_v.h"
#include "vprotocol_example.h"

int mca_vprotocol_example_isend_init(void *addr,                           
                      size_t count,                         
                      struct ompi_datatype_t *datatype,              
                      int dst,
                      int tag, 
                      mca_pml_base_send_mode_t sendmode,                               
                      struct ompi_communicator_t* comm,
                      struct ompi_request_t **request )
{
  V_OUTPUT_VERBOSE(50, "request\tpisend \tcomm %d\tto %d\ttag %d\tsize %ld", comm->c_contextid, dst, tag, (long) count);
  return mca_pml_v.host_pml.pml_isend_init(addr, count, datatype, dst, tag, sendmode, comm, request);
}

int mca_vprotocol_example_isend(void *addr, 
                     size_t count,
                     ompi_datatype_t * datatype,
                     int dst,
                     int tag,
                     mca_pml_base_send_mode_t sendmode, 
                     struct ompi_communicator_t *comm,
                     struct ompi_request_t **request)
{
  V_OUTPUT_VERBOSE(50, "request\tisend \tcomm %d\tto %d\ttag %d\tsize %ld", comm->c_contextid, dst, tag, (long) count);
  return mca_pml_v.host_pml.pml_isend(addr, count, datatype, dst, tag, sendmode, comm, request);
}

int mca_vprotocol_example_send(void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int dst,
                      int tag,
                      mca_pml_base_send_mode_t sendmode,
                      struct ompi_communicator_t *comm)
{
  V_OUTPUT_VERBOSE(50, "request\tsend \tcomm %d\tto %d\ttag %d\tsize %ld", comm->c_contextid, dst, tag, (long) count);
  return mca_pml_v.host_pml.pml_send(addr, count, datatype, dst, tag, sendmode, comm);
}

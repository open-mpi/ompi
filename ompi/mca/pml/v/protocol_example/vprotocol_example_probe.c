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

int mca_vprotocol_example_probe( int src, int tag,
                       struct ompi_communicator_t *comm,
                       ompi_status_public_t * status )
{
  V_OUTPUT_VERBOSE(50, "mca_vprotocol_example_probe(%d, %d, %d)", src, tag, comm->c_contextid);
  return mca_pml_v.host_pml.pml_probe(src, tag, comm, status);
}
                        
int mca_vprotocol_example_iprobe( int src, int tag,
                        struct ompi_communicator_t *comm,
                        int *matched, ompi_status_public_t * status )
{  
  V_OUTPUT_VERBOSE(60, "mca_vprotocol_example_iprobe(%d, %d, %d)", src, tag, comm->c_contextid);
  return mca_pml_v.host_pml.pml_iprobe(src, tag, comm, matched, status);
}

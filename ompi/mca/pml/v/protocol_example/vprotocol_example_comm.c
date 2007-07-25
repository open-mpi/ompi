/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
 
#include "../pml_v.h"
#include "vprotocol_example.h"

int mca_vprotocol_example_add_comm(struct ompi_communicator_t* comm)
{
  V_OUTPUT_VERBOSE(30, "vprotocol_example_add_comm(%d)", comm->c_contextid);
  return mca_pml_v.host_pml.pml_add_comm(comm);
}

int mca_vprotocol_example_del_comm(struct ompi_communicator_t* comm)
{
  V_OUTPUT_VERBOSE(30, "vprotocol_example_del_comm(%d)", comm->c_contextid);
  return mca_pml_v.host_pml.pml_del_comm(comm);
}

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
#include "vprotocol_example.h"
#include "vprotocol_example_start.h"

OMPI_DECLSPEC int mca_vprotocol_example_start(size_t count, ompi_request_t **requests)
{ 
  V_OUTPUT_VERBOSE(50, "starting %ld requests", (long) count);
  return mca_pml_v.host_pml.pml_start(count, requests);
}

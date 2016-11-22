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

int mca_vprotocol_example_add_procs(struct ompi_proc_t **procs, size_t nprocs)
{
  V_OUTPUT_VERBOSE(30, "adding %ld procs", (long) nprocs);
  return mca_pml_v.host_pml.pml_add_procs(procs, nprocs);
}

int mca_vprotocol_example_del_procs(struct ompi_proc_t **procs, size_t nprocs)
{
  V_OUTPUT_VERBOSE(30, "removing %ld procs", (long) nprocs);
  return mca_pml_v.host_pml.pml_del_procs(procs, nprocs);
}

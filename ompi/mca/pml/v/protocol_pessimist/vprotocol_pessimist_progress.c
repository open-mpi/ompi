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
#include "vprotocol_pessimist.h"

int mca_vprotocol_pessimist_progress(void)
{
  return mca_pml_v.host_pml.pml_progress();
}

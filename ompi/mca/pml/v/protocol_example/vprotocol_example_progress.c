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

int mca_vprotocol_example_enable(bool enable)
{
  V_OUTPUT_VERBOSE(15, "enable=%d", enable);
  return mca_pml_v.host_pml.pml_enable(enable);
}

int mca_vprotocol_example_progress(void)
{
  V_OUTPUT_VERBOSE(100, "progress...");
  return mca_pml_v.host_pml.pml_progress();
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/mca.h"
#include "../pml_v.h"
#include "../pml_v_protocol_base.h"
#include "vprotocol_example.h"

static int mca_vprotocol_example_component_open(void);
static int mca_vprotocol_example_component_close(void);

static mca_pml_v_protocol_base_module_t *mca_vprotocol_example_component_init( int* priority,
                            bool, bool);
static int mca_vprotocol_example_component_finalize(void);


static int _priority;


mca_pml_v_protocol_base_component_2_0_0_t mca_vprotocol_example_component =
{
  /* First, the mca_base_component_t struct containing meta
   * information about the component itself */
  .pmlm_version = {
    MCA_VPROTOCOL_BASE_VERSION_2_0_0,

    .mca_component_name = "example",
    MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                          OMPI_RELEASE_VERSION),
    .mca_open_component = mca_vprotocol_example_component_open,
    .mca_close_component = mca_vprotocol_example_component_close,
  },
  .pmlm_data = {
      /* component is not checkpointable */
      MCA_BASE_METADATA_PARAM_NONE
  },

  .pmlm_init = mca_vprotocol_example_component_init,
  .pmlm_finalize = mca_vprotocol_example_component_finalize,
};

/** MCA level functions
  */

int mca_vprotocol_example_component_open(void)
{
  _priority = mca_param_register_int( "priority", -1);
  V_OUTPUT_VERBOSE(10, "vprotocol_example_open, read priority %d", _priority);
  return OMPI_SUCCESS;
}

int mca_vprotocol_example_component_close(void)
{
  V_OUTPUT_VERBOSE(10, "vprotocol_example_close");
  return OMPI_SUCCESS;
}

/** VPROTOCOL level functions (same as PML one)
  */

mca_pml_v_protocol_base_module_t *mca_vprotocol_example_component_init( int* priority,
                                                                          bool enable_progress_threads,
                                                                          bool enable_mpi_threads)
{
  V_OUTPUT_VERBOSE(10, "vprotocol_example_init");
  *priority = _priority;

/**
  * Some protocols requires sanity check about thread support (those making piecewise deterministic assumption)
  if(enable_mpi_threads)
  {
    OPAL_OUTPUT_VERBOSE( mca_pml_v_verbose, mca_pml_v_output, "vprotocol_example.init: threads are enabled, and not supported by vprotocol example fault tolerant layer, will not load"));
    return NULL;
  }
  */

/**
  * Insert your own protocol initialization here
  */

  return &mca_vprotocol_example.super;
}

int mca_vprotocol_example_component_finalize(void)
{
  V_OUTPUT_VERBOSE(10, "vprotocol_example_finalize");

/**
  * Insert your own garbage collecting here
  */

  return OMPI_SUCCESS;
}

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/pml/base/static-components.h"

static int mca_pml_base_progress(void) 
{
    return OMPI_SUCCESS;
}


/*
 * Global variables
 */
int mca_pml_base_output = -1;
mca_pml_base_module_t mca_pml = {
    NULL,                    /* pml_add_procs */
    NULL,                    /* pml_del_procs */
    NULL,                    /* pml_add_ptls */
    NULL,                    /* pml_control */
    mca_pml_base_progress,   /* pml_progress */
    NULL,                    /* pml_add_comm */
    NULL,                    /* pml_del_comm */
    NULL,                    /* pml_irecv_init */
    NULL,                    /* pml_irecv */
    NULL,                    /* pml_recv */
    NULL,                    /* pml_isend_init */
    NULL,                    /* pml_isend */
    NULL,                    /* pml_send */
    NULL,                    /* pml_iprobe */
    NULL,                    /* pml_probe */
    NULL                     /* pml_start */
};

ompi_list_t mca_pml_base_components_available;
mca_pml_base_component_t mca_pml_base_selected_component;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pml_base_open(void)
{
  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("pml", 0, mca_pml_base_static_components, 
                               &mca_pml_base_components_available)) {
    return OMPI_ERROR;
  }

  /* Set a sentinel in case we don't select any components (e.g.,
     ompi_info) */

  mca_pml_base_selected_component.pmlm_finalize = NULL;

  /* All done */

  return OMPI_SUCCESS;
}

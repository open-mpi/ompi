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
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/pcm/base/base_kill_track.h"
#include "util/output.h"
#include "event/event.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#ifdef WIN32
    const mca_base_component_t *mca_pcm_base_static_components[] = {NULL};
#else 
#include "mca/pcm/base/static-components.h"
#endif

/*
 * Global variables
 */
int mca_pcm_base_output = 0;
ompi_list_t mca_pcm_base_components_available;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pcm_base_open(void)
{
  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("pcm", 0, mca_pcm_base_static_components, 
                               &mca_pcm_base_components_available)) {
      ompi_output_verbose(5, mca_pcm_base_output, 
                          "pcm: error opening components");
    return OMPI_ERROR;
  }

  mca_pcm_base_kill_init();

  /* All done */

  return OMPI_SUCCESS;
}

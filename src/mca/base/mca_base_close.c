/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "include/constants.h"

/*
 * Main MCA shutdown.
 */
int mca_base_close(void)
{
  extern bool mca_base_opened;
  if (mca_base_opened) {
    /* Clear out all the registered MCA params */

    mca_base_param_finalize();

    /* Close down the component repository */

    mca_base_component_repository_finalize();
  }
  mca_base_opened = false;

  /* All done */

  return OMPI_SUCCESS;
}

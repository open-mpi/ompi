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

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"


int mca_oob_base_close(void)
{
  ompi_list_item_t* item;

  /* Finalize all the oob modules and free their list items */
  for (item =  ompi_list_remove_first(&mca_oob_base_modules);
       item != NULL;
       item =  ompi_list_remove_first(&mca_oob_base_modules)) {
    mca_oob_base_info_t* base = (mca_oob_base_info_t *) item;
    base->oob_module->oob_fini();
  }

  /* Close all remaining available modules (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(mca_oob_base_output, &mca_oob_base_components, NULL);

  OBJ_DESTRUCT(&mca_oob_base_modules);
  OBJ_DESTRUCT(&mca_oob_base_components);

  if( NULL != mca_oob_base_include )
    free(mca_oob_base_include);
  if( NULL != mca_oob_base_exclude )
    free(mca_oob_base_exclude);

  /* All done */
  return OMPI_SUCCESS;
}


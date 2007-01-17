/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>

#include "orte/orte_constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"


int mca_oob_base_close(void)
{
  opal_list_item_t* item;
  static bool already_closed = false;

  /* Sanity check.  This may be able to be removed when the rml/oob
     interface is re-worked (the current infrastructure may invoke
     this function twice: once as a standalone, and once via the rml
     oob component). */
  if (already_closed) {
      return ORTE_SUCCESS;
  }
    

  /* Finalize all the oob modules and free their list items */
  for (item =  opal_list_remove_first(&mca_oob_base_modules);
       item != NULL;
       item =  opal_list_remove_first(&mca_oob_base_modules)) {
    mca_oob_base_info_t* base = (mca_oob_base_info_t *) item;
    base->oob_module->oob_fini();
    OBJ_RELEASE(base);
  }

  /* Close all remaining available modules (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(mca_oob_base_output, &mca_oob_base_components, NULL);

  OBJ_DESTRUCT(&mca_oob_base_modules);
  OBJ_DESTRUCT(&mca_oob_base_components);
  OBJ_DESTRUCT(&mca_oob_base_exception_handlers);

  if( NULL != mca_oob_base_include )
    free(mca_oob_base_include);
  if( NULL != mca_oob_base_exclude )
    free(mca_oob_base_exclude);

  /* All done */
  already_closed = true;
  return ORTE_SUCCESS;
}


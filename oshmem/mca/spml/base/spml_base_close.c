/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

#include <stdio.h>

#include "oshmem/constants.h"
#include "opal/mca/mca.h" /* TODO: remove redefined in spml.h*/
#include "opal/mca/base/base.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"
#include "oshmem/mca/spml/base/spml_base_request.h"
#include "opal/runtime/opal_progress.h"


int mca_spml_base_finalize(void)
{
    if (NULL != mca_spml_base_selected_component.spmlm_finalize) {
        return mca_spml_base_selected_component.spmlm_finalize();
    }
    return OSHMEM_SUCCESS;
}

     
int mca_spml_base_close(void)
{
    int i;
    /* turn off the progress code for the spml */
     /*TODO: Irit Restore */
      /*if( NULL != mca_spml.spml_progress ) {
          opal_progress_unregister(mca_spml.spml_progress);
      } */

    /*TODO: Remove*/
      /* Blatently ignore the return code (what would we do to recover,
       * anyway?  This module is going away, so errors don't matter anymore) 
       */

      /**
       * Destruct the send and receive queues. The ompi_free_list_t destructor
       * will return the memory to the mpool, so this has to be done before the
       * mpool get released by the SPML close function.
       */
      OBJ_DESTRUCT(&mca_spml_base_put_requests);
      OBJ_DESTRUCT(&mca_spml_base_get_requests);
 
     /* TODO: Restore  mca_spml.spml_progress = mca_spml_base_progress;*/

      for( i = 0; i < opal_pointer_array_get_size(&mca_spml_base_spml); i++) {
          char * tmp_val;
          tmp_val = (char *)opal_pointer_array_get_item(&mca_spml_base_spml, i);
          if( NULL == tmp_val) {
              continue;
          }
          free(tmp_val);
      } 
      OBJ_DESTRUCT(&mca_spml_base_spml);

      mca_base_components_close(mca_spml_base_output,
                                &mca_spml_base_components_available, NULL);

      /* All done */

    return OSHMEM_SUCCESS;
}


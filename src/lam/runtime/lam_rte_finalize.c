/*
 * $HEADER$
 */

/** @file **/

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "lam/util/output.h"
#include "lam/threads/mutex.h"
#include "mca/lam/pcm/base/base.h"
#include "mca/lam/oob/base/base.h"
#include "mca/lam/registry/base/base.h"


/**
 * Leave a LAM RTE.
 *
 * @retval LAM_SUCCESS Upon success.
 * @retval LAM_ERROR Upon failure.
 *
 * This function performs 
 */
int lam_rte_finalize(void)
{
  mca_registry_base_close();
  mca_oob_base_close();
  mca_pcm_base_close();

  /* All done */

  return LAM_SUCCESS;
}

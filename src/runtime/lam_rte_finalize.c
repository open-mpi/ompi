/*
 * $HEADER$
 */

/** @file **/

#include "lam_config.h"

#include "constants.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "mca/pcm/base/base.h"
#include "mca/oob/base/base.h"
#include "mca/registry/base/base.h"


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

/*
 * $HEADER$
 */

/** @file **/

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/runtime/runtime.h"
#include "lam/util/output.h"
#include "lam/threads/mutex.h"

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
  /* All done */

  return LAM_SUCCESS;
}

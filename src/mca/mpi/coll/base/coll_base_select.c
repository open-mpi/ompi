/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mca/mca.h"
#include "mca/mpi/coll/coll.h"
#include "mca/mpi/coll/base/base.h"


int mca_coll_base_select(lam_list_t *selected, bool *allow_multi_user_threads, 
                         bool *have_hidden_threads)
{
  /* JMS Need to implement */

  *allow_multi_user_threads = true;
  *have_hidden_threads = false;

  /* All done */

  return LAM_SUCCESS;
}

/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


int mca_coll_base_select(ompi_list_t *selected, bool *allow_multi_user_threads, 
                         bool *have_hidden_threads)
{
  /* JMS Need to implement */

  *allow_multi_user_threads = true;
  *have_hidden_threads = false;

  /* All done */

  return OMPI_SUCCESS;
}

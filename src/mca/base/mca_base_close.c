/*
 * $HEADER$
 */

#include "lam_config.h"

#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"


/*
 * Main MCA shutdown.
 */
int mca_base_close(void)
{
  extern bool mca_base_opened;
  if (mca_base_opened) {
    /* Clear out all the registered MCA params */

    mca_base_param_finalize();

    /* Close down the module repository */

    mca_base_module_repository_finalize();
  }
  mca_base_opened = false;

  /* All done */

  return LAM_SUCCESS;
}

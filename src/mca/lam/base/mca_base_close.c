/*
 * $HEADER$
 */

#include "lam_config.h"

#include "lam/util/output.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"


/*
 * Main MCA shutdown.
 */
int mca_base_close(void)
{
  extern bool mca_base_opened;
  if (mca_base_opened) {
    /* Clear out all the registered MCA params */

    mca_base_param_finalize();

    /* Close down the module registry */

    mca_base_module_registry_finalize();
  }
  mca_base_opened = false;

  /* All done */

  return LAM_SUCCESS;
}

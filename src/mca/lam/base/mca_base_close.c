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
  /* Clear out all the registered MCA params */

  mca_base_param_finalize();

  /* Close down the module registry */

  mca_base_module_registry_finalize();

  /* All done */

  return LAM_SUCCESS;
}

/* -*- C -*-
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "include/constants.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/gpr/base/base.h"
#include "gpr_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_gpr_base_component_t mca_gpr_proxy_component = {
  {
    MCA_GPR_BASE_VERSION_1_0_0,

    "proxy", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_gpr_proxy_open,  /* module open */
    mca_gpr_proxy_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_gpr_proxy_init,    /* module init */
  mca_gpr_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static mca_gpr_base_module_t mca_gpr_proxy = {
    gpr_proxy_get,
    gpr_proxy_put,
    gpr_proxy_delete_segment,
    gpr_proxy_subscribe,
    gpr_proxy_unsubscribe,
    gpr_proxy_synchro,
    gpr_proxy_delete_object,
    gpr_proxy_index,
    gpr_proxy_test_internals
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/*
 * globals needed within proxy component
 */
ompi_process_name_t *mca_gpr_my_replica;


/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int mca_gpr_proxy_open(void)
{
    return OMPI_SUCCESS;
}

/*
 * ditto for this one
 */
int mca_gpr_proxy_close(void)
{
    return OMPI_SUCCESS;
}

mca_gpr_base_module_t* mca_gpr_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{
    /* If we're NOT the seed, then we want to be selected, so do all
       the setup and return the module */
    if (!ompi_process_info.seed) {

      /* Return a module (choose an arbitrary, positive priority --
         it's only relevant compared to other ns components).  If
         we're not the seed, then we don't want to be selected, so
         return NULL. */

      *priority = 10;

      /* We allow multi user threads but don't have any hidden threads */

      *allow_multi_user_threads = true;
      *have_hidden_threads = false;

      /* define the replica for us to use - for now, use only the seed */
      mca_gpr_my_replica = ompi_name_server.create_process_name(0,0,0);

      /* Return the module */

      initialized = true;
      return &mca_gpr_proxy;
    } else {
      return NULL;
    }
}

/*
 * finalize routine
 */
int mca_gpr_proxy_finalize(void)
{

  if (initialized) {
    initialized = false;
  }

  /* All done */

  return OMPI_SUCCESS;
}

/* -*- C -*-
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI Name Server
 *
 * The Open MPI Name Server provides unique name ranges for processes
 * within the universe. Each universe will have one name server
 * running within the seed daemon.  This is done to prevent the
 * inadvertent duplication of names.
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "include/constants.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/ns/base/base.h"
#include "ns_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_ns_base_component_t mca_ns_proxy_component = {
  {
    MCA_NS_BASE_VERSION_1_0_0,

    "proxy", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_ns_proxy_open,  /* module open */
    mca_ns_proxy_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_ns_proxy_init,    /* module init */
  mca_ns_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static mca_ns_base_module_t mca_ns_proxy = {
    ns_base_create_cellid,
    ns_base_create_jobid,
    ns_base_create_process_name,
    ns_base_reserve_range,
    ns_base_free_name,
    ns_base_get_proc_name_string,
    ns_base_get_vpid_string,
    ns_base_get_jobid_string,
    ns_base_get_cellid_string,
    ns_base_get_vpid,
    ns_base_get_jobid,
    ns_base_get_cellid,
    ns_base_compare
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/*
 * globals needed within proxy component
 */


/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int mca_ns_proxy_open(void)
{
    return OMPI_SUCCESS;
}

/*
 * ditto for this one
 */
int mca_ns_proxy_close(void)
{
    return OMPI_SUCCESS;
}

mca_ns_base_module_t* mca_ns_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority)
{
    /* If we're NOT the seed, then we want to be selected, so do all
       the setup and return the module */
    ompi_output(mca_ns_base_output, "ns_proxy: entered init\n");
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
      mca_ns_my_replica.cellid = 0;
      mca_ns_my_replica.jobid = 0;
      mca_ns_my_replica.vpid = 0;

      /* Return the module */

      initialized = true;
      return &mca_ns_proxy;
    } else {
      return NULL;
    }
}

/*
 * finalize routine
 */
int mca_ns_proxy_finalize(void)
{
  /* free all tracking storage, but only if this component was initialized */

  if (initialized) {
    initialized = false;
  }

  /* All done */

  return OMPI_SUCCESS;
}

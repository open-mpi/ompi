/* -*- C -*-
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI Name Server
 *
 * The Open MPI Name Server provides unique name ranges for processes within the
 * universe. Each universe will have one name server running within the seed daemon.
 * This is done to prevent the inadvertent duplication of names.
 *
 */

/*
 * includes
 */
#include "ompi_config.h"
#include "include/constants.h"
#include "mca/mca.h"
#include "mca/ns/ns.h"
#include "ns_replica.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_ns_1_0_0_t mca_ns_replica_module = {
  {
    MCA_NS_BASE_VERSION_1_0_0,

    "replica", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_ns_replica_open,  /* module open */
    mca_ns_replica_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_ns_replica_init,    /* module init */
  mca_ns_replica_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
mca_ns_t mca_ns_replica = {
    ns_replica_create_cellid,
    ns_replica_create_jobid,
    ns_replica_create_process_name,
    ns_replica_reserve_range,
    ns_replica_free_name,
    ns_replica_get_proc_name_string,
    ns_replica_get_vpid_string,
    ns_replica_get_jobid_string,
    ns_replica_get_cellid_string,
    ns_replica_get_vpid,
    ns_replica_get_jobid,
    ns_replica_get_cellid,
    ns_replica_compare
};

/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int
mca_ns_replica_open(void)
{
    return OMPI_SUCCESS;
}

/*
 * ditto for this one
 */
int
mca_ns_replica_close(void)
{
    return OMPI_SUCCESS;
}

mca_ns_t* mca_ns_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    /* should I be a replica or not? */

    /* if no, return NULL */

    /* if yes, setup storage for tracking names and return &mca_ns_replica */
}

/*
 * finalize routine
 */
int
mca_ns_replica_finalize(void)
{
    /* free all tracking storage */

    /* return OMPI_SUCCESS */
}

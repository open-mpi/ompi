/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/ns/base/base.h"
#include "ns_proxy.h"

/**
 * globals
 */

/*
 * functions
 */

ompi_process_id_t ns_proxy_create_cellid(void)
{
  /* JMS fill in here */
  return 1;
}

ompi_process_id_t ns_proxy_create_jobid(void)
{
  /* JMS fill in here */
  return 0;
}


ompi_process_id_t ns_proxy_reserve_range(ompi_process_id_t job, ompi_process_id_t range)
{
  /* JMS fill in here */
  return 0;
}


int ns_proxy_free_name(ompi_process_name_t* name)
{
    return OMPI_SUCCESS;
}

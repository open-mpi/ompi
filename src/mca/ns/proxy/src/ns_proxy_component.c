/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
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
#include "mca/base/mca_base_param.h"
#include "mca/ns/base/base.h"
#include "ns_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
OMPI_COMP_EXPORT mca_ns_base_component_t mca_ns_proxy_component = {
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
    mca_ns_proxy_create_cellid,
    mca_ns_base_assign_cellid_to_process,
    mca_ns_proxy_create_jobid,
    mca_ns_base_create_process_name,
    mca_ns_base_copy_process_name,
    mca_ns_base_convert_string_to_process_name,
    mca_ns_proxy_reserve_range,
    mca_ns_base_free_name,
    mca_ns_base_get_proc_name_string,
    mca_ns_base_get_vpid_string,
    mca_ns_base_get_jobid_string,
    mca_ns_base_convert_jobid_to_string,
    mca_ns_base_convert_string_to_jobid,
    mca_ns_base_get_cellid_string,
    mca_ns_base_get_vpid,
    mca_ns_base_get_jobid,
    mca_ns_base_get_cellid,
    mca_ns_base_compare,
    mca_ns_base_pack_name,
    mca_ns_base_unpack_name,
    mca_ns_base_pack_jobid,
    mca_ns_base_unpack_jobid
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/*
 * globals needed within proxy component
 */

ompi_process_name_t* mca_ns_my_replica=NULL;
int mca_ns_proxy_debug=0;

/*
 * Open the proxy component and obtain the name of my replica.
 */
int mca_ns_proxy_open(void)
{
    int id;

    id = mca_base_param_register_int("ns", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &mca_ns_proxy_debug);

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
    /* If we are NOT to host a replica, then we want to be selected, so do all
       the setup and return the module */
    /*    ompi_output(mca_ns_base_output, "ns_proxy: entered init\n"); */
    if (NULL != ompi_process_info.ns_replica) {

	/* Return a module (choose an arbitrary, positive priority --
	   it's only relevant compared to other ns components).  If
	   we're not the seed, then we don't want to be selected, so
	   return NULL. */

	*priority = 10;

	/* We allow multi user threads but don't have any hidden threads */

	*allow_multi_user_threads = true;
	*have_hidden_threads = false;

	/* define the replica for us to use */
	/* default to seed for now */
	mca_ns_my_replica = mca_ns_proxy.copy_process_name(ompi_process_info.ns_replica);
	if (NULL == mca_ns_my_replica) {  /* can't operate */
	    return NULL;
	}

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
    if (mca_ns_proxy_debug) {
	ompi_output(0, "finalizing ns proxy");
    }

  /* free all tracking storage, but only if this component was initialized */

  if (initialized) {
    initialized = false;
  }

  /* All done */

  return OMPI_SUCCESS;
}

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/bproc.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "include/constants.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "svc_bproc_soh.h"


mca_svc_base_module_t mca_svc_bproc_soh_module = {
    mca_svc_bproc_soh_module_init,
    mca_svc_bproc_soh_module_fini
};


/**
 *  Process a BProc update notice
 */

static void mca_svc_bproc_soh_cbfunc()
{

}


/**
 * Register a callback to receive BProc update notifications
 */

int mca_svc_bproc_soh_module_init(mca_svc_base_module_t* module)
{
	bool registration_successful=true;  /* added strictly to allow compilation
	- should be removed by Greg/Nathan */
	
	bproc_node_info_t node_info;
	int node_num;
	char *segment, *jobid_string;
	
	jobid_string = ompi_name_server.get_jobid_string(ompi_rte_get_self());
	asprintf(&segment, "%s-bproc", OMPI_RTE_VM_STATUS_SEGMENT);
	 		
	/* Greg/Nathan - we need to initialize a registry segment that
	 * has info from each node on the BProc cluster. From what I read
	 * in the BProc documentation, we want each process to call this
	 * function and add that info to our segment. Please feel free
	 * to correct this info if incorrect...
	 */
	 node_num = bproc_currnode();
	 

	/* Greg/Nathan - this is where you need to add code so that
	 * BProc will call you back whenever there is a change
	 * or info that you want to get. I have named the callback
	 * function "mca_svc_bproc_soh_cbfunc".
	 */
	 if (registration_successful) {
	 	return OMPI_SUCCESS;
	 } else {
	 	return OMPI_ERROR;
	 }
}


/**
 *  Cleanup
 */

int mca_svc_bproc_soh_module_fini(mca_svc_base_module_t* module)
{
	/* Greg/Nathan - all you need to do here is de-register the
	 * callback from BProc.
	 */
    return OMPI_SUCCESS;
}


/*
 * $HEADER$
 */
/** @file:
 *
 */
#include <stdio.h>

#include "ompi_config.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/ns/base/base.h"
#include "ns_replica.h"

/**
 * globals
 */

/*
 * functions
 */

ompi_process_id_t ns_replica_create_cellid(void)
{
    ompi_output(mca_ns_base_output, "entered replica create cellid\n");
    if ((OMPI_NAME_SERVICE_MAX-1) >= mca_ns_replica_last_used_cellid) {
	ompi_output(mca_ns_base_output, "create cellid: last_used %d\n", mca_ns_replica_last_used_cellid);

	mca_ns_replica_last_used_cellid = mca_ns_replica_last_used_cellid + 1;
	return(mca_ns_replica_last_used_cellid);
    } else {
	return(1);
    }
}

ompi_process_id_t ns_replica_create_jobid(void)
{
    mca_ns_replica_name_tracker_t *new;

    if ((OMPI_NAME_SERVICE_MAX-1) >= mca_ns_replica_last_used_jobid) {
	mca_ns_replica_last_used_jobid = mca_ns_replica_last_used_jobid + 1;
	new = OBJ_NEW(mca_ns_replica_name_tracker_t);
	new->job = mca_ns_replica_last_used_jobid;
	new->last_used_vpid = 0;
	ompi_list_append(&mca_ns_replica_name_tracker, &new->item);
	return(mca_ns_replica_last_used_jobid);
    } else {
	return(0);
    }
}


ompi_process_id_t ns_replica_reserve_range(ompi_process_id_t job, ompi_process_id_t range)
{
    mca_ns_replica_name_tracker_t *ptr;
    ompi_process_id_t start;

    for (ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_first(&mca_ns_replica_name_tracker);
	 ptr != (mca_ns_replica_name_tracker_t*)ompi_list_get_end(&mca_ns_replica_name_tracker);
	 ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_next(ptr)) {
	if (job == ptr->job) { /* found the specified job */
	    if ((OMPI_NAME_SERVICE_MAX-range-1) >= ptr->last_used_vpid) {  /* requested range available */
		start = ptr->last_used_vpid + 1;
		ptr->last_used_vpid = ptr->last_used_vpid + range;
		return(start);
	    }
	}
    }
    return(0);
}


int ns_replica_free_name(ompi_process_name_t* name)
{
    return OMPI_SUCCESS;
}

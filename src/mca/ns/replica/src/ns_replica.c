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

mca_ns_base_cellid_t ns_replica_create_cellid(void)
{
    if ((MCA_NS_BASE_CELLID_MAX-2) >= mca_ns_replica_last_used_cellid) {
	mca_ns_replica_last_used_cellid = mca_ns_replica_last_used_cellid + 1;
	return(mca_ns_replica_last_used_cellid);
    } else {
	return MCA_NS_BASE_CELLID_MAX;
    }
}

mca_ns_base_jobid_t ns_replica_create_jobid(void)
{
    mca_ns_replica_name_tracker_t *new;

    if ((MCA_NS_BASE_JOBID_MAX-2) >= mca_ns_replica_last_used_jobid) {
	mca_ns_replica_last_used_jobid = mca_ns_replica_last_used_jobid + 1;
	new = OBJ_NEW(mca_ns_replica_name_tracker_t);
	new->job = mca_ns_replica_last_used_jobid;
	new->last_used_vpid = 0;
	ompi_list_append(&mca_ns_replica_name_tracker, &new->item);
	return(mca_ns_replica_last_used_jobid);
    } else {
	return MCA_NS_BASE_JOBID_MAX;
    }
}


mca_ns_base_vpid_t ns_replica_reserve_range(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range)
{
    mca_ns_replica_name_tracker_t *ptr;
    mca_ns_base_vpid_t start;

    for (ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_first(&mca_ns_replica_name_tracker);
	 ptr != (mca_ns_replica_name_tracker_t*)ompi_list_get_end(&mca_ns_replica_name_tracker);
	 ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_next(ptr)) {
	if (job == ptr->job) { /* found the specified job */
	    if ((MCA_NS_BASE_VPID_MAX-range-2) >= ptr->last_used_vpid) {  /* requested range available */
		start = ptr->last_used_vpid + 1;
		ptr->last_used_vpid = ptr->last_used_vpid + range;
		return(start);
	    }
	}
    }
    return MCA_NS_BASE_VPID_MAX;
}


int ns_replica_free_name(ompi_process_name_t* name)
{
    return OMPI_SUCCESS;
}

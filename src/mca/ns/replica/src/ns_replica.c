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
/** @file:
 *
 */
#include "ompi_config.h"
#include <stdio.h>

#include "threads/mutex.h"

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

mca_ns_base_cellid_t mca_ns_replica_create_cellid(void)
{
    OMPI_THREAD_LOCK(&mca_ns_replica_mutex);

    if ((MCA_NS_BASE_CELLID_MAX-2) >= mca_ns_replica_last_used_cellid) {
	mca_ns_replica_last_used_cellid = mca_ns_replica_last_used_cellid + 1;
	OMPI_THREAD_UNLOCK(&mca_ns_replica_mutex);
	return(mca_ns_replica_last_used_cellid);
    } else {
	OMPI_THREAD_UNLOCK(&mca_ns_replica_mutex);
	return MCA_NS_BASE_CELLID_MAX;
    }
}

mca_ns_base_jobid_t mca_ns_replica_create_jobid(void)
{
    mca_ns_replica_name_tracker_t *new_nt;

    OMPI_THREAD_LOCK(&mca_ns_replica_mutex);

    if ((MCA_NS_BASE_JOBID_MAX-2) >= mca_ns_replica_last_used_jobid) {
	mca_ns_replica_last_used_jobid = mca_ns_replica_last_used_jobid + 1;
	new_nt = OBJ_NEW(mca_ns_replica_name_tracker_t);
	new_nt->job = mca_ns_replica_last_used_jobid;
	new_nt->last_used_vpid = 0;
	ompi_list_append(&mca_ns_replica_name_tracker, &new_nt->item);
	OMPI_THREAD_UNLOCK(&mca_ns_replica_mutex);
	return(mca_ns_replica_last_used_jobid);
    } else {
	OMPI_THREAD_UNLOCK(&mca_ns_replica_mutex);
	return MCA_NS_BASE_JOBID_MAX;
    }
}


mca_ns_base_vpid_t
mca_ns_replica_reserve_range(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range)
{
    mca_ns_replica_name_tracker_t *ptr;
    mca_ns_base_vpid_t start;

    OMPI_THREAD_LOCK(&mca_ns_replica_mutex);

    for (ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_first(&mca_ns_replica_name_tracker);
	 ptr != (mca_ns_replica_name_tracker_t*)ompi_list_get_end(&mca_ns_replica_name_tracker);
	 ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_next(ptr)) {
	if (job == ptr->job) { /* found the specified job */
	    if ((MCA_NS_BASE_VPID_MAX-range-2) >= ptr->last_used_vpid) {  /* requested range available */
		start = ptr->last_used_vpid;
		if (0 == job && start == 0) { /* vpid=0 reserved for job=0 */
		    start = 1;
		}
		ptr->last_used_vpid = start + range;
		OMPI_THREAD_UNLOCK(&mca_ns_replica_mutex);
		return(start);
	    }
	}
    }
    OMPI_THREAD_UNLOCK(&mca_ns_replica_mutex);
    return MCA_NS_BASE_VPID_MAX;
}

mca_ns_base_vpid_t
mca_ns_replica_get_allocated_vpids(mca_ns_base_jobid_t job)
{
    mca_ns_replica_name_tracker_t *ptr;

    OMPI_THREAD_LOCK(&mca_ns_replica_mutex);

    for (ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_first(&mca_ns_replica_name_tracker);
         ptr != (mca_ns_replica_name_tracker_t*)ompi_list_get_end(&mca_ns_replica_name_tracker);
         ptr = (mca_ns_replica_name_tracker_t*)ompi_list_get_next(ptr)) {
         if (job == ptr->job) { /* found the specified job */
             return ptr->last_used_vpid;
         }
     }
    OMPI_THREAD_UNLOCK(&mca_ns_replica_mutex);
    return MCA_NS_BASE_VPID_MAX;
}

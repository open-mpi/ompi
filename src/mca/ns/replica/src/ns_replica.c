/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/ns/base/base.h"
#include "ns_replica.h"

/**
 * globals
 */
/* constructor - used to initialize state of name instance */
static void ompi_name_construct(ompi_process_name_t* name)
{
    name->cellid = 0;
    name->jobid = 0;
    name->vpid = 0;
}

/* destructor - used to free any resources held by instance */
static void ompi_name_destructor(ompi_process_name_t* name)
{
}

OBJ_CLASS_INSTANCE(
		   ompi_process_name_t,   /* type name */
		   ompi_object_t,         /* parent "class" name */
		   ompi_name_construct,   /* constructor */
		   ompi_name_destructor);  /* destructor */

/*
 * functions
 */

ompi_process_id_t ns_replica_create_cellid(void)
{
    if ((OMPI_NAME_SERVICE_MAX-1) < last_used_cellid) {
	last_used_cellid = last_used_cellid + 1;
	return(last_used_cellid);
    } else {
	return(0);
    }
}

ompi_process_id_t ns_replica_create_jobid(void)
{
    ompi_name_tracker_t *new;

    if ((OMPI_NAME_SERVICE_MAX-1) < last_used_jobid) {
	last_used_jobid = last_used_jobid + 1;
	new = OBJ_NEW(ompi_name_tracker_t);
	new->job = last_used_jobid;
	new->last_used_vpid = 0;
	ompi_list_append(&ompi_name_tracker, &new->item);
	return(last_used_jobid);
    } else {
	return(0);
    }
}


ompi_process_id_t ns_replica_reserve_range(ompi_process_id_t job, ompi_process_id_t range)
{
    ompi_name_tracker_t *ptr;
    ompi_process_id_t start;

    for (ptr = (ompi_name_tracker_t*)ompi_list_get_first(&ompi_name_tracker);
	 ptr != (ompi_name_tracker_t*)ompi_list_get_end(&ompi_name_tracker);
	 ptr = (ompi_name_tracker_t*)ompi_list_get_next(ptr)) {
	if (job == ptr->job) { /* found the specified job */
	    if ((OMPI_NAME_SERVICE_MAX-range-1) > ptr->last_used_vpid) {  /* requested range available */
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

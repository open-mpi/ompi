/* -*- C -*-
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"


void mca_gpr_replica_silent_mode_on(void)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_silent_mode = true;
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}

void mca_gpr_replica_silent_mode_off(void)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_silent_mode = false;
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}

void mca_gpr_replica_notify_on(ompi_registry_notify_id_t sub_number)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_notify_on_nl(ompi_rte_get_self(), sub_number);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}

void mca_gpr_replica_notify_on_nl(ompi_process_name_t *proc,
				  ompi_registry_notify_id_t sub_number)
{
    mca_gpr_replica_notify_off_t *ptr, *nextptr;

    for (ptr = (mca_gpr_replica_notify_off_t*)ompi_list_get_first(&mca_gpr_replica_notify_off_list);
	 ptr != (mca_gpr_replica_notify_off_t*)ompi_list_get_end(&mca_gpr_replica_notify_off_list);
	 ) {
	nextptr = (mca_gpr_replica_notify_off_t*)ompi_list_get_next(ptr);
	if (0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, ptr->proc, proc)) {
	    if ((OMPI_REGISTRY_NOTIFY_ID_MAX == sub_number) ||
		(ptr->sub_number == sub_number)) {
		ompi_list_remove_item(&mca_gpr_replica_notify_off_list, &ptr->item);
		OBJ_RELEASE(ptr);
	    }
	}
	ptr = nextptr;
    }
}


void mca_gpr_replica_notify_off(ompi_registry_notify_id_t sub_number)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_notify_off_nl(ompi_rte_get_self(), sub_number);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


void mca_gpr_replica_notify_off_nl(ompi_process_name_t *proc,
				   ompi_registry_notify_id_t sub_number)
{
    mca_gpr_replica_notify_off_t *ptr;

    /* check to see if this is already on the list - return if so */
    for (ptr = (mca_gpr_replica_notify_off_t*)ompi_list_get_first(&mca_gpr_replica_notify_off_list);
	 ptr != (mca_gpr_replica_notify_off_t*)ompi_list_get_end(&mca_gpr_replica_notify_off_list);
	 ptr = (mca_gpr_replica_notify_off_t*)ompi_list_get_next(ptr)) {
	if (0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, ptr->proc, proc)) {
	    if (OMPI_REGISTRY_NOTIFY_ID_MAX == sub_number) { /* if wild card, remove all others on list */
		ompi_list_remove_item(&mca_gpr_replica_notify_off_list, &ptr->item);
		OBJ_RELEASE(ptr);
	    } else if (ptr->sub_number == sub_number) {
		return;
	    }
	}
    }

    /* either wild card or not already on list - add it */
    ptr = OBJ_NEW(mca_gpr_replica_notify_off_t);
    ptr->sub_number = sub_number;
    ptr->proc = ompi_name_server.copy_process_name(proc);
    ompi_list_append(&mca_gpr_replica_notify_off_list, &ptr->item);
}


void mca_gpr_replica_triggers_active(mca_ns_base_jobid_t jobid)
{

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_triggers_active_nl(jobid);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


void mca_gpr_replica_triggers_active_nl(mca_ns_base_jobid_t jobid)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_notify_off_t *ptr, *nextptr;

    /* traverse the registry */
    /* enable triggers on segments from this jobid */
    for (seg = (mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
	 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
	 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {

	if (seg->owning_job == jobid) {
	    seg->triggers_active = true;
	}
    }

    /* check the list of process names with notification turned off
     * and turn on those from this jobid
     */
    for (ptr = (mca_gpr_replica_notify_off_t*)ompi_list_get_first(&mca_gpr_replica_notify_off_list);
	 ptr != (mca_gpr_replica_notify_off_t*)ompi_list_get_end(&mca_gpr_replica_notify_off_list);
	 ) {

	nextptr = (mca_gpr_replica_notify_off_t*)ompi_list_get_next(ptr);

	if (jobid == ompi_name_server.get_jobid(ptr->proc)) {
	    ompi_list_remove_item(&mca_gpr_replica_notify_off_list, &ptr->item);
	    OBJ_RELEASE(ptr);
	}
	ptr = nextptr;
    }
}


void mca_gpr_replica_triggers_inactive(mca_ns_base_jobid_t jobid)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_triggers_inactive_nl(jobid);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


void mca_gpr_replica_triggers_inactive_nl(mca_ns_base_jobid_t jobid)
{
    mca_gpr_replica_segment_t *seg;

    /* traverse the registry */
    /* disable triggers on segments from this jobid */
    for (seg = (mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
	 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
	 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {

	if (seg->owning_job == jobid) {
	    seg->triggers_active = false;
	}
    }

}


int mca_gpr_replica_assume_ownership(char *segment)
{
    int rc;
    mca_ns_base_jobid_t jobid;
    mca_gpr_replica_segment_t *seg;

    /* protect against error */
    if (NULL == segment) {
	return OMPI_ERROR;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    jobid = ompi_name_server.get_jobid(ompi_rte_get_self());

    /* find the segment */
    seg = mca_gpr_replica_find_seg(true, segment, jobid);
    if (NULL == seg) {  /* segment couldn't be found or created */
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	return OMPI_ERROR;
    }

    rc = mca_gpr_replica_assume_ownership_nl(seg, jobid);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    return rc;
}

int mca_gpr_replica_assume_ownership_nl(mca_gpr_replica_segment_t *seg, mca_ns_base_jobid_t jobid)
{

    seg->owning_job = jobid;

    return OMPI_SUCCESS;
}

/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"


void mca_gpr_replica_cleanup_job(mca_ns_base_jobid_t jobid)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_cleanup_job_nl(jobid);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


void mca_gpr_replica_cleanup_job_nl(mca_ns_base_jobid_t jobid)
{
    mca_gpr_replica_segment_t *seg, *next_seg;
    mca_gpr_replica_trigger_list_t *trig, *next_trig;

    /* traverse the registry */
    for (seg = (mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
	 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);) {

	next_seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg);

	if (jobid == seg->owning_job) {  /* this is a segment associated with this jobid - remove it */

	    mca_gpr_replica_delete_segment_nl(seg);

	} else {  /* check this seg subscriptions/synchros with recipients from this jobid */
	    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
		 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);) {

		next_trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig);

		if (trig->owning_job == jobid) {
		    mca_gpr_replica_remove_trigger(trig->local_idtag);
		}
		trig = next_trig;
	    }
	}
	seg = next_seg;
    }
}


void mca_gpr_replica_cleanup_proc(bool purge, ompi_process_name_t *proc)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_cleanup_proc_nl(purge, ompi_rte_get_self());
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


void mca_gpr_replica_cleanup_proc_nl(bool purge, ompi_process_name_t *proc)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_trigger_list_t *trig;
    char *procname;
    mca_ns_base_jobid_t jobid;

    procname = ompi_name_server.get_proc_name_string(proc);
    jobid = ompi_name_server.get_jobid(proc);

    /* search all segments for this process name - remove all references
     */
    for (seg = (mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
	 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
	 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {

	if (jobid == seg->owning_job) {
	    /* adjust any startup synchro and/or shutdown synchros owned
	     * by the associated jobid by one.
	     */
	    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
		 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
		 trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig)) {
		if ((OMPI_REGISTRY_SYNCHRO_MODE_STARTUP & trig->synch_mode) ||
		    (OMPI_REGISTRY_SYNCHRO_MODE_SHUTDOWN & trig->synch_mode)) {
		    trig->count--;
		}
	    }
	    mca_gpr_replica_check_synchros(seg);
	}

	if (purge) {
	    /* remove name from the dictionary and set all associated object keys to invalid */
	    mca_gpr_replica_delete_key(seg, procname);
	}
    }

    if (purge) {
	/* purge all subscriptions with this process as recipient */
	mca_gpr_replica_purge_subscriptions(proc);
    }

    return;
}

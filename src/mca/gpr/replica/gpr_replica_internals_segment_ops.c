/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"

mca_gpr_replica_segment_t *mca_gpr_replica_define_segment(char *segment,
							  mca_ns_base_jobid_t jobid)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_key_t key;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] define_segment: name %s jobid %d",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), segment, (int)jobid);
    }

    key = mca_gpr_replica_define_key(NULL, segment);
    if (MCA_GPR_REPLICA_KEY_MAX == key) {  /* got some kind of error code */
 	return NULL;
    }

    /* need to add the segment to the registry */
    seg = OBJ_NEW(mca_gpr_replica_segment_t);
    seg->name = strdup(segment);
    seg->key = key;
    seg->owning_job = jobid;
    seg->triggers_active = false;
    ompi_list_append(&mca_gpr_replica_head.registry, &seg->item);


    return seg;
}


mca_gpr_replica_segment_t *mca_gpr_replica_find_seg(bool create, char *segment,
						    mca_ns_base_jobid_t jobid)
{
    mca_gpr_replica_keytable_t *ptr_seg;
    mca_gpr_replica_segment_t *seg;


    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	 ptr_seg != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	 ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    /* search mca_gpr_replica_head to find segment */
	    for (seg=(mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
		 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
		 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {
		if(seg->key == ptr_seg->key) {
		    return(seg);
		}
	    }
	}
    }


    if (create) {
	/* didn't find the dictionary entry - create it */
	return mca_gpr_replica_define_segment(segment, jobid);
    }
    return NULL;  /* don't create it - just return NULL */
}

int mca_gpr_replica_empty_segment(mca_gpr_replica_segment_t *seg)
{
    mca_gpr_replica_core_t *ptr;
    mca_gpr_replica_keytable_t *keytab;
    mca_gpr_replica_keylist_t *keylst;
    mca_gpr_replica_trigger_list_t *trig;

    /* need to free memory from each entry - remove_last returns pointer to the entry */
    /* need to purge all subscriptions/synchros from notify tracker, and delete from segment */

    /* empty the segment's registry */
    while (NULL != (ptr = (mca_gpr_replica_core_t*)ompi_list_remove_first(&seg->registry_entries))) {
	OBJ_RELEASE(ptr);
    }

    /* empty the segment's dictionary */
    while (NULL != (keytab = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(&seg->keytable))) {
	OBJ_RELEASE(keytab);
    }

    /* empty the list of free keys */
    while (NULL != (keylst = (mca_gpr_replica_keylist_t*)ompi_list_remove_first(&seg->freekeys))) {
	OBJ_RELEASE(keylst);
    }

    /* empty the list of triggers */
    while (NULL != (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_remove_first(&seg->triggers))) {
	OBJ_RELEASE(trig);
    }

    /* now remove segment from global registry */
    ompi_list_remove_item(&mca_gpr_replica_head.registry, &seg->item);
    OBJ_RELEASE(seg);


    return OMPI_SUCCESS;
}


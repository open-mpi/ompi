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

static ompi_process_name_t* mca_gpr_replica_find_recipient(ompi_registry_notify_id_t idtag);

static void mca_gpr_replica_dump_load_string(ompi_buffer_t buffer, char *tmp);

void mca_gpr_replica_dump(int output_id)
{
    ompi_buffer_t buffer;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr_replica_dump: entered for output on %d",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), output_id);
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    if (mca_gpr_replica_compound_cmd_mode) {
	mca_gpr_base_pack_dump(mca_gpr_replica_compound_cmd);
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	return;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&buffer, 0)) {
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	return;
    }

    mca_gpr_replica_dump_nl(buffer);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    mca_gpr_base_print_dump(buffer, output_id);
    ompi_buffer_free(buffer);

}


void mca_gpr_replica_dump_nl(ompi_buffer_t buffer)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_core_t *reg;
    mca_gpr_replica_key_t *key;
    mca_gpr_replica_trigger_list_t *trig;
    ompi_process_name_t *recip;
    char *token, **tokptr;
    int num_objects, num_trigs, cnt;
    uint i;
    char *tmp_out;

    /* loop through all segments */
    for (seg = (mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
	 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
	 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {

	asprintf(&tmp_out, "GPR Dump for Segment: %s\tOwner: %d", seg->name, (int)seg->owning_job);
	mca_gpr_replica_dump_load_string(buffer, tmp_out);

	num_objects = ompi_list_get_size(&seg->registry_entries);
	num_trigs = ompi_list_get_size(&seg->triggers);

	asprintf(&tmp_out, "\tNumber of objects: %d\tNumber of triggers: %d\n", num_objects, num_trigs);
	mca_gpr_replica_dump_load_string(buffer, tmp_out);

	/* loop through all objects and print their tokens */
	cnt = 0;
	for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	     reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	     reg = (mca_gpr_replica_core_t*)ompi_list_get_next(reg)) {

	    asprintf(&tmp_out, "\tInfo for object %d\tObject size: %d", cnt, reg->object_size);
	    mca_gpr_replica_dump_load_string(buffer, tmp_out);

	    /* parse the keys into tokens and print them */
	    for (i=0, key=reg->keys; i < (uint)reg->num_keys; i++, key++) {
		token = mca_gpr_replica_get_token(seg, *key);
		if (NULL == token) { /* key couldn't be found */
		    asprintf(&tmp_out, "\t\tKey num: %d - No entry found for key %X",
			     i, *key);
		} else {
		    asprintf(&tmp_out, "\t\tKey num: %d - Key %d Token: %s",
			     i, *key, token);
		    free(token);
		}
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
	    }
	}

	/* loop through all triggers and print recipient name, type, and associated action */
	cnt = 0;
	for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	     trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
	     trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig)) {


	    if (OMPI_REGISTRY_SYNCHRO_MODE_NONE == trig->synch_mode) {  /* subscription */
		asprintf(&tmp_out, "\tData for trigger %d\tType: SUBSCRIPTION", cnt);
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
		asprintf(&tmp_out, "\t\tAssociated with notify number: %d",trig->local_idtag);
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
		/* find recipient info from notify list */
		recip = mca_gpr_replica_find_recipient(trig->local_idtag);
		if (NULL == recip) {
		    asprintf(&tmp_out, "\tIntended recipient: LOCAL");
		} else {
		    asprintf(&tmp_out, "\tIntended recipient: [%d,%d,%d]", OMPI_NAME_ARGS(*recip));
		}
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
		ompi_pack_string(buffer, "\tActions:");
		if (OMPI_REGISTRY_NOTIFY_MODIFICATION & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_MODIFICATION");
		}
		if (OMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER");
		}
		if (OMPI_REGISTRY_NOTIFY_DELETE_ENTRY & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_DELETE_ENTRY");
		}
		if (OMPI_REGISTRY_NOTIFY_ADD_ENTRY & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_ADD_ENTRY");
		}
		if (OMPI_REGISTRY_NOTIFY_ON_STARTUP & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_ON_STARTUP");
		}
		if (OMPI_REGISTRY_NOTIFY_ON_SHUTDOWN & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_ON_SHUTDOWN");
		}
		if (OMPI_REGISTRY_NOTIFY_PRE_EXISTING & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_PRE_EXISTING");
		}
		if (OMPI_REGISTRY_NOTIFY_INCLUDE_STARTUP_DATA & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_INCLUDE_STARTUP_DATA");
		}
		if (OMPI_REGISTRY_NOTIFY_INCLUDE_SHUTDOWN_DATA & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_INCLUDE_SHUTDOWN_DATA");
		}
		if (OMPI_REGISTRY_NOTIFY_ONE_SHOT & trig->action) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_NOTIFY_ONE_SHOT");
		}

	    } else {  /* synchro */
		asprintf(&tmp_out, "\tData for trigger %d\tType: SYNCHRO", cnt);
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
		asprintf(&tmp_out, "\t\tAssociated with notify number: %d",trig->local_idtag);
		mca_gpr_replica_dump_load_string(buffer, tmp_out);

		/* find recipient info from notify list */
		recip = mca_gpr_replica_find_recipient(trig->local_idtag);
		if (NULL == recip) {
		    asprintf(&tmp_out, "\tIntended recipient: LOCAL");
		} else {
		    asprintf(&tmp_out, "\tIntended recipient: [%d,%d,%d]", OMPI_NAME_ARGS(*recip));
		}
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
		ompi_pack_string(buffer, "\tSynchro Mode:");
		if (OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_ASCENDING");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_DESCENDING");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_LEVEL & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_LEVEL");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_GT_EQUAL & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_GT_EQUAL");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_LT_EQUAL & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_LT_EQUAL");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_CONTINUOUS & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_CONTINUOUS");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_STARTUP & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_STARTUP");
		}
		if (OMPI_REGISTRY_SYNCHRO_MODE_SHUTDOWN & trig->synch_mode) {
		    ompi_pack_string(buffer, "\t\tOMPI_REGISTRY_SYNCHRO_MODE_SHUTDOWN");
		}
		asprintf(&tmp_out, "\tTrigger level: %d\tCurrent count: %d", trig->trigger, trig->count);
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
		asprintf(&tmp_out, "\tTransition status: %d", trig->above_below);
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
	    }
	    asprintf(&tmp_out, "\tAddressing mode: %X\tNumber of tokens: %d", trig->addr_mode, trig->num_keys);
	    mca_gpr_replica_dump_load_string(buffer, tmp_out);

	    for (i=0, tokptr=trig->tokens; i < trig->num_keys; i++, tokptr++) {
		asprintf(&tmp_out, "\t\tToken: %s", *tokptr);
		mca_gpr_replica_dump_load_string(buffer, tmp_out);
	    }
	    ompi_pack_string(buffer, "\n");
	    cnt++;
	}
	ompi_pack_string(buffer, "\n\n");
    }

    return;
}


static ompi_process_name_t *mca_gpr_replica_find_recipient(ompi_registry_notify_id_t idtag)
{
    mca_gpr_replica_notify_request_tracker_t *trackptr;

    for (trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
	 trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker);
	 trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {
	if (trackptr->local_idtag == idtag) {
	    return trackptr->requestor;
	}
    }
    return NULL;
}


static void mca_gpr_replica_dump_load_string(ompi_buffer_t buffer, char *tmp)
{
    ompi_pack_string(buffer, tmp);
    free(tmp);
}

/*
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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"

ompi_registry_notify_id_t
mca_gpr_replica_synchro(ompi_registry_synchro_mode_t synchro_mode,
			ompi_registry_mode_t addr_mode,
			char *segment, char **tokens, int trigger,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    int rc;
    ompi_registry_notify_id_t local_idtag;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_key_t *keys;
    int num_keys;

    /* protect against errors */
    if (NULL == segment) {
	return OMPI_REGISTRY_NOTIFY_ID_MAX;
    }

    seg = mca_gpr_replica_find_seg(true, segment, ompi_name_server.get_jobid(ompi_rte_get_self()));
    if (NULL == seg) { /* segment couldn't be found */
	return OMPI_REGISTRY_NOTIFY_ID_MAX;
    }


    if (mca_gpr_replica_compound_cmd_mode) {
	mca_gpr_base_pack_synchro(mca_gpr_replica_compound_cmd,
				  synchro_mode, addr_mode,
				  segment, tokens, trigger);

	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	/* enter request on notify tracking system */
	local_idtag = mca_gpr_replica_enter_notify_request(seg, OMPI_REGISTRY_NOTIFY_NONE,
							   NULL, 0, cb_func, user_tag);

	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

	ompi_pack(mca_gpr_replica_compound_cmd, &local_idtag, 1, OMPI_INT32);

	return local_idtag;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* enter request on notify tracking system */
    local_idtag = mca_gpr_replica_enter_notify_request(seg, OMPI_REGISTRY_NOTIFY_NONE,
						       NULL, 0, cb_func, user_tag);

    /* convert tokens to keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    /* process synchro request */
    rc = mca_gpr_replica_synchro_nl(synchro_mode, addr_mode,
				    seg, keys, num_keys, trigger, local_idtag);

    mca_gpr_replica_check_synchros(seg);

    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    mca_gpr_replica_process_callbacks();

    return local_idtag;
}

int mca_gpr_replica_synchro_nl(ompi_registry_synchro_mode_t synchro_mode,
			       ompi_registry_mode_t addr_mode,
			       mca_gpr_replica_segment_t *seg,
			       mca_gpr_replica_key_t *keys,
			       int num_keys,
			       int trigger,
			       ompi_registry_notify_id_t id_tag)
{
    mca_gpr_replica_trigger_list_t *trig;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: synchro entered on segment %s trigger %d",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), seg->name, trigger);
    }

    /* construct the trigger */
    if (NULL != (trig = mca_gpr_replica_construct_trigger(synchro_mode,
							  OMPI_REGISTRY_NOTIFY_NONE,
							  addr_mode, seg, keys, num_keys,
							  trigger, id_tag))) {
	return OMPI_SUCCESS;
    } else {
	return OMPI_ERROR;
    }
}

int mca_gpr_replica_cancel_synchro(ompi_registry_notify_id_t synch_number)
{
    ompi_registry_notify_id_t rc;

    if (mca_gpr_replica_compound_cmd_mode) {
	return mca_gpr_base_pack_cancel_synchro(mca_gpr_replica_compound_cmd,
						mca_gpr_replica_silent_mode,
						synch_number);
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    rc = mca_gpr_replica_cancel_synchro_nl(synch_number);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    if (OMPI_REGISTRY_NOTIFY_ID_MAX == rc) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

ompi_registry_notify_id_t
mca_gpr_replica_cancel_synchro_nl(ompi_registry_notify_id_t synch_number)
{

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: cancel_synchro entered for synch %d",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), synch_number);
    }

    /* find trigger on replica and remove it - return requestor's id_tag */
    return mca_gpr_replica_remove_trigger(synch_number);

}



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


int mca_gpr_replica_begin_compound_cmd(void)
{
    size_t size;

    OMPI_THREAD_LOCK(&mca_gpr_replica_wait_for_compound_mutex);

    while (mca_gpr_replica_compound_cmd_mode) {
	mca_gpr_replica_compound_cmd_waiting++;
	ompi_condition_wait(&mca_gpr_replica_compound_cmd_condition, &mca_gpr_replica_wait_for_compound_mutex);
	mca_gpr_replica_compound_cmd_waiting--;
    }

    mca_gpr_replica_compound_cmd_mode = true;
    ompi_buffer_size(mca_gpr_replica_compound_cmd, &size);
    if (0 < size) {
	ompi_buffer_free(mca_gpr_replica_compound_cmd);
    }
    ompi_buffer_init(&mca_gpr_replica_compound_cmd, 0);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_wait_for_compound_mutex);
    return OMPI_SUCCESS;
}


int mca_gpr_replica_stop_compound_cmd(void)
{
    size_t size;

    OMPI_THREAD_LOCK(&mca_gpr_replica_wait_for_compound_mutex);

    mca_gpr_replica_compound_cmd_mode = false;
    ompi_buffer_size(mca_gpr_replica_compound_cmd, &size);
    if (0 < size) {
	ompi_buffer_free(mca_gpr_replica_compound_cmd);
    }

    if (mca_gpr_replica_compound_cmd_waiting) {
	ompi_condition_signal(&mca_gpr_replica_compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_wait_for_compound_mutex);
    return OMPI_SUCCESS;
}


ompi_list_t* mca_gpr_replica_exec_compound_cmd(bool return_requested)
{
    ompi_buffer_t results;
    ompi_list_t *return_list=NULL;
    size_t size;
    bool compound_cmd_detected=false;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] Executing compound command",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_wait_for_compound_mutex);

    results = mca_gpr_replica_process_command_buffer(mca_gpr_replica_compound_cmd,
						     NULL,
						     &return_requested,
						     &compound_cmd_detected);

    if (return_requested) {
	/* construct list of compound_value structs */
    } else {
	return_list = NULL;
    }

    ompi_buffer_free(results);

    mca_gpr_replica_compound_cmd_mode = false;
    ompi_buffer_size(mca_gpr_replica_compound_cmd, &size);
    if (0 < size) {
	ompi_buffer_free(mca_gpr_replica_compound_cmd);
    }
    if (mca_gpr_replica_compound_cmd_waiting) {
	ompi_condition_signal(&mca_gpr_replica_compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_wait_for_compound_mutex);

    mca_gpr_replica_process_callbacks();

    return return_list;
}

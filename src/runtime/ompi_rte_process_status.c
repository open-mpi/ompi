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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "util/proc_info.h"
#include "util/sys_info.h"

#include "mca/gpr/base/base.h"

#include "runtime/runtime.h"

ompi_rte_process_status_t *ompi_rte_get_process_status(ompi_process_name_t *proc)
{
    char *segment, *tokens[2];
    ompi_registry_value_t *value;
    ompi_rte_process_status_t *stat_ptr;
    ompi_list_t *returned_list;

    /* setup tokens and segments for this job */
    asprintf(&segment, "%s-%s", OMPI_RTE_JOB_STATUS_SEGMENT, ompi_name_server.get_jobid_string(proc));

    tokens[0] = ompi_name_server.get_proc_name_string(proc);
    tokens[1] = NULL;

    returned_list = ompi_registry.get(OMPI_REGISTRY_XAND, segment, tokens);

    free(segment);
    free(tokens[0]);

    if (NULL != (value = (ompi_registry_value_t*)ompi_list_remove_first(returned_list))) {
	stat_ptr = ompi_rte_unpack_process_status(value);

    return stat_ptr;
    }

    return NULL;
}


int ompi_rte_set_process_status(ompi_rte_process_status_t *status,
				ompi_process_name_t *proc)
{
    char *segment;
    char *tokens[2];
    void *addr;
    int size;
    ompi_buffer_t buffer;

    /* setup keys and segment for this job */
    asprintf(&segment, "%s-%s", OMPI_RTE_JOB_STATUS_SEGMENT, ompi_name_server.get_jobid_string(proc));
    tokens[0] = ompi_name_server.get_proc_name_string(proc);
    tokens[1] = NULL;

    /* create the buffer to store the status information */
    ompi_buffer_init(&buffer, 0);
    ompi_pack(buffer, &status->rank, 1, OMPI_INT32);
    ompi_pack(buffer, &status->local_pid, 1, OMPI_INT32);
    ompi_pack_string(buffer, status->nodename);
    ompi_pack(buffer, &status->status_key, 1, OMPI_PROCESS_STATUS);
    ompi_pack(buffer, &status->exit_code, 1, OMPI_EXIT_CODE);

    /* peek the buffer and resulting size */
    ompi_buffer_get(buffer, &addr, &size);

    ompi_registry.put(OMPI_REGISTRY_XAND | OMPI_REGISTRY_OVERWRITE,
		      segment, tokens, addr, size);

    if ((OMPI_PROC_STOPPED == status->status_key) ||
	(OMPI_PROC_KILLED == status->status_key) ||
	(OMPI_PROC_EXITED == status->status_key)) {
	ompi_registry.cleanup_process(true, proc);  /* purge subscriptions */
    } else if (OMPI_PROC_TERMINATING == status->status_key) {
	ompi_registry.cleanup_process(false, proc);  /* just cleanup - don't purge subs */
    }

    /* cleanup */
    free(tokens[0]);
    free(segment);
    ompi_buffer_free(buffer);

    return OMPI_SUCCESS;
}


ompi_rte_process_status_t
*ompi_rte_unpack_process_status(ompi_registry_value_t *value)
{
    ompi_buffer_t buffer;
    ompi_rte_process_status_t *stat_ptr;

    stat_ptr = (ompi_rte_process_status_t*)malloc(sizeof(ompi_rte_process_status_t));

    /* transfer ownership of registry object to buffer and unpack */
    ompi_buffer_init_preallocated(&buffer, value->object, value->object_size);
    value->object = NULL;
    value->object_size = 0;
    OBJ_RELEASE(value);

    ompi_unpack(buffer, &stat_ptr->rank, 1, OMPI_INT32);
    ompi_unpack(buffer, &stat_ptr->local_pid, 1, OMPI_INT32);
    ompi_unpack_string(buffer, &stat_ptr->nodename);
    ompi_unpack(buffer, &stat_ptr->status_key, 1, OMPI_PROCESS_STATUS);
    ompi_unpack(buffer, &stat_ptr->exit_code, 1, OMPI_EXIT_CODE);

    return stat_ptr;
}

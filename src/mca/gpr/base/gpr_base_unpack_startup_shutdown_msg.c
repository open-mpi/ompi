/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - unpack functions
 *
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "mca/gpr/base/base.h"

ompi_buffer_t
mca_gpr_base_unpack_get_startup_msg(ompi_buffer_t buffer,
				    ompi_list_t *recipients)
{
    mca_gpr_cmd_flag_t command;
    int32_t num_recipients, i;
    ompi_process_name_t proc;
    ompi_name_server_namelist_t *peer;
    ompi_buffer_t msg;
    void *addr;
    int size;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_GET_STARTUP_MSG_CMD != command)) {
	return NULL;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_recipients, 1, OMPI_INT32)) {
	return NULL;
    }

    for (i=0; i<num_recipients; i++) {
	if (OMPI_SUCCESS != ompi_unpack(buffer, &proc, 1, OMPI_NAME)) {
	    return NULL;
	}
	peer = OBJ_NEW(ompi_name_server_namelist_t);
	peer->name = ompi_name_server.copy_process_name(&proc);;
	ompi_list_append(recipients, &peer->item);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&msg, 0)) {
	return NULL;
    }

    ompi_buffer_get(buffer, &addr, &size);
    if (0 < size) {
	ompi_pack(msg, addr, size, OMPI_BYTE);
    }

    return msg;
}


ompi_buffer_t
mca_gpr_base_unpack_get_shutdown_msg(ompi_buffer_t buffer,
				     ompi_list_t *recipients)
{
    mca_gpr_cmd_flag_t command;
    int32_t num_recipients, i;
    ompi_process_name_t proc;
    ompi_name_server_namelist_t *peer;
    ompi_buffer_t msg;
    void *addr;
    int size;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_GET_SHUTDOWN_MSG_CMD != command)) {
	return NULL;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_recipients, 1, OMPI_INT32)) {
	return NULL;
    }

    for (i=0; i<num_recipients; i++) {
	if (OMPI_SUCCESS != ompi_unpack(buffer, &proc, 1, OMPI_NAME)) {
	    return NULL;
	}
	peer = OBJ_NEW(ompi_name_server_namelist_t);
	peer->name = ompi_name_server.copy_process_name(&proc);;
	ompi_list_append(recipients, &peer->item);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&msg, 0)) {
	return NULL;
    }

    ompi_buffer_get(buffer, &addr, &size);
    if (0 < size) {
		ompi_pack(msg, addr, size, OMPI_BYTE);
    }

    return msg;
}

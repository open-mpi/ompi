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
    int32_t num_objects, num_recipients, i;
    ompi_process_name_t proc;
    ompi_name_server_namelist_t *peer;
    ompi_buffer_t msg;
    char *segment=NULL;
    ompi_registry_object_t *data_object;
    ompi_registry_object_size_t data_obj_size;

    if ((OMPI_SUCCESS != ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_GET_STARTUP_MSG_CMD != command)) {
        ompi_output(0, "unpacking startup msg: got bad command %d", (int)command);
        	return NULL;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_recipients, 1, OMPI_INT32)) {
        ompi_output(0, "unpacking startup msg: got bad num recipients");
	return NULL;
    }

    ompi_output(0, "unpacking startup msg: %d recipients", num_recipients);
    
    for (i=0; i<num_recipients; i++) {
	if (OMPI_SUCCESS != ompi_unpack(buffer, &proc, 1, OMPI_NAME)) {
	    return NULL;
	}
	peer = OBJ_NEW(ompi_name_server_namelist_t);
	peer->name = ompi_name_server.copy_process_name(&proc);
    ompi_output(0, "\tproc [%d,%d,%d] added to list as [%d,%d,%d]",
            OMPI_NAME_ARGS(proc), OMPI_NAME_ARGS(*(peer->name)));
	ompi_list_append(recipients, &peer->item);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&msg, 0)) {
	return NULL;
    }

    while (0 < ompi_unpack_string(buffer, &segment)) {
        ompi_output(0, "\transferring data for segment %s", segment);
        ompi_pack_string(msg, segment);
        ompi_unpack(buffer, &num_objects, 1, OMPI_INT32);  /* unpack #data objects */
        ompi_pack(msg, &num_objects, 1, OMPI_INT32);

        if (0 < num_objects) {
            for (i=0; i < num_objects; i++) {
                ompi_unpack(buffer, &data_obj_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE);
                data_object = (ompi_registry_object_t)malloc(data_obj_size);
                ompi_unpack(buffer, data_object, data_obj_size, OMPI_BYTE);
                ompi_pack(msg, &data_obj_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE);
                ompi_pack(msg, data_object, data_obj_size, OMPI_BYTE);
                free(data_object);
            }
        }
        free(segment);
    }

    return msg;
}

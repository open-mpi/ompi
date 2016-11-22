/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_psm2.h"
#include "mtl_psm2_types.h"
#include "psm2.h"
#include "ompi/communicator/communicator.h"
#include "ompi/message/message.h"


int ompi_mtl_psm2_iprobe(struct mca_mtl_base_module_t* mtl,
                              struct ompi_communicator_t *comm,
                              int src,
                              int tag,
                              int *flag,
                              struct ompi_status_public_t *status)
{
    psm2_mq_tag_t mqtag, tagsel;
    psm2_mq_status2_t mqstat;
    psm2_error_t err;

    PSM2_MAKE_TAGSEL(src, tag, comm->c_contextid, mqtag, tagsel);

    err = psm2_mq_iprobe2(ompi_mtl_psm2.mq,
            PSM2_MQ_ANY_ADDR, &mqtag, &tagsel, &mqstat);
    if (err == PSM2_OK) {
	*flag = 1;
	if(MPI_STATUS_IGNORE != status) {
            status->MPI_SOURCE = mqstat.msg_tag.tag1;
            status->MPI_TAG = mqstat.msg_tag.tag0;
            status->_ucount = mqstat.nbytes;

            switch (mqstat.error_code) {
	    case PSM2_OK:
		status->MPI_ERROR = OMPI_SUCCESS;
		break;
	    case PSM2_MQ_TRUNCATION:
		status->MPI_ERROR = MPI_ERR_TRUNCATE;
		break;
	    default:
		status->MPI_ERROR = MPI_ERR_INTERN;
            }
        }

        return OMPI_SUCCESS;
    }
    else if (err == PSM2_MQ_INCOMPLETE) {
	*flag = 0;
	return OMPI_SUCCESS;
    }
    else
	return OMPI_ERROR;
}


int
ompi_mtl_psm2_improbe(struct mca_mtl_base_module_t *mtl,
                     struct ompi_communicator_t *comm,
                     int src,
                     int tag,
                     int *matched,
                     struct ompi_message_t **message,
                     struct ompi_status_public_t *status)
{
    struct ompi_message_t* msg;
    psm2_mq_tag_t mqtag, tagsel;
    psm2_mq_status2_t mqstat;
    psm2_mq_req_t mqreq;
    psm2_error_t err;

    PSM2_MAKE_TAGSEL(src, tag, comm->c_contextid, mqtag, tagsel);

    err = psm2_mq_improbe2(ompi_mtl_psm2.mq,
            PSM2_MQ_ANY_ADDR, &mqtag, &tagsel, &mqreq, &mqstat);
    if (err == PSM2_OK) {

	if(MPI_STATUS_IGNORE != status) {
            status->MPI_SOURCE = mqstat.msg_tag.tag1;
            status->MPI_TAG = mqstat.msg_tag.tag0;
            status->_ucount = mqstat.nbytes;

            switch (mqstat.error_code) {
	    case PSM2_OK:
		status->MPI_ERROR = OMPI_SUCCESS;
		break;
	    case PSM2_MQ_TRUNCATION:
		status->MPI_ERROR = MPI_ERR_TRUNCATE;
		break;
	    default:
		status->MPI_ERROR = MPI_ERR_INTERN;
            }
        }

	msg = ompi_message_alloc();
	if(NULL == msg) {
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}

	msg->comm = comm;
	msg->req_ptr = mqreq;
	msg->peer = mqstat.msg_tag.tag1;
	msg->count = mqstat.nbytes;

	*message = msg;
	*matched = 1;
	return OMPI_SUCCESS;
    } else if(err == PSM2_MQ_INCOMPLETE) {
	*matched = 0;
	*message = MPI_MESSAGE_NULL;
	return OMPI_SUCCESS;
    } else {
	return OMPI_ERROR;
    }
}

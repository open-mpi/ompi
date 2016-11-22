/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/communicator/communicator.h"
#include "opal/datatype/opal_convertor.h"

#include "mtl_psm2.h"
#include "mtl_psm2_types.h"
#include "mtl_psm2_request.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

int
ompi_mtl_psm2_send(struct mca_mtl_base_module_t* mtl,
                 struct ompi_communicator_t* comm,
                 int dest,
                 int tag,
                 struct opal_convertor_t *convertor,
                 mca_pml_base_send_mode_t mode)
{
    psm2_error_t err;
    mca_mtl_psm2_request_t mtl_psm2_request;
    psm2_mq_tag_t mqtag;
    uint32_t flags = 0;
    int ret;
    size_t length;
    ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, dest );
    mca_mtl_psm2_endpoint_t* psm2_endpoint = ompi_mtl_psm2_get_endpoint (mtl, ompi_proc);

    assert(mtl == &ompi_mtl_psm2.super);

    PSM2_MAKE_MQTAG(comm->c_contextid, comm->c_my_rank, tag, mqtag);

    ret = ompi_mtl_datatype_pack(convertor,
                                 &mtl_psm2_request.buf,
                                 &length,
                                 &mtl_psm2_request.free_after);


    mtl_psm2_request.length = length;
    mtl_psm2_request.convertor = convertor;
    mtl_psm2_request.type = OMPI_mtl_psm2_ISEND;

    if (OMPI_SUCCESS != ret) return ret;

    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS)
	flags |= PSM2_MQ_FLAG_SENDSYNC;

    err = psm2_mq_send2(ompi_mtl_psm2.mq,
		      psm2_endpoint->peer_addr,
		      flags,
		      &mqtag,
		      mtl_psm2_request.buf,
		      length);

    if (mtl_psm2_request.free_after) {
	free(mtl_psm2_request.buf);
    }

    return err == PSM2_OK ? OMPI_SUCCESS : OMPI_ERROR;
}

int
ompi_mtl_psm2_isend(struct mca_mtl_base_module_t* mtl,
                  struct ompi_communicator_t* comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode,
                  bool blocking,
                  mca_mtl_request_t * mtl_request)
{
    psm2_error_t psm2_error;
    psm2_mq_tag_t mqtag;
    uint32_t flags = 0;
    int ret;
    mca_mtl_psm2_request_t * mtl_psm2_request = (mca_mtl_psm2_request_t*) mtl_request;
    size_t length;
    ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, dest );
    mca_mtl_psm2_endpoint_t* psm2_endpoint = ompi_mtl_psm2_get_endpoint (mtl, ompi_proc);

    assert(mtl == &ompi_mtl_psm2.super);

    PSM2_MAKE_MQTAG(comm->c_contextid, comm->c_my_rank, tag, mqtag);


    ret = ompi_mtl_datatype_pack(convertor,
                                 &mtl_psm2_request->buf,
                                 &length,
                                 &mtl_psm2_request->free_after);

    mtl_psm2_request->length= length;
    mtl_psm2_request->convertor = convertor;
    mtl_psm2_request->type = OMPI_mtl_psm2_ISEND;

    if (OMPI_SUCCESS != ret) return ret;

    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS)
	flags |= PSM2_MQ_FLAG_SENDSYNC;

    psm2_error = psm2_mq_isend2(ompi_mtl_psm2.mq,
			     psm2_endpoint->peer_addr,
			     flags,
			     &mqtag,
			     mtl_psm2_request->buf,
			     length,
			     mtl_psm2_request,
			     &mtl_psm2_request->psm2_request);

    return psm2_error == PSM2_OK ? OMPI_SUCCESS : OMPI_ERROR;
}

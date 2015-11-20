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
#include "ompi/communicator/communicator.h"
#include "ompi/message/message.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "opal/util/show_help.h"

#include "mtl_psm2.h"
#include "mtl_psm2_types.h"
#include "mtl_psm2_request.h"

int
ompi_mtl_psm2_irecv(struct mca_mtl_base_module_t* mtl,
                  struct ompi_communicator_t *comm,
                  int src,
                  int tag,
                  struct opal_convertor_t *convertor,
                  struct mca_mtl_request_t *mtl_request)
{
    int ret;
    psm2_error_t err;
    mca_mtl_psm2_request_t * mtl_psm2_request = (mca_mtl_psm2_request_t*) mtl_request;
    psm2_mq_tag_t mqtag;
    psm2_mq_tag_t tagsel;
    size_t length;

    ret = ompi_mtl_datatype_recv_buf(convertor,
                                     &mtl_psm2_request->buf,
                                     &length,
                                     &mtl_psm2_request->free_after);

    if (OMPI_SUCCESS != ret) return ret;

    mtl_psm2_request->length = length;
    mtl_psm2_request->convertor = convertor;
    mtl_psm2_request->type = OMPI_mtl_psm2_IRECV;

    PSM2_MAKE_TAGSEL(src, tag, comm->c_contextid, mqtag, tagsel);

    err = psm2_mq_irecv2(ompi_mtl_psm2.mq,
		       PSM2_MQ_ANY_ADDR,
		       &mqtag,
		       &tagsel,
		       0,
		       mtl_psm2_request->buf,
		       length,
		       mtl_psm2_request,
		       &mtl_psm2_request->psm2_request);

    if (err) {
      opal_show_help("help-mtl-psm2.txt",
		     "error posting receive", true,
		     psm2_error_get_string(err),
		     mtl_psm2_request->buf, length);
      return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_psm2_imrecv(struct mca_mtl_base_module_t* mtl,
                    struct opal_convertor_t *convertor,
                    struct ompi_message_t **message,
                    struct mca_mtl_request_t *mtl_request)
{
    mca_mtl_psm2_request_t *mtl_psm2_request =
	    (mca_mtl_psm2_request_t*) mtl_request;
    size_t length;
    psm2_error_t err;
    int ret;

    mtl_psm2_request->psm2_request =
	    (psm2_mq_req_t)(*message)->req_ptr;

    ret = ompi_mtl_datatype_recv_buf(convertor,
                                     &mtl_psm2_request->buf,
                                     &length,
                                     &mtl_psm2_request->free_after);

    if (OMPI_SUCCESS != ret) return ret;

    mtl_psm2_request->length = length;
    mtl_psm2_request->convertor = convertor;
    mtl_psm2_request->type = OMPI_mtl_psm2_IRECV;


    err = psm2_mq_imrecv(ompi_mtl_psm2.mq, 0,
	    mtl_psm2_request->buf, length, mtl_psm2_request,
	    &mtl_psm2_request->psm2_request);

    if(err) {
      opal_show_help("help-mtl-psm2.txt",
		     "error posting receive", true,
		     psm2_error_get_string(err),
		     mtl_psm2_request->buf, length);
      return OMPI_ERROR;
    }

    *message = MPI_MESSAGE_NULL;
    return OMPI_SUCCESS;
}

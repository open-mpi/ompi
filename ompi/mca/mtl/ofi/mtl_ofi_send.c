/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
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
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_ofi.h"
#include "mtl_ofi_types.h"
#include "mtl_ofi_request.h"
#include "mtl_ofi_endpoint.h"

static int
ompi_mtl_ofi_send_callback(struct fi_cq_tagged_entry *wc,
                           ompi_mtl_ofi_request_t *ofi_req)
{
    assert(ofi_req->completion_count > 0);
    ofi_req->completion_count--;
    return OMPI_SUCCESS;
}

static int
ompi_mtl_ofi_send_error_callback(struct fi_cq_err_entry *error,
                                 ompi_mtl_ofi_request_t *ofi_req)
{
    switch(error->err) {
        case FI_EMSGSIZE:
            ofi_req->status.MPI_ERROR = MPI_ERR_TRUNCATE;
            break;
        default:
            ofi_req->status.MPI_ERROR = MPI_ERR_INTERN;
    }
    return ofi_req->event_callback(NULL, ofi_req);
}

static int
ompi_mtl_ofi_send_ack_callback(struct fi_cq_tagged_entry *wc,
                               ompi_mtl_ofi_request_t *ofi_req)
{
    ompi_mtl_ofi_request_t *parent_req = ofi_req->parent;

    free(ofi_req);

    parent_req->event_callback(NULL, parent_req);

    return OMPI_SUCCESS;
}

static int
ompi_mtl_ofi_send_ack_error_callback(struct fi_cq_err_entry *error,
                                     ompi_mtl_ofi_request_t *ofi_req)
{
    ompi_mtl_ofi_request_t *parent_req = ofi_req->parent;

    free(ofi_req);

    parent_req->status.MPI_ERROR = MPI_ERR_INTERN;

    return parent_req->error_callback(error, parent_req);
}

static int
ompi_mtl_ofi_isend_callback(struct fi_cq_tagged_entry *wc,
                            ompi_mtl_ofi_request_t *ofi_req)
{
    assert(ofi_req->completion_count > 0);
    ofi_req->completion_count--;

    if (0 == ofi_req->completion_count) {
        /* Request completed */
        if (OPAL_UNLIKELY(NULL != ofi_req->buffer)) {
            free(ofi_req->buffer);
            ofi_req->buffer = NULL;
        }

        ofi_req->super.ompi_req->req_status.MPI_ERROR =
            ofi_req->status.MPI_ERROR;

        ofi_req->super.completion_callback(&ofi_req->super);
    }

    return OMPI_SUCCESS;
}

static inline int
ompi_mtl_ofi_send_start(struct mca_mtl_base_module_t *mtl,
                        struct ompi_communicator_t *comm,
                        int dest,
                        int tag,
                        struct opal_convertor_t *convertor,
                        mca_pml_base_send_mode_t mode,
                        ompi_mtl_ofi_request_t *ofi_req)
{
    int ret;
    void *start;
    size_t length;
    ssize_t ret_length;
    bool free_after;
    uint64_t match_bits;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    ompi_mtl_ofi_request_t *ack_req = NULL; /* For synchronous send */

    ompi_proc = ompi_comm_peer_lookup(comm, dest);
    endpoint = ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];

    ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ret) return ret;

    ofi_req->buffer = (free_after) ? start : NULL;
    ofi_req->length = length;
    ofi_req->status.MPI_ERROR = OMPI_SUCCESS;

    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_SYNCHRONOUS == mode)) {
        ack_req = malloc(sizeof(ompi_mtl_ofi_request_t));
        assert(ack_req);
        ack_req->parent = ofi_req;
        ack_req->event_callback = ompi_mtl_ofi_send_ack_callback;
        ack_req->error_callback = ompi_mtl_ofi_send_ack_error_callback;

        ofi_req->completion_count = 2;
        MTL_OFI_SET_SEND_BITS(match_bits, comm->c_contextid,
                              comm->c_my_rank, tag, MTL_OFI_SYNC_SEND);
        ret_length = fi_trecv(ompi_mtl_ofi.ep,
                              NULL,
                              0,
                              ompi_mtl_ofi.mr,
                              endpoint->peer_fiaddr,
                              match_bits | MTL_OFI_SYNC_SEND_ACK,
                              0, /* Exact match, no ignore bits */
                              (void *) &ack_req->ctx);
        if (OPAL_UNLIKELY(ret_length < 0)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: fi_trecv failed: %s(%zd)",
                                __FILE__, __LINE__,
                                strerror(errno), ret_length);
            return ompi_mtl_ofi_get_error(ret);
        }
    } else {
        ofi_req->completion_count = 1;
        MTL_OFI_SET_SEND_BITS(match_bits, comm->c_contextid,
                              comm->c_my_rank, tag, 0);
    }

    ret_length = fi_tsend(ompi_mtl_ofi.ep,
                          start,
                          length,
                          ompi_mtl_ofi.mr,
                          endpoint->peer_fiaddr,
                          match_bits,
                          (void *) &ofi_req->ctx);

    if (OPAL_UNLIKELY(0 > ret_length)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_tsend failed: %zd",
                            __FILE__, __LINE__, ret_length);
        return ompi_mtl_ofi_get_error(ret);
    }

    return OMPI_SUCCESS;
}

int
ompi_mtl_ofi_send(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode)
{
    int ret = OMPI_SUCCESS;
    ompi_mtl_ofi_request_t ofi_req;

    /**
     * Create a send request, start it and wait until it completes.
     */
    ofi_req.event_callback = ompi_mtl_ofi_send_callback;
    ofi_req.error_callback = ompi_mtl_ofi_send_error_callback;

    ret = ompi_mtl_ofi_send_start(mtl, comm, dest, tag,
                                  convertor, mode, &ofi_req);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        if (NULL != ofi_req.buffer) {
            free(ofi_req.buffer);
        }
        return ret;
    }

    /**
     * Wait until the request is completed.
     * ompi_mtl_ofi_send_callback() updates this variable.
     */
    while (0 < ofi_req.completion_count) {
        ompi_mtl_ofi_progress();
    }

    if (OPAL_UNLIKELY(NULL != ofi_req.buffer)) {
        free(ofi_req.buffer);
    }

    return ofi_req.status.MPI_ERROR;
}


int
ompi_mtl_ofi_isend(struct mca_mtl_base_module_t *mtl,
                   struct ompi_communicator_t *comm,
                   int dest,
                   int tag,
                   struct opal_convertor_t *convertor,
                   mca_pml_base_send_mode_t mode,
                   bool blocking,
                   mca_mtl_request_t *mtl_request)
{
    int ret = OMPI_SUCCESS;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;

    ofi_req->event_callback = ompi_mtl_ofi_isend_callback;
    ofi_req->error_callback = ompi_mtl_ofi_send_error_callback;

    ret = ompi_mtl_ofi_send_start(mtl, comm, dest, tag,
                                  convertor, mode, ofi_req);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret && NULL != ofi_req->buffer)) {
        free(ofi_req->buffer);
    }

    return ret;
}

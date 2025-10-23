/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_iprobe.c
 *
 * UBCL PML iprobe related functions
 *
 */

#include "ompi/constants.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/message/message.h"
#include "ompi/proc/proc.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ubcl_api.h"

int mca_pml_ubcl_iprobe(int src, int tag, struct ompi_communicator_t *comm,
                        int *matched, ompi_status_public_t *status)
{
    OPAL_OUTPUT_VERBOSE((75, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_IPROBE\n"));
    ubcl_status_t ubcl_status;
    uint64_t cid;
    uint64_t rank;

    if (OMPI_ANY_SOURCE == src) {
        rank = UBCL_ANY_SOURCE;
    } else {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, src);
        mca_common_ubcl_endpoint_t *endpoint = NULL;
        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        rank = endpoint->rank;
    }

    cid = ompi_comm_get_local_cid(comm);
    ubcl_cid_t ubcl_cid= mca_pml_ubcl_compute_ubcl_cid(tag, cid);

    /* Call the UBCL api for iprobe */
    ubcl_iprobe(rank, tag, ubcl_cid, matched, &ubcl_status);
    if (*matched) {
        mca_common_ubcl_status_to_ompi(status, ubcl_status, comm, src);
    }

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_probe(int src, int tag, struct ompi_communicator_t *comm,
                       ompi_status_public_t *status)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_PROBE\n"));
    int match = 0;

    /* Loop over pml iprobe */
    while (!match) {
        mca_pml_ubcl_iprobe(src, tag, comm, &match, status);
    }

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_improbe(int src, int tag, struct ompi_communicator_t *comm,
                         int *matched, struct ompi_message_t **message,
                         ompi_status_public_t *status)
{
    OPAL_OUTPUT_VERBOSE((75, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_IMPROBE\n"));
    ubcl_status_t ubcl_status;
    uint64_t rank;
    uint64_t cid;
    if (OMPI_ANY_SOURCE == src) {
        rank = UBCL_ANY_SOURCE;
    } else {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, src);
        mca_common_ubcl_endpoint_t *endpoint = NULL;
        endpoint = (mca_common_ubcl_endpoint_t *) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        rank = endpoint->rank;
    }

    cid = ompi_comm_get_local_cid(comm);
    ubcl_cid_t ubcl_cid = mca_pml_ubcl_compute_ubcl_cid(tag, cid);

    ubcl_message_t *ubcl_message;

    /* Call the UBCL api for improbe */
    ubcl_improbe(rank, tag, ubcl_cid, matched, &ubcl_message, &ubcl_status);
    if (*matched) {
        mca_common_ubcl_status_to_ompi(status, ubcl_status, comm, src);
        *message = ompi_message_alloc();
        if (message == NULL) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        (*message)->req_ptr = ubcl_message;
        (*message)->comm = comm;
        (*message)->peer = mca_common_ubcl_get_mpi_rank(src, comm, ubcl_status.remote);
        (*message)->count = ubcl_status.size;
    }

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_mprobe(int src, int tag, struct ompi_communicator_t *comm,
                        struct ompi_message_t **message,
                        ompi_status_public_t *status)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_MPROBE\n"));
    int match = 0;

    /* Loop over pml improbe */
    while (!match) {
        mca_pml_ubcl_improbe(src, tag, comm, &match, message, status);
    }

    return OMPI_SUCCESS;
}

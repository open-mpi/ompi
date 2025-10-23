/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2024 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl.c
 *
 * UBCL PML
 *
 * Implementation of API defined in pml.h. To see parameters and return values
 * of these functions, refer to ompi/mca/pml/pml.h.
 */

#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_endpoint.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"
#include "ompi/proc/proc.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "opal/class/opal_object.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/hwloc/hwloc-internal.h"
#include "opal/prefetch.h"
#include "opal/util/proc.h"
#include "ubcl_api.h"

/**
 * PML UBCL Module
 *
 * pml_max_contextid and pml_max_tag are computed given Portas4 module
 * match_bits and platform int size
 */
mca_pml_ubcl_module_t mca_pml_ubcl_module = {
    .super = {
        .pml_add_procs      = mca_pml_ubcl_add_procs,
        .pml_del_procs      = mca_pml_ubcl_del_procs,
        .pml_enable         = mca_pml_ubcl_enable,
        .pml_progress       = mca_pml_ubcl_progress,
        .pml_add_comm       = mca_pml_ubcl_add_comm,
        .pml_del_comm       = mca_pml_ubcl_del_comm,
        .pml_irecv_init     = mca_pml_ubcl_irecv_init,
        .pml_irecv          = mca_pml_ubcl_irecv,
        .pml_recv           = mca_pml_ubcl_recv,
        .pml_isend_init     = mca_pml_ubcl_isend_init,
        .pml_isend          = mca_pml_ubcl_isend,
        .pml_send           = mca_pml_ubcl_send,
        .pml_iprobe         = mca_pml_ubcl_iprobe,
        .pml_probe          = mca_pml_ubcl_probe,
        .pml_start          = mca_pml_ubcl_start,
        .pml_improbe        = mca_pml_ubcl_improbe,
        .pml_mprobe         = mca_pml_ubcl_mprobe,
        .pml_imrecv         = mca_pml_ubcl_imrecv,
        .pml_mrecv          = mca_pml_ubcl_mrecv,
        .pml_dump           = mca_pml_ubcl_dump,
        .pml_max_contextid  = PML_UBCL_MAX_CID, /** Comes from pml_ubcl.h */
        .pml_max_tag        = PML_UBCL_MAX_TAG, /** Comes from pml_ubcl.h */
        .pml_flags          = MCA_PML_BASE_FLAG_REQUIRE_WORLD,
        .pml_get_transports = NULL
    }
};

int mca_pml_ubcl_add_comm(struct ompi_communicator_t *comm)
{
    mca_pml_ubcl_comm_t *new_ubcl_comm;
    ompi_group_t *comm_group;

    new_ubcl_comm = malloc(sizeof(mca_pml_ubcl_comm_t));
    if (NULL == new_ubcl_comm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (OMPI_COMM_IS_INTER(comm)) {
        comm_group = comm->c_remote_group;
        new_ubcl_comm->size = ompi_comm_remote_size(comm);
    } else {
        comm_group = comm->c_local_group;
        new_ubcl_comm->size = ompi_comm_size(comm);
    }

    new_ubcl_comm->array = malloc(new_ubcl_comm->size * sizeof(uint64_t));
    if (NULL == new_ubcl_comm->array) {
        free(new_ubcl_comm);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Build array comm_rank -> ubcl_rank */
    for (uint32_t i = 0; i < new_ubcl_comm->size; i++) {
        struct ompi_proc_t *proc;
        mca_common_ubcl_endpoint_t *endpoint;
        proc = ompi_group_peer_lookup(comm_group, i);
        /* In OMPI 5 we sometimes get procs here that didn't go through
         * 'add_procs'. We create them here to avoid any issue, 'add_procs'
         * tests if an endpoint is already created so there is no issue if it's
         * called later  */
        endpoint = proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        if (NULL == endpoint) {
            mca_pml_ubcl_add_procs(&proc, 1);
            endpoint = proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
        } else {
            mca_pml_ubcl_endpoint_retain(proc);
        }
        new_ubcl_comm->array[i] = endpoint->rank;
    }

    comm->c_pml_comm = new_ubcl_comm;

    OPAL_OUTPUT_VERBOSE(
        (50, mca_pml_ubcl_component.output, "UBCL_MODULE_ADD_COMM %s\n", ompi_comm_print_cid(comm)));

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_del_comm(struct ompi_communicator_t *comm)
{
    mca_pml_ubcl_comm_t *pml_comm;
    ompi_group_t *comm_group;
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_DEL_COMM\n"));

    if (NULL == comm->c_pml_comm) {
        mca_pml_ubcl_error(OMPI_ERR_BAD_PARAM,
                           "error: suspicious free of a communicator that PML UBCL has never allocated");
    }

    /* Important to be decrementing refcount/removing endpoints,
     * that way if we create new communicators after MPI_Init we
     * can free the endpoints reliably when needed */
    if (OMPI_COMM_IS_INTER(comm)) {
        comm_group = comm->c_remote_group;
    } else {
        comm_group = comm->c_local_group;
    }
    pml_comm = (mca_pml_ubcl_comm_t *) comm->c_pml_comm;

    for (uint32_t i = 0; i < pml_comm->size; i++) {
        struct ompi_proc_t *proc;
        proc = ompi_group_peer_lookup(comm_group, i);
        mca_pml_ubcl_endpoint_release(proc);
    }

    free(pml_comm->array);
    free(pml_comm);

    return OMPI_SUCCESS;
}

/**
 * Call for BTLs that we don't care of
 */
int mca_pml_ubcl_enable(bool enable)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_ENABLE\n"));
    return OMPI_SUCCESS;
}

int mca_pml_ubcl_dump(struct ompi_communicator_t *comm, int verbose)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_DUMP\n"));
    return OMPI_ERROR;
}

int mca_pml_ubcl_start(size_t count, ompi_request_t **requests)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_START\n"));

    return mca_pml_ubcl_request_start(count, requests);
}

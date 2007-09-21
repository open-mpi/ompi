/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/base/base.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"
#include "pml_cm_recvreq.h"

ompi_pml_cm_t ompi_pml_cm = {
    {
        mca_pml_cm_add_procs,
        mca_pml_cm_del_procs,
        mca_pml_cm_enable,
        mca_pml_cm_progress,
        mca_pml_cm_add_comm,
        mca_pml_cm_del_comm,
        mca_pml_cm_irecv_init,
        mca_pml_cm_irecv,
        mca_pml_cm_recv,
        mca_pml_cm_isend_init,
        mca_pml_cm_isend,
        mca_pml_cm_send,
        mca_pml_cm_iprobe,
        mca_pml_cm_probe,
        mca_pml_cm_start,
        mca_pml_cm_dump,
        0,
        0
    }
};


int
mca_pml_cm_enable(bool enable)
{
    /* BWB - FIX ME - need to have this actually do something,
       maybe? */
    ompi_free_list_init(&mca_pml_base_send_requests,
                        sizeof(mca_pml_cm_hvy_send_request_t) + ompi_mtl->mtl_request_size,
                        OBJ_CLASS(mca_pml_cm_hvy_send_request_t),
                        ompi_pml_cm.free_list_num,
                        ompi_pml_cm.free_list_max,
                        ompi_pml_cm.free_list_inc,
                        NULL);

    ompi_free_list_init(&mca_pml_base_recv_requests,
                        sizeof(mca_pml_cm_hvy_recv_request_t) + ompi_mtl->mtl_request_size,
                        OBJ_CLASS(mca_pml_cm_hvy_recv_request_t),
                        ompi_pml_cm.free_list_num,
                        ompi_pml_cm.free_list_max,
                        ompi_pml_cm.free_list_inc,
                        NULL);

    return OMPI_SUCCESS;
}


int
mca_pml_cm_add_comm(ompi_communicator_t* comm)
{
    /* setup our per-communicator data */
    comm->c_pml_comm = NULL;

    return OMPI_SUCCESS;
}


int
mca_pml_cm_del_comm(ompi_communicator_t* comm)
{
    /* clean up our per-communicator data */
    comm->c_pml_comm = NULL;

    return OMPI_SUCCESS;
}


int
mca_pml_cm_add_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    int ret;
    size_t i;
    struct mca_mtl_base_endpoint_t **endpoints;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    for (i = 0 ; i < nprocs ; ++i) {
        if (procs[i]->proc_arch != ompi_proc_local()->proc_arch) {
            return OMPI_ERR_NOT_SUPPORTED;
        }
    }
#endif

    /* make sure remote procs are using the same PML as us */
    if (OMPI_SUCCESS != (ret = mca_pml_base_pml_check_selected("cm",
                                                              procs,
                                                              nprocs))) {
        return ret;
    }

    endpoints = (struct mca_mtl_base_endpoint_t**)malloc(nprocs * sizeof(struct mca_mtl_base_endpoint_t*));
    if (NULL == endpoints) return OMPI_ERROR;

#if OMPI_ENABLE_DEBUG
    for (i = 0 ; i < nprocs ; ++i) {
        endpoints[i] = NULL;
    }
#endif

    ret = OMPI_MTL_CALL(add_procs(ompi_mtl, nprocs, procs, endpoints));
    if (OMPI_SUCCESS != ret) {
        free(endpoints);
        return ret;
    }

    for (i = 0 ; i < nprocs ; ++i) {
        procs[i]->proc_pml = (struct mca_pml_base_endpoint_t*) endpoints[i];
    }

    free(endpoints);
    return OMPI_SUCCESS;
}


int
mca_pml_cm_del_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    int ret;
    size_t i;
    struct mca_mtl_base_endpoint_t **endpoints;

    endpoints = (struct mca_mtl_base_endpoint_t**)malloc(nprocs * sizeof(struct mca_mtl_base_endpoint_t*));
    if (NULL == endpoints) return OMPI_ERROR;

    for (i = 0 ; i < nprocs ; ++i) {
        endpoints[i] = (struct mca_mtl_base_endpoint_t*) procs[i]->proc_pml;
    }

    ret = OMPI_MTL_CALL(del_procs(ompi_mtl, nprocs, procs, endpoints));
    if (OMPI_SUCCESS != ret) {
        free(endpoints);
        return ret;
    }

    free(endpoints);
    return OMPI_SUCCESS;
}


/* print any available useful information from this communicator */
int
mca_pml_cm_dump(struct ompi_communicator_t* comm, int verbose)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

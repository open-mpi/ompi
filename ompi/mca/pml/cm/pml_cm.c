/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
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

#include "pml_cm.h"
#include "pml_cm_proc.h"
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
    return OMPI_SUCCESS;
}


int
mca_pml_cm_add_comm(ompi_communicator_t* comm)
{
    mca_pml_cm_proc_t *pml_proc;
    int i;

    /* setup our per-communicator data */
    comm->c_pml_comm = NULL;

    /* setup our proc cache on the communicator.  This should be
       something that can be safely cast to a mca_pml_proc_t* */
    comm->c_pml_procs = (mca_pml_proc_t**) malloc(
        comm->c_remote_group->grp_proc_count * sizeof(mca_pml_proc_t*));
    if(NULL == comm->c_pml_procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(i = 0 ; i < comm->c_remote_group->grp_proc_count ; i++){
        pml_proc = OBJ_NEW(mca_pml_cm_proc_t);
        pml_proc->base.proc_ompi = comm->c_remote_group->grp_proc_pointers[i];
        comm->c_pml_procs[i] = (mca_pml_proc_t*) pml_proc;
    }

    return OMPI_SUCCESS;
}


int
mca_pml_cm_del_comm(ompi_communicator_t* comm)
{
    int i;

    /* clean up our per-communicator data */
    comm->c_pml_comm = NULL;

    /* clean up our proc cache on the communicator */
    if (comm->c_pml_procs != NULL) {
        for(i = 0 ; i < comm->c_remote_group->grp_proc_count ; i++){
            mca_pml_cm_proc_t *pml_proc = 
                (mca_pml_cm_proc_t*) comm->c_pml_procs[i];
            OBJ_RELEASE(pml_proc);
        }
        free(comm->c_pml_procs);
        comm->c_pml_procs = NULL;
    }

    return OMPI_SUCCESS;
}


int
mca_pml_cm_add_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    int ret;
    size_t i;
    struct mca_mtl_base_endpoint_t **endpoints;

    endpoints = malloc(nprocs * sizeof(struct mca_mtl_base_endpoint_t*));
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

    endpoints = malloc(nprocs * sizeof(struct mca_mtl_base_endpoint_t*));
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


void
mca_pml_cm_thin_send_request_completion(struct mca_mtl_request_t *mtl_request)
{
    mca_pml_cm_send_request_t *base_request = 
        (mca_pml_cm_send_request_t*) mtl_request->ompi_req;
    MCA_PML_CM_THIN_SEND_REQUEST_PML_COMPLETE(((mca_pml_cm_thin_send_request_t*) base_request));
}
void
mca_pml_cm_hvy_send_request_completion(struct mca_mtl_request_t *mtl_request)
{
    mca_pml_cm_send_request_t *base_request = 
        (mca_pml_cm_send_request_t*) mtl_request->ompi_req;
    MCA_PML_CM_HVY_SEND_REQUEST_PML_COMPLETE(((mca_pml_cm_hvy_send_request_t*) base_request));
}



void
mca_pml_cm_thin_recv_request_completion(struct mca_mtl_request_t *mtl_request)
{
    mca_pml_cm_request_t *base_request = 
        (mca_pml_cm_request_t*) mtl_request->ompi_req;
    MCA_PML_CM_THIN_RECV_REQUEST_PML_COMPLETE(((mca_pml_cm_thin_recv_request_t*) base_request));
}

void
mca_pml_cm_hvy_recv_request_completion(struct mca_mtl_request_t *mtl_request)
{
    mca_pml_cm_request_t *base_request = 
        (mca_pml_cm_request_t*) mtl_request->ompi_req;
    MCA_PML_CM_HVY_RECV_REQUEST_PML_COMPLETE(((mca_pml_cm_hvy_recv_request_t*) base_request));
}


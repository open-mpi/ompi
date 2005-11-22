/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "class/ompi_bitmap.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/btl/base/base.h"
#include "pml_dr.h"
#include "pml_dr_component.h"
#include "pml_dr_comm.h"
#include "pml_dr_proc.h"
#include "pml_dr_hdr.h"
#include "pml_dr_recvfrag.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_recvreq.h"
#include "mca/bml/base/base.h"

mca_pml_dr_t mca_pml_dr = {
    {
    mca_pml_dr_add_procs,
    mca_pml_dr_del_procs,
    mca_pml_dr_enable,
    mca_pml_dr_progress,
    mca_pml_dr_add_comm,
    mca_pml_dr_del_comm,
    mca_pml_dr_irecv_init,
    mca_pml_dr_irecv,
    mca_pml_dr_recv,
    mca_pml_dr_isend_init,
    mca_pml_dr_isend,
    mca_pml_dr_send,
    mca_pml_dr_iprobe,
    mca_pml_dr_probe,
    mca_pml_dr_start,
    32768,
    INT_MAX
    }
};

int mca_pml_dr_enable(bool enable)
{
    if( false == enable ) return OMPI_SUCCESS;
    mca_pml_dr.enabled = true;
    return OMPI_SUCCESS;
}

int mca_pml_dr_add_comm(ompi_communicator_t* comm)
{
    /* allocate pml specific comm data */
    mca_pml_dr_comm_t* pml_comm = OBJ_NEW(mca_pml_dr_comm_t);
    mca_pml_dr_proc_t* pml_proc = NULL;
    int i;

    if (NULL == pml_comm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_pml_dr_comm_init_size(pml_comm, comm->c_remote_group->grp_proc_count);
    comm->c_pml_comm = pml_comm;
    comm->c_pml_procs = (mca_pml_proc_t**)malloc(
                                                 comm->c_remote_group->grp_proc_count * sizeof(mca_pml_proc_t));
    if(NULL == comm->c_pml_procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(i=0; i<comm->c_remote_group->grp_proc_count; i++){
        pml_proc = OBJ_NEW(mca_pml_dr_proc_t);
        pml_proc->base.proc_ompi = comm->c_remote_group->grp_proc_pointers[i];
        comm->c_pml_procs[i] = (mca_pml_proc_t*) pml_proc; /* comm->c_remote_group->grp_proc_pointers[i]->proc_pml; */
    }
    return OMPI_SUCCESS;
}

int mca_pml_dr_del_comm(ompi_communicator_t* comm)
{
    OBJ_RELEASE(comm->c_pml_comm);
    comm->c_pml_comm = NULL;
    if(comm->c_pml_procs != NULL)
        free(comm->c_pml_procs);
    comm->c_pml_procs = NULL;
    return OMPI_SUCCESS;
}


/*
 *   For each proc setup a datastructure that indicates the PTLs
 *   that can be used to reach the destination.
 *
 */

int mca_pml_dr_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    ompi_bitmap_t reachable;
    struct mca_bml_base_endpoint_t ** bml_endpoints = NULL;
    int rc;

    if(nprocs == 0)
        return OMPI_SUCCESS;

    OBJ_CONSTRUCT(&reachable, ompi_bitmap_t);
    rc = ompi_bitmap_init(&reachable, nprocs);
    if(OMPI_SUCCESS != rc)
        return rc;

    bml_endpoints = (struct mca_bml_base_endpoint_t **) malloc ( nprocs *
		     sizeof(struct mca_bml_base_endpoint_t*));
    if ( NULL == bml_endpoints ) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
   
    rc = mca_bml.bml_add_procs(
                               nprocs,
                               procs,
                               bml_endpoints,
                               &reachable
                               );
    if(OMPI_SUCCESS != rc)
        return rc;

    rc = mca_bml.bml_register(
                              MCA_BTL_TAG_PML,
                              mca_pml_dr_recv_frag_callback,
                              NULL);

    /* initialize free list of receive buffers */
    ompi_free_list_init(
                        &mca_pml_dr.buffers,
                        sizeof(mca_pml_dr_buffer_t) + mca_pml_dr.eager_limit,
                        OBJ_CLASS(mca_pml_dr_buffer_t),
                        0,
                        mca_pml_dr.free_list_max,
                        mca_pml_dr.free_list_inc,
                        NULL);

    if ( NULL != bml_endpoints ) {
	free ( bml_endpoints) ;
    }
    return rc;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_dr_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    return mca_bml.bml_del_procs(nprocs, procs);
}

int mca_pml_dr_component_fini(void)
{
    /* FIX */
    return OMPI_SUCCESS;
}


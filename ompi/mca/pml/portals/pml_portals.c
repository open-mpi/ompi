/*
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_portals.h"
#include "ompi/communicator/communicator.h"
#include "opal/class/opal_list.h"

mca_pml_portals_t mca_pml_portals = {
    {
        mca_pml_portals_add_procs,
        mca_pml_portals_del_procs,
        mca_pml_portals_enable,
        mca_pml_portals_progress,
        mca_pml_portals_add_comm,
        mca_pml_portals_del_comm,
        mca_pml_portals_irecv_init,
        mca_pml_portals_irecv,
        mca_pml_portals_recv,
        mca_pml_portals_isend_init,
        mca_pml_portals_isend,
        mca_pml_portals_send,
        mca_pml_portals_iprobe,
        mca_pml_portals_probe,
        mca_pml_portals_start,
        32768, /* max tag value */
        100 /* max cid - BWB - fix me */
    }
};

int mca_pml_portals_add_comm(ompi_communicator_t* comm)
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_del_comm(ompi_communicator_t* comm)
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_add_ptls(opal_list_t *ptls)
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_add_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_del_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    return OMPI_SUCCESS;
}


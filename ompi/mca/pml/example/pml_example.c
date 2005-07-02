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
#include "pml_example.h"
#include "pml_example_recvreq.h"
#include "pml_example_sendreq.h"

mca_pml_example_t mca_pml_example = {
    {
        mca_pml_example_add_procs,
        mca_pml_example_del_procs,
        mca_pml_example_add_ptls,
        mca_pml_example_control,
        mca_pml_example_progress,
        mca_pml_example_add_comm,
        mca_pml_example_del_comm,
        mca_pml_example_irecv_init,
        mca_pml_example_irecv,
        mca_pml_example_recv,
        mca_pml_example_isend_init,
        mca_pml_example_isend,
        mca_pml_example_send,
        mca_pml_example_iprobe,
        mca_pml_example_probe,
        mca_pml_example_start
    }
};

int mca_pml_example_add_comm(ompi_communicator_t* comm)
{
    return OMPI_SUCCESS;
}

int mca_pml_example_del_comm(ompi_communicator_t* comm)
{
    return OMPI_SUCCESS;
}

int mca_pml_example_add_ptls(ompi_list_t *ptls)
{
    return OMPI_SUCCESS;
}

int mca_pml_example_control(int param, void* value, size_t size)
{
    return OMPI_SUCCESS;
}

int mca_pml_example_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    return OMPI_SUCCESS;
}

int mca_pml_example_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    return OMPI_SUCCESS;
}


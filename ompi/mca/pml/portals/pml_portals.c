/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
        mca_pml_portals_dump,
        32768, /* max tag value */
        100 /* max cid - BWB - fix me */
    }
};

int mca_pml_portals_enable(bool enable)
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_add_comm(ompi_communicator_t* comm)
{
    return OMPI_SUCCESS;
}

int mca_pml_portals_del_comm(ompi_communicator_t* comm)
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

/* print any available useful information from this communicator */
int mca_pml_portals_dump(struct ompi_communicator_t* comm, int verbose)
{
    return OMPI_SUCCESS;
}

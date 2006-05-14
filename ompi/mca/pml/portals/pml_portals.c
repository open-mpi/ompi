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

ompi_pml_portals_t ompi_pml_portals = {
    {
        ompi_pml_portals_add_procs,
        ompi_pml_portals_del_procs,
        ompi_pml_portals_enable,
        ompi_pml_portals_progress,
        ompi_pml_portals_add_comm,
        ompi_pml_portals_del_comm,
        ompi_pml_portals_irecv_init,
        ompi_pml_portals_irecv,
        ompi_pml_portals_recv,
        ompi_pml_portals_isend_init,
        ompi_pml_portals_isend,
        ompi_pml_portals_send,
        ompi_pml_portals_iprobe,
        ompi_pml_portals_probe,
        ompi_pml_portals_start,
        ompi_pml_portals_dump,
        (1UL << 30), /* max tag value - must allow negatives */
        8191  /* max cid - 2^13 - 1 */
    }
};


int
ompi_pml_portals_enable(bool enable)
{
    return OMPI_SUCCESS;
}


int
ompi_pml_portals_add_comm(ompi_communicator_t* comm)
{
    size_t comm_size = comm->c_remote_group->grp_proc_count;
    size_t i;

    /* allocate portals pml specific information */
    comm->c_pml_comm = NULL;
    comm->c_pml_procs = malloc(comm_size * sizeof(ompi_pml_portals_proc_t*));

    if (NULL == comm->c_pml_procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0 ; i < comm_size ; ++i) {
        comm->c_pml_procs[i] = comm->c_remote_group->grp_proc_pointers[i]->proc_pml;
        OBJ_RETAIN(comm->c_pml_procs[i]);
    }

    return OMPI_SUCCESS;
}


int
ompi_pml_portals_del_comm(ompi_communicator_t* comm)
{
    size_t comm_size = comm->c_remote_group->grp_proc_count;
    size_t i;

    for (i = 0 ; i < comm_size ; ++i) {
        OBJ_RELEASE(comm->c_pml_procs[i]);
    }
    free(comm->c_pml_procs);

    return OMPI_SUCCESS;
}


int
ompi_pml_portals_add_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    size_t i;
    static bool done_init = false;
    int ret;

    if (0 == nprocs) return OMPI_SUCCESS;

    /* allocate space for our pml information */
    for (i = 0 ; i < nprocs ; ++i) {
        procs[i]->proc_pml = (mca_pml_proc_t*) OBJ_NEW(ompi_pml_portals_proc_t);
        procs[i]->proc_pml->proc_ompi = procs[i];
    }

    ompi_pml_portals_add_procs_compat(procs, nprocs);

    if (!done_init) { 

        /* do all initialization that should happen *after* PtlNIInit
           here.  Can't do it before this because we need the modex to
           wire everything up in the utcp case */

        done_init = true;
    }

    return OMPI_SUCCESS;
}


int
ompi_pml_portals_del_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    size_t i;

    if (0 == nprocs) return OMPI_SUCCESS;

    /* allocate space for our pml information */
    for (i = 0 ; i < nprocs ; ++i) {
        OBJ_RELEASE(procs[i]->proc_pml);
    }

    return OMPI_SUCCESS;
}

/* print any available useful information from this communicator */
int
ompi_pml_portals_dump(struct ompi_communicator_t* comm, int verbose)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


/* class information for OMPI proc structure */
OBJ_CLASS_INSTANCE(ompi_pml_portals_proc_t, opal_list_item_t,
                   NULL, NULL);


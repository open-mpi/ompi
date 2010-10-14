/*
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <portals4.h>
#include <portals4_runtime.h>

#include "ompi/mca/mtl/mtl.h"

#include "mtl_portals4.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_request.h"

mca_mtl_portals4_module_t ompi_mtl_portals4 = {
    {
        8191,        /* max cid - 2^13 - 1 */
        (1UL << 30), /* max tag value - must allow negatives */
        0,           /* request reserve space */
        0,           /* flags */

        ompi_mtl_portals4_add_procs,
        ompi_mtl_portals4_del_procs,
        ompi_mtl_portals4_finalize,

        ompi_mtl_portals4_send,
        ompi_mtl_portals4_isend,
        ompi_mtl_portals4_irecv,
        ompi_mtl_portals4_iprobe,

        NULL        /* cancel */
    }
};


int
ompi_mtl_portals4_add_procs(struct mca_mtl_base_module_t *mtl,
                           size_t nprocs,
                           struct ompi_proc_t** procs, 
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    mca_mtl_portals4_module_t *mtl_portals4 = (mca_mtl_portals4_module_t*) mtl;
    int i;
    struct runtime_proc_t *world_procs;
    int world_size = runtime_get_size();
    runtime_get_nidpid_map(&world_procs);

    if (world_size != (int) nprocs) return OMPI_ERROR;

    mtl_portals4->endpoints = malloc(sizeof(struct mca_mtl_base_endpoint_t) * nprocs);
    if (NULL == mtl_portals4->endpoints) {
        return OMPI_ERROR;
    }

    for (i = 0 ; i < (int) nprocs ; ++i) {
        mtl_peer_data[i] = &(mtl_portals4->endpoints[i]);
        mtl_peer_data[i]->ptl_proc.phys.nid = world_procs[i].nid;
        mtl_peer_data[i]->ptl_proc.phys.pid = world_procs[i].pid;
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_del_procs(struct mca_mtl_base_module_t *mtl,
                           size_t nprocs,
                           struct ompi_proc_t** procs, 
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_finalize(struct mca_mtl_base_module_t *mtl)
{
    PtlNIFini(ompi_mtl_portals4.ptl_ni_h);
    PtlFini();

    return OMPI_SUCCESS;
}

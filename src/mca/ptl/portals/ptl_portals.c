/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "portals_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#include "include/constants.h"
#include "util/output.h"

#include "ptl_portals.h"
#include "ptl_portals_compat.h"

mca_ptl_portals_module_t mca_ptl_portals_module = {
    {
        &mca_ptl_portals_component.super,
        1,   /* max size of request cache */
        1,   /* byest required by ptl for a request */
        128, /* max size of first frag */
        0,   /* min size of frag */
        128, /* max size of frag */
        0,   /* exclusivity */
        50,  /* latency */
        0,   /* bandwidth */
        0,   /* ptl flags */

        mca_ptl_portals_add_procs,
        mca_ptl_portals_del_procs,
        mca_ptl_portals_finalize,
        mca_ptl_portals_send,
        NULL,
        NULL,
        mca_ptl_portals_matched,
        mca_ptl_portals_request_init,
        mca_ptl_portals_request_fini,

        NULL,
        NULL,
        NULL,

        NULL,  /* PTL stack */
        NULL   /* PML use */
    },

    0,
};



int
mca_ptl_portals_add_procs(struct mca_ptl_base_module_t* ptl,
                          size_t nprocs, struct ompi_proc_t **procs,
                          struct mca_ptl_base_peer_t** peers,
                          ompi_bitmap_t* reachable)
{
    int ret;
    struct ompi_proc_t *local_proc = ompi_proc_local();
    struct ompi_proc_t *curr_proc;
    ptl_process_id_t *portals_procs;
    size_t i;

    /* make sure our environment is fully initialized.  At end of this
       call, we have a working network handle on our module and
       portals_procs will have the portals process identifier for each
       proc (ordered, in theory) */
    ret = mca_ptl_portals_add_procs_compat((struct mca_ptl_portals_module_t*) ptl,
                                           nprocs, procs, &portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];
        /* BWB - do we want to send to self?  No for now */
        if (curr_proc == local_proc) continue;

    }

    return OMPI_SUCCESS;
}

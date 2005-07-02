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
#include "mca/pml/pml.h"
#include "mca/ptl/base/ptl_base_sendreq.h"

#include "ptl_portals.h"
#include "ptl_portals_compat.h"
#include "ptl_portals_send.h"
#include "ptl_portals_recv.h"

mca_ptl_portals_module_t mca_ptl_portals_module = {
    {
        &mca_ptl_portals_component.super,
        0,   /* max size of request cache */
        sizeof(mca_ptl_portals_send_frag_t),   /* byes required by ptl for a request */
        0,   /* max size of first frag */
        0,   /* min size of frag */
        0,   /* max size of frag */
        60,   /* exclusivity - higher than sm, lower than self */
        0,   /* latency */
        0,   /* bandwidth */
        MCA_PTL_PUT,   /* ptl flags */

        mca_ptl_portals_add_procs,
        mca_ptl_portals_del_procs,
        mca_ptl_portals_finalize,
        mca_ptl_portals_send,
        mca_ptl_portals_send,
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
    unsigned long distance;
    struct mca_ptl_portals_module_t *myptl = 
        (struct mca_ptl_portals_module_t*) ptl;

    /* make sure our environment is fully initialized.  At end of this
       call, we have a working network handle on our module and
       portals_procs will have the portals process identifier for each
       proc (ordered, in theory) */
    ret = mca_ptl_portals_add_procs_compat(myptl, nprocs, procs, 
                                           &portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];
        /* BWB - do we want to send to self?  No for now */
        if (curr_proc == local_proc) continue;

        /* make sure we can reach the process */
        ret = PtlNIDist(myptl->ni_handle,
                        portals_procs[i],
                        &distance);
        if (ret != PTL_OK) {
            ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                                "Could not find distance to process %d", i);
            continue;
        }

        /* set the peer as a pointer to the address */
        peers[i] = (struct mca_ptl_base_peer_t*) &(portals_procs[i]);

        /* and here we can reach */
        ompi_bitmap_set_bit(reachable, i);
    }

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_del_procs(struct mca_ptl_base_module_t *ptl,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_ptl_base_peer_t **peers)
{
    /* yeah, I have no idea what to do here */

    return OMPI_SUCCESS;
}



int
mca_ptl_portals_module_enable(struct mca_ptl_portals_module_t *ptl,
                              int enable)
{
    int i, ret;

    if (enable == 0) {
        /* disable the unexpected receive queue */
        /* BWB - not really sure how - would have to track a lot more data... */
    } else {
        /* only do all the hard stuff if we haven't created the queue */
        if (ptl->eq_handles[MCA_PTL_PORTALS_EQ_SIZE - 1] != PTL_EQ_NONE) {
            return OMPI_SUCCESS;
        }

        /* create an event queue, then the match entries for the match
           entries */
        for (i = 0 ; i < MCA_PTL_PORTALS_EQ_SIZE ; ++i) {
            ret = PtlEQAlloc(ptl->ni_handle,
                             ptl->eq_sizes[i],
                             PTL_EQ_HANDLER_NONE,
                             &(ptl->eq_handles[i]));
            if (ret != PTL_OK) {
                ompi_output(mca_ptl_portals_component.portals_output,
                            "Failed to allocate event queue: %d", ret);
                return OMPI_ERROR;
            }
        }

        for (i = 0 ; i < ptl->first_frag_num_entries ; ++i) {
            ret = ptl_portals_post_recv_md(ptl, NULL);
            if (OMPI_SUCCESS != ret) return ret;
        }
    }

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_finalize(struct mca_ptl_base_module_t *ptl_base)
{
    struct mca_ptl_portals_module_t *ptl =
        (struct mca_ptl_portals_module_t *) ptl_base;
    int ret;

    ret = PtlNIFini(ptl->ni_handle);
    if (PTL_OK != ret) {
        ompi_output_verbose(20, mca_ptl_portals_component.portals_output,
                            "PtlNIFini returned %d", ret);
        return OMPI_ERROR;
    }
    ompi_output_verbose(20, mca_ptl_portals_component.portals_output,
                        "successfully finalized module");

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_request_init(struct mca_ptl_base_module_t *ptl,
			     struct mca_ptl_base_send_request_t *req)
{
    OBJ_CONSTRUCT(req + 1, mca_ptl_portals_send_frag_t);
    return OMPI_SUCCESS;
}


void
mca_ptl_portals_request_fini(struct mca_ptl_base_module_t *ptl,
			     struct mca_ptl_base_send_request_t *req)
{
    OBJ_DESTRUCT(req + 1);
}


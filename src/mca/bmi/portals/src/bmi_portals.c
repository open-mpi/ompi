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
#include "mca/bmi/bmi.h"

#include "bmi_portals.h"
#include "bmi_portals_compat.h"

mca_bmi_portals_module_t mca_bmi_portals_module = {
    {
        &mca_bmi_portals_component.super,

        /* NOTE: All these default values are set in
           component_open() */

        0,   /* max size of first frag */
        0,   /* min send size */
        0,   /* max send size */
        0,   /* min rdma size */
        0,   /* max rdma size */
        0,   /* exclusivity - higher than sm, lower than self */
        0,   /* latency */
        0,   /* bandwidth */
        0,   /* bmi flags */

        mca_bmi_portals_add_procs,
        mca_bmi_portals_del_procs,
        mca_bmi_portals_register,
        mca_bmi_portals_finalize,

        mca_bmi_portals_alloc,
        mca_bmi_portals_free,
        mca_bmi_portals_prepare_src,
        mca_bmi_portals_prepare_dst,
        mca_bmi_portals_send,
        mca_bmi_portals_put,
        mca_bmi_portals_get,
    },
};



int
mca_bmi_portals_add_procs(struct mca_bmi_base_module_t* bmi,
                          size_t nprocs, struct ompi_proc_t **procs,
                          struct mca_bmi_base_endpoint_t** peers,
                          ompi_bitmap_t* reachable)
{
    int ret;
    struct ompi_proc_t *local_proc = ompi_proc_local();
    struct ompi_proc_t *curr_proc;
    ptl_process_id_t *portals_procs;
    size_t i;
    unsigned long distance;
    struct mca_bmi_portals_module_t *mybmi = 
        (struct mca_bmi_portals_module_t*) bmi;

    /* make sure our environment is fully initialized.  At end of this
       call, we have a working network handle on our module and
       portals_procs will have the portals process identifier for each
       proc (ordered, in theory) */
    ret = mca_bmi_portals_add_procs_compat(mybmi, nprocs, procs, 
                                           &portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];
        /* BWB - do we want to send to self?  No for now */
        if (curr_proc == local_proc) continue;

        /* make sure we can reach the process */
        ret = PtlNIDist(mybmi->ni_handle,
                        portals_procs[i],
                        &distance);
        if (ret != PTL_OK) {
            ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                                "Could not find distance to process %d", i);
            continue;
        }

        /* set the peer as a pointer to the address */
        peers[i] = (struct mca_bmi_base_endpoint_t*) &(portals_procs[i]);

        /* and here we can reach */
        ompi_bitmap_set_bit(reachable, i);
    }

    return OMPI_SUCCESS;
}


int
mca_bmi_portals_del_procs(struct mca_bmi_base_module_t *bmi,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_bmi_base_endpoint_t **peers)
{
    /* yeah, I have no idea what to do here */

    return OMPI_SUCCESS;
}



int
mca_bmi_portals_finalize(struct mca_bmi_base_module_t *bmi_base)
{
    struct mca_bmi_portals_module_t *bmi =
        (struct mca_bmi_portals_module_t *) bmi_base;
    int ret;

    if (PTL_INVALID_HANDLE != bmi->ni_handle) {
        ret = PtlNIFini(bmi->ni_handle);
        if (PTL_OK != ret) {
            ompi_output_verbose(20, mca_bmi_portals_component.portals_output,
                                "PtlNIFini returned %d", ret);
            return OMPI_ERROR;
        }
    }
    ompi_output_verbose(20, mca_bmi_portals_component.portals_output,
                        "successfully finalized module");

    return OMPI_SUCCESS;
}

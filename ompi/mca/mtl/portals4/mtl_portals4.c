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
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <portals4.h>

#include "orca/include/rte_orca.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/mtl/mtl.h"
#include "opal/class/opal_list.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "mtl_portals4.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_recv_short.h"

extern mca_mtl_base_component_2_0_0_t mca_mtl_portals4_component;

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
        ompi_mtl_portals4_imrecv,
        ompi_mtl_portals4_improbe,

        ompi_mtl_portals4_cancel,
        ompi_mtl_portals4_add_comm,
        ompi_mtl_portals4_del_comm
    }
};


int
ompi_mtl_portals4_add_procs(struct mca_mtl_base_module_t *mtl,
                            size_t nprocs,
                            struct ompi_proc_t** procs, 
                            struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    int ret, me;
    size_t i;

    /* Get the list of ptl_process_id_t from the runtime and copy into structure */
    for (i = 0 ; i < nprocs ; ++i) {
        ptl_process_t *id;
        size_t size;

        if( procs[i] == ompi_proc_local_proc ) {
            me = i;
        }

        if (procs[i]->proc_arch != ompi_proc_local()->proc_arch) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "Portals 4 MTL does not support heterogeneous operations.");
            opal_output_verbose(1, ompi_mtl_base_output,
                                "Proc %s architecture %x, mine %x.",
                                ORCA_NAME_PRINT(&procs[i]->proc_name), 
                                procs[i]->proc_arch, ompi_proc_local()->proc_arch);
            return OMPI_ERR_NOT_SUPPORTED;
        }

        mtl_peer_data[i] = malloc(sizeof(struct mca_mtl_base_endpoint_t));
        if (NULL == mtl_peer_data[i]) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: malloc failed: %d\n",
                                __FILE__, __LINE__, ret);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ret = ompi_modex_recv(&mca_mtl_portals4_component.mtl_version,
                              procs[i], (void**) &id, &size);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: ompi_modex_recv failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        } else if (sizeof(ptl_process_t) != size) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: ompi_modex_recv failed: %d\n",
                                __FILE__, __LINE__, ret);
            return OMPI_ERR_BAD_PARAM;
        }
 
        mtl_peer_data[i]->ptl_proc = *id;
    }

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    ret = ompi_mtl_portals4_flowctl_add_procs(me, nprocs, mtl_peer_data);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: flowctl_add_procs failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }
#endif

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_del_procs(struct mca_mtl_base_module_t *mtl,
                            size_t nprocs,
                            struct ompi_proc_t** procs, 
                            struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    size_t i;

    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != mtl_peer_data[i]) {
            free(mtl_peer_data[i]);
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_finalize(struct mca_mtl_base_module_t *mtl)
{
    opal_progress_unregister(ompi_mtl_portals4_progress);
    while (0 != ompi_mtl_portals4_progress()) { }

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    ompi_mtl_portals4_flowctl_fini();
#endif
    ompi_mtl_portals4_recv_short_fini();

    PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
    PtlMDRelease(ompi_mtl_portals4.zero_md_h);
    PtlMDRelease(ompi_mtl_portals4.md_h);
    PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.read_idx);
    PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.recv_idx);
    PtlEQFree(ompi_mtl_portals4.send_eq_h);
    PtlEQFree(ompi_mtl_portals4.recv_eq_h);
    PtlNIFini(ompi_mtl_portals4.ni_h);
    PtlFini();

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_add_comm(struct mca_mtl_base_module_t *mtl,
                           struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}

int
ompi_mtl_portals4_del_comm(struct mca_mtl_base_module_t *mtl,
                           struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}

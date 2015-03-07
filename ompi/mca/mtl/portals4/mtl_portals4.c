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
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <portals4.h>

#include "ompi/proc/proc.h"
#include "ompi/mca/mtl/mtl.h"
#include "opal/class/opal_list.h"
#include "opal/mca/pmix/pmix.h"

#include "mtl_portals4.h"
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
                            struct ompi_proc_t** procs)
{
    int ret, me;
    size_t i;
    bool new_found = false;

    /* Get the list of ptl_process_id_t from the runtime and copy into structure */
    for (i = 0 ; i < nprocs ; ++i) {
        ptl_process_t *modex_id;
        size_t size;

        if( procs[i] == ompi_proc_local_proc ) {
            me = i;
        }

        if (procs[i]->super.proc_arch != ompi_proc_local()->super.proc_arch) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "Portals 4 MTL does not support heterogeneous operations.");
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "Proc %s architecture %x, mine %x.",
                                OMPI_NAME_PRINT(&procs[i]->super.proc_name), 
                                procs[i]->super.proc_arch, ompi_proc_local()->super.proc_arch);
            return OMPI_ERR_NOT_SUPPORTED;
        }

        OPAL_MODEX_RECV(ret, &mca_mtl_portals4_component.mtl_version,
                        &procs[i]->super, (uint8_t**)&modex_id, &size);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: ompi_modex_recv failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        } else if (sizeof(ptl_process_t) != size) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: ompi_modex_recv failed: %d\n",
                                __FILE__, __LINE__, ret);
            return OMPI_ERR_BAD_PARAM;
        }

        if (NULL == procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
            ptl_process_t *peer_id;
            peer_id = malloc(sizeof(ptl_process_t));
            if (NULL == peer_id) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: malloc failed: %d\n",
                                    __FILE__, __LINE__, ret);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            *peer_id = *modex_id;
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4] = peer_id;

            new_found = true;
        } else {
            ptl_process_t *proc = (ptl_process_t*) procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4];
            if (proc->phys.nid != modex_id->phys.nid ||
                proc->phys.pid != modex_id->phys.pid) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: existing peer and modex peer don't match\n",
                                    __FILE__, __LINE__);
                return OMPI_ERROR;
            }
        }
    }

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    if (new_found) {
        ret = ompi_mtl_portals4_flowctl_add_procs(me, nprocs, procs);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: flowctl_add_procs failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }
    }
#endif

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_del_procs(struct mca_mtl_base_module_t *mtl,
                            size_t nprocs,
                            struct ompi_proc_t** procs) 
{
    size_t i;

    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
            free(procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4] = NULL;
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
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    {
        int i;
        int num_mds = ompi_mtl_portals4_get_num_mds();

        for (i = 0 ; i < num_mds ; ++i) {
            PtlMDRelease(ompi_mtl_portals4.send_md_hs[i]);
        }

        free(ompi_mtl_portals4.send_md_hs);
    }
#else
    PtlMDRelease(ompi_mtl_portals4.send_md_h);
#endif

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

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
 * Copyright (c) 2010-2015 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <portals4.h>

#include "ompi/communicator/communicator.h"
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

static int
portals4_init_interface(void)
{
    unsigned int ret;
    ptl_md_t md;
    ptl_me_t me;

    /* create event queues */
    ret = PtlEQAlloc(ompi_mtl_portals4.ni_h,
                     ompi_mtl_portals4.send_queue_size,
                     &ompi_mtl_portals4.send_eq_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    ret = PtlEQAlloc(ompi_mtl_portals4.ni_h,
                     ompi_mtl_portals4.recv_queue_size,
                     &ompi_mtl_portals4.recv_eq_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Create send and long message (read) portal table entries */
    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_ONLY_USE_ONCE |
                     PTL_PT_ONLY_TRUNCATE |
                     PTL_PT_FLOWCTRL,
                     ompi_mtl_portals4.recv_eq_h,
                     REQ_RECV_TABLE_ID,
                     &ompi_mtl_portals4.recv_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    if (ompi_mtl_portals4.recv_idx != REQ_RECV_TABLE_ID) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc did not allocate the requested PT: %d\n",
                            __FILE__, __LINE__, ompi_mtl_portals4.recv_idx);
        goto error;
    }

    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_ONLY_USE_ONCE |
                     PTL_PT_ONLY_TRUNCATE,
                     ompi_mtl_portals4.send_eq_h,
                     REQ_READ_TABLE_ID,
                     &ompi_mtl_portals4.read_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    if (ompi_mtl_portals4.read_idx != REQ_READ_TABLE_ID) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc did not allocate the requested PT: %d\n",
                            __FILE__, __LINE__, ompi_mtl_portals4.read_idx);
        goto error;
    }

    /* bind zero-length md for sending acks */
    md.start     = NULL;
    md.length    = 0;
    md.options   = 0;
    md.eq_handle = PTL_EQ_NONE;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                    &md,
                    &ompi_mtl_portals4.zero_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Bind MD across all memory */
    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = 0;
    md.eq_handle = ompi_mtl_portals4.send_eq_h;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                    &md,
                    &ompi_mtl_portals4.send_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Handle long overflows */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = ompi_mtl_portals4.uid;
    me.options = PTL_ME_OP_PUT |
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_COMM_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE;
    if (ompi_mtl_portals4.use_logical) {
        me.match_id.rank = PTL_RANK_ANY;
    } else {
        me.match_id.phys.nid = PTL_NID_ANY;
        me.match_id.phys.pid = PTL_PID_ANY;
    }
    me.match_bits = MTL_PORTALS4_LONG_MSG;
    me.ignore_bits = MTL_PORTALS4_CONTEXT_MASK |
        MTL_PORTALS4_SOURCE_MASK |
        MTL_PORTALS4_TAG_MASK;

    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.recv_idx,
                      &me,
                      PTL_OVERFLOW_LIST,
                      NULL,
                      &ompi_mtl_portals4.long_overflow_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* attach short unex recv blocks */
    ret = ompi_mtl_portals4_recv_short_init();
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: short receive block initialization failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ompi_mtl_portals4.opcount = 0;
#if OPAL_ENABLE_DEBUG
    ompi_mtl_portals4.recv_opcount = 0;
#endif

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    ret = ompi_mtl_portals4_flowctl_init();
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: ompi_mtl_portals4_flowctl_init failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
#endif

    return OMPI_SUCCESS;

 error:
    if (!PtlHandleIsEqual(ompi_mtl_portals4.long_overflow_me_h, PTL_INVALID_HANDLE)) {
        PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.zero_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(ompi_mtl_portals4.zero_md_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.send_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(ompi_mtl_portals4.send_md_h);
    }
    if (ompi_mtl_portals4.read_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.read_idx);
    }
    if (ompi_mtl_portals4.recv_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.recv_idx);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.send_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(ompi_mtl_portals4.send_eq_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.recv_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(ompi_mtl_portals4.recv_eq_h);
    }
    return OMPI_ERROR;
}

static int
create_maptable(size_t        nprocs,
                ompi_proc_t **procs)
{
    int ret;
    size_t i;
    ptl_process_t *maptable;

    maptable = malloc(sizeof(ptl_process_t) * nprocs);
    if (NULL == maptable) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: malloc failed\n",
                            __FILE__, __LINE__);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i=0;i<nprocs;i++) {
        ptl_process_t *modex_id;
        size_t size;

        OPAL_MODEX_RECV(ret, &mca_mtl_portals4_component.mtl_version,
                        &procs[i]->super.proc_name, (uint8_t**)&modex_id, &size);
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

        maptable[i].phys.pid = modex_id->phys.pid;
        maptable[i].phys.nid = modex_id->phys.nid;
        opal_output_verbose(50, ompi_mtl_base_framework.framework_output,
            "logical: global rank=%d pid=%x nid=%x\n",
            (int)i, maptable[i].phys.pid, maptable[i].phys.nid);
    }

    ret = PtlSetMap(ompi_mtl_portals4.ni_h, nprocs, maptable);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: logical mapping failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
        "logical mapping OK\n");

    free(maptable);

    return OMPI_SUCCESS;
}

static int
create_endpoint(ompi_proc_t *proc)
{
    ptl_process_t *endpoint;

    endpoint = malloc(sizeof(ptl_process_t));
    if (NULL == endpoint) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: malloc failed: %s\n",
                            __FILE__, __LINE__, strerror(errno));
        return OMPI_ERR_OUT_OF_RESOURCE;
    } else {
        if (ompi_mtl_portals4.use_logical) {
            endpoint->phys.nid = 0;
            endpoint->phys.pid = 0;
            endpoint->rank = proc->super.proc_name.vpid;
        } else {
            int ret;
            ptl_process_t *modex_id;
            size_t size;

            OPAL_MODEX_RECV(ret, &mca_mtl_portals4_component.mtl_version,
                            &proc->super.proc_name, (uint8_t**)&modex_id, &size);
            if (OMPI_SUCCESS != ret) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: ompi_modex_recv failed: %d\n",
                                    __FILE__, __LINE__, ret);
                return ret;
            } else if (sizeof(ptl_process_t) != size) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: ompi_modex_recv failed (size mismatch): %d\n",
                                    __FILE__, __LINE__, ret);
                return OMPI_ERR_BAD_PARAM;
            }

            *endpoint = *modex_id;
        }
    }

    proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4] = endpoint;

    return OMPI_SUCCESS;
}

ompi_proc_t *
ompi_mtl_portals4_get_proc_group(struct ompi_group_t *group, int rank)
{
    int ret;

    ompi_proc_t *proc = ompi_group_peer_lookup (group, rank);
    if (NULL == proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
        ret = create_endpoint(proc);
        if (OMPI_SUCCESS != ret) {
            return NULL;
        }
#if 0
    } else {
        /*
         * sanity check
         */
        int ret;
        ptl_process_t *modex_id;
        size_t size;

        OPAL_MODEX_RECV(ret, &mca_mtl_portals4_component.mtl_version,
                        &proc->super.proc_name, (uint8_t**)&modex_id, &size);

        ptl_process_t *peer = (ptl_process_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4];
        if (ompi_mtl_portals4.use_logical) {
            if ((size_t)peer->rank != proc->super.proc_name.vpid) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: existing peer and rank don't match\n",
                                __FILE__, __LINE__);
                return OMPI_ERROR;
            }
        }
        else if (peer->phys.nid != modex_id->phys.nid ||
                 peer->phys.pid != modex_id->phys.pid) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: existing peer and modex peer don't match\n",
                                __FILE__, __LINE__);
            return OMPI_ERROR;
        }
#endif
    }

    return proc;
}

static int
add_endpoints(size_t        nprocs,
              ompi_proc_t **procs)
{
    int ret;
    size_t i;

    /* Get the list of ptl_process_id_t from the runtime and copy into structure */
    for (i = 0 ; i < nprocs ; ++i) {
        if (procs[i]->super.proc_arch != ompi_proc_local()->super.proc_arch) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "Portals 4 MTL does not support heterogeneous operations.");
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "Proc %s architecture %x, mine %x.",
                                OMPI_NAME_PRINT(&procs[i]->super.proc_name),
                                procs[i]->super.proc_arch, ompi_proc_local()->super.proc_arch);
            return OMPI_ERR_NOT_SUPPORTED;
        }

        if (NULL == procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
            ret = create_endpoint(procs[i]);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
#if 0
        } else {
            /*
             * sanity check
             */
            int ret;
            ptl_process_t *modex_id;
            size_t size;

            OPAL_MODEX_RECV(ret, &mca_mtl_portals4_component.mtl_version,
                            &procs[i]->super.proc_name, (uint8_t**)&modex_id, &size);

            ptl_process_t *proc = (ptl_process_t*) procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4];
            if (ompi_mtl_portals4.use_logical) {
                if ((size_t)proc->rank != procs[i]->super.proc_name.vpid) {
                    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: existing peer and rank don't match\n",
                                    __FILE__, __LINE__);
                    return OMPI_ERROR;
                }
            }
            else if (proc->phys.nid != modex_id->phys.nid ||
                     proc->phys.pid != modex_id->phys.pid) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: existing peer and modex peer don't match\n",
                                    __FILE__, __LINE__);
                return OMPI_ERROR;
            }
#endif
        }
    }

    return OMPI_SUCCESS;
}

#define NEED_ALL_PROCS (ompi_mtl_portals4.use_logical || ompi_mtl_portals4.use_flowctl)

int
ompi_mtl_portals4_add_procs(struct mca_mtl_base_module_t *mtl,
                            size_t nprocs,
                            struct ompi_proc_t** procs)
{
    int ret;

    /*
     * The PML handed us a list of procs that need Portals4
     * peer info.  Complete those procs here.
     */
    ret = add_endpoints(nprocs,
                        procs);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: add_endpoints failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    if (1 == ompi_mtl_portals4.need_init) {
        if (1 == ompi_mtl_portals4.use_logical) {
            ret = create_maptable(nprocs, procs);
            if (OMPI_SUCCESS != ret) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: ompi_mtl_portals4_add_procs::create_maptable() failed: %d\n",
                                    __FILE__, __LINE__, ret);
                return ret;
            }
        }

        /*
         * This is the first time through here.  Initialize
         * Portals4 and register the progress thread.
         */
        portals4_init_interface();

        /* activate progress callback */
        ret = opal_progress_register(ompi_mtl_portals4_progress);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: opal_progress_register failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
        opal_output_verbose(50, ompi_mtl_base_framework.framework_output,
                            "add_procs() - me=%d\n", ompi_proc_local_proc->super.proc_name.vpid);

        opal_output_verbose(50, ompi_mtl_base_framework.framework_output,
                            "add_procs() - adding flowctl procs\n");

        ret = ompi_mtl_portals4_flowctl_add_procs(ompi_proc_local_proc->super.proc_name.vpid,
                                                  nprocs,
                                                  procs);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: flowctl_add_procs failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }
#endif

        ompi_mtl_portals4.need_init = 0;
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_del_procs(struct mca_mtl_base_module_t *mtl,
                            size_t nprocs,
                            struct ompi_proc_t** procs)
{
    size_t i;

    opal_output_verbose(50, ompi_mtl_base_framework.framework_output,
        "del_procs() - enter\n");

    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
            free(procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4] = NULL;
        }
    }

    opal_output_verbose(50, ompi_mtl_base_framework.framework_output,
        "del_procs() - exit\n");

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

    if (!PtlHandleIsEqual(ompi_mtl_portals4.long_overflow_me_h, PTL_INVALID_HANDLE)) {
        PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.zero_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(ompi_mtl_portals4.zero_md_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.send_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(ompi_mtl_portals4.send_md_h);
    }
    if (ompi_mtl_portals4.read_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.read_idx);
    }
    if (ompi_mtl_portals4.recv_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.recv_idx);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.send_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(ompi_mtl_portals4.send_eq_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.recv_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(ompi_mtl_portals4.recv_eq_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.ni_h, PTL_INVALID_HANDLE)) {
        PtlNIFini(ompi_mtl_portals4.ni_h);
    }

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

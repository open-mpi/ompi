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

#include "ompi/mca/mtl/mtl.h"
#include "ompi/communicator/communicator.h"
#include "opal/class/opal_list.h"

#include "mtl_portals.h"
#include "mtl_portals_endpoint.h"
#include "mtl_portals_request.h"
#include "mtl_portals_recv_short.h"


mca_mtl_portals_module_t ompi_mtl_portals = {
    {
        8191,        /* max cid - 2^13 - 1 */
        (1UL << 30), /* max tag value - must allow negatives */
        0,           /* request reserve space */
        0,           /* flags */

        ompi_mtl_portals_add_procs,
        ompi_mtl_portals_del_procs,
        ompi_mtl_portals_finalize,

        NULL,
        ompi_mtl_portals_isend,
        ompi_mtl_portals_irecv,
        ompi_mtl_portals_iprobe,

        NULL        /* cancel */
    }
};


/* BWB - fix me - this should be an ompi_free_list_item_t */
OBJ_CLASS_INSTANCE(ompi_mtl_portals_event_t, opal_list_item_t,
                   NULL, NULL);



int
ompi_mtl_portals_add_procs(struct mca_mtl_base_module_t *mtl,
                           size_t nprocs,
                           struct ompi_proc_t** procs, 
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    int ret = OMPI_SUCCESS;
    ptl_process_id_t *portals_procs = NULL;
    size_t i;

    assert(mtl == &ompi_mtl_portals.base);

    /* if we havne't already initialized the network, do so now.  We
       delay until add_procs because if we want the automatic runtime
       environment setup the common code does for the utcp
       implementation, we can't do it until modex information can be
       received. */
    if (PTL_INVALID_HANDLE == ompi_mtl_portals.ptl_ni_h) {
        ptl_md_t md;
        ptl_handle_md_t md_h;
        ptl_process_id_t ptlproc;
        uint64_t match_bits = 0;

        ret = ompi_common_portals_ni_initialize(&(ompi_mtl_portals.ptl_ni_h));
        if (OMPI_SUCCESS != ret) goto cleanup;

        /* initialize the event queues */
        ret = PtlEQAlloc(ompi_mtl_portals.ptl_ni_h,
                         ompi_mtl_portals.ptl_expected_queue_size,
                         PTL_EQ_HANDLER_NONE,
                         &(ompi_mtl_portals.ptl_eq_h));
        assert(ret == PTL_OK);

        ret = PtlEQAlloc(ompi_mtl_portals.ptl_ni_h,
                         ompi_mtl_portals.ptl_unexpected_queue_size,
                         PTL_EQ_HANDLER_NONE,
                         &(ompi_mtl_portals.ptl_unexpected_recv_eq_h));
        assert(ret == PTL_OK);

        /* create insertion point for matched receives */
        ptlproc.nid = 0;
        ptlproc.pid = 0;

        ret = PtlMEAttach(ompi_mtl_portals.ptl_ni_h,
                          OMPI_MTL_PORTALS_SEND_TABLE_ID,
                          ptlproc,
                          ~match_bits,
                          match_bits,
                          PTL_RETAIN,
                          PTL_INS_AFTER,
                          &(ompi_mtl_portals.ptl_match_ins_me_h));
        assert(ret == PTL_OK);

        /* create unexpected message match entry */
        ptlproc.nid = PTL_NID_ANY;
        ptlproc.pid = PTL_PID_ANY;

        /* unexpected message match entry should receive from anyone,
           so ignore bits are all 1 */
        ret = PtlMEInsert(ompi_mtl_portals.ptl_match_ins_me_h,
                          ptlproc,
                          match_bits,
                          ~match_bits, 
                          PTL_RETAIN,
                          PTL_INS_AFTER,
                          &(ompi_mtl_portals.ptl_unexpected_me_h));
        assert(ret == PTL_OK);

        md.start = NULL;
        md.length = 0;
        md.threshold = PTL_MD_THRESH_INF;
        md.max_size = 0;
        md.options = (PTL_MD_OP_PUT | PTL_MD_TRUNCATE | PTL_MD_ACK_DISABLE);
        md.eq_handle = ompi_mtl_portals.ptl_unexpected_recv_eq_h;

        ret = PtlMDAttach(ompi_mtl_portals.ptl_unexpected_me_h,
                          md, 
                          PTL_RETAIN, 
                          &md_h);
        assert(ret == PTL_OK);

        ret = ompi_mtl_portals_recv_short_enable((mca_mtl_portals_module_t*) mtl);

        opal_progress_register(ompi_mtl_portals_progress);
    }

    /* get the list of ptl_process_id_t structures for the given proc
       structures.  If the Portals runtime environment supports
       comm_spawn, we'll be able to support it as well. */
    portals_procs = malloc(sizeof(ptl_process_id_t) * nprocs);
    if (NULL == portals_procs) goto cleanup;
    ret = ompi_common_portals_get_procs(nprocs, procs, portals_procs);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* copy the ptl_process_id_t information into our per-proc data
       store */
    for (i = 0 ; i < nprocs ; ++i) {
        mtl_peer_data[i] = malloc(sizeof(struct mca_mtl_base_endpoint_t));
        if (NULL == mtl_peer_data[i]) goto cleanup;
 
        mtl_peer_data[i]->ptl_proc.nid = portals_procs[i].nid;
        mtl_peer_data[i]->ptl_proc.pid = portals_procs[i].pid;
    }
    
 cleanup:
    if (NULL != portals_procs) free(portals_procs);
    return ret;
}


int
ompi_mtl_portals_del_procs(struct mca_mtl_base_module_t *mtl,
                           size_t nprocs,
                           struct ompi_proc_t** procs, 
                           struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    size_t i;

    assert(mtl == &ompi_mtl_portals.base);

    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != mtl_peer_data[i]) {
            free(mtl_peer_data[i]);
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals_finalize(struct mca_mtl_base_module_t *mtl)
{
    assert(mtl == &ompi_mtl_portals.base);

    ompi_mtl_portals_recv_short_disable((mca_mtl_portals_module_t *) mtl);

    opal_progress_unregister(ompi_mtl_portals_progress);

    while (0 != ompi_mtl_portals_progress()) { }

    ompi_common_portals_ni_finalize();
    ompi_common_portals_finalize();

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals_progress(void)
{
    int count = 0, ret;
    ptl_event_t ev;
    ompi_mtl_portals_request_t *ptl_request;

    while (true) {
        ret = PtlEQGet(ompi_mtl_portals.ptl_eq_h, &ev);
        if (PTL_OK == ret) {
            if (ev.type == PTL_EVENT_UNLINK) continue;

            if (NULL != ev.md.user_ptr) {
                ptl_request = ev.md.user_ptr;
                ret = ptl_request->event_callback(&ev, ptl_request);

                if (OMPI_SUCCESS != ret) {
                    opal_output(stderr," Error returned from the even callback.  Error code - %d \n",ret);
                    abort();
                }
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break;
        } else {
            opal_output(stderr," Error returned from PtlEQGet.  Error code - %d \n",ret);
            abort();
        }
    }

    return count;
}

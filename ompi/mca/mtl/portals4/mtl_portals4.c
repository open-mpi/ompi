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
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
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
#include "opal/class/opal_list.h"

#include "mtl_portals4.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_recv_short.h"

mca_mtl_portals4_module_t ompi_mtl_portals4 = {
    {
        8191,        /* max cid - 2^13 - 1 */
        (1UL << 30), /* max tag value - must allow negatives */
        0,           /* request reserve space */
        0,           /* flags */

        ompi_mtl_portals4_add_procs,
        ompi_mtl_portals4_del_procs,
        ompi_mtl_portals4_finalize,

        NULL, /* send */
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
    int ret;
    ptl_md_t md;
    ptl_me_t me;
    size_t i;
    struct runtime_proc_t *ptlprocs;
    int nptlprocs;
    ptl_pt_index_t pt;
    
    /* create event queue */
    ret = PtlEQAlloc(ompi_mtl_portals4.ni_h,
                     ompi_mtl_portals4.queue_size,
                     &(ompi_mtl_portals4.eq_h));
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlEQAlloc failed: %d\n",
                    __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    /* Create portal table entries */
    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_FLOWCTRL,
                     ompi_mtl_portals4.eq_h,
                     PTL_SEND_TABLE_ID,
                     &pt);
    if (PTL_OK != ret) {
        PtlEQFree(ompi_mtl_portals4.eq_h);
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlPTAlloc failed: %d\n",
                    __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }
    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_FLOWCTRL,
                     ompi_mtl_portals4.eq_h,
                     PTL_READ_TABLE_ID,
                     &pt);
    if (PTL_OK != ret) {
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_SEND_TABLE_ID);
        PtlEQFree(ompi_mtl_portals4.eq_h);
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlPTAlloc failed: %d\n",
                    __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    /* bind zero-length md for sending acks */
    md.start     = NULL;
    md.length    = 0;
    md.options   = 0;
    md.eq_handle = PTL_EQ_NONE;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                    &md,
                    &ompi_mtl_portals4.zero_md_h ); 
    if (PTL_OK != ret) {
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_READ_TABLE_ID);
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_SEND_TABLE_ID);
        PtlEQFree(ompi_mtl_portals4.eq_h);
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlMDBind failed: %d\n",
                    __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    /* Handle long overflows */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.ac_id.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | PTL_ME_ACK_DISABLE | PTL_ME_EVENT_COMM_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = PTL_LONG_MSG;
    me.ignore_bits = PTL_CONTEXT_MASK | PTL_SOURCE_MASK | PTL_TAG_MASK;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      PTL_SEND_TABLE_ID,
                      &me,
                      PTL_OVERFLOW,
                      NULL,
                      &ompi_mtl_portals4.long_overflow_me_h);
    if (PTL_OK != ret) {
        PtlMDRelease(ompi_mtl_portals4.zero_md_h);
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_READ_TABLE_ID);
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_SEND_TABLE_ID);
        PtlEQFree(ompi_mtl_portals4.eq_h);
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlMEAppend failed: %d\n",
                    __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    /* attach short unex recv blocks */
    ret = ompi_mtl_portals4_recv_short_init((mca_mtl_portals4_module_t*) mtl);
    if (OMPI_SUCCESS != ret) {
        PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
        PtlMDRelease(ompi_mtl_portals4.zero_md_h);
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_READ_TABLE_ID);
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_SEND_TABLE_ID);
        PtlEQFree(ompi_mtl_portals4.eq_h);
        opal_output(ompi_mtl_base_output,
                    "%s:%d: short receive block initialization failed: %d\n",
                    __FILE__, __LINE__, ret);
        return ret;
    }

    /* activate progress callback */
    opal_progress_register(ompi_mtl_portals4_progress);

    /* Get the list of ptl_process_id_t from the runtime and copy into structure */
    nptlprocs = runtime_get_nidpid_map(&ptlprocs);
    if ((size_t)nptlprocs != nprocs) {
        PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
        PtlMDRelease(ompi_mtl_portals4.zero_md_h);
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_READ_TABLE_ID);
        PtlPTFree(ompi_mtl_portals4.ni_h, PTL_SEND_TABLE_ID);
        PtlEQFree(ompi_mtl_portals4.eq_h);
        opal_output(ompi_mtl_base_output,
                    "%s:%d: nptlprocs != nprocs: %d\n",
                    __FILE__, __LINE__, ret);
        return OMPI_ERR_NOT_SUPPORTED;
    }
    for (i = 0 ; i < nprocs ; ++i) {
        mtl_peer_data[i] = malloc(sizeof(struct mca_mtl_base_endpoint_t));
        if (NULL == mtl_peer_data[i]) {
            PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
            PtlMDRelease(ompi_mtl_portals4.zero_md_h);
            PtlPTFree(ompi_mtl_portals4.ni_h, PTL_READ_TABLE_ID);
            PtlPTFree(ompi_mtl_portals4.ni_h, PTL_SEND_TABLE_ID);
            PtlEQFree(ompi_mtl_portals4.eq_h);
            opal_output(ompi_mtl_base_output,
                        "%s:%d: malloc failed: %d\n",
                        __FILE__, __LINE__, ret);
            return OMPI_ERROR;
        }
 
        mtl_peer_data[i]->ptl_proc.phys.nid = ptlprocs[i].nid;
        mtl_peer_data[i]->ptl_proc.phys.pid = ptlprocs[i].pid;
    }

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

    ompi_mtl_portals4_recv_short_fini(&ompi_mtl_portals4);
    PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
    PtlMDRelease(ompi_mtl_portals4.zero_md_h);
    PtlEQFree(ompi_mtl_portals4.eq_h);
    PtlPTFree(ompi_mtl_portals4.ni_h, PTL_READ_TABLE_ID);
    PtlPTFree(ompi_mtl_portals4.ni_h, PTL_SEND_TABLE_ID);

    PtlNIFini(ompi_mtl_portals4.ni_h);
    PtlFini();

    return OMPI_SUCCESS;
}

int
ompi_mtl_portals4_cancel(struct mca_mtl_base_module_t* mtl,
                         mca_mtl_request_t *mtl_request,
                         int flag)
{
    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_progress(void)
{
    int count = 0, ret;
    ptl_event_t ev;
    ompi_mtl_portals4_request_t *ptl_request;

    while (true) {
	ret = PtlEQGet(ompi_mtl_portals4.eq_h, &ev);
        if (PTL_OK == ret) {
            OPAL_OUTPUT_VERBOSE((ompi_mtl_base_output, 50,
                                 "Found event of type %d\n", ev.type));
            switch (ev.type) {
            case PTL_EVENT_GET:
            case PTL_EVENT_PUT:
            case PTL_EVENT_PUT_OVERFLOW:
            case PTL_EVENT_ATOMIC:
            case PTL_EVENT_ATOMIC_OVERFLOW:
                if (NULL != ev.user_ptr) {
                    ptl_request = ev.user_ptr;
                    ret = ptl_request->event_callback(&ev, ptl_request);
                    if (OMPI_SUCCESS != ret) {
                        opal_output(ompi_mtl_base_output,
                                    "Error returned from target event callback: %d", ret);
                        abort();
                    }
                }
                break;
            case PTL_EVENT_REPLY:
            case PTL_EVENT_SEND:
            case PTL_EVENT_ACK:
                if (NULL != ev.user_ptr) {
                    ptl_request = ev.user_ptr;
                    ret = ptl_request->event_callback(&ev, ptl_request);
                    if (OMPI_SUCCESS != ret) {
                        opal_output(ompi_mtl_base_output,
                                    "Error returned from initiator event callback: %d", ret);
                        abort();
                    }
                }
                break;
            case PTL_EVENT_PT_DISABLED:
                /* do stuff - flow control */
                opal_output(ompi_mtl_base_output, "Unhandled flow control event.");
                abort();
                break;
            case PTL_EVENT_AUTO_UNLINK:
                break;
            case PTL_EVENT_AUTO_FREE:
                if (OMPI_SUCCESS != (ret = ompi_mtl_portals4_recv_short_block_repost(&ev))) {
                    opal_output(ompi_mtl_base_output,
                                "Error returned from PTL_EVENT_FREE callback: %d", ret);
                    abort();
                }
                break;
            case PTL_EVENT_SEARCH:
                if (NULL != ev.user_ptr) {
                    ptl_request = ev.user_ptr;
                    ret = ptl_request->event_callback(&ev, ptl_request);
                    if (OMPI_SUCCESS != ret) {
                        opal_output(ompi_mtl_base_output,
                                    "Error returned from target event callback: %d", ret);
                        abort();
                    }
                }
                break;
            default:
                opal_output(ompi_mtl_base_output,
                            "Unknown event type %d (error: %d)", (int)ev.type, ret);
                abort();
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break;
        } else {
            opal_output(ompi_mtl_base_output,
                        "Error returned from PtlEQGet: %d", ret);
            abort();
        }
    }

    return count;
}

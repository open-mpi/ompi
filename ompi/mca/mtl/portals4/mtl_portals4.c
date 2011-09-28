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

#include "orte/util/name_fns.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/mtl/mtl.h"
#include "opal/class/opal_list.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "mtl_portals4.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_request.h"
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

        NULL, /* send */
        ompi_mtl_portals4_isend,
        ompi_mtl_portals4_irecv,
        ompi_mtl_portals4_iprobe,

        NULL,       /* cancel */
        NULL,       /* add_comm */
        NULL        /* del_comm */
    }
};


int
ompi_mtl_portals4_add_procs(struct mca_mtl_base_module_t *mtl,
                            size_t nprocs,
                            struct ompi_proc_t** procs, 
                            struct mca_mtl_base_endpoint_t **mtl_peer_data)
{
    int ret;
    size_t i;

    /* Get the list of ptl_process_id_t from the runtime and copy into structure */
    for (i = 0 ; i < nprocs ; ++i) {
        ptl_process_t *id;
        size_t size;

        if (procs[i]->proc_arch != ompi_proc_local()->proc_arch) {
            opal_output(ompi_mtl_base_output,
                        "Portals 4 MTL does not support heterogeneous operations.");
            opal_output(ompi_mtl_base_output,
                        "Proc %s architecture %x, mine %x.",
                        ORTE_NAME_PRINT(&procs[i]->proc_name), 
                        procs[i]->proc_arch, ompi_proc_local()->proc_arch);
            return OMPI_ERR_NOT_SUPPORTED;
        }

        mtl_peer_data[i] = malloc(sizeof(struct mca_mtl_base_endpoint_t));
        if (NULL == mtl_peer_data[i]) {
            opal_output(ompi_mtl_base_output,
                        "%s:%d: malloc failed: %d\n",
                        __FILE__, __LINE__, ret);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ret = ompi_modex_recv(&mca_mtl_portals4_component.mtl_version,
                              procs[i], (void**) &id, &size);
        if (OMPI_SUCCESS != ret) {
            opal_output(ompi_mtl_base_output,
                        "%s:%d: ompi_modex_recv failed: %d\n",
                        __FILE__, __LINE__, ret);
            return ret;
        } else if (sizeof(ptl_process_t) != size) {
            opal_output(ompi_mtl_base_output,
                        "%s:%d: ompi_modex_recv failed: %d\n",
                        __FILE__, __LINE__, ret);
            return OMPI_ERR_BAD_PARAM;
        }

        mtl_peer_data[i]->ptl_proc = *id;
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
    PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.read_idx);
    PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.send_idx);
    PtlEQFree(ompi_mtl_portals4.eq_h);
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
    ompi_mtl_portals4_base_request_t *ptl_request;

    while (true) {
	ret = PtlEQGet(ompi_mtl_portals4.eq_h, &ev);
        if (PTL_OK == ret) {
            OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
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
                opal_output(ompi_mtl_base_output, "Unhandled send flow control event.");
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

	ret = PtlEQGet(ompi_mtl_portals4.tmp_eq_h, &ev);
        if (PTL_OK == ret) {
            OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
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
                opal_output(ompi_mtl_base_output, "Unhandled read flow control event.");
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

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
#include "pml_portals.h"
#include "ompi/request/request.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "pml_portals_datatype.h"


struct pml_portals_recv_info_t {
    opal_list_item_t super;
    ptl_event_t ev;
};
typedef struct pml_portals_recv_info_t pml_portals_recv_info_t;
OBJ_CLASS_INSTANCE(pml_portals_recv_info_t, opal_list_item_t, NULL, NULL);


static int
get_data(ptl_event_t ev, ptl_md_t md, ompi_convertor_t *convertor)
{
    ptl_handle_md_t md_h;

    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "calling get_data for %ld", ev.hdr_data));

    /* create the floating md */
    md.threshold = 1;
    md.options = PTL_MD_EVENT_START_DISABLE;
    md.eq_handle = ompi_pml_portals.portals_blocking_receive_queue;

    PtlMDBind(ompi_pml_portals.portals_ni_h, md,
              PTL_RETAIN, &md_h);

    PtlGet(md_h, ev.initiator, PML_PTLS_INDEX_READ,
           0, ev.hdr_data, 0);

    PtlEQWait(ompi_pml_portals.portals_blocking_receive_queue, &ev);
    assert(ev.type == PTL_EVENT_REPLY_END);
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "get: REPLY_END event received"));

    PtlMDUnlink(md_h);

    return OMPI_SUCCESS;
}

int
ompi_pml_portals_irecv_init(void *addr,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int src,
                           int tag,
                           struct ompi_communicator_t *comm,
                           struct ompi_request_t **request)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_pml_portals_irecv(void *addr,
                       size_t count,
                       ompi_datatype_t * datatype,
                       int src,
                       int tag,
                       struct ompi_communicator_t *comm,
                       struct ompi_request_t **request)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_pml_portals_recv(void *buf,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status)
{
    uint64_t ignore_bits, match_bits;
    opal_list_item_t *list_item;
    int ret, free_after;
    ptl_md_t md, new_md;
    ptl_handle_md_t md_h;
    ptl_handle_me_t me_h;
    ptl_process_id_t portals_proc;
    ompi_pml_portals_proc_t *pml_portals_proc = 
        (ompi_pml_portals_proc_t*) comm->c_pml_procs[src];
    ptl_event_t ev;

    /* BWB - fix me - need some way of finding source in ANY_SOURCE case */
    ompi_convertor_copy_and_prepare_for_send(comm->c_pml_procs[comm->c_my_rank]->proc_ompi->proc_convertor,
                                             datatype,
                                             count,
                                             buf,
                                             0,
                                             &ompi_pml_portals.portals_blocking_receive_convertor);

    if (MPI_ANY_SOURCE == src) {
        portals_proc.nid = PTL_NID_ANY;
        portals_proc.pid = PTL_PID_ANY;
    } else {
        portals_proc = pml_portals_proc->proc_id;
    }
    ompi_pml_portals_prepare_md_recv(&ompi_pml_portals.portals_blocking_receive_convertor, &md, &free_after);

    PML_PTLS_RECV_BITS(match_bits, ignore_bits, comm->c_contextid, src, tag);

    /* first, make sure it's not in the queue of processed unexpected msgs */
    list_item = opal_list_get_first(&(ompi_pml_portals.portals_unexpected_events));
    while (list_item != opal_list_get_end(&(ompi_pml_portals.portals_unexpected_events))) {
        opal_list_item_t *next = opal_list_get_next(list_item);
        pml_portals_recv_info_t * info = (pml_portals_recv_info_t*) list_item;

        if ((info->ev.match_bits & ~ignore_bits) == match_bits) {
            /* we have a match... */
            OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                                "recv: event waiting in queue\n"));
            get_data(ev, md, &ompi_pml_portals.portals_blocking_receive_convertor);
            opal_list_remove_item(&(ompi_pml_portals.portals_unexpected_events),
                                  list_item);
            OBJ_RELEASE(list_item);
            goto cleanup;
        } else {
            OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                                "recv: ignoring data: %lx", info->ev.match_bits));
        }
        list_item = next;
    }

 restart_search:
    /* now check the unexpected event queue */
    while (true) {
        int ret = PtlEQGet(ompi_pml_portals.portals_unexpected_receive_queue,
                           &ev);
        if (PTL_OK == ret) {
            if ((ev.match_bits & ~ignore_bits) == match_bits) {
                /* we have a match... */
                OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                                    "recv: event waiting in portals\n"));
                get_data(ev, md, &ompi_pml_portals.portals_blocking_receive_convertor);
                goto cleanup;
            } else {
                pml_portals_recv_info_t *item = OBJ_NEW(pml_portals_recv_info_t);
                OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                                    "recv: ignoring data: %lx", ev.match_bits));
                item->ev = ev;
                opal_list_append(&(ompi_pml_portals.portals_unexpected_events),
                                 &(item->super));
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break;
        } else {
            abort();
        }

    }

    /* now post a receive */
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "receive from: %u, %u\n", portals_proc.nid, portals_proc.pid));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "receive match bits: %lx, %lx\n", match_bits, ignore_bits));
    PtlMEInsert(ompi_pml_portals.portals_unexpected_me_h,
                portals_proc,
                match_bits,
                ignore_bits,
                PTL_RETAIN,
                PTL_INS_BEFORE,
                &me_h);

    md.threshold = 0;
    md.options |= (PTL_MD_OP_PUT | PTL_MD_EVENT_START_DISABLE);
    md.eq_handle = ompi_pml_portals.portals_blocking_receive_queue;
    PtlMDAttach(me_h, md, PTL_RETAIN, &md_h);

    /* now try to make active */
    new_md = md;
    new_md.threshold = 3;
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "  calling PtlMDUpdate(\n"));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "    md_handle   = %d\n",md_h));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "    old md               =>\n"));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      start              = %p\n",md.start));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      length             = %d\n",md.length));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      threshold          = %d\n",md.threshold));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      options            = %d\n",md.options));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      user_ptr           = %p\n",md.user_ptr));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      eventq             = %d\n",md.eq_handle));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "    new md               =>\n"));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      start              = %p\n",new_md.start));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      length             = %d\n",new_md.length));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      threshold          = %d\n",new_md.threshold));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      options            = %d\n",new_md.options));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      user_ptr           = %p\n",new_md.user_ptr));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "      eventq             = %d\n",new_md.eq_handle));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "    test eventq          = %d )\n",ompi_pml_portals.portals_unexpected_receive_queue));
    
    ret = PtlMDUpdate(md_h, &md, &new_md, ompi_pml_portals.portals_unexpected_receive_queue);
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "recv: Update ret: %d\n", ret));
    if (ret == PTL_MD_NO_UPDATE) {
        OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                            "recv: no update :(\n"));
        /* a message has arrived since we searched - look again */
        PtlMDUnlink(md_h);
        if (free_after) { free(md.start); }
        goto restart_search;
    }

    /* wait for our completion event */
    PtlEQWait(ompi_pml_portals.portals_blocking_receive_queue, &ev);
    assert(ev.type == PTL_EVENT_PUT_END);
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "recv: PUT_END event received"));

    PtlMDUnlink(md_h);

 cleanup:
    ompi_pml_portals_free_md_recv(&ompi_pml_portals.portals_blocking_receive_convertor, &md, free_after);

    return OMPI_SUCCESS;
}


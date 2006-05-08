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
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "pml_portals_datatype.h"

int
ompi_pml_portals_isend_init(void* buf,
                            size_t count,
                            ompi_datatype_t* datatype,
                            int dst,
                            int tag,
                            mca_pml_base_send_mode_t sendmode,
                            ompi_communicator_t* comm,
                            ompi_request_t** request)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_pml_portals_isend(void* buf,
                       size_t count,
                       ompi_datatype_t* datatype,
                       int dst,
                       int tag,
                       mca_pml_base_send_mode_t sendmode,
                       ompi_communicator_t* comm,
                       ompi_request_t** request)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_pml_portals_send(void *buf,
                      size_t count,
                      ompi_datatype_t* datatype,
                      int dst,
                      int tag,
                      mca_pml_base_send_mode_t sendmode,
                      ompi_communicator_t* comm)
{
    int ret, free_after;
    static int msg_count = 1;
    uint64_t match_bits;

    ptl_md_t md;
    ptl_handle_me_t me_h;
    ptl_handle_md_t md_h;
    ptl_event_t ev;
    ompi_pml_portals_proc_t *portals_proc = 
        (ompi_pml_portals_proc_t*) comm->c_pml_procs[dst];

    if (MCA_PML_BASE_SEND_SYNCHRONOUS == sendmode) abort();

    ompi_convertor_copy_and_prepare_for_send(comm->c_pml_procs[dst]->proc_ompi->proc_convertor,
                                             datatype,
                                             count,
                                             buf,
                                             0,
                                             &ompi_pml_portals.portals_blocking_send_convertor);
    PtlMEAttach(ompi_pml_portals.portals_ni_h,
                PML_PTLS_INDEX_READ,
                portals_proc->proc_id,
                msg_count,
                0,
                PTL_RETAIN,
                PTL_INS_AFTER,
                &me_h);

    ompi_pml_portals_prepare_md_send(&ompi_pml_portals.portals_blocking_send_convertor, 
                                     &md, &free_after);
    md.threshold = 100;
    md.options |= (PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE);
    md.eq_handle = ompi_pml_portals.portals_blocking_send_queue;
    PtlMDAttach(me_h, md, PTL_RETAIN, &md_h);
    PML_PTLS_SEND_BITS(match_bits, comm->c_contextid, comm->c_my_rank, tag);

    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "send to: %u, %u\n", 
                         portals_proc->proc_id.nid, portals_proc->proc_id.pid));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "send match bits: %lx\n", match_bits));
    PtlPut(md_h, PTL_ACK_REQ, portals_proc->proc_id,
           PML_PTLS_INDEX_RECV, 0,
           match_bits, 0, msg_count);

    msg_count++;

    /* our send end event */
    ret = PtlEQWait(ompi_pml_portals.portals_blocking_send_queue, &ev);
    assert(ret == PTL_OK);
    assert(ev.type == PTL_EVENT_SEND_END);
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "send: SEND_END event received"));

    /* our ack / get event */
    ret = PtlEQWait(ompi_pml_portals.portals_blocking_send_queue, &ev);
    assert(ret == PTL_OK);
    assert((ev.type == PTL_EVENT_ACK) || (ev.type == PTL_EVENT_GET_END));
    OPAL_OUTPUT_VERBOSE((100, ompi_pml_portals.portals_output,
                        "send: GET_END/ACK event received"));

    ompi_pml_portals_free_md_send(&md, free_after);

    PtlMDUnlink(md_h);

    ompi_convertor_cleanup(&ompi_pml_portals.portals_blocking_send_convertor);

    return OMPI_SUCCESS;
}


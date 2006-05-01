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
    ompi_convertor_t convertor;
    int ret, free_after;
    static int msg_count = 0;
    uint64_t match_bits;

    ptl_md_t md;
    ptl_handle_me_t me_h;
    ptl_handle_md_t md_h;
    ptl_event_t ev;
    ompi_pml_portals_proc_t *portals_proc = 
        (ompi_pml_portals_proc_t*) comm->c_pml_procs[dst];


    if (MCA_PML_BASE_SEND_SYNCHRONOUS == sendmode) abort();

    OBJ_CONSTRUCT(&convertor, ompi_convertor_t);
    ompi_convertor_copy_and_prepare_for_send(comm->c_pml_procs[dst]->proc_ompi->proc_convertor,
                                             datatype,
                                             count,
                                             buf,
                                             0,
                                             &convertor);

    PtlMEAttach(ompi_pml_portals.portals_ni_h,
                PML_PTLS_INDEX_READ,
                portals_proc->proc_id,
                msg_count,
                0,
                PTL_UNLINK,
                PTL_INS_AFTER,
                &me_h);

    ompi_pml_portals_prepare_md_send(&convertor, &md, &free_after);
    md.threshold = 2;
    md.options |= (PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE);
    md.eq_handle = ompi_pml_portals.portals_blocking_send_queue;

    PtlMDAttach(me_h, md, PTL_UNLINK, &md_h);

    PML_PTLS_SEND_BITS(match_bits, comm->c_contextid, comm->c_my_rank, tag);

    printf("send to: %d, %d\n", portals_proc->proc_id.nid, portals_proc->proc_id.pid);
    printf("send match bits: %lx\n", match_bits);
    PtlPut(md_h, PTL_ACK_REQ, portals_proc->proc_id,
           PML_PTLS_INDEX_RECV, 0,
           match_bits, 0, msg_count);

    msg_count++;

    /* our send end event */
    ret = PtlEQWait(ompi_pml_portals.portals_blocking_send_queue, &ev);
    assert(ret == PTL_OK);
    assert(ev.type == PTL_EVENT_SEND_END);

    /* our ack / get event */
    ret = PtlEQWait(ompi_pml_portals.portals_blocking_send_queue, &ev);
    assert(ret == PTL_OK);
#if OMPI_PML_PORTALS_HAVE_EVENT_UNLINK
    if (ev.type == PTL_EVENT_UNLINK) {
        ret = PtlEQWait(ompi_pml_portals.portals_blocking_send_queue, &ev);
        assert(ret == PTL_OK);
    }
#endif
    assert((ev.type == PTL_EVENT_ACK) || (ev.type == PTL_EVENT_GET_END));

    ompi_pml_portals_free_md_send(&md, free_after);

    OBJ_DESTRUCT(&convertor);

    return OMPI_SUCCESS;
}


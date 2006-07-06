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

#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_portals.h"
#include "mtl_portals_request.h"
#include "mtl_portals_endpoint.h"

/* called when no ack is necessary */
static int
ompi_mtl_portals_send_progress_no_ack(ptl_event_t *ev,
                                      struct ompi_mtl_portals_request_t *ptl_request)
{
    switch (ev->type) {
    case PTL_EVENT_SEND_END:
        {
            /* the get finished, so we're done. */
            if (ptl_request->free_after) {
                free(ev->md.start);
            }
            ptl_request->super.completion_callback(&ptl_request->super);
        }

    default:
        break;
    }

    return OMPI_SUCCESS;
}

/* called when a send that should wait for an ack or longer shold be progressed */
static int
ompi_mtl_portals_send_progress(ptl_event_t *ev,
                               struct ompi_mtl_portals_request_t* ptl_request)
{
    switch (ev->type) {
    case PTL_EVENT_ACK:
    case PTL_EVENT_GET_END:
    case PTL_EVENT_PUT_END:
        /* we only receive an ack if the message was received into an
           expected message.  Otherwise, we don't get an ack, but mark
           completion when the message was pulled (long message) or
           acked via an explicit put (short synchronous message). */
        {
            if (ptl_request->free_after) {
                free(ev->md.start);
            }
            ptl_request->super.completion_callback(&ptl_request->super);
        }
        break;

    default:
        break;
    }
    
    return OMPI_SUCCESS;
}


int
ompi_mtl_portals_send(struct mca_mtl_base_module_t* mtl,
                      struct ompi_communicator_t* comm,
                      int dest,
                      int tag,
                      struct ompi_convertor_t *convertor,
                      mca_pml_base_send_mode_t mode)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_mtl_portals_isend(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t* comm,
                       int dest,
                       int tag,
                       struct ompi_convertor_t *convertor,
                       mca_pml_base_send_mode_t mode,
                       bool blocking,
                       mca_mtl_request_t *mtl_request)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    ptl_handle_me_t me_h;
    mca_mtl_base_endpoint_t *endpoint = 
        (mca_mtl_base_endpoint_t*) comm->c_pml_procs[dest]->proc_ompi->proc_pml;
    ompi_mtl_portals_request_t *ptl_request = 
        (ompi_mtl_portals_request_t*) mtl_request;
    size_t buflen;
    ompi_ptr_t ptr;

    assert(mtl == &ompi_mtl_portals.base);

    ret = ompi_mtl_datatype_pack(convertor, &md.start, &buflen,
                                 &(ptl_request->free_after));
    if (OMPI_SUCCESS != ret) return ret;
    md.length = buflen;

    ptl_request->event_callback = ompi_mtl_portals_send_progress;

    if ((MCA_PML_BASE_SEND_READY == mode)) {
        /* ready send (length doesn't matter) or short non-sync send.
           Eagerly send data and don't wait for completion */
        PTL_SET_SEND_BITS(match_bits, comm->c_contextid,
                          comm->c_my_rank,
                          tag, PTL_READY_MSG);
#if OMPI_MTL_PORTALS_DEBUG
        printf("ready send bits: 0x%016llx\n", match_bits);
#endif

        md.threshold = 1;
        md.options = PTL_MD_EVENT_START_DISABLE;
        md.user_ptr = ptl_request;
        md.eq_handle = ompi_mtl_portals.ptl_eq_h;

        ret = PtlMDBind(ompi_mtl_portals.ptl_ni_h,
                        md,
                        PTL_UNLINK,
                        &(md_h));
        if (OMPI_SUCCESS != ret) {
            if (ptl_request->free_after) free(md.start);
            return ompi_common_portals_error_ptl_to_ompi(ret);
        }

        ptl_request->event_callback = ompi_mtl_portals_send_progress_no_ack;

        ret = PtlPut(md_h,
                     PTL_NO_ACK_REQ,
                     endpoint->ptl_proc,
                     OMPI_MTL_PORTALS_SEND_TABLE_ID,
                     0,
                     match_bits,
                     0,
                     0);
        if (OMPI_SUCCESS != ret) {
            PtlMDUnlink(md_h);
            if (ptl_request->free_after) free(md.start);
            return ompi_common_portals_error_ptl_to_ompi(ret);
        }
    } else {
        /* it's a long message - same protocol for all send modes
           other than ready */
        PTL_SET_SEND_BITS(match_bits, comm->c_contextid,
                          comm->c_my_rank,
                          tag, PTL_LONG_MSG);
#if OMPI_MTL_PORTALS_DEBUG
        printf("long send bits: 0x%016llx (%d)\n", match_bits, dest);
#endif
        
        md.threshold = 2; /* send, {ack, get} */
        md.options = PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE;
        md.user_ptr = ptl_request;
        md.eq_handle = ompi_mtl_portals.ptl_eq_h;

        ptr.pval = ptl_request;
        ret = PtlMEAttach(ompi_mtl_portals.ptl_ni_h,
                          OMPI_MTL_PORTALS_READ_TABLE_ID,
                          endpoint->ptl_proc,
                          (ptl_match_bits_t) ptr.lval,
                          0,
                          PTL_UNLINK,
                          PTL_INS_AFTER,
                          &me_h);
        if (OMPI_SUCCESS != ret) {
            if (ptl_request->free_after) free(md.start);
            return ompi_common_portals_error_ptl_to_ompi(ret);
        }

        ret = PtlMDAttach(me_h,
                          md,
                          PTL_UNLINK,
                          &(md_h));

        if (OMPI_SUCCESS != ret) {
            PtlMEUnlink(me_h);
            if (ptl_request->free_after) free(md.start);
            return ompi_common_portals_error_ptl_to_ompi(ret);
        }

        ret = PtlPut(md_h,
                     PTL_ACK_REQ,
                     endpoint->ptl_proc,
                     OMPI_MTL_PORTALS_SEND_TABLE_ID,
                     0,
                     match_bits,
                     0,
                     (ptl_hdr_data_t) ptr.lval);
        if (OMPI_SUCCESS != ret) {
            PtlMDUnlink(md_h);
            if (ptl_request->free_after) free(md.start);
            return ompi_common_portals_error_ptl_to_ompi(ret);
        }
    }

    return OMPI_SUCCESS;
}


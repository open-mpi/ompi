/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

/*
 * utility functions for dealing with remote datatype and op structures
 */

#include "ompi_config.h"

#include "ompi/op/op.h"

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_obj_convert.h"

int
ompi_osc_pt2pt_process_op(ompi_osc_pt2pt_module_t *module,
                          ompi_osc_pt2pt_send_header_t *header,
                          struct ompi_datatype_t *datatype,
                          ompi_op_t *op,
                          void *inbuf,
                          size_t inbuflen)
{
    unsigned char *target_buffer;

    /* compute target buffer location */
    target_buffer = (unsigned char*) module->p2p_win->w_baseptr + 
        (header->hdr_target_disp * module->p2p_win->w_disp_unit);

    /* BWB - fix me - change back to the pointer comparison when the
       replace o_f_to_c_index is set properly */
    /*    if (op == &ompi_mpi_op_replace) { */
    if (header->hdr_target_op == ompi_mpi_op_replace.o_f_to_c_index) {
        ompi_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;
        ompi_proc_t *proc;

        /* create convertor */
        OBJ_CONSTRUCT(&convertor, ompi_convertor_t);

        /* initialize convertor */
        proc = ompi_comm_peer_lookup(module->p2p_comm, header->hdr_origin);
        ompi_convertor_copy_and_prepare_for_recv(proc->proc_convertor,
                                                 datatype,
                                                 header->hdr_target_count,
                                                 target_buffer,
                                                 0,
                                                 &convertor);

        /* short circuit the reduction operation MPI_REPLACE - it just
           replaces the data, so push it out into the user's buffer.
           This lets us avoid both the overhead of using the op
           invocation and dealing with non-contiguous reductions
           (since there are never user-defined reductions in
           MPI_ACCUMULATE) */
        iov.iov_len = inbuflen;
        iov.iov_base = (IOVBASE_TYPE*)inbuf;
        max_data = iov.iov_len;
        ompi_convertor_unpack(&convertor, 
                              &iov,
                              &iov_count,
                              &max_data);
        OBJ_DESTRUCT(&convertor);
    } else {
        /* reductions other than MPI_REPLACE.  Since user-defined
           reductions aren't allowed, these all have to be over
           contigous data.  We make sure to only send complete
           datatypes in these cases, so we can unpack directly from
           the user buffer*/
        /* BWB - FIX ME - this won't work if endianness is different.
           Talk to George about a ddt function that allows us to fix
           endianness "in place' or what else we could do here to keep
           performance from sucking... */

        ompi_op_reduce(op, inbuf, target_buffer, header->hdr_target_count,
                       datatype);
    } 

    return OMPI_SUCCESS;
}

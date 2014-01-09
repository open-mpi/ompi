/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "opal/datatype/opal_convertor.h"

#include "btl_portals4.h"

int mca_btl_portals4_send(struct mca_btl_base_module_t* btl_base,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    struct mca_btl_portals4_module_t* portals4_btl = (struct mca_btl_portals4_module_t*) btl_base;
    mca_btl_portals4_frag_t *frag = (mca_btl_portals4_frag_t*) descriptor;
    ptl_match_bits_t match_bits, msglen_type;
    ptl_size_t put_length;
    int64_t offset;
    ptl_handle_md_t md_h;
    void *base;
    int ret;

    frag->endpoint = endpoint;
    frag->hdr.tag = tag;

    put_length       = frag->segments[0].base.seg_len;
    if (put_length > portals4_btl->super.btl_eager_limit)
         msglen_type = BTL_PORTALS4_LONG_MSG;
    else msglen_type = BTL_PORTALS4_SHORT_MSG;

    BTL_PORTALS4_SET_SEND_BITS(match_bits, 0, 0, tag, msglen_type);

    ompi_btl_portals4_get_md(frag->segments[0].base.seg_addr.pval, &md_h, &base, portals4_btl);
    offset = (ptl_size_t) ((char*) frag->segments[0].base.seg_addr.pval - (char*) base);

    /* reserve space in the event queue for rdma operations immediately */
    while (OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, 1) >
           portals4_btl->portals_max_outstanding_ops) {
        OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                             "Call to mca_btl_portals4_component_progress (4)\n"));
        mca_btl_portals4_component_progress();
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                         "mca_btl_portals4_send: Incrementing portals_outstanding_ops=%d\n",
        portals4_btl->portals_outstanding_ops));

    OPAL_OUTPUT_VERBOSE((50, ompi_btl_base_framework.framework_output,
                         "PtlPut frag=%p pid=%x tag=%x len=%ld match_bits=%lx\n",
                         (void*)frag,  endpoint->ptl_proc.phys.pid, tag, 
                         put_length, (uint64_t)match_bits));

    ret = PtlPut(md_h,
                 (ptl_size_t) offset,
                 put_length, /* fragment length */
                 (mca_btl_portals4_component.portals_need_ack ? PTL_ACK_REQ : PTL_NO_ACK_REQ),
                 endpoint->ptl_proc,
                 portals4_btl->recv_idx,
                 match_bits,                     /* match bits */
                 0,                              /* remote offset - not used */
                 (void *) frag,                  /* user ptr */
                 tag);                           /* hdr_data: tag */
    if (ret != PTL_OK) {
        opal_output(ompi_btl_base_framework.framework_output, "mca_btl_portals4_send: PtlPut failed with error %d", ret);
        return OMPI_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlPut frag=%p pid=%x tag=%x addr=%p len=%ld match_bits=%lx\n",
        (void*)frag,  endpoint->ptl_proc.phys.pid, tag, (void *)offset, put_length, (uint64_t)match_bits));

    return OMPI_SUCCESS;
}

/* NOT IMPLEMENTED */
int mca_btl_portals4_sendi(struct mca_btl_base_module_t* btl_base,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct opal_convertor_t* convertor,
                          void* header,
                          size_t header_size,
                          size_t payload_size,
                          uint8_t order,
                          uint32_t flags,
                          mca_btl_base_tag_t tag, 
                          mca_btl_base_descriptor_t** des)
{
    opal_output(ompi_btl_base_framework.framework_output, "mca_btl_portals_sendi is not implemented");
    abort();
    return OMPI_SUCCESS;
}

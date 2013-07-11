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

static int mca_btl_portals4_try_to_use_fixed_md(void *start,
                    int length,
                    ptl_handle_md_t *md_h,
                    int64_t *offset,
                    mca_btl_portals4_frag_t *frag);

int mca_btl_portals4_send(struct mca_btl_base_module_t* btl_base,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    mca_btl_portals4_frag_t *frag = (mca_btl_portals4_frag_t*) descriptor;
    ptl_match_bits_t match_bits, msglen_type;
    ptl_size_t put_length, put_local_offset;
    int64_t offset;
    ptl_handle_md_t md_h;
    int ret;

    frag->endpoint = endpoint;
    frag->hdr.tag = tag;

    put_local_offset = (ptl_size_t) frag->segments[0].base.seg_addr.pval;
    put_length       = frag->segments[0].base.seg_len;
    if (put_length > mca_btl_portals4_module.super.btl_eager_limit)
         msglen_type = BTL_PORTALS4_LONG_MSG;
    else msglen_type = BTL_PORTALS4_SHORT_MSG;

    BTL_PORTALS4_SET_SEND_BITS(match_bits, 0, 0, tag, msglen_type);

    /* reserve space in the event queue for rdma operations immediately */
    while (OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, 1) >
           mca_btl_portals4_module.portals_max_outstanding_ops) {
        OPAL_THREAD_ADD32(&mca_btl_portals4_module.portals_outstanding_ops, -1);
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "Call to mca_btl_portals4_component_progress (4)\n"));
        mca_btl_portals4_component_progress();
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "mca_btl_portals4_send: Incrementing portals_outstanding_ops=%d\n",
        mca_btl_portals4_module.portals_outstanding_ops));

    ret = mca_btl_portals4_try_to_use_fixed_md((void*)put_local_offset, put_length, &md_h, &offset, frag);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_btl_base_framework.framework_output,
                         "PtlPut frag=%p pid=%x tag=%x addr=%p len=%ld match_bits=%lx\n",
                         (void*)frag,  endpoint->ptl_proc.phys.pid, tag, 
                         (void *)put_local_offset, put_length, (uint64_t)match_bits));
    ret = PtlPut(md_h,
                 (ptl_size_t) offset,
                 put_length, /* fragment length */
                 (mca_btl_portals4_component.portals_need_ack ? PTL_ACK_REQ : PTL_NO_ACK_REQ),
                 endpoint->ptl_proc,
                 mca_btl_portals4_module.recv_idx,
                 match_bits,                     /* match bits */
                 0,                              /* remote offset - not used */
                 (void *) frag,                  /* user ptr */
                 tag);                           /* hdr_data: tag */
    if (ret != PTL_OK) {
        opal_output(0, "mca_btl_portals4_send: PtlPut failed with error %d", ret);
        return OMPI_ERROR;
    }

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
    opal_output(0, "mca_btl_portals_sendi is not implemented");
    abort();
    return OMPI_SUCCESS;
}

static int
mca_btl_portals4_try_to_use_fixed_md(void *start,
                    int length,
                    ptl_handle_md_t *md_h,
                    int64_t *offset,
                    mca_btl_portals4_frag_t *frag)
{
    int ret;
    ptl_md_t md;
    int64_t addr;

    addr = ((int64_t)start & ~EXTENDED_ADDR);

    /* If fixed_md_distance is defined for MD and if the memory buffer is strictly contained in one of them, then use one */
    if ((0 != mca_btl_portals4_module.fixed_md_distance) &&
        (((addr % mca_btl_portals4_module.fixed_md_distance) + length) < mca_btl_portals4_module.fixed_md_distance)) {
               if (0 == length) OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                                    "          Memory  : [ %16lx -   (len = 0)      ] is in fixed MD number: %d\n",
                                    (unsigned long) start, (int) (addr / mca_btl_portals4_module.fixed_md_distance)));
                else OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                         "          Memory  : [ %16lx - %16lx ] is in fixed MD number: %d\n",
                         (unsigned long) start, (long int)start + length - 1, (int)(addr / mca_btl_portals4_module.fixed_md_distance)));
                /* Use the fixed MD */
                *md_h = mca_btl_portals4_module.fixed_md_h[addr / mca_btl_portals4_module.fixed_md_distance];
                *offset = (addr % mca_btl_portals4_module.fixed_md_distance);
                frag->md_h = PTL_INVALID_HANDLE;
    }
    else {
        if (0 == mca_btl_portals4_module.fixed_md_distance)
             OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                 "\nWARNING: Memory cannot be connected to a fixed MD\n"));
        else OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                 "\nWARNING: Memory outside the scope of the fixed MD %d\n",
                 (int)(addr / mca_btl_portals4_module.fixed_md_distance)));

        /* Bind the MD (and unbind it where necessary) */
        md.start     = start;
        md.length    = length;
        md.options   = 0;
        md.eq_handle = mca_btl_portals4_module.recv_eq_h;
        md.ct_handle = PTL_CT_NONE;

        ret = PtlMDBind(mca_btl_portals4_module.portals_ni_h,
                        &md,
                        &frag->md_h);
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                                "%s:%d: PtlMDBind failed: %d\n",
                                __FILE__, __LINE__, ret);
            return mca_btl_portals4_get_error(ret);
        }
        *md_h = frag->md_h;
        *offset = 0;
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "try_to_use_fixed_md: frag=%p start=%p len=%lx offset=%lx\n",
        (void*)frag, (void *)start, (unsigned long)length, (unsigned long)*offset));

    return OMPI_SUCCESS;
}

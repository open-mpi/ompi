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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ud.h"
#include "btl_ud_frag.h"
#include "ompi/mca/mpool/openib/mpool_openib.h"



static inline void mca_btl_ud_frag_common_constructor( mca_btl_ud_frag_t* frag)
{
    frag->ud_reg = (mca_mpool_openib_registration_t*)frag->base.super.user_data;
    frag->sg_entry.lkey = frag->ud_reg->mr->lkey;
    frag->base.des_flags = 0;
}


static void mca_btl_ud_send_frag_common_constructor(mca_btl_ud_frag_t* frag)
{

    mca_btl_ud_frag_common_constructor(frag);
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;

    /* We do not include the mca_btl_ud_ib_header_t data when sending */
    frag->hdr = (mca_btl_ud_header_t*)(frag+1);
    frag->segment.seg_addr.pval = frag->hdr + 1;

    frag->sg_entry.addr = (unsigned long)frag->hdr;

    frag->wr_desc.sr_desc.wr_id = (unsigned long) frag;
    frag->wr_desc.sr_desc.sg_list = &frag->sg_entry;
    frag->wr_desc.sr_desc.num_sge = 1;
    frag->wr_desc.sr_desc.opcode = IBV_WR_SEND;
    frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED;
    frag->wr_desc.sr_desc.next = NULL;
    frag->wr_desc.sr_desc.wr.ud.remote_qkey = mca_btl_ud_component.ib_qkey;
}

static void mca_btl_ud_recv_frag_common_constructor(mca_btl_ud_frag_t* frag)
{
    mca_btl_ud_frag_common_constructor(frag);
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;

    /* Receive frag headers start 40 bytes later */
    frag->hdr = (mca_btl_ud_header_t*)((unsigned char*)frag) +
        sizeof(mca_btl_ud_frag_t) + sizeof(mca_btl_ud_ib_header_t);
    frag->segment.seg_addr.pval = frag->hdr + 1;

    frag->sg_entry.addr = (unsigned long)(frag + 1);
    frag->segment.seg_len = frag->size;
    frag->sg_entry.length = frag->size +
        sizeof(mca_btl_ud_ib_header_t) + sizeof(mca_btl_ud_header_t);

    frag->wr_desc.rd_desc.wr_id = (unsigned long) frag;
    frag->wr_desc.rd_desc.sg_list = &frag->sg_entry;
    frag->wr_desc.rd_desc.num_sge = 1;
    frag->wr_desc.rd_desc.next = NULL;
}

static void mca_btl_ud_send_frag_eager_constructor(mca_btl_ud_frag_t* frag)
{
    frag->size = mca_btl_ud_component.eager_limit;
    mca_btl_ud_send_frag_common_constructor(frag);
}


static void mca_btl_ud_send_frag_max_constructor(mca_btl_ud_frag_t* frag)
{
    frag->size = mca_btl_ud_component.max_send_size;
    mca_btl_ud_send_frag_common_constructor(frag);
}

static void mca_btl_ud_recv_frag_max_constructor(mca_btl_ud_frag_t* frag)
{
    frag->size = mca_btl_ud_component.max_send_size;
    mca_btl_ud_recv_frag_common_constructor(frag);
}


static void mca_btl_ud_recv_frag_eager_constructor(mca_btl_ud_frag_t* frag)
{
    frag->size = mca_btl_ud_component.eager_limit;
    mca_btl_ud_recv_frag_common_constructor(frag);
}

static void mca_btl_ud_send_frag_frag_constructor(mca_btl_ud_frag_t* frag)
{
    frag->size = 0;
    mca_btl_ud_send_frag_common_constructor(frag);
}


OBJ_CLASS_INSTANCE(
                   mca_btl_ud_frag_t,
                   mca_btl_base_descriptor_t,
                   NULL,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_ud_send_frag_eager_t,
                   mca_btl_base_descriptor_t,
                   mca_btl_ud_send_frag_eager_constructor,
                   NULL);


OBJ_CLASS_INSTANCE(
                   mca_btl_ud_send_frag_max_t,
                   mca_btl_base_descriptor_t,
                   mca_btl_ud_send_frag_max_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_ud_send_frag_frag_t,
                   mca_btl_base_descriptor_t,
                   mca_btl_ud_send_frag_frag_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_ud_recv_frag_eager_t,
                   mca_btl_base_descriptor_t,
                   mca_btl_ud_recv_frag_eager_constructor,
                   NULL);


OBJ_CLASS_INSTANCE(
                   mca_btl_ud_recv_frag_max_t,
                   mca_btl_base_descriptor_t,
                   mca_btl_ud_recv_frag_max_constructor,
                   NULL);



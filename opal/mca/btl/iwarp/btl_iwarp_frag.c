/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_iwarp.h"
#include "btl_iwarp_frag.h"
#include "btl_iwarp_eager_rdma.h"

int mca_btl_iwarp_frag_init(opal_free_list_item_t* item, void* ctx)
{
    mca_btl_iwarp_frag_init_data_t* init_data = (mca_btl_iwarp_frag_init_data_t *) ctx;
    mca_btl_iwarp_frag_t *frag = to_base_frag(item);

    if(MCA_BTL_IWARP_FRAG_RECV == frag->type) {
        to_recv_frag(frag)->qp_idx = init_data->order;
        to_com_frag(frag)->sg_entry.length =
            mca_btl_iwarp_component.qp_infos[init_data->order].size +
            sizeof(mca_btl_iwarp_header_t) +
            sizeof(mca_btl_iwarp_header_coalesced_t) +
            sizeof(mca_btl_iwarp_control_header_t);
    }

    if(MCA_BTL_IWARP_FRAG_SEND == frag->type)
        to_send_frag(frag)->qp_idx = init_data->order;

    frag->list = init_data->list;

    return OPAL_SUCCESS;
}

static void base_constructor(mca_btl_iwarp_frag_t *frag)
{
    frag->base.order = MCA_BTL_NO_ORDER;
}

static void com_constructor(mca_btl_iwarp_com_frag_t *frag)
{
    mca_btl_iwarp_frag_t *base_frag = to_base_frag(frag);
    mca_btl_iwarp_reg_t* reg =
        (mca_btl_iwarp_reg_t*)base_frag->base.super.registration;

    frag->registration = reg;

    if(reg) {
        frag->sg_entry.lkey = reg->mr->lkey;
    }
    frag->n_wqes_inflight = 0;
}

static void out_constructor(mca_btl_iwarp_out_frag_t *frag)
{
    mca_btl_iwarp_frag_t *base_frag = to_base_frag(frag);

    base_frag->base.des_segments = &base_frag->segment;
    base_frag->base.des_segment_count = 1;

    frag->sr_desc.wr_id = (uint64_t)(uintptr_t)frag;
    frag->sr_desc.sg_list = &to_com_frag(frag)->sg_entry;
    frag->sr_desc.num_sge = 1;
    frag->sr_desc.opcode = IBV_WR_SEND;
    frag->sr_desc.send_flags = IBV_SEND_SIGNALED;
    frag->sr_desc.next = NULL;
}

static void in_constructor(mca_btl_iwarp_in_frag_t *frag)
{
    mca_btl_iwarp_frag_t *base_frag = to_base_frag(frag);

    base_frag->base.des_segments = &base_frag->segment;
    base_frag->base.des_segment_count = 1;
}

static void send_constructor(mca_btl_iwarp_send_frag_t *frag)
{
    mca_btl_iwarp_frag_t *base_frag = to_base_frag(frag);

    base_frag->type = MCA_BTL_IWARP_FRAG_SEND;

    frag->chdr = (mca_btl_iwarp_header_t*)base_frag->base.super.ptr;
    frag->hdr = (mca_btl_iwarp_header_t*)
        (((unsigned char*)base_frag->base.super.ptr) +
        sizeof(mca_btl_iwarp_header_coalesced_t) +
        sizeof(mca_btl_iwarp_control_header_t));
    base_frag->segment.seg_addr.pval = frag->hdr + 1;
    to_com_frag(frag)->sg_entry.addr = (uint64_t)(uintptr_t)frag->hdr;
    frag->coalesced_length = 0;
    OBJ_CONSTRUCT(&frag->coalesced_frags, opal_list_t);
}

static void recv_constructor(mca_btl_iwarp_recv_frag_t *frag)
{
    mca_btl_iwarp_frag_t *base_frag = to_base_frag(frag);

    base_frag->type = MCA_BTL_IWARP_FRAG_RECV;

    frag->hdr = (mca_btl_iwarp_header_t*)base_frag->base.super.ptr;
    base_frag->segment.seg_addr.pval =
        ((unsigned char* )frag->hdr) + sizeof(mca_btl_iwarp_header_t);
    to_com_frag(frag)->sg_entry.addr = (uint64_t)(uintptr_t)frag->hdr;

    frag->rd_desc.wr_id = (uint64_t)(uintptr_t)frag;
    frag->rd_desc.sg_list = &to_com_frag(frag)->sg_entry;
    frag->rd_desc.num_sge = 1;
    frag->rd_desc.next = NULL;
}

static void send_control_constructor(mca_btl_iwarp_send_control_frag_t *frag)
{
    to_base_frag(frag)->type = MCA_BTL_IWARP_FRAG_CONTROL;
    /* adjusting headers because there is no coalesce header in control messages */
    frag->hdr = frag->chdr;
    to_base_frag(frag)->segment.seg_addr.pval = frag->hdr + 1;
    to_com_frag(frag)->sg_entry.addr = (uint64_t)(uintptr_t)frag->hdr;
}

static void put_constructor(mca_btl_iwarp_put_frag_t *frag)
{
    to_base_frag(frag)->type = MCA_BTL_IWARP_FRAG_SEND_USER;
    to_out_frag(frag)->sr_desc.opcode = IBV_WR_RDMA_WRITE;
    frag->cb.func = NULL;
}

static void get_constructor(mca_btl_iwarp_get_frag_t *frag)
{
    to_base_frag(frag)->type = MCA_BTL_IWARP_FRAG_RECV_USER;

    frag->sr_desc.wr_id = (uint64_t)(uintptr_t)frag;
    frag->sr_desc.sg_list = &to_com_frag(frag)->sg_entry;
    frag->sr_desc.num_sge = 1;
    frag->sr_desc.opcode = IBV_WR_RDMA_READ;
    frag->sr_desc.send_flags = IBV_SEND_SIGNALED;
    frag->sr_desc.next = NULL;
}

static void coalesced_constructor(mca_btl_iwarp_coalesced_frag_t *frag)
{
    mca_btl_iwarp_frag_t *base_frag = to_base_frag(frag);

    base_frag->type = MCA_BTL_IWARP_FRAG_COALESCED;

    base_frag->base.des_segments = &base_frag->segment;
    base_frag->base.des_segment_count = 1;
}

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_frag_t,
                   mca_btl_base_descriptor_t,
                   base_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_com_frag_t,
                   mca_btl_iwarp_frag_t,
                   com_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_out_frag_t,
                   mca_btl_iwarp_com_frag_t,
                   out_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_in_frag_t,
                   mca_btl_iwarp_com_frag_t,
                   in_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_send_frag_t,
                   mca_btl_iwarp_out_frag_t,
                   send_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_recv_frag_t,
                   mca_btl_iwarp_in_frag_t,
                   recv_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_send_control_frag_t,
                   mca_btl_iwarp_send_frag_t,
                   send_control_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_put_frag_t,
                   mca_btl_iwarp_out_frag_t,
                   put_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_get_frag_t,
                   mca_btl_iwarp_in_frag_t,
                   get_constructor,
                   NULL);

OBJ_CLASS_INSTANCE(
                   mca_btl_iwarp_coalesced_frag_t,
                   mca_btl_iwarp_frag_t,
                   coalesced_constructor,
                   NULL);

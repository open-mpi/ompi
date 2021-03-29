/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2018 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_SM_VIRTUAL_H
#define MCA_BTL_SM_VIRTUAL_H

#include "opal_config.h"

#include "opal/mca/btl/sm/btl_sm_types.h"

#if SIZEOF_VOID_P == 8
#    define MCA_BTL_SM_OFFSET_MASK 0xffffffffll
#    define MCA_BTL_SM_OFFSET_BITS 32
#    define MCA_BTL_SM_BITNESS     64
#else
#    define MCA_BTL_SM_OFFSET_MASK 0x00ffffffl
#    define MCA_BTL_SM_OFFSET_BITS 24
#    define MCA_BTL_SM_BITNESS     32
#endif

/***
 * One or more FIFO components may be a pointer that must be
 * accessed by multiple processes.  Since the shared region may
 * be mmapped differently into each process's address space,
 * these pointers will be relative to some base address.  Here,
 * we define inline functions to translate between relative
 * addresses and virtual addresses.
 */

/* This only works for finding the relative address for a pointer within my_segment */
static inline fifo_value_t virtual2relative(char *addr)
{
    return (fifo_value_t)((intptr_t)(addr - mca_btl_sm_component.my_segment))
           | ((fifo_value_t) MCA_BTL_SM_LOCAL_RANK << MCA_BTL_SM_OFFSET_BITS);
}

static inline fifo_value_t virtual2relativepeer(struct mca_btl_base_endpoint_t *endpoint,
                                                char *addr)
{
    return (fifo_value_t)((intptr_t)(addr - endpoint->segment_base))
           | ((fifo_value_t) endpoint->peer_smp_rank << MCA_BTL_SM_OFFSET_BITS);
}

static inline void *relative2virtual(fifo_value_t offset)
{
    return (void *) (intptr_t)(
        (offset & MCA_BTL_SM_OFFSET_MASK)
        + mca_btl_sm_component.endpoints[offset >> MCA_BTL_SM_OFFSET_BITS].segment_base);
}

#endif /* MCA_BTL_SM_VIRTUAL_H */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"

static inline void mca_btl_vader_frag_constructor (mca_btl_vader_frag_t *frag)
{
    frag->hdr = (mca_btl_vader_hdr_t*)frag->base.super.ptr;
    if(frag->hdr != NULL) {
        frag->hdr->my_smp_rank = mca_btl_vader_component.my_smp_rank;
    }
}

OBJ_CLASS_INSTANCE(mca_btl_vader_frag_t, mca_btl_base_descriptor_t,
		   mca_btl_vader_frag_constructor, NULL);

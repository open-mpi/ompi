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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_VADER_SEND_FRAG_H
#define MCA_BTL_VADER_SEND_FRAG_H

#include "ompi_config.h"

#define MCA_BTL_VADER_FLAG_INLINE      0
#define MCA_BTL_VADER_FLAG_SINGLE_COPY 1

struct mca_btl_vader_hdr_t {
    volatile void *next;    /* next item in fifo. many peers may touch this */
    volatile bool complete; /* fragment completion (usually 1 byte) */
    mca_btl_base_tag_t tag; /* tag associated with this fragment (used to lookup callback) */
    char pad[2];
    int flags;              /* vader send flags */
    int my_smp_rank;        /* smp rank of owning process */
    size_t len;             /* length of data following this header */
};
typedef struct mca_btl_vader_hdr_t mca_btl_vader_hdr_t;

/**
 * shared memory send fragment derived type.
 */
struct mca_btl_vader_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;
    struct mca_btl_base_endpoint_t *endpoint;
    /* pointer written to the FIFO, this is the base of the shared memory region */
    mca_btl_vader_hdr_t *hdr;
    ompi_free_list_t *my_list;
};

typedef struct mca_btl_vader_frag_t mca_btl_vader_frag_t;

OBJ_CLASS_DECLARATION(mca_btl_vader_frag_t);

#define MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag, rc)			\
    do {								\
	ompi_free_list_item_t *item;					\
	OMPI_FREE_LIST_GET(&mca_btl_vader_component.vader_frags_eager, item, rc); \
	frag = (mca_btl_vader_frag_t *) item;				\
	frag->hdr->complete = false;					\
	frag->hdr->flags = MCA_BTL_VADER_FLAG_INLINE;			\
	frag->my_list = &mca_btl_vader_component.vader_frags_eager;	\
    } while (0)

#define MCA_BTL_VADER_FRAG_ALLOC_USER(frag, rc)				\
    do {								\
	ompi_free_list_item_t *item;					\
	OMPI_FREE_LIST_GET(&mca_btl_vader_component.vader_frags_user, item, rc); \
	frag = (mca_btl_vader_frag_t *) item;				\
	frag->hdr->complete = false;					\
	frag->hdr->flags = MCA_BTL_VADER_FLAG_INLINE;			\
	frag->my_list = &mca_btl_vader_component.vader_frags_user;	\
    } while (0)


#define MCA_BTL_VADER_FRAG_RETURN(frag)					\
	OMPI_FREE_LIST_RETURN((frag)->my_list, (ompi_free_list_item_t *)(frag))

#endif /* MCA_BTL_VADER_SEND_FRAG_H */

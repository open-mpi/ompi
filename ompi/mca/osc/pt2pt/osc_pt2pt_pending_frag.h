/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Pending frags are fragments that have been received on the target,
 * but can not yet be processed (because ordering is turned on).
 * Because receive memory descriptors are precious resources, rather
 * than keeping a descriptor until the right sequence number, we
 * instead malloc a buffer (as part of the pending frag) and copy the
 * message.
 */

#ifndef OSC_PT2PT_PENDING_FRAG_H
#define OSC_PT2PT_PENDING_FRAG_H

/** Incoming fragment that has to be queued */
struct ompi_osc_pt2pt_pending_frag_t {
    opal_list_item_t super;

    /* This is a pointer to the top of the fragment (which is always
       the header).  Save as a header to make the casting a bit less
       onerous during sequence number lookups. */
    ompi_osc_pt2pt_frag_header_t *header;
};
typedef struct ompi_osc_pt2pt_pending_frag_t ompi_osc_pt2pt_pending_frag_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_pending_frag_t);

/*
 * Note: module lock must be held during this operation
 */
static inline ompi_osc_pt2pt_pending_frag_t*
ompi_osc_pt2pt_pending_frag_create(ompi_osc_pt2pt_module_t *module,
                                  void *ptr,
                                  size_t size)
{
    size_t total_size = sizeof(ompi_osc_pt2pt_pending_frag_t) + size;
    ompi_osc_pt2pt_pending_frag_t *ret =
        (ompi_osc_pt2pt_pending_frag_t*) malloc(total_size);
    if (NULL == ret) return NULL;

    OBJ_CONSTRUCT(&ret, ompi_osc_pt2pt_pending_frag_t);
    memcpy(ret->header, ptr, size);

    return ret;
}


/*
 * Note: module lock must be held for this operation
 */
static inline int
ompi_osc_pt2pt_pending_frag_destroy(ompi_osc_pt2pt_module_t *module,
                                   ompi_osc_pt2pt_pending_frag_t* frag)
{
    OBJ_DESTRUCT(&frag);
    free(frag);

    return OMPI_SUCCESS;
}

#endif

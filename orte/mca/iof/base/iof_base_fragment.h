/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef _IOF_BASE_FRAGMENT_
#define _IOF_BASE_FRAGMENT_

#include "orte_config.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "opal/event/event.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_header.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 *  Fragment used to hold message header/data.
 */

struct orte_iof_base_frag_t {
    opal_free_list_item_t super;
    orte_iof_base_header_t frag_hdr;
    orte_process_name_t frag_src;
    unsigned char frag_data[ORTE_IOF_BASE_MSG_MAX];
    unsigned char* frag_ptr;
    size_t frag_len;
    struct iovec frag_iov[2];
    struct orte_iof_base_endpoint_t* frag_owner;
};
typedef struct orte_iof_base_frag_t orte_iof_base_frag_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_base_frag_t);


/**
 *  Free-list allocation of fragments.
 */

#define ORTE_IOF_BASE_FRAG_ALLOC(frag,rc) { \
    opal_free_list_item_t* item; \
    OPAL_FREE_LIST_GET(&orte_iof_base.iof_fragments, item,rc); \
    if((frag = (orte_iof_base_frag_t*)item) == NULL) { \
        opal_output(0, "ORTE_IOF_BASE_FRAG_ALLOC failed with status=%d\n", rc); \
    } \
    frag->frag_owner = NULL; \
    frag->frag_ptr = frag->frag_data; \
    frag->frag_len = 0; \
}

#define ORTE_IOF_BASE_FRAG_RETURN(frag) \
    OPAL_FREE_LIST_RETURN(&orte_iof_base.iof_fragments, \
                          &frag->super);


/**
 * Send an acknowledgment to the peer that this fragment has been received.
 */

#define orte_iof_base_frag_ack(frag, do_close) _orte_iof_base_frag_ack((frag), (do_close), __FILE__,__LINE__)
int _orte_iof_base_frag_ack(orte_iof_base_frag_t*, bool do_close, const char*, int);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


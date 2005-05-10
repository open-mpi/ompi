/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
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
/**
 * @file
 */

#ifndef MCA_PTL_PORTALS_RECV_FRAG_H
#define MCA_PTL_PORTALS_RECV_FRAG_H

#include "mca/ptl/base/ptl_base_recvfrag.h"

/**
 *  PORTALS received fragment derived type.
 */
struct mca_ptl_portals_recv_frag_t {
    mca_ptl_base_recv_frag_t frag_recv;  /**< base receive fragment descriptor */
    void *frag_data;
    size_t frag_size;
    ptl_process_id_t frag_source;
};
typedef struct mca_ptl_portals_recv_frag_t mca_ptl_portals_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_ptl_portals_recv_frag_t);


extern int ptl_portals_post_recv_md(struct mca_ptl_portals_module_t *ptl,
                                    void *data_ptr);
extern int mca_ptl_portals_process_recv_event(struct mca_ptl_portals_module_t *ptl, 
                                              ptl_event_t *ev);


#endif

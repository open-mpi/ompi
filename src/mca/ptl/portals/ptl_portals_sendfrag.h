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


#ifndef MCA_PTL_PORTALS_SENDFRAG_H_
#define MCA_PTL_PORTALS_SENDFRAG_H_

#include "mca/ptl/base/ptl_base_sendfrag.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    struct mca_ptl_portals_send_frag_t {
        mca_ptl_base_send_frag_t frag_send;
    };
    typedef struct mca_ptl_portals_send_frag_t mca_ptl_portals_send_frag_t;

    OBJ_CLASS_DECLARATION (mca_ptl_portals_send_frag_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* MCA_PTL_PORTALS_SENDFRAG_H_ */

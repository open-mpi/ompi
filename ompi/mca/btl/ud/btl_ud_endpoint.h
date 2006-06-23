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

#ifndef MCA_BTL_IB_ENDPOINT_H
#define MCA_BTL_IB_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "btl_ud_frag.h"
#include "btl_ud.h"
#include <errno.h>
#include <string.h>
#include <infiniband/verbs.h>
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/openib/mpool_openib.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_btl_ud_endpoint_t);


struct mca_btl_ud_frag_t;

struct mca_btl_ud_addr_t {
    uint32_t                    qp_num_hp;
    uint32_t                    qp_num_lp;
    /* QP number  (Low and High priority) */

    uint32_t                    psn_hp;
    uint32_t                    psn_lp;
    /* Port sequence number (Low and High) */

    uint16_t                    lid;
    uint16_t                    subnet;
    /* Local Identifier & Subnet */
};
typedef struct mca_btl_ud_addr_t mca_btl_ud_addr_t;



/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair and address information is exchanged at startup.
 * The UD BTL is connectionless, so no connection is ever established.
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    /*opal_mutex_t                endpoint_lock;*/
    /**< lock for concurrent access to endpoint state */

    mca_btl_ud_addr_t           rem_addr;
    /**< Remote address information */

    struct ibv_ah*              rmt_ah_hp;
    struct ibv_ah*              rmt_ah_lp;
    /**< Remote Address Handle (Low and High) */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_ud_endpoint_t;

inline int mca_btl_ud_endpoint_post_send(struct mca_btl_ud_module_t* ud_btl,
                                         mca_btl_ud_endpoint_t * endpoint,
                                         struct mca_btl_ud_frag_t * frag);
int mca_btl_ud_endpoint_init_qp(
                                mca_btl_base_module_t* btl,
                                struct ibv_cq* cq,
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
                                struct ibv_srq* srq,
#endif
                                struct ibv_qp** qp,
                                uint32_t lcl_psn);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

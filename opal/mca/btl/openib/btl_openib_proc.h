/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_IB_PROC_H
#define MCA_BTL_IB_PROC_H

#include "opal/class/opal_object.h"
#include "opal/util/proc.h"
#include "btl_openib.h"
#include "btl_openib_endpoint.h"

BEGIN_C_DECLS

/* Must forward reference this to avoid include file loop */
struct opal_btl_openib_connect_base_module_data_t;

/**
 * Data received from the modex.  For each openib BTL module/port in
 * the peer, we'll receive two things:
 *
 * 1. Data about the peer's port
 * 2. An array of CPCs that the peer has available on that port, each
 *    of which has its own meta data
 *
 * Hence, these two items need to be bundled together;
 */
typedef struct mca_btl_openib_proc_modex_t {
    /** Information about the peer's port */
    mca_btl_openib_modex_message_t pm_port_info;

    /** Array of the peer's CPCs available on this port */
    opal_btl_openib_connect_base_module_data_t *pm_cpc_data;

    /** Length of the pm_cpc_data array */
    uint8_t pm_cpc_data_count;
} mca_btl_openib_proc_modex_t;

/**
 * The list element to hold pointers to openin_btls that are using this
 * ib_proc.
 */

struct mca_btl_openib_proc_btlptr_t {
    opal_list_item_t super;
    mca_btl_openib_module_t* openib_btl;
};
typedef struct mca_btl_openib_proc_btlptr_t mca_btl_openib_proc_btlptr_t;

OBJ_CLASS_DECLARATION(mca_btl_openib_proc_btlptr_t);

/**
 * Represents the state of a remote process and the set of addresses
 * that it exports. Also cache an instance of mca_btl_base_endpoint_t for
 * each
 * BTL instance that attempts to open a connection to the process.
 */
struct mca_btl_openib_proc_t {
    /** allow proc to be placed on a list */
    opal_list_item_t super;

    /** pointer to corresponding opal_proc_t */
    const opal_proc_t *proc_opal;

    /** modex messages from this proc; one for each port in the peer */
    mca_btl_openib_proc_modex_t *proc_ports;

    /** length of proc_ports array */
    uint8_t proc_port_count;

    /** list of openib_btl's that touched this proc **/
    opal_list_t openib_btls;

    /** array of endpoints that have been created to access this proc */
    volatile struct mca_btl_base_endpoint_t **proc_endpoints;

    /** number of endpoints (length of proc_endpoints array) */
    volatile size_t proc_endpoint_count;

    /** lock to protect against concurrent access to proc state */
    opal_mutex_t proc_lock;
};
typedef struct mca_btl_openib_proc_t mca_btl_openib_proc_t;

OBJ_CLASS_DECLARATION(mca_btl_openib_proc_t);

mca_btl_openib_proc_t* mca_btl_openib_proc_get_locked(opal_proc_t* proc);
int mca_btl_openib_proc_insert(mca_btl_openib_proc_t*, mca_btl_base_endpoint_t*);
int mca_btl_openib_proc_remove(opal_proc_t* proc,
                               mca_btl_base_endpoint_t* module_endpoint);
int mca_btl_openib_proc_reg_btl(mca_btl_openib_proc_t* ib_proc,
                                mca_btl_openib_module_t* openib_btl);


END_C_DECLS

#endif

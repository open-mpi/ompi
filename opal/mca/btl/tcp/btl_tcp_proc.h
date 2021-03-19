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
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved
 * Copyright (c) 2019      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_TCP_PROC_H
#define MCA_BTL_TCP_PROC_H

#include "btl_tcp.h"
#include "btl_tcp_addr.h"
#include "btl_tcp_endpoint.h"
#include "opal/class/opal_object.h"
#include "opal/util/proc.h"

BEGIN_C_DECLS

/**
 * Represents the state of a remote process and the set of addresses
 * that it exports. Also cache an instance of mca_btl_base_endpoint_t for
 * each
 * BTL instance that attempts to open a connection to the process.
 */
struct mca_btl_tcp_proc_t {
    opal_list_item_t super;
    /**< allow proc to be placed on a list */

    opal_proc_t *proc_opal;
    /**< pointer to corresponding opal_proc_t */

    struct mca_btl_tcp_addr_t *proc_addrs;
    /**< array of addresses exported by peer */

    size_t proc_addr_count;
    /**< number of addresses published by endpoint */

    struct mca_btl_base_endpoint_t **proc_endpoints;
    /**< array of endpoints that have been created to access this proc */

    size_t proc_endpoint_count;
    /**< number of endpoints */

    opal_hash_table_t btl_index_to_endpoint;
    /**< interface match table, matches btl_index to remote addresses of type mca_btl_tcp_addr_t */

    opal_mutex_t proc_lock;
    /**< lock to protect against concurrent access to proc state */
};
typedef struct mca_btl_tcp_proc_t mca_btl_tcp_proc_t;
OBJ_CLASS_DECLARATION(mca_btl_tcp_proc_t);

mca_btl_tcp_proc_t *mca_btl_tcp_proc_create(opal_proc_t *proc);
mca_btl_tcp_proc_t *mca_btl_tcp_proc_lookup(const opal_process_name_t *name);
int mca_btl_tcp_proc_insert(mca_btl_tcp_proc_t *, mca_btl_base_endpoint_t *);
int mca_btl_tcp_proc_remove(mca_btl_tcp_proc_t *, mca_btl_base_endpoint_t *);
void mca_btl_tcp_proc_accept(mca_btl_tcp_proc_t *, struct sockaddr *, int);
bool mca_btl_tcp_proc_tosocks(mca_btl_tcp_addr_t *, struct sockaddr_storage *);

END_C_DECLS
#endif

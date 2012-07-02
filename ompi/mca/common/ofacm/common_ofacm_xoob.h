/*
 * Copyright (c) 2007-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
 *
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef COMMON_OFACM_XOOB_H
#define COMMON_OFACM_XOOB_H

#include "opal/class/opal_hash_table.h"
#include "connect.h"

extern ompi_common_ofacm_base_component_t ompi_common_ofacm_xoob;

typedef enum {
    XOOB_ADDR_CONNECTING = 100,
    XOOB_ADDR_CONNECTED,
    XOOB_ADDR_CLOSED
} ompi_common_ofacm_ib_addr_state_t;

struct ib_address_t {
    opal_list_item_t super;
    void *key;                                /* the key with size 80bit - [subnet(64) LID(16bit)] */
    uint64_t subnet_id;                       /* caching subnet_id  */
    uint16_t lid;                             /* caching lid */
    opal_list_t pending_contexts;             /* list of endpoints that use this ib_address */
    struct ompi_common_ofacm_base_qp_t *qps;  /* pointer to qp that will be used
                                                 for communication with the
                                                 destination */
    uint32_t remote_xrc_rcv_qp_num;           /* remote xrc qp number */
    opal_mutex_t addr_lock;                   /* protection */
    ompi_common_ofacm_ib_addr_state_t status; /* ib port status */
};
typedef struct ib_address_t 
               ib_address_t;

struct ompi_common_ofacm_xoob_local_connection_context_t {
    ompi_common_ofacm_base_local_connection_context_t super;
    ib_address_t *addr;
    uint32_t xrc_recv_qp_num; /* in xrc we will use it as recv qp */
    uint32_t xrc_recv_psn;
};
typedef struct ompi_common_ofacm_xoob_local_connection_context_t 
               ompi_common_ofacm_xoob_local_connection_context_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_common_ofacm_xoob_local_connection_context_t);

struct ompi_common_ofacm_xoob_module_t {
    ompi_common_ofacm_base_module_t super;
    opal_hash_table_t ib_addr_table; /**< used only for xrc.hash-table that
                                       keeps table of all lids/subnets */
};
typedef struct ompi_common_ofacm_xoob_module_t
               ompi_common_ofacm_xoob_module_t;

struct pending_context_t {
    opal_list_item_t super;
    ompi_common_ofacm_xoob_local_connection_context_t *xcontext;
};
typedef struct pending_context_t
               pending_context_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(pending_context_t);

#endif

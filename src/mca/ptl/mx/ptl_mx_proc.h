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
#ifndef MCA_PTL_MX_PROC_H
#define MCA_PTL_MX_PROC_H

#include "ompi_config.h"
#include <myriexpress.h>
#include "mca/ns/ns.h"
#include "class/ompi_object.h"
#include "proc/proc.h"
#include "ptl_mx.h"
#include "ptl_mx_peer.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif



/**
 *  Represents the state of a remote process and the set of addresses
 *  that it exports. Also cache an instance of mca_ptl_base_peer_t for each
 *  PTL instance that attempts to open a connection to the process.
 */
struct mca_ptl_mx_proc_t {
    ompi_list_item_t super;             /**< allow proc to be placed on a list */
    ompi_proc_t *proc_ompi;             /**< pointer to corresponding ompi_proc_t */
    ompi_process_name_t proc_name;      /**< globally unique identifier for the process */
    mx_endpoint_addr_t *proc_addrs;     /**< peer endpoint address */
    size_t proc_addr_count;             /**< number of addresses published by peer */
    mca_ptl_mx_peer_t **proc_peers;     /**< array of peers that have been created to access this proc */
    size_t proc_peer_count;             /**< number of peers */
    ompi_mutex_t proc_lock;             /**< lock to protect against concurrent access to proc state */
};
typedef struct mca_ptl_mx_proc_t mca_ptl_mx_proc_t;


OBJ_CLASS_DECLARATION(mca_ptl_mx_proc_t);


mca_ptl_mx_proc_t* mca_ptl_mx_proc_create(ompi_proc_t* ompi_proc);
mca_ptl_mx_proc_t* mca_ptl_mx_proc_lookup(const ompi_process_name_t*);

int mca_ptl_mx_proc_insert(mca_ptl_mx_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer);
int mca_ptl_mx_proc_remove(mca_ptl_mx_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif


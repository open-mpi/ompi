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

#ifndef MCA_BTL_MX_PROC_H
#define MCA_BTL_MX_PROC_H

#include "orte/mca/ns/ns.h"
#include "opal/class/opal_object.h"
#include "ompi/proc/proc.h"
#include "btl_mx.h"
#include "btl_mx_endpoint.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_BTL_MX_NOT_CONNECTED   0x0000
#define MCA_BTL_MX_NOT_REACHEABLE  0x0001
#define MCA_BTL_MX_CONNECTED       0x0002

    /**
     * Represents the state of a remote process and the set of addresses
     * that it exports. Also cache an instance of mca_btl_base_endpoint_t for
     * each
     * BTL instance that attempts to open a connection to the process.
     */
    struct mca_btl_mx_proc_t {
        opal_list_item_t super;                  
        /**< allow proc to be placed on a list */

        ompi_proc_t *proc_ompi;                  
        /**< pointer to corresponding ompi_proc_t */

        int status;  /**< status of the connection */

        mca_btl_mx_addr_t  *mx_peers;  /**< peers addresses */
        int mx_peers_count;

        size_t proc_addr_index;                  
        /**< next remote address that will be used to establish the connection */

        struct mca_btl_base_endpoint_t **proc_endpoints; 
        /**< array of endpoints that have been created to access this proc */    

        size_t proc_endpoint_count;                  
        /**< number of endpoints */

        opal_mutex_t proc_lock;                  
        /**< lock to protect against concurrent access to proc state */
    };
    typedef struct mca_btl_mx_proc_t mca_btl_mx_proc_t;

    OBJ_CLASS_DECLARATION(mca_btl_mx_proc_t);

    mca_btl_mx_proc_t* mca_btl_mx_proc_create(ompi_proc_t* ompi_proc);
    int mca_btl_mx_proc_insert(mca_btl_mx_proc_t*, mca_btl_base_endpoint_t*);
    int mca_btl_mx_proc_connect( mca_btl_mx_endpoint_t* module_endpoint );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

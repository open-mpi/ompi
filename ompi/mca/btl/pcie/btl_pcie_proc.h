/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_PCIE_PROC_H
#define MCA_BTL_PCIE_PROC_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "ompi/proc/proc.h"

#include "btl_pcie.h"
#include "btl_pcie_endpoint.h"

BEGIN_C_DECLS

/**
 * Represents the state of a remote process and the set of addresses
 * that it exports. Also cache an instance of mca_btl_base_endpoint_t for
 * each
 * BTL instance that attempts to open a connection to the process.
 */
struct mca_btl_pcie_proc_t {
    opal_list_item_t super;                  
    /**< allow proc to be placed on a list */

    ompi_proc_t *proc_ompi;                  
    /**< pointer to corresponding ompi_proc_t */

    orte_process_name_t proc_guid;           
    /**< globally unique identifier for the process */

    size_t proc_addr_count;                  
    /**< number of addresses published by endpoint */

    struct mca_btl_base_endpoint_t *endpoint_proc; 
    /**<  endpoint that has been created to access this proc */    

    size_t proc_endpoint_count;                  
    /**< number of endpoints */

    opal_mutex_t proc_lock;                  
    /**< lock to protect against concurrent access to proc state */
    
};
typedef struct mca_btl_pcie_proc_t mca_btl_pcie_proc_t;
OBJ_CLASS_DECLARATION(mca_btl_pcie_proc_t);

int  mca_btl_pcie_proc_create(ompi_proc_t* ompi_proc, 
                              mca_btl_pcie_module_t* pcie_btl, 
                              mca_btl_pcie_proc_t** ret_proc);

END_C_DECLS

#endif /* #ifndef MCA_BTL_PCIE_PROC_H */

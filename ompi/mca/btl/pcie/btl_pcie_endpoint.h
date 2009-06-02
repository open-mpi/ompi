/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_PCIE_ENDPOINT_H
#define MCA_BTL_PCIE_ENDPOINT_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/event/event.h"

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/pml/pml.h"

#include "btl_pcie_ddriver.h"
#include "btl_pcie_frag.h"
#include "btl_pcie.h"
#include "btl_pcie_fifo.h"

BEGIN_C_DECLS

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_pcie_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_pcie_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */
    
    /** the name of the remote PCIE device */
    char* rem_dev_name;
    /** the name of the local PCIE device */
    char* lcl_dev_name;
    
    /** the pcie adapter - returned by dd_open */
    DD_adapter_handle pcie_adapter;
    
    /** local pcie SMA memory for this endpoint */
    char *lcl_sma_ptr;
    
    /** remote pcie SMA memory for this endpoint */
    char *rem_sma_ptr;
    
    /** remote fragment starting point (in which to
     *   deliver data via "rdma" write 
     */
    char *rem_frag_base;
    char *lcl_frag_base;
    
    char *lcl_dma_status;
    
    btl_pcie_fifo_t recv_fifo;
    
    btl_pcie_fifo_t send_fifo;


};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;

typedef mca_btl_base_endpoint_t  mca_btl_pcie_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_pcie_endpoint_t);


/*
 * Initialize an endpoint 
 */ 
int mca_btl_pcie_endpoint_init(mca_btl_base_endpoint_t* endpoint);

/*
 * Finalize an endpoint 
 */ 
int mca_btl_pcie_endpoint_fini(mca_btl_base_endpoint_t* endpoint);

END_C_DECLS

#endif /* #ifndef MCA_BTL_PCIE_ENDPOINT_H */

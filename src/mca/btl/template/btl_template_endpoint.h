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

#ifndef MCA_BTL_TEMPLATE_ENDPOINT_H
#define MCA_BTL_TEMPLATE_ENDPOINT_H

#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "btl_template_frag.h"
#include "btl_template.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OBJ_CLASS_DECLARATION(mca_btl_template_endpoint_t);

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    ompi_list_item_t            super;

    struct mca_btl_template_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_template_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_template_endpoint_t;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2014 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_IO_OMPIO_REQUEST_H
#define MCA_IO_OMPIO_REQUEST_H

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "io_ompio.h"

BEGIN_C_DECLS

extern opal_list_t mca_io_ompio_pending_requests;
extern bool mca_io_ompio_progress_is_registered;

/**
 * Type of request.
 */
typedef enum {
    MCA_OMPIO_REQUEST_WRITE,
    MCA_OMPIO_REQUEST_READ,
    MCA_OMPIO_REQUEST_WRITE_ALL,
    MCA_OMPIO_REQUEST_READ_ALL,
} mca_ompio_request_type_t;


/**
 * Main structure for OMPIO requests
 */
struct mca_ompio_request_t {
    ompi_request_t                                 req_ompi;
    mca_ompio_request_type_t                       req_type;
    void                                          *req_data;
    opal_list_item_t                               req_item;
    mca_fbtl_base_module_progress_fn_t      req_progress_fn;
    mca_fbtl_base_module_request_free_fn_t      req_free_fn;
};
typedef struct mca_ompio_request_t mca_ompio_request_t;
OBJ_CLASS_DECLARATION(mca_ompio_request_t);

#define GET_OMPIO_REQ_FROM_ITEM(ITEM) ((mca_ompio_request_t *)((char *)ITEM - offsetof(struct mca_ompio_request_t,req_item)))


OMPI_DECLSPEC int mca_io_ompio_component_progress ( void);

END_C_DECLS

#endif /* MCA_IO_OMPIO_REQUEST_H */

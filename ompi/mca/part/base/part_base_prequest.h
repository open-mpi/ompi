/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PART_BASE_REQUEST_H
#define MCA_PART_BASE_REQUEST_H

#include "ompi_config.h"
#include "opal/class/opal_free_list.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "opal/datatype/opal_convertor.h"
BEGIN_C_DECLS

/**
 * External list for the partitioned requests. 
 */
OMPI_DECLSPEC extern opal_free_list_t mca_part_base_psend_requests;
OMPI_DECLSPEC extern opal_free_list_t mca_part_base_precv_requests;

typedef enum {
    MCA_PART_REQUEST_NULL,
    MCA_PART_REQUEST_PSEND,
    MCA_PART_REQUEST_PRECV
} mca_part_base_request_type_t;


/**
 *  Base type for Partitioned P2P requests
 */
struct mca_part_base_prequest_t {

    ompi_request_t req_ompi;              /**< base request */
    volatile int32_t req_part_complete;   /**< flag indicating if the pt-2-pt layer is done with this request */
    volatile int32_t req_free_called;     /**< flag indicating if the user has freed this request */
    mca_part_base_request_type_t req_type; /**< MPI request type - used for test */
    struct ompi_communicator_t *req_comm; /**< communicator pointer */
    struct ompi_datatype_t *req_datatype; /**< pointer to data type */
    opal_convertor_t req_convertor;       /**< always need the convertor */

    void *req_addr;                       /**< pointer to application buffer */
    size_t req_parts;                     /**< number of partitions */
    size_t req_count;                     /**< count of user datatype elements */
    int32_t req_peer;                     /**< peer process - rank w/in this communicator */
    int32_t req_tag;                      /**< user defined tag */
    struct ompi_proc_t* req_proc;         /**< peer process */
};
typedef struct mca_part_base_prequest_t mca_part_base_prequest_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_part_base_prequest_t);

END_C_DECLS

#endif


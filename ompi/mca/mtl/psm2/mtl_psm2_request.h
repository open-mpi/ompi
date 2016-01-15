/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_PSM2_REQUEST_H
#define OMPI_MTL_PSM2_REQUEST_H

#include "opal/datatype/opal_convertor.h"


typedef enum {
    OMPI_mtl_psm2_ISEND,
    OMPI_mtl_psm2_IRECV
} mca_mtl_psm2_request_type_t;

struct mca_mtl_psm2_request_t {
    struct mca_mtl_request_t super;
    mca_mtl_psm2_request_type_t type;
    psm2_mq_req_t psm2_request;
    /* psm2_segment_t psm2_segment[1]; */
    void *buf;
    size_t length;
    struct opal_convertor_t *convertor;
    bool free_after;
};
typedef struct mca_mtl_psm2_request_t mca_mtl_psm2_request_t;

#endif

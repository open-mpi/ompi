/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
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
#ifndef MCA_PART_BASE_PSEND_REQUEST_H
#define MCA_PART_BASE_PSEND_REQUEST_H

#include "ompi_config.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/base/part_base_prequest.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/peruse/peruse-internal.h"

BEGIN_C_DECLS

/**
 * Base type for send requests
 */
struct mca_part_base_psend_request_t {
    mca_part_base_prequest_t req_base;       /**< base request type - common data structure for use by wait/test */
    const void *req_addr;                    /**< pointer to send buffer - may not be application buffer */
    size_t req_bytes_packed;                 /**< packed size of a message given the datatype and count */
};
typedef struct mca_part_base_psend_request_t mca_part_base_psend_request_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION( mca_part_base_psend_request_t );

END_C_DECLS

#endif

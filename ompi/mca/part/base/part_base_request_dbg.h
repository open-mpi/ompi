/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2011-2020 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_PART_BASE_REQUEST_DBG_H
#define MCA_PART_BASE_REQUEST_DBG_H

/*
 * This file contains definitions used by both OMPI and debugger plugins.
 * For more information on why we do this see the Notice to developers
 * comment at the top of the ompi_msgq_dll.c file.
 */

/**
 * Type of request.
 */
typedef enum {
    MCA_PART_REQUEST_NULL,
    MCA_PART_REQUEST_PSEND,
    MCA_PART_REQUEST_PRECV
} mca_part_base_request_type_t;

#endif /* MCA_PART_BASE_REQUEST_DBG_H */

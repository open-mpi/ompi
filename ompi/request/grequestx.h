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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_GEN_REQUESTX_H
#define OMPI_GEN_REQUESTX_H

#include "ompi_config.h"
#include "ompi/request/grequest.h"

BEGIN_C_DECLS

typedef int (ompi_grequestx_wait_function)(int, void **, double, MPI_Status *);
typedef int ompi_grequestx_class;
/**
 * Start an extended generalized request
 */
OMPI_DECLSPEC int ompi_grequestx_start(
    MPI_Grequest_query_function *gquery,
    MPI_Grequest_free_function *gfree,
    MPI_Grequest_cancel_function *gcancel,
    ompi_grequestx_poll_function *gpoll,
    void* gstate,
    ompi_request_t** request);

OMPI_DECLSPEC int ompi_grequestx_class_create(
    MPI_Grequest_query_function *gquery,
    MPI_Grequest_free_function *gfree,
    MPI_Grequest_cancel_function *gcancel,
    ompi_grequestx_poll_function *gpoll,
    ompi_grequestx_wait_function *gwait,
    ompi_grequestx_class *greq_class);

OMPI_DECLSPEC int ompi_grequestx_class_allocate(
    ompi_grequestx_class greq_class,
    void *extra_state,
    ompi_request_t** request);

END_C_DECLS

#endif

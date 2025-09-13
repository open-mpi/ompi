/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2018-2025 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_PML_BASE_BSEND_H_
#define _MCA_PML_BASE_BSEND_H_

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/allocator/allocator.h"

BEGIN_C_DECLS

enum ompi_bsend_buffer_type_t {
    BASE_BSEND_BUF = 0, 
    COMM_BSEND_BUF, 
    SESSION_BSEND_BUF, 
};
typedef enum ompi_bsend_buffer_type_t ompi_bsend_buffer_type_t;

struct mca_pml_bsend_buffer_t {
    opal_object_t *super;
    mca_allocator_base_component_t* allocator_component;
    mca_allocator_base_module_t* bsend_allocator;  /* sub-allocator to manage users buffer */
    opal_mutex_t     bsend_mutex;
    opal_condition_t bsend_condition;
    size_t           bsend_usersize;   /* user provided buffer size */
    unsigned char    *bsend_userbase;  /* user provided buffer base */
    unsigned char    *bsend_base;      /* adjusted base of user buffer */
    unsigned char    *bsend_addr;       /* current offset into user buffer */
    size_t           bsend_size;       /* adjusted size of user buffer */
    size_t           bsend_count;      /* number of outstanding requests */
    size_t           bsend_pagesz;     /* mmap page size */
    int              bsend_pagebits;   /* number of bits in pagesz */
};
typedef struct mca_pml_bsend_buffer_t mca_pml_bsend_buffer_t;

OMPI_DECLSPEC int mca_pml_base_bsend_init (void);

OMPI_DECLSPEC int mca_pml_base_bsend_attach(ompi_bsend_buffer_type_t type, void *obj, void* addr, size_t size);
OMPI_DECLSPEC int mca_pml_base_bsend_detach(ompi_bsend_buffer_type_t type, void *obj, void* addr, size_t* size);

OMPI_DECLSPEC int mca_pml_base_bsend_request_alloc(ompi_request_t*);
OMPI_DECLSPEC int mca_pml_base_bsend_request_start(ompi_request_t*);
OMPI_DECLSPEC int mca_pml_base_bsend_request_fini(ompi_request_t*);
OMPI_DECLSPEC void*  mca_pml_base_bsend_request_alloc_buf(ompi_communicator_t *comm,  size_t length );
OMPI_DECLSPEC int mca_pml_base_bsend_request_free(ompi_communicator_t *comm, void* addr);

OMPI_DECLSPEC int mca_pml_base_bsend_flush(ompi_bsend_buffer_type_t type, void *obj);
OMPI_DECLSPEC int mca_pml_base_bsend_iflush(ompi_bsend_buffer_type_t type, void *obj, ompi_request_t **req);

END_C_DECLS

#endif


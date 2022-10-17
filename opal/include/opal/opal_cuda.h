/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * This file is intended only to carry shared types. If actual cuda
 * symbols are required, they need to be added to a new common cuda
 * component.
 */

#ifndef OPAL_CUDA_H
#define OPAL_CUDA_H
#include "opal/mca/rcache/rcache.h"

#define MEMHANDLE_SIZE 8
#define EVTHANDLE_SIZE 8

struct mca_opal_cuda_reg_data_t {
    uint64_t memHandle[MEMHANDLE_SIZE];
    uint64_t evtHandle[EVTHANDLE_SIZE];
    uint64_t event;
    opal_ptr_t memh_seg_addr;
    size_t memh_seg_len;
};
typedef struct mca_opal_cuda_reg_data_t mca_opal_cuda_reg_data_t;

struct mca_opal_cuda_reg_t {
    mca_rcache_base_registration_t base;
    mca_opal_cuda_reg_data_t data;
};
typedef struct mca_opal_cuda_reg_t mca_opal_cuda_reg_t;
#endif /* OPAL_CUDA_H */

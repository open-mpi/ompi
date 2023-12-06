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
 * Copyright (c) 2023      Advanced Micro Devices, Inc. 
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef OPAL_GPU_H
#define OPAL_GPU_H
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/accelerator/accelerator.h"

struct mca_opal_gpu_reg_data_t {
    opal_accelerator_ipc_handle_t       ipcHandle;
    opal_accelerator_ipc_event_handle_t ipcEventHandle;
    opal_accelerator_event_t            *event;
    opal_ptr_t                          memh_seg_addr;
    size_t                              memh_seg_len;
};
typedef struct mca_opal_gpu_reg_data_t mca_opal_gpu_reg_data_t;

struct mca_opal_gpu_reg_t {
    mca_rcache_base_registration_t base;
    mca_opal_gpu_reg_data_t data;
};
typedef struct mca_opal_gpu_reg_t mca_opal_gpu_reg_t;
#endif /* OPAL_GPU_H */

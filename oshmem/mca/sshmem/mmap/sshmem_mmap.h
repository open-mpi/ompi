/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SSHMEM_MMAP_EXPORT_H
#define MCA_SSHMEM_MMAP_EXPORT_H

#include "oshmem_config.h"

#include "oshmem/mca/sshmem/sshmem.h"

BEGIN_C_DECLS

/**
 * globally exported variable to hold the mmap component.
 */
typedef struct mca_sshmem_mmap_component_t {
    /* base component struct */
    mca_sshmem_base_component_t super;
    /* priority for mmap component */
    int priority;
    int is_anonymous;
    int is_start_addr_fixed;
} mca_sshmem_mmap_component_t;

OSHMEM_DECLSPEC extern mca_sshmem_mmap_component_t
mca_sshmem_mmap_component;

typedef struct mca_sshmem_mmap_module_t {
    mca_sshmem_base_module_t super;
} mca_sshmem_mmap_module_t;
extern mca_sshmem_mmap_module_t mca_sshmem_mmap_module;

END_C_DECLS

#endif /* MCA_SHMEM_MMAP_EXPORT_H */

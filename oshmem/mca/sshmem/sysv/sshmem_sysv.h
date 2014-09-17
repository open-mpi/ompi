/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SSHMEM_SYSV_EXPORT_H
#define MCA_SSHMEM_SYSV_EXPORT_H

#include "oshmem_config.h"

#include "opal/util/sys_limits.h"

#include "oshmem/mca/sshmem/sshmem.h"

BEGIN_C_DECLS

/**
 * globally exported variable to hold the sysv component.
 */
typedef struct mca_sshmem_sysv_component_t {
    /* base component struct */
    mca_sshmem_base_component_t super;
    /* priority for sysv component */
    int priority;
    int use_hp;
} mca_sshmem_sysv_component_t;

OSHMEM_MODULE_DECLSPEC extern mca_sshmem_sysv_component_t
mca_sshmem_sysv_component;

typedef struct mca_sshmem_sysv_module_t {
    mca_sshmem_base_module_t super;
} mca_sshmem_sysv_module_t;
extern mca_sshmem_sysv_module_t mca_sshmem_sysv_module;

OSHMEM_MODULE_DECLSPEC extern size_t sshmem_sysv_gethugepagesize(void);

END_C_DECLS

#endif /* MCA_SSHMEM_SYSV_EXPORT_H */

/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SSHMEM_VERBS_EXPORT_H
#define MCA_SSHMEM_VERBS_EXPORT_H

#include "oshmem_config.h"

#include "oshmem/mca/sshmem/sshmem.h"

BEGIN_C_DECLS

#include <infiniband/verbs.h>
#include "opal/class/opal_list.h"
#include "opal/class/opal_value_array.h"

typedef struct openib_device_t {
    struct ibv_device **ib_devs;
    struct ibv_device *ib_dev;
    struct ibv_context *ib_dev_context;
    struct ibv_device_attr ib_dev_attr;
    struct ibv_pd *ib_pd;
    opal_value_array_t ib_mr_array;
    struct ibv_mr *ib_mr_shared;
} openib_device_t;

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)

#   if MPAGE_ENABLE < 3
#       define IBV_EXP_ACCESS_ALLOCATE_MR IBV_ACCESS_ALLOCATE_MR
#       define IBV_EXP_ACCESS_SHARED_MR_USER_READ IBV_ACCESS_SHARED_MR_USER_READ
#       define IBV_EXP_ACCESS_SHARED_MR_USER_WRITE IBV_ACCESS_SHARED_MR_USER_WRITE
#       define IBV_EXP_ACCESS_NO_RDMA IBV_ACCESS_NO_RDMA
#       define ibv_exp_reg_shared_mr    ibv_reg_shared_mr_ex
#       define ibv_exp_reg_shared_mr_in ibv_reg_shared_mr_in

struct ibv_exp_reg_mr_in {
    struct ibv_pd *pd;
    void *addr;
    size_t length;
    uint64_t access;
    uint32_t comp_mask;
};

static inline struct ibv_mr *ibv_exp_reg_mr(struct ibv_exp_reg_mr_in *in)
{
    return ibv_reg_mr(in->pd, in->addr, in->length, in->access);
}
#   endif

static inline void mca_sshmem_verbs_fill_shared_mr(struct ibv_exp_reg_shared_mr_in *mr, struct ibv_pd *pd, uint32_t handle, void *addr, uint64_t access)
{
    mr->pd = pd;
    mr->addr = addr;
    mr->mr_handle = handle;
#if defined(MPAGE_HAVE_SMR_EXP_ACCESS)
    mr->exp_access = access;
#else
    mr->access = access;
#endif
    mr->comp_mask = 0;
}
#endif /* MPAGE_ENABLE */


/**
 * globally exported variable to hold the verbs component.
 */
typedef struct mca_sshmem_verbs_component_t {
    /* base component struct */
    mca_sshmem_base_component_t super;
    /* priority for verbs component */
    int priority;
    char* hca_name;
    int mr_interleave_factor;
    int has_shared_mr;
} mca_sshmem_verbs_component_t;

OSHMEM_MODULE_DECLSPEC extern mca_sshmem_verbs_component_t
mca_sshmem_verbs_component;

typedef struct mca_sshmem_verbs_module_t {
    mca_sshmem_base_module_t super;
} mca_sshmem_verbs_module_t;
extern mca_sshmem_verbs_module_t mca_sshmem_verbs_module;

END_C_DECLS

#endif /* MCA_SSHMEM_VERBS_EXPORT_H */

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_MPOOL_GPUSM_H
#define MCA_MPOOL_GPUSM_H

#include "opal_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/mpool/mpool.h"

BEGIN_C_DECLS

#define MEMHANDLE_SIZE 8
#define EVTHANDLE_SIZE 8
struct mca_mpool_gpusm_registration_t { 
    mca_mpool_base_registration_t base;
    uint64_t memHandle[MEMHANDLE_SIZE];
    uint64_t evtHandle[EVTHANDLE_SIZE];
	uint64_t event;
};  
typedef struct mca_mpool_gpusm_registration_t mca_mpool_gpusm_registration_t; 
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_mpool_gpusm_registration_t); 

struct mca_mpool_gpusm_component_t {
    mca_mpool_base_component_t super;
};
typedef struct mca_mpool_gpusm_component_t mca_mpool_gpusm_component_t;

OPAL_DECLSPEC extern mca_mpool_gpusm_component_t mca_mpool_gpusm_component;

struct mca_mpool_base_resources_t {
    void *reg_data;
    size_t sizeof_reg;
    int (*register_mem)(void *base, size_t size, mca_mpool_base_registration_t *newreg,
                        mca_mpool_base_registration_t *hdrreg);
    int (*deregister_mem)(void *reg_data, mca_mpool_base_registration_t *reg);
};
typedef struct mca_mpool_base_resources_t mca_mpool_base_resources_t;

struct mca_mpool_gpusm_module_t {
    mca_mpool_base_module_t super;
    struct mca_mpool_base_resources_t resources;
    opal_free_list_t reg_list;
}; typedef struct mca_mpool_gpusm_module_t mca_mpool_gpusm_module_t;

/*
 *  Initializes the mpool module.
 */
void mca_mpool_gpusm_module_init(mca_mpool_gpusm_module_t *mpool);

/**
  * register block of memory
  */
int mca_mpool_gpusm_register(mca_mpool_base_module_t* mpool, void *addr,
        size_t size, uint32_t flags, mca_mpool_base_registration_t **reg);

/**
 * deregister memory
 */
int mca_mpool_gpusm_deregister(mca_mpool_base_module_t *mpool,
        mca_mpool_base_registration_t *reg);

/**
 * find registration for a given block of memory
 */
int mca_mpool_gpusm_find(struct mca_mpool_base_module_t* mpool, void* addr,
        size_t size, mca_mpool_base_registration_t **reg);

/**
 * finalize mpool
 */
void mca_mpool_gpusm_finalize(struct mca_mpool_base_module_t *mpool);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OPAL_SUCCESS or failure status
 */
int mca_mpool_gpusm_ft_event(int state);

END_C_DECLS
#endif

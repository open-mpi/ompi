/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "coll_ml.h"
#include "coll_ml_inlines.h"
#include "coll_ml_mca.h"
#include "coll_ml_lmngr.h"
#ifndef HAVE_POSIX_MEMALIGN
#include "opal/align.h"
#include "opal_stdint.h"
#endif
#include "opal/util/sys_limits.h"

/* Constructor for list memory manager */
static void construct_lmngr(mca_coll_ml_lmngr_t *lmngr)
{
    mca_coll_ml_component_t *cm = &mca_coll_ml_component;

    ML_VERBOSE(7, ("Constructing new list manager %p", (void *)lmngr));

    /* No real memory is allocated, only basic init.
    The real memory will be allocated on demand, on first block allocation */

    /* I caching this block size, alignment and list size
    since maybe in future we will want to define different parameters
    for lists */
    lmngr->list_block_size = cm->lmngr_block_size;
    lmngr->list_alignment = cm->lmngr_alignment;
    lmngr->list_size = cm->lmngr_size;
    lmngr->n_resources = 0;
    lmngr->base_addr = NULL; /* If the base addr is not null, the struct was initilized
                                and memory was allocated */
    /* Not sure that lock is required */
    OBJ_CONSTRUCT(&lmngr->mem_lock, opal_mutex_t);

    /* Only construct the list, no memry initialisation */
    OBJ_CONSTRUCT(&lmngr->blocks_list, opal_list_t);
}

static void destruct_lmngr(mca_coll_ml_lmngr_t *lmngr)
{
    int max_nc = lmngr->n_resources;
    int rc, i;
    bcol_base_network_context_t *nc;
    opal_list_item_t *item;

    ML_VERBOSE(6, ("Destructing list manager %p", (void *)lmngr));

    while (NULL != (item = opal_list_remove_first(&lmngr->blocks_list))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&lmngr->blocks_list);

    if (NULL != lmngr->alloc_base) {
        for( i = 0; i < max_nc; i++ ) {
            nc = lmngr->net_context[i];
            rc = nc->deregister_memory_fn(nc->context_data,
                    lmngr->reg_desc[nc->context_id]);
            if(rc != OMPI_SUCCESS) {
                ML_ERROR(("Failed to unregister , lmngr %p", (void *)lmngr));
            }
        }

        ML_VERBOSE(10, ("Release base addr %p", lmngr->alloc_base));

        free(lmngr->alloc_base);
        lmngr->alloc_base = NULL;
        lmngr->base_addr = NULL;
    }

    lmngr->list_block_size = 0;
    lmngr->list_alignment = 0;
    lmngr->list_size = 0;
    lmngr->n_resources = 0;

    OBJ_DESTRUCT(&lmngr->mem_lock);
}

OBJ_CLASS_INSTANCE(mca_coll_ml_lmngr_t,
        opal_object_t,
        construct_lmngr,
        destruct_lmngr);

int mca_coll_ml_lmngr_tune(mca_coll_ml_lmngr_t *lmngr, 
        size_t block_size, size_t list_size, size_t alignment)
{
    ML_VERBOSE(7, ("Tunning list manager"));

    if (OPAL_UNLIKELY(NULL == lmngr->base_addr)) {
        ML_VERBOSE(7, ("The list manager is already initialized, you can not tune it"));
        return OMPI_ERROR;
    }

    lmngr->list_block_size = block_size;
    lmngr->list_alignment = alignment;
    lmngr->list_size = list_size;

    return OMPI_SUCCESS;
}

int mca_coll_ml_lmngr_reg(void)
{
    int tmp, ret = OMPI_SUCCESS;

    mca_coll_ml_component_t *cm = &mca_coll_ml_component;

#define CHECK(expr) do {\
    tmp = (expr); \
    if (0 > tmp) ret = tmp; \
 } while (0)

    ML_VERBOSE(7, ("Setting parameters for list manager"));

    cm->lmngr_size = 8;
    CHECK(mca_base_component_var_register(&mca_coll_ml_component.super.collm_version,
                                          "memory_manager_list_size", "Memory manager list size",
                                          MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &cm->lmngr_size));

    /* The size list couldn't be less than possible max of ML modules,
       it = max supported communicators by ML */
    if (cm->lmngr_size < cm->max_comm) {
        cm->lmngr_size = cm->max_comm;
    }

    mca_coll_ml_component.lmngr_block_size = cm->payload_buffer_size *
      cm->n_payload_buffs_per_bank *
      cm->n_payload_mem_banks *
      cm->lmngr_size;

    CHECK(mca_base_component_var_register(&mca_coll_ml_component.super.collm_version,
                                          "memory_manager_block_size", "Memory manager block size", 
                                          MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_coll_ml_component.lmngr_block_size));

    cm->lmngr_alignment = opal_getpagesize();
    CHECK(mca_base_component_var_register(&mca_coll_ml_component.super.collm_version,
                                          "memory_manager_alignment", "Memory manager alignment",
                                          MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_coll_ml_component.lmngr_block_size));

    return ret;
}

static int lmngr_register(mca_coll_ml_lmngr_t *lmngr, bcol_base_network_context_t *nc)
{
    int rc, j;
    int max_nc = lmngr->n_resources;

    rc = nc->register_memory_fn(nc->context_data,
            lmngr->base_addr, 
            lmngr->list_size * lmngr->list_block_size,
            &lmngr->reg_desc[nc->context_id]);

    if(rc != OMPI_SUCCESS) {
        int ret_val;
        ML_VERBOSE(7, ("Failed to register [%d], unrolling the registration", rc));
        /* deregistser the successful registrations */
        for( j = 0; j < max_nc; j++ ) {
            /* set the registration parameter to point to the current
             * resource description */
            nc = lmngr->net_context[j];
            ret_val = nc->deregister_memory_fn(nc->context_data,
                    lmngr->reg_desc[nc->context_id]);
            if(ret_val != OMPI_SUCCESS) {
                return ret_val;
            }
        }

        return rc;
    }

    return OMPI_SUCCESS;
}

static int mca_coll_ml_lmngr_init(mca_coll_ml_lmngr_t *lmngr)
{
    int i, num_blocks;
    int rc;
    unsigned char *addr;
    bcol_base_network_context_t *nc;

    ML_VERBOSE(7, ("List initialization"));

#ifdef HAVE_POSIX_MEMALIGN
    if((errno = posix_memalign(&lmngr->base_addr, 
                    lmngr->list_alignment, 
                    lmngr->list_size * lmngr->list_block_size)) != 0) {
        ML_ERROR(("Failed to allocate memory: %d [%s]", errno, strerror(errno)));
        return OMPI_ERROR;
    }
    lmngr->alloc_base = lmngr->base_addr;
#else
    lmngr->alloc_base =
        malloc(lmngr->list_size * lmngr->list_block_size + lmngr->list_alignment);
    if(NULL == lmngr->alloc_base) {
        ML_ERROR(("Failed to allocate memory: %d [%s]", errno, strerror(errno)));
        return OMPI_ERROR;
    }

    lmngr->base_addr = (void*)OPAL_ALIGN((uintptr_t)lmngr->alloc_base,
            lmngr->list_alignment, uintptr_t);
#endif
    
    assert(lmngr->n_resources < MCA_COLL_ML_MAX_REG_INFO);

    for(i= 0 ;i < lmngr->n_resources ;i++) {
        nc = lmngr->net_context[i];
        ML_VERBOSE(7, ("Call registration for resource index %d", i));
        rc = lmngr_register(lmngr, nc);
        if (OMPI_SUCCESS != rc) {
            ML_ERROR(("Failed to lmngr register: %d [%s]", errno, strerror(errno)));
            return rc;
        }
    }

    /* slice the memory to blocks */
    addr = (unsigned char *) lmngr->base_addr;
    for(num_blocks = 0; num_blocks < (int)lmngr->list_size; num_blocks++) {
        mca_bcol_base_lmngr_block_t *item = OBJ_NEW(mca_bcol_base_lmngr_block_t);
        item->base_addr = (void *)addr;
        item->lmngr = lmngr;
        /* ML_VERBOSE(10, ("Appending block # %d %p", num_blocks, (void *)addr)); */
        opal_list_append(&lmngr->blocks_list, (opal_list_item_t *)item);
        /* advance the address */
        addr += lmngr->list_block_size;
    }

    ML_VERBOSE(7, ("List initialization done %d", 
                opal_list_get_size(&lmngr->blocks_list)));
    return OMPI_SUCCESS;
}

mca_bcol_base_lmngr_block_t* mca_coll_ml_lmngr_alloc (
        mca_coll_ml_lmngr_t *lmngr)
{
    int rc;
    opal_list_t *list = &lmngr->blocks_list;

    /* Check if the list manager was initialized */
    if(OPAL_UNLIKELY(NULL == lmngr->base_addr)) {
        ML_VERBOSE(7 ,("Starting memory initialization"));
        rc = mca_coll_ml_lmngr_init(lmngr);
        if (OMPI_SUCCESS != rc) {
            ML_ERROR(("Failed to init memory"));
            return NULL;
        }
    }

    if(OPAL_UNLIKELY(opal_list_is_empty(list))) {
        /* Upper layer need to handle the NULL */
        ML_VERBOSE(1, ("List manager is empty."));
        return NULL;
    }

    return (mca_bcol_base_lmngr_block_t *)opal_list_remove_first(list);
}

void mca_coll_ml_lmngr_free(mca_bcol_base_lmngr_block_t *block)
{
    opal_list_append(&block->lmngr->blocks_list, (opal_list_item_t *)block);
}

int mca_coll_ml_lmngr_append_nc(mca_coll_ml_lmngr_t *lmngr, bcol_base_network_context_t *nc)
{
    int i, rc;

    ML_VERBOSE(7, ("Append new network context %p to list manager %p",
                nc, lmngr));

    if (NULL == nc) {
        return OMPI_ERROR;
    }

    /* check if we already have the context on the list.
       if we do have - do not do anything, just return success
     */
    if (OPAL_UNLIKELY(MCA_COLL_ML_MAX_REG_INFO == lmngr->n_resources)) {
        ML_ERROR(("MPI overflows maximum supported network contexts is %d", MCA_COLL_ML_MAX_REG_INFO));
        return OMPI_ERROR;
    }

    for (i = 0; i < lmngr->n_resources; i++) {
        if (lmngr->net_context[i] == nc) {
            ML_VERBOSE(7, ("It is not new "));
            return OMPI_SUCCESS;
        }
    }

    ML_VERBOSE(7, ("Adding new context"));
    
    /* Setting context id */
    nc->context_id = lmngr->n_resources;
    lmngr->net_context[lmngr->n_resources] = nc;

    lmngr->n_resources++;

    /* Register the memory with new context */
    if (NULL != lmngr->base_addr) {
        rc = lmngr_register(lmngr, nc);
        if (OMPI_SUCCESS == rc) {
            return rc;
        }
    }

    return OMPI_SUCCESS;
}

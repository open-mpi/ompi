/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/bcol/bcol.h"
#include "opal/util/show_help.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/pml/pml.h"  /* need this for the max tag size */

#include "bcol_ptpcoll.h"
#include "bcol_ptpcoll_utils.h"
#include "bcol_ptpcoll_bcast.h"
#include "bcol_ptpcoll_allreduce.h"
#include "bcol_ptpcoll_reduce.h"

#define BCOL_PTP_CACHE_LINE_SIZE 128

/*
 * Local functions
 */
static int alloc_allreduce_offsets_array(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int rc = OMPI_SUCCESS, i = 0;
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int n_exchanges = k_node->n_exchanges;

    /* Precalculate the allreduce offsets */
    if (0 < k_node->n_exchanges) {
        ptpcoll_module->allgather_offsets = (int **) calloc (n_exchanges, sizeof(int *));

        if (!ptpcoll_module->allgather_offsets) {
            return OMPI_ERROR;
        }

        for (i = 0; i < n_exchanges ; i++) {
            ptpcoll_module->allgather_offsets[i] = (int *) calloc (NOFFSETS, sizeof(int));

            if (!ptpcoll_module->allgather_offsets[i]){
                return OMPI_ERROR;
            }
        }
    }

    return rc;
}

static int free_allreduce_offsets_array(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int rc = OMPI_SUCCESS, i = 0;
    netpatterns_k_exchange_node_t *k_node = &ptpcoll_module->knomial_exchange_tree;
    int n_exchanges = k_node->n_exchanges;

    if (ptpcoll_module->allgather_offsets) {
        for (i=0; i < n_exchanges; i++) {
            free (ptpcoll_module->allgather_offsets[i]);
        }
    }

    free(ptpcoll_module->allgather_offsets);
    ptpcoll_module->allgather_offsets = NULL;
    return rc;
}

static void
mca_bcol_ptpcoll_module_construct(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    uint64_t i;
    /* Pointer to component */
    ptpcoll_module->narray_node = NULL;
    ptpcoll_module->allgather_offsets = NULL;
    ptpcoll_module->super.bcol_component = (mca_bcol_base_component_t *) &mca_bcol_ptpcoll_component;
    ptpcoll_module->super.list_n_connected = NULL;
    ptpcoll_module->super.hier_scather_offset = 0;
    /* no header support in ptp */
    ptpcoll_module->super.header_size = 0;
    /* No network context */
    ptpcoll_module->super.network_context = NULL;
    /* set the upper limit on the tag */
    i = 2;
    ptpcoll_module->tag_mask = 1;
    while ( i <= (uint64_t) mca_pml.pml_max_tag && i > 0) {
        i <<= 1;
    }
    ptpcoll_module->ml_mem.ml_buf_desc = NULL;
    ptpcoll_module->tag_mask = i - 1;
}

static void
mca_bcol_ptpcoll_module_destruct(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int i;
    mca_bcol_ptpcoll_local_mlmem_desc_t *ml_mem = &ptpcoll_module->ml_mem;

    if (NULL != ml_mem->ml_buf_desc) {
        /* Release the memory structs that were cache ML memory data */
        uint32_t i, j, ci;
        for (i = 0; i < ml_mem->num_banks; i++) {
            for (j = 0; j < ml_mem->num_buffers_per_bank; j++) {
                ci = i * ml_mem->num_buffers_per_bank + j;
                if (NULL != ml_mem->ml_buf_desc[ci].requests) {
                    free(ml_mem->ml_buf_desc[ci].requests);
                }
            }
        }
        /* release the buffer descriptor */
        free(ml_mem->ml_buf_desc);
        ml_mem->ml_buf_desc = NULL;
    }

    if (NULL != ptpcoll_module->allgather_offsets) {
        free_allreduce_offsets_array(ptpcoll_module);
    }

    if (NULL != ptpcoll_module->narray_node) {
        for (i = 0; i < ptpcoll_module->group_size; i++) {
            if (NULL != ptpcoll_module->narray_node[i].children_ranks) {
                free(ptpcoll_module->narray_node[i].children_ranks);
            }
        }

        free(ptpcoll_module->narray_node);
        ptpcoll_module->narray_node = NULL;
    }

    OBJ_DESTRUCT(&ptpcoll_module->collreqs_free);

    if (NULL != ptpcoll_module->super.list_n_connected) {
        free(ptpcoll_module->super.list_n_connected);
        ptpcoll_module->super.list_n_connected = NULL;
    }

    for (i = 0; i < BCOL_NUM_OF_FUNCTIONS; i++){
        OPAL_LIST_DESTRUCT((&ptpcoll_module->super.bcol_fns_table[i]));
    }


    if (NULL != ptpcoll_module->kn_proxy_extra_index) {
        free(ptpcoll_module->kn_proxy_extra_index);
        ptpcoll_module->kn_proxy_extra_index = NULL;
    }

    if (NULL != ptpcoll_module->alltoall_iovec) {
        free(ptpcoll_module->alltoall_iovec);
        ptpcoll_module->alltoall_iovec = NULL;
    }

    if (NULL != ptpcoll_module->narray_knomial_proxy_extra_index) {
        free(ptpcoll_module->narray_knomial_proxy_extra_index);
        ptpcoll_module->narray_knomial_proxy_extra_index = NULL;
    }

    if (NULL != ptpcoll_module->narray_knomial_node) {
        for(i = 0; i < ptpcoll_module->full_narray_tree_size; i++) {
            netpatterns_cleanup_narray_knomial_tree (ptpcoll_module->narray_knomial_node + i);
        }
        free(ptpcoll_module->narray_knomial_node);
        ptpcoll_module->narray_knomial_node = NULL;
    }

    netpatterns_cleanup_recursive_knomial_allgather_tree_node(&ptpcoll_module->knomial_allgather_tree);
    netpatterns_cleanup_recursive_knomial_tree_node(&ptpcoll_module->knomial_exchange_tree);

}

OBJ_CLASS_INSTANCE(mca_bcol_ptpcoll_module_t,
                   mca_bcol_base_module_t,
                   mca_bcol_ptpcoll_module_construct,
                   mca_bcol_ptpcoll_module_destruct);

static int init_ml_buf_desc(mca_bcol_ptpcoll_ml_buffer_desc_t **desc, void *base_addr, uint32_t num_banks,
                            uint32_t num_buffers_per_bank, uint32_t size_buffer, uint32_t header_size, int group_size, int pow_k)
{
    uint32_t i, j, ci;
    mca_bcol_ptpcoll_ml_buffer_desc_t *tmp_desc = NULL;
    int k_nomial_radix = mca_bcol_ptpcoll_component.k_nomial_radix;
    int pow_k_val = (0 == pow_k) ? 1 : pow_k;
    int num_to_alloc =
        ((k_nomial_radix - 1) * pow_k_val * 2 + 1 > mca_bcol_ptpcoll_component.narray_radix) ?
        (k_nomial_radix - 1) * pow_k_val * 2 + 1 :
        mca_bcol_ptpcoll_component.narray_radix * 2;


    *desc = (mca_bcol_ptpcoll_ml_buffer_desc_t *)calloc(num_banks * num_buffers_per_bank,
                                                        sizeof(mca_bcol_ptpcoll_ml_buffer_desc_t));
    if (NULL == *desc) {
        PTPCOLL_ERROR(("Failed to allocate memory"));
        return OMPI_ERROR;
    }

    tmp_desc = *desc;

    for (i = 0; i < num_banks; i++) {
        for (j = 0; j < num_buffers_per_bank; j++) {
            ci = i * num_buffers_per_bank + j;
            tmp_desc[ci].bank_index = i;
            tmp_desc[ci].buffer_index = j;
            /* *2  is for gather session  +1 for extra peer */
            tmp_desc[ci].requests = (ompi_request_t **)
                calloc(num_to_alloc, sizeof(ompi_request_t *));
            if (NULL == tmp_desc[ci].requests) {
                PTPCOLL_ERROR(("Failed to allocate memory for requests"));
                return OMPI_ERROR;
            }
            /*
             * ptpcoll don't have any header, but other bcols may to have. So
             * we need to take it in account.
             */
            tmp_desc[ci].data_addr = (void *)
                ((unsigned char*)base_addr + ci * size_buffer + header_size);
            PTPCOLL_VERBOSE(10, ("ml memory cache setup %d %d - %p", i, j, tmp_desc[ci].data_addr));

            /* init reduce implementation flags */
            tmp_desc[ci].reduce_init_called = false;
            tmp_desc[ci].reduction_status = 0;
        }
    }

    return OMPI_SUCCESS;
}

static void mca_bcol_ptpcoll_set_small_msg_thresholds(struct mca_bcol_base_module_t *super)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module =
        (mca_bcol_ptpcoll_module_t *) super;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;

    /* Subtract out the maximum header size when calculating the thresholds. This
     * will account for the headers used by the basesmuma component. If we do not
     * take these headers into account we may overrun our buffer. */

    /* Set the Allgather threshold equals to a ML buff size */
    super->small_message_thresholds[BCOL_ALLGATHER] =
        (ptpcoll_module->ml_mem.size_buffer - BCOL_HEADER_MAX) /
        ompi_comm_size(ptpcoll_module->super.sbgp_partner_module->group_comm);

    /* Set the Bcast threshold, all Bcast algths have the same threshold */
    super->small_message_thresholds[BCOL_BCAST] =
        (ptpcoll_module->ml_mem.size_buffer - BCOL_HEADER_MAX);

    /* Set the Alltoall threshold, the Ring algth sets some limitation */
    super->small_message_thresholds[BCOL_ALLTOALL] =
        (ptpcoll_module->ml_mem.size_buffer - BCOL_HEADER_MAX) / 2;

    /* Set the Allreduce threshold, NARRAY algth sets some limitation */
    super->small_message_thresholds[BCOL_ALLREDUCE] =
        (ptpcoll_module->ml_mem.size_buffer - BCOL_HEADER_MAX) / ptpcoll_module->k_nomial_radix;

    /* Set the Reduce threshold, NARRAY algth sets some limitation */
    super->small_message_thresholds[BCOL_REDUCE] =
        (ptpcoll_module->ml_mem.size_buffer - BCOL_HEADER_MAX) / cm->narray_radix;
}

/*
 * Cache information about ML memory
 */
static int mca_bcol_ptpcoll_cache_ml_memory_info(struct mca_bcol_base_memory_block_desc_t *payload_block,
                                                 uint32_t data_offset,
                                                 struct mca_bcol_base_module_t *bcol,
                                                 void *reg_data)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module = (mca_bcol_ptpcoll_module_t *) bcol;
    mca_bcol_ptpcoll_local_mlmem_desc_t *ml_mem = &ptpcoll_module->ml_mem;
    struct mca_bcol_base_memory_block_desc_t *desc = payload_block;
    int group_size = ptpcoll_module->super.sbgp_partner_module->group_size;

    PTPCOLL_VERBOSE(10, ("mca_bcol_ptpcoll_init_buffer_memory was called"));

    /* cache ml mem desc tunings localy */
    ml_mem->num_banks = desc->num_banks;
    ml_mem->num_buffers_per_bank = desc->num_buffers_per_bank;
    ml_mem->size_buffer = desc->size_buffer;

    PTPCOLL_VERBOSE(10, ("ML buffer configuration num banks %d num_per_bank %d size %d base addr %p",
                         desc->num_banks, desc->num_buffers_per_bank, desc->size_buffer, desc->block->base_addr));

    /* Set first bank index for release */
    ml_mem->bank_index_for_release = 0;

    if (OMPI_SUCCESS != init_ml_buf_desc(&ml_mem->ml_buf_desc,
                                         desc->block->base_addr,
                                         ml_mem->num_banks,
                                         ml_mem->num_buffers_per_bank,
                                         ml_mem->size_buffer,
                                         data_offset,
                                         group_size,
                                         ptpcoll_module->pow_k)) {
        PTPCOLL_VERBOSE(10, ("Failed to allocate rdma memory descriptor\n"));
        return OMPI_ERROR;
    }

    PTPCOLL_VERBOSE(10, ("ptpcoll_module = %p, ml_mem_desc = %p.\n",
                         ptpcoll_module));

    return OMPI_SUCCESS;
}

/*
 * Load ptpcoll bcol functions
 */
static void load_func(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int fnc;

    /* reset everything to NULL */
    for (fnc = 0; fnc < BCOL_NUM_OF_FUNCTIONS; fnc++) {

        /*ptpcoll_module->super.bcol_function_table[fnc] = NULL;*/
        ptpcoll_module->super.bcol_function_table[fnc] = NULL;
        ptpcoll_module->super.bcol_function_init_table[fnc] = NULL;
    }

    ptpcoll_module->super.bcol_function_init_table[BCOL_BARRIER] = bcol_ptpcoll_barrier_init;

    ptpcoll_module->super.bcol_function_init_table[BCOL_BCAST] = bcol_ptpcoll_bcast_init;
    ptpcoll_module->super.bcol_function_init_table[BCOL_ALLREDUCE] = bcol_ptpcoll_allreduce_init;
    ptpcoll_module->super.bcol_function_init_table[BCOL_ALLGATHER] = bcol_ptpcoll_allgather_init;
    ptpcoll_module->super.bcol_function_table[BCOL_BCAST] = bcol_ptpcoll_bcast_k_nomial_anyroot;
    ptpcoll_module->super.bcol_function_init_table[BCOL_ALLTOALL] = NULL;
    ptpcoll_module->super.bcol_function_init_table[BCOL_SYNC] = mca_bcol_ptpcoll_memsync_init;
    ptpcoll_module->super.bcol_function_init_table[BCOL_REDUCE] = bcol_ptpcoll_reduce_init;

    /* ML memory cacher */
    ptpcoll_module->super.bcol_memory_init = mca_bcol_ptpcoll_cache_ml_memory_info;

    /* Set thresholds */
    ptpcoll_module->super.set_small_msg_thresholds = mca_bcol_ptpcoll_set_small_msg_thresholds;

    /* setup recursive k-ing tree */
    ptpcoll_module->super.k_nomial_tree = mca_bcol_ptpcoll_setup_knomial_tree;
}

int mca_bcol_ptpcoll_setup_knomial_tree(mca_bcol_base_module_t *super)
{
    mca_bcol_ptpcoll_module_t *p2p_module = (mca_bcol_ptpcoll_module_t *) super;
    int rc = 0;

    rc = netpatterns_setup_recursive_knomial_allgather_tree_node(
        p2p_module->super.sbgp_partner_module->group_size,
        p2p_module->super.sbgp_partner_module->my_index,
        mca_bcol_ptpcoll_component.k_nomial_radix,
        super->list_n_connected,
        &p2p_module->knomial_allgather_tree);

    return rc;
}

/* The function used to calculate size */
static int calc_full_tree_size(int radix, int group_size, int *num_leafs)
{
    int level_cnt = 1;
    int total_cnt = 0;

    while( total_cnt < group_size ) {
        total_cnt += level_cnt;
        level_cnt *= radix;
    }

    if (total_cnt > group_size) {
        *num_leafs = level_cnt / radix;
        return total_cnt - level_cnt / radix;
    } else {
        *num_leafs = level_cnt;
        return group_size;
    }
}

/* Setup N-array scatter Knomial-gather static information */
static int load_narray_knomial_tree (mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int rc, i, peer;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;

    ptpcoll_module->full_narray_tree_size = calc_full_tree_size(
        cm->narray_knomial_radix,
        ptpcoll_module->group_size,
        &ptpcoll_module->full_narray_tree_num_leafs);

    ptpcoll_module->narray_knomial_proxy_extra_index = (int *)
        malloc(sizeof(int) * (cm->narray_knomial_radix));
    if (NULL == ptpcoll_module->narray_knomial_proxy_extra_index) {
        PTPCOLL_ERROR(("Failed to allocate memory"));
        goto Error;
    }

    ptpcoll_module->narray_knomial_node = calloc(
        ptpcoll_module->full_narray_tree_size,
        sizeof(netpatterns_narray_knomial_tree_node_t));
    if(NULL == ptpcoll_module->narray_knomial_node) {
        goto Error;
    }

    PTPCOLL_VERBOSE(10 ,("My type is proxy, full tree size = %d [%d]",
                         ptpcoll_module->full_narray_tree_size,
                         cm->narray_knomial_radix
                        ));

    if (ptpcoll_module->super.sbgp_partner_module->my_index <
        ptpcoll_module->full_narray_tree_size) {
        if (ptpcoll_module->super.sbgp_partner_module->my_index <
            ptpcoll_module->group_size - ptpcoll_module->full_narray_tree_size) {
            ptpcoll_module->narray_type = PTPCOLL_PROXY;
            for (i = 0; i < cm->narray_knomial_radix; i++) {
                peer =
                    ptpcoll_module->super.sbgp_partner_module->my_index *
                    cm->narray_knomial_radix + i +
                    ptpcoll_module->full_narray_tree_size;
                if (peer >= ptpcoll_module->group_size) {
                    break;
                }
                ptpcoll_module->narray_knomial_proxy_extra_index[i] = peer;
            }
            ptpcoll_module->narray_knomial_proxy_num = i;
        } else {
            ptpcoll_module->narray_type = PTPCOLL_IN_GROUP;;
        }
        /* Setting node info */
        for(i = 0; i < ptpcoll_module->full_narray_tree_size; i++) {
            rc = netpatterns_setup_narray_knomial_tree(
                cm->narray_knomial_radix,
                i,
                ptpcoll_module->full_narray_tree_size,
                &ptpcoll_module->narray_knomial_node[i]);
            if(OMPI_SUCCESS != rc) {
                goto Error;
            }
        }
    } else {
        ptpcoll_module->narray_type = PTPCOLL_EXTRA;
        ptpcoll_module->narray_knomial_proxy_extra_index[0] =
            (ptpcoll_module->super.sbgp_partner_module->my_index -
             ptpcoll_module->full_narray_tree_size) /
            cm->narray_knomial_radix;
    }

    return OMPI_SUCCESS;

Error:
    if (NULL != ptpcoll_module->narray_knomial_node) {
        free(ptpcoll_module->narray_knomial_node);
    }
    if (NULL != ptpcoll_module->narray_knomial_proxy_extra_index) {
        free(ptpcoll_module->narray_knomial_proxy_extra_index);
    }
    return OMPI_ERROR;
}

/* Setup N-array static information */
static int load_narray_tree(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int rc, i;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;

    ptpcoll_module->narray_node = calloc(ptpcoll_module->group_size,
                                         sizeof(netpatterns_tree_node_t));
    if(NULL == ptpcoll_module->narray_node ) {
        goto Error;
    }

    for(i = 0; i < ptpcoll_module->group_size; i++) {
        rc = netpatterns_setup_narray_tree(
            cm->narray_radix,
            i,
            ptpcoll_module->group_size,
            &ptpcoll_module->narray_node[i]);
        if(OMPI_SUCCESS != rc) {
            goto Error;
        }
    }

    return OMPI_SUCCESS;

Error:
    if (NULL != ptpcoll_module->narray_node) {
        free(ptpcoll_module->narray_node);
    }
    return OMPI_ERROR;
}

static int load_knomial_info(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int i;
    mca_bcol_ptpcoll_component_t *cm = &mca_bcol_ptpcoll_component;

    ptpcoll_module->k_nomial_radix =
        cm->k_nomial_radix > ptpcoll_module->group_size ?
        ptpcoll_module->group_size :
        cm->k_nomial_radix;

    ptpcoll_module->pow_k = pow_k_calc(ptpcoll_module->k_nomial_radix,
                                       ptpcoll_module->group_size,
                                       &ptpcoll_module->pow_knum);

    ptpcoll_module->kn_proxy_extra_index = (int *)
        malloc(sizeof(int) * (ptpcoll_module->k_nomial_radix - 1));
    if (NULL == ptpcoll_module->kn_proxy_extra_index) {
        PTPCOLL_ERROR(("Failed to allocate memory"));
        goto Error;
    }

    /* Setting peer type for K-nomial algorithm*/
    if (ptpcoll_module->super.sbgp_partner_module->my_index < ptpcoll_module->pow_knum ) {
        if (ptpcoll_module->super.sbgp_partner_module->my_index <
            ptpcoll_module->group_size - ptpcoll_module->pow_knum) {
            for (i = 0;
                 i < (ptpcoll_module->k_nomial_radix - 1) &&
                     ptpcoll_module->super.sbgp_partner_module->my_index *
                     (ptpcoll_module->k_nomial_radix - 1)  +
                     i + ptpcoll_module->pow_knum < ptpcoll_module->group_size
                     ; i++) {
                ptpcoll_module->pow_ktype = PTPCOLL_KN_PROXY;
                ptpcoll_module->kn_proxy_extra_index[i] =
                    ptpcoll_module->super.sbgp_partner_module->my_index *
                    (ptpcoll_module->k_nomial_radix - 1) +
                    i + ptpcoll_module->pow_knum;
                PTPCOLL_VERBOSE(10 ,("My type is proxy, pow_knum = %d [%d] my extra %d",
                                     ptpcoll_module->pow_knum,
                                     ptpcoll_module->pow_k,
                                     ptpcoll_module->kn_proxy_extra_index[i]));
            }
            ptpcoll_module->kn_proxy_extra_num = i;
        } else {
            PTPCOLL_VERBOSE(10 ,("My type is in group, pow_knum = %d [%d]", ptpcoll_module->pow_knum,
                                 ptpcoll_module->pow_k));
            ptpcoll_module->pow_ktype = PTPCOLL_KN_IN_GROUP;
        }
    } else {
        ptpcoll_module->pow_ktype = PTPCOLL_KN_EXTRA;
        ptpcoll_module->kn_proxy_extra_index[0] = (ptpcoll_module->super.sbgp_partner_module->my_index -
                                                   ptpcoll_module->pow_knum) / (ptpcoll_module->k_nomial_radix - 1);
        PTPCOLL_VERBOSE(10 ,("My type is extra , pow_knum = %d [%d] my proxy %d",
                             ptpcoll_module->pow_knum,
                             ptpcoll_module->pow_k,
                             ptpcoll_module->kn_proxy_extra_index[0]));
    }

    return OMPI_SUCCESS;

Error:
    if (NULL == ptpcoll_module->kn_proxy_extra_index) {
        free(ptpcoll_module->kn_proxy_extra_index);
    }

    return OMPI_ERROR;
}

static int load_binomial_info(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    ptpcoll_module->pow_2 = pow_k_calc(2,
                                       ptpcoll_module->group_size,
                                       &ptpcoll_module->pow_2num);

    assert(ptpcoll_module->pow_2num == 1 << ptpcoll_module->pow_2);
    assert(ptpcoll_module->pow_2num  <= ptpcoll_module->group_size);

    /* Setting peer type for binary algorithm*/
    if (ptpcoll_module->super.sbgp_partner_module->my_index < ptpcoll_module->pow_2num ) {
        if (ptpcoll_module->super.sbgp_partner_module->my_index <
            ptpcoll_module->group_size - ptpcoll_module->pow_2num) {
            PTPCOLL_VERBOSE(10 ,("My type is proxy, pow_2num = %d [%d]", ptpcoll_module->pow_2num,
                                 ptpcoll_module->pow_2));
            ptpcoll_module->pow_2type = PTPCOLL_PROXY;
            ptpcoll_module->proxy_extra_index = ptpcoll_module->super.sbgp_partner_module->my_index +
                ptpcoll_module->pow_2num;
        } else {
            PTPCOLL_VERBOSE(10 ,("My type is in group, pow_2num = %d [%d]", ptpcoll_module->pow_2num,
                                 ptpcoll_module->pow_2));
            ptpcoll_module->pow_2type = PTPCOLL_IN_GROUP;
        }
    } else {
        PTPCOLL_VERBOSE(10 ,("My type is extra , pow_2num = %d [%d]", ptpcoll_module->pow_2num,
                             ptpcoll_module->pow_2));
        ptpcoll_module->pow_2type = PTPCOLL_EXTRA;
        ptpcoll_module->proxy_extra_index = ptpcoll_module->super.sbgp_partner_module->my_index -
            ptpcoll_module->pow_2num;
    }
    return OMPI_SUCCESS;
}

static int load_recursive_knomial_info(mca_bcol_ptpcoll_module_t *ptpcoll_module)
{
    int rc = OMPI_SUCCESS;
    rc = netpatterns_setup_recursive_knomial_tree_node(
        ptpcoll_module->group_size,
        ptpcoll_module->super.sbgp_partner_module->my_index,
        mca_bcol_ptpcoll_component.k_nomial_radix,
        &ptpcoll_module->knomial_exchange_tree);
    return rc;
}

static int bcol_ptpcoll_collreq_init(opal_free_list_item_t *item, void* ctx)
{
    mca_bcol_ptpcoll_module_t *ptpcoll_module= (mca_bcol_ptpcoll_module_t *) ctx;
    mca_bcol_ptpcoll_collreq_t *collreq = (mca_bcol_ptpcoll_collreq_t *) item;

    switch(mca_bcol_ptpcoll_component.barrier_alg) {
    case 1:
        collreq->requests = (ompi_request_t **)
            calloc(2, sizeof(ompi_request_t *));
        break;
    case 2:
        collreq->requests = (ompi_request_t **)
            calloc(2 * ptpcoll_module->k_nomial_radix, sizeof(ompi_request_t *));
        break;
    }

    if (NULL == collreq->requests) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}

/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.  This is where
 * the backing shared-memory file is created.
 */
mca_bcol_base_module_t **mca_bcol_ptpcoll_comm_query(mca_sbgp_base_module_t *sbgp,
                                                     int *num_modules)
{
    int rc;
    /* local variables */
    struct ompi_communicator_t *comm = sbgp->group_comm;
    mca_bcol_ptpcoll_module_t *ptpcoll_module = NULL;
    mca_bcol_base_module_t **ptpcoll_modules = NULL;
    int iovec_size;

    /* initialize local variables */
    *num_modules = 0;

    /*
     * This is activated only for intra-communicators
     */
    if (OMPI_COMM_IS_INTER(comm) ) {
        return NULL;
    }

    /* allocate and initialize an sm-v2  module */
    ptpcoll_modules = (mca_bcol_base_module_t **) malloc(sizeof(mca_bcol_base_module_t *));
    if (NULL == ptpcoll_modules) {
        return NULL;
    }

    ptpcoll_module = OBJ_NEW(mca_bcol_ptpcoll_module_t);
    if (NULL == ptpcoll_module) {
        free(ptpcoll_modules);
        return NULL;
    }

    /* On this stage we support only one single module */
    ptpcoll_modules[*num_modules] = &(ptpcoll_module->super);

    (*num_modules)++;
    /* set the subgroup */
    ptpcoll_module->super.sbgp_partner_module = sbgp;
    /* caching some useful information */
    ptpcoll_module->group_size =
        ptpcoll_module->super.sbgp_partner_module->group_size;

    rc = load_binomial_info(ptpcoll_module);
    if (OMPI_SUCCESS != rc) {
        PTPCOLL_VERBOSE(10, ("Failed to load knomial info"));
        goto CLEANUP;
    }

    rc = load_knomial_info(ptpcoll_module);
    if (OMPI_SUCCESS != rc) {
        PTPCOLL_VERBOSE(10, ("Failed to load knomial info"));
        goto CLEANUP;
    }

    rc = load_narray_tree(ptpcoll_module);
    if (OMPI_SUCCESS != rc) {
        PTPCOLL_VERBOSE(10, ("Failed to load narray tree"));
        goto CLEANUP;
    }

    rc = load_narray_knomial_tree(ptpcoll_module);
    if (OMPI_SUCCESS != rc) {
        PTPCOLL_VERBOSE(10, ("Failed to load narray-knomila tree"));
        goto CLEANUP;
    }

    rc = load_recursive_knomial_info(ptpcoll_module);
    if (OMPI_SUCCESS != rc) {
        PTPCOLL_VERBOSE(10, ("Failed to load recursive knomial tree"));
        goto CLEANUP;
    }

    /* creating collfrag free list */
    OBJ_CONSTRUCT(&ptpcoll_module->collreqs_free, opal_free_list_t);
    rc = opal_free_list_init (&ptpcoll_module->collreqs_free,
                              sizeof(mca_bcol_ptpcoll_collreq_t),
                              BCOL_PTP_CACHE_LINE_SIZE,
                              OBJ_CLASS(mca_bcol_ptpcoll_collreq_t),
                              0, BCOL_PTP_CACHE_LINE_SIZE,
                              256 /* free_list_num */,
                              -1  /* free_list_max, -1 = infinite */,
                              32  /* free_list_inc */,
                              NULL, 0, NULL,
                              bcol_ptpcoll_collreq_init,
                              ptpcoll_module);
    if (OMPI_SUCCESS != rc) {
        goto CLEANUP;
    }

    load_func(ptpcoll_module);

    rc = alloc_allreduce_offsets_array(ptpcoll_module);
    if (OMPI_SUCCESS != rc) {
        goto CLEANUP;
    }

    /* Allocating iovec for PTP alltoall */
    iovec_size = ptpcoll_module->group_size / 2 + ptpcoll_module->group_size % 2;
    ptpcoll_module->alltoall_iovec = (struct iovec *) malloc(sizeof(struct iovec)
                                                             * iovec_size);
    ptpcoll_module->log_group_size = lognum(ptpcoll_module->group_size);

    rc = mca_bcol_base_bcol_fns_table_init(&(ptpcoll_module->super));
    if (OMPI_SUCCESS != rc) {
        goto CLEANUP;
    }

    /* Zero copy is supported */
    ptpcoll_module->super.supported_mode = MCA_BCOL_BASE_ZERO_COPY;

    /* return */
    return ptpcoll_modules;

CLEANUP:

    OBJ_RELEASE(ptpcoll_module);
    free(ptpcoll_modules);
    return NULL;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "osc_rdma_comm.h"

#include "ompi/mca/bml/base/base.h"

#define NODE_ID_TO_RANK(module, peer_data, node_id) ((int)(peer_data)->len)

/**
 * @brief find the btl endpoint for a process
 *
 * @param[in] module           osc rdma module
 * @param[in] peer_id          process rank in the module communicator
 * @param[in] module_btl_index btl index to use
 *
 * @returns NULL on error
 * @returns btl endpoint on success
 */
static int ompi_osc_rdma_peer_btl_endpoint (struct ompi_osc_rdma_module_t *module,
                                            int peer_id, uint8_t *btl_index_out,
                                            struct mca_btl_base_endpoint_t **endpoint)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup (module->comm, peer_id);
    mca_bml_base_endpoint_t *bml_endpoint = mca_bml_base_get_endpoint(proc);

    if (module->use_accelerated_btl) {
        opal_output_verbose(MCA_BASE_VERBOSE_TRACE, ompi_osc_base_framework.framework_output,
                            "rank %d: accelerated btl search for peer %d",
                            ompi_comm_rank(module->comm), peer_id);
        mca_bml_base_btl_t *bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma,
                                                                  module->accelerated_btl);
        if (NULL != bml_btl) {
            *btl_index_out = 0;
            *endpoint = bml_btl->btl_endpoint;

            return OMPI_SUCCESS;
        }
    } else {
        mca_bml_base_btl_t *bml_btl;
        opal_output_verbose(MCA_BASE_VERBOSE_TRACE, ompi_osc_base_framework.framework_output,
                            "rank %d: alternate btl search for peer %d",
                            ompi_comm_rank(module->comm), peer_id);

        /* the non accelerated case is a bit difficult compared to the
         * accelerated case.  The right BTL could be in either the
         * rdma or eager endpoint list, because we're using the am
         * rdma interface to provide RDMA semantics.  The important
         * part is that we search the alternate_btls list in order,
         * since it is sorted by latency.
         */
        for (int osc_btl_idx = 0 ; osc_btl_idx < module->alternate_btl_count ; ++osc_btl_idx) {
            mca_btl_base_module_t *search_btl = ompi_osc_rdma_selected_btl(module, osc_btl_idx);
            const char *source = NULL;

            opal_output_verbose(MCA_BASE_VERBOSE_TRACE, ompi_osc_base_framework.framework_output,
                                "rank %d comparing with btl %s, %d",
                                ompi_comm_rank(module->comm),
                                search_btl->btl_component->btl_version.mca_component_name,
                                osc_btl_idx);

            source = "rdma";
            bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, search_btl);
            if (NULL == bml_btl) {
                source = "eager";
                bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_eager, search_btl);
            }
            if (NULL != bml_btl) {
                *btl_index_out = osc_btl_idx;
                *endpoint = bml_btl->btl_endpoint;

                opal_output_verbose(MCA_BASE_VERBOSE_TRACE, ompi_osc_base_framework.framework_output,
                                    "rank %d found btl for peer %d (%s, %d, %s)",
                                    ompi_comm_rank(module->comm), peer_id,
                                    bml_btl->btl->btl_component->btl_version.mca_component_name,
                                    osc_btl_idx, source);

                return OMPI_SUCCESS;
            }
        }
    }

    opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_osc_base_framework.framework_output,
                        "rank %d: failed peer search for peer %d",
                        ompi_comm_rank(module->comm), peer_id);

    /* unlikely but can happen when creating a peer for self */
    return OMPI_ERR_UNREACH;
}

int ompi_osc_rdma_new_peer (struct ompi_osc_rdma_module_t *module, int peer_id, ompi_osc_rdma_peer_t **peer_out) {
    struct mca_btl_base_endpoint_t *endpoint = NULL;
    ompi_osc_rdma_peer_t *peer;
    uint8_t module_btl_index = UINT8_MAX;

    *peer_out = NULL;

    /* find a btl/endpoint to use for this peer */
    int ret = ompi_osc_rdma_peer_btl_endpoint (module, peer_id, &module_btl_index, &endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (MPI_WIN_FLAVOR_DYNAMIC == module->flavor) {
        peer = (ompi_osc_rdma_peer_t *) OBJ_NEW(ompi_osc_rdma_peer_dynamic_t);
    } else if (module->same_size && module->same_disp_unit) {
        /* use a smaller peer object when same_size and same_disp_unit are set */
        peer = (ompi_osc_rdma_peer_t *) OBJ_NEW(ompi_osc_rdma_peer_basic_t);
    } else {
        peer = (ompi_osc_rdma_peer_t *) OBJ_NEW(ompi_osc_rdma_peer_extended_t);
    }

    peer->data_endpoint  = endpoint;
    peer->data_btl_index = module_btl_index;
    peer->rank           = peer_id;

    *peer_out = peer;

    return OMPI_SUCCESS;
}

/**
 * @brief finish initializing a peer object
 *
 * @param[in] module         osc rdma module
 * @param[in] peer           peer object to set up
 *
 * This function reads the registration handle and state pointer from the peer that holds that data. If necessary
 * it will then ready information about the peer from its state data structure. This information includes the
 * displacement unit, base pointer, window size, and registation handle (if applicable).
 */
static int ompi_osc_rdma_peer_setup (ompi_osc_rdma_module_t *module, ompi_osc_rdma_peer_t *peer)
{
    ompi_osc_rdma_peer_extended_t *ex_peer = (ompi_osc_rdma_peer_extended_t *) peer;
    uint64_t peer_data_size;
    uint64_t peer_data_offset, array_pointer;
    struct mca_btl_base_endpoint_t *array_endpoint;
    ompi_osc_rdma_region_t *array_peer_data, *node_peer_data;
    ompi_osc_rdma_rank_data_t rank_data;
    int registration_handle_size = 0;
    int node_id, node_rank, array_index;
    uint8_t array_btl_index;
    int ret, disp_unit;
    char *peer_data;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "configuring peer for rank %d", peer->rank);

    if (module->use_memory_registration) {
        assert(module->use_accelerated_btl);
        registration_handle_size = module->accelerated_btl->btl_registration_handle_size;
    }

    /* each node is responsible for holding a part of the rank -> node/local rank mapping array. this code
     * calculates the node and offset the mapping can be found. once the mapping has been read the state
     * part of the peer structure can be initialized. */
    node_id = peer->rank / RANK_ARRAY_COUNT(module);
    array_peer_data = (ompi_osc_rdma_region_t *) ((intptr_t) module->node_comm_info + node_id * module->region_size);

    /* the node leader rank is stored in the length field */
    node_rank = NODE_ID_TO_RANK(module, array_peer_data, node_id);
    array_index = peer->rank % RANK_ARRAY_COUNT(module);

    array_pointer = array_peer_data->base + array_index * sizeof (rank_data);

    /* lookup the btl endpoint needed to retrieve the mapping */
    ret = ompi_osc_rdma_peer_btl_endpoint (module, node_rank, &array_btl_index, &array_endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return OMPI_ERR_UNREACH;
    }

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "reading region data for %d from rank: %d, index: %d, pointer: 0x%" PRIx64
                     ", size: %lu", peer->rank, node_rank, array_index, array_pointer, sizeof (rank_data));

    ret = ompi_osc_get_data_blocking (module, array_btl_index, array_endpoint, array_pointer,
                                      (mca_btl_base_registration_handle_t *) array_peer_data->btl_handle_data,
                                      &rank_data, sizeof (rank_data));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    /* initialize the state part of the peer object. NTH: for now the state data is for every node is stored on
     * every node. this gives a good balance of code complexity and memory usage at this time. we take advantage
     * of this by re-using the endpoint and pointer stored in the node_comm_info array. */
    node_peer_data = (ompi_osc_rdma_region_t *) ((intptr_t) module->node_comm_info + rank_data.node_id * module->region_size);

    peer->state = node_peer_data->base + module->state_offset + module->state_size * rank_data.rank;

    if (registration_handle_size) {
        peer->state_handle = (mca_btl_base_registration_handle_t *) node_peer_data->btl_handle_data;
    }

    ret = ompi_osc_rdma_peer_btl_endpoint (module, NODE_ID_TO_RANK(module, node_peer_data, rank_data.node_id),
                                           &peer->state_btl_index, &peer->state_endpoint);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return OPAL_ERR_UNREACH;
    }

    /* nothing more to do for dynamic memory windows */
    if (MPI_WIN_FLAVOR_DYNAMIC == module->flavor) {
        return OMPI_SUCCESS;
    }

    /* read window data from the target rank */
    if (module->same_disp_unit) {
        /* do not bother reading the displacement unit as it is already known */
        peer_data_offset = offsetof (ompi_osc_rdma_state_t, regions);
    } else {
        peer_data_offset = offsetof (ompi_osc_rdma_state_t, disp_unit);
    }

    peer_data_size = module->state_size - peer_data_offset;
    peer_data = alloca (peer_data_size);

    /* read window data from the end of the target's state structure */
    ret = ompi_osc_get_data_blocking (module, peer->state_btl_index, peer->state_endpoint,
                                      peer->state + peer_data_offset, peer->state_handle,
                                      peer_data, peer_data_size);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (!module->same_disp_unit) {
        /* unpack displacement */
        memcpy (&ex_peer->disp_unit, peer_data, sizeof (ex_peer->disp_unit));
        peer_data += offsetof (ompi_osc_rdma_state_t, regions) - offsetof (ompi_osc_rdma_state_t, disp_unit);
        disp_unit = ex_peer->disp_unit;
    } else {
        disp_unit = module->disp_unit;
    }

    ompi_osc_rdma_region_t *base_region = (ompi_osc_rdma_region_t *) peer_data;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "peer %d: remote base region: 0x%" PRIx64 ", size: %" PRId64
                     ", flags: 0x%x, disp_unit: %d", peer->rank, base_region->base, base_region->len,
                     peer->flags, disp_unit);
    (void)disp_unit;  // silence compiler warning

    if (ompi_osc_rdma_peer_local_base (peer)) {
        /* for now we store the local address in the standard place. do no overwrite it */
        return OMPI_SUCCESS;
    }

    ex_peer->super.base = base_region->base;

    /* save size and base */
    if (!module->same_size) {
        ex_peer->size = base_region->len;
    }

    if (base_region->len) {
        if (registration_handle_size) {
            ex_peer->super.base_handle = malloc (registration_handle_size);
            if (OPAL_UNLIKELY(NULL == ex_peer->super.base_handle)) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            peer->flags |= OMPI_OSC_RDMA_PEER_BASE_FREE;

            memcpy (ex_peer->super.base_handle, base_region->btl_handle_data, registration_handle_size);
        }

        if (MPI_WIN_FLAVOR_ALLOCATE == module->flavor) {
            ex_peer->super.super.data_endpoint = ex_peer->super.super.state_endpoint;
            ex_peer->super.super.data_btl_index = ex_peer->super.super.state_btl_index;
        }
    }

    return OMPI_SUCCESS;
}

/**
 * @brief lookup (or allocate) a peer for a rank (internal)
 *
 * @param[in] module         osc rdma module
 * @param[in] peer_id        rank of remote peer (in module communicator)
 *
 * @returns peer object on success
 * @returns NULL on error
 *
 * This is an internal function for looking up or allocating a peer object for a window rank. This
 * function requires the peer lock to be held and is only expected to be called from itself or
 * the ompi_osc_rdma_peer_lookup() helper function.
 */
static struct ompi_osc_rdma_peer_t *ompi_osc_rdma_peer_lookup_internal (struct ompi_osc_rdma_module_t *module, int peer_id)
{
    ompi_osc_rdma_peer_t *peer;
    int ret;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_DEBUG, "looking up peer data for rank %d", peer_id);

    peer = ompi_osc_module_get_peer (module, peer_id);
    if (NULL != peer) {
        return peer;
    }

    ret = ompi_osc_rdma_new_peer (module, peer_id, &peer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return NULL;
    }

    ret = ompi_osc_rdma_peer_setup (module, peer);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OBJ_RELEASE(peer);
        return NULL;
    }

    ret = ompi_osc_module_add_peer (module, peer);
    if (OPAL_SUCCESS != ret) {
        /* out of memory */
        OBJ_RELEASE(peer);
        return NULL;
    }

    /* ensure the peer hash is updated before we drop the lock */
    opal_atomic_wmb ();

    return peer;
}

struct ompi_osc_rdma_peer_t *ompi_osc_rdma_peer_lookup (struct ompi_osc_rdma_module_t *module, int peer_id)
{
    struct ompi_osc_rdma_peer_t *peer;

    opal_mutex_lock (&module->peer_lock);
    peer = ompi_osc_rdma_peer_lookup_internal (module, peer_id);
    opal_mutex_unlock (&module->peer_lock);

    return peer;
}


/******* peer objects *******/

static void ompi_osc_rdma_peer_construct (ompi_osc_rdma_peer_t *peer)
{
    memset ((char *) peer + sizeof (peer->super), 0, sizeof (*peer) - sizeof (peer->super));
}

static void ompi_osc_rdma_peer_destruct (ompi_osc_rdma_peer_t *peer)
{
    if (peer->state_handle && (peer->flags & OMPI_OSC_RDMA_PEER_STATE_FREE)) {
        free (peer->state_handle);
    }
}

OBJ_CLASS_INSTANCE(ompi_osc_rdma_peer_t, opal_list_item_t,
                   ompi_osc_rdma_peer_construct,
                   ompi_osc_rdma_peer_destruct);

static void ompi_osc_rdma_peer_basic_construct (ompi_osc_rdma_peer_basic_t *peer)
{
    memset ((char *) peer + sizeof (peer->super), 0, sizeof (*peer) - sizeof (peer->super));
}

static void ompi_osc_rdma_peer_basic_destruct (ompi_osc_rdma_peer_basic_t *peer)
{
    if (peer->base_handle && (peer->super.flags & OMPI_OSC_RDMA_PEER_BASE_FREE)) {
        free (peer->base_handle);
    }
}

OBJ_CLASS_INSTANCE(ompi_osc_rdma_peer_basic_t, ompi_osc_rdma_peer_t,
                   ompi_osc_rdma_peer_basic_construct,
                   ompi_osc_rdma_peer_basic_destruct);

OBJ_CLASS_INSTANCE(ompi_osc_rdma_peer_extended_t, ompi_osc_rdma_peer_basic_t,
                   NULL, NULL);

static void ompi_osc_rdma_peer_dynamic_construct (ompi_osc_rdma_peer_dynamic_t *peer)
{
    memset ((char *) peer + sizeof (peer->super), 0, sizeof (*peer) - sizeof (peer->super));
}

static void ompi_osc_rdma_peer_dynamic_destruct (ompi_osc_rdma_peer_dynamic_t *peer)
{
    if (peer->regions) {
        free (peer->regions);
    }
}

OBJ_CLASS_INSTANCE(ompi_osc_rdma_peer_dynamic_t, ompi_osc_rdma_peer_t,
                   ompi_osc_rdma_peer_dynamic_construct,
                   ompi_osc_rdma_peer_dynamic_destruct);

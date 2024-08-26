/* -*- Mode: C; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "opal/include/opal/align.h"

#ifdef HAVE_XPMEM_H
#include "opal/mca/rcache/base/base.h"
#include <xpmem.h>
#endif


/* shared memory structure */
/* first 16 * 1024 bytes (16KB) are used for the leader */
/* next 2* 64 * comm_size bytes are used for  sync variables */
/* next 8 * 1024 * comm_size are used for per_rank data (8KB per rank) */
/* offsets for the shared memory region */
#define CACHE_LINE_SIZE 64
#define LEADER_SHM_SIZE 16384
#define PER_RANK_SHM_SIZE 8192

extern uint64_t mca_coll_acoll_xpmem_buffer_size;
extern int mca_coll_acoll_without_xpmem;
extern int mca_coll_acoll_xpmem_use_sr_buf;


/* Function to allocate scratch buffer */
static inline void *coll_acoll_buf_alloc(coll_acoll_reserve_mem_t *reserve_mem_ptr, uint64_t size)
{
    void *temp_ptr = NULL;
    /* If requested size is within the pre-allocated range, use the
       pre-allocated buffer if not in use. */
    if ((true == reserve_mem_ptr->reserve_mem_allocate)
        && (size <= reserve_mem_ptr->reserve_mem_size)
        && (false == reserve_mem_ptr->reserve_mem_in_use)) {
        if (NULL == reserve_mem_ptr->reserve_mem) {
            reserve_mem_ptr->reserve_mem = malloc(reserve_mem_ptr->reserve_mem_size);
        }
        temp_ptr = reserve_mem_ptr->reserve_mem;

        /* Mark the buffer as "in use" */
        if (NULL != temp_ptr) {
            reserve_mem_ptr->reserve_mem_in_use = true;
        }
    } else {
        /* If requested size if greater than that of the pre-allocated
           buffer or if the pre-allocated buffer is in use, create new buffer */
        temp_ptr = malloc(size);
    }

    return temp_ptr;
}

/* Function to free scratch buffer */
static inline void coll_acoll_buf_free(coll_acoll_reserve_mem_t *reserve_mem_ptr, void *ptr)
{
    /* Free the buffer only if it is not the reserved (pre-allocated) one */
    if ((false == reserve_mem_ptr->reserve_mem_allocate)
        || (false == reserve_mem_ptr->reserve_mem_in_use)) {
        if (NULL != ptr) {
            free(ptr);
        }
    } else if (reserve_mem_ptr->reserve_mem == ptr) {
        /* Mark the reserved buffer as free to be used */
        reserve_mem_ptr->reserve_mem_in_use = false;
    }
}

/* Function to check if subcomms structure is allocated and initialized */
static inline int check_and_create_subc(ompi_communicator_t *comm,
                                        mca_coll_acoll_module_t *acoll_module,
                                        coll_acoll_subcomms_t **subc_ptr)
{
    int cid = ompi_comm_get_local_cid(comm);
    int num_subc = acoll_module->num_subc;
    coll_acoll_subcomms_t *subc;

    /* Return if max comms is not positive */
    if (acoll_module->max_comms <= 0) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:acoll WARNING Set mca_coll_acoll_max_comms to positive value to use acoll!"));
        *subc_ptr = NULL;
        return MPI_SUCCESS;
    }

    /* Allocate memory for subcomms array */
    if (NULL == acoll_module->subc) {
        acoll_module->subc = malloc(sizeof(coll_acoll_subcomms_t**) * acoll_module->max_comms);
        if (NULL == acoll_module->subc) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    /* Check if subcomms structure is already created for the communicator */
    for (int i = 0; i < num_subc; i++) {
        if (acoll_module->subc[i]->cid == cid) {
            *subc_ptr = acoll_module->subc[i];
            return MPI_SUCCESS;
        }
    }

    /* Subcomms structure is not present, create one if within limit*/
    if (num_subc == acoll_module->max_comms) {
        OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                     "coll:acoll WARNING Falling back to base since max communicators limit %d exceeded, set mca_coll_acoll_max_comms to higher value to use acoll!", acoll_module->max_comms));
        *subc_ptr = NULL;
        return MPI_SUCCESS;
    }
    *subc_ptr = (coll_acoll_subcomms_t *)malloc(sizeof(coll_acoll_subcomms_t));
    if (NULL == *subc_ptr) {
        return MPI_SUCCESS;
    }
    /* Update the module with the new subc */
    acoll_module->subc[num_subc] = *subc_ptr;
    acoll_module->num_subc++;

    /* Initialize elements of subc */
    subc = *subc_ptr;
    subc->cid = cid;
    subc->initialized = 0;
    subc->is_root_node = 0;
    subc->is_root_sg = 0;
    subc->is_root_numa = 0;
    subc->outer_grp_root = -1;
    subc->subgrp_root = 0;
    subc->num_nodes = 1;
    subc->prev_init_root = -1;
    subc->num_root_change = 0;
    subc->numa_root = 0;
    subc->socket_ldr_root = -1;
    subc->orig_comm = comm;
    subc->local_comm = NULL;
    subc->local_r_comm = NULL;
    subc->leader_comm = NULL;
    subc->subgrp_comm = NULL;
    subc->socket_comm = NULL;
    subc->socket_ldr_comm = NULL;
    for (int j = 0; j < MCA_COLL_ACOLL_NUM_LAYERS; j++) {
        for (int k = 0; k < MCA_COLL_ACOLL_NUM_BASE_LYRS; k++) {
            subc->base_comm[k][j] = NULL;
            subc->base_root[k][j] = -1;
        }
        subc->local_root[j] = 0;
    }

    subc->numa_comm = NULL;
    subc->numa_comm_ldrs = NULL;
    subc->node_comm = NULL;
    subc->inter_comm = NULL;
    subc->initialized_data = false;
    subc->initialized_shm_data = false;
    subc->data = NULL;
#ifdef HAVE_XPMEM_H
    subc->xpmem_buf_size = mca_coll_acoll_xpmem_buffer_size;
    subc->without_xpmem = mca_coll_acoll_without_xpmem;
    subc->xpmem_use_sr_buf = mca_coll_acoll_xpmem_use_sr_buf;
#endif
    return MPI_SUCCESS;

}

/* Function to compare integer elements */
static int compare_values(const void *ptra, const void *ptrb)
{
    int a = *((int *) ptra);
    int b = *((int *) ptrb);

    if (a < b) {
        return -1;
    } else if (a > b) {
        return 1;
    }

    return 0;
}

/* Function to map ranks from parent communicator to sub-communicator */
static inline int comm_grp_ranks_local(ompi_communicator_t *comm, ompi_communicator_t *local_comm,
                                       int *is_root_node, int *local_root, int **ranks_buf,
                                       int root)
{
    ompi_group_t *local_grp, *grp;
    int local_size = ompi_comm_size(local_comm);
    int *ranks = malloc(local_size * sizeof(int));
    int *local_ranks = malloc(local_size * sizeof(int));
    int i, err;

    /* Create parent (comm) and sub-comm (local_comm) groups */
    err = ompi_comm_group(comm, &grp);
    err = ompi_comm_group(local_comm, &local_grp);
    /* Initialize ranks for sub-communicator (local_comm) */
    for (i = 0; i < local_size; i++) {
        local_ranks[i] = i;
    }

    /* Translate the ranks among the 2 communicators */
    err = ompi_group_translate_ranks(local_grp, local_size, local_ranks, grp, ranks);
    if (ranks_buf != NULL) {
        *ranks_buf = malloc(local_size * sizeof(int));
        memcpy(*ranks_buf, ranks, local_size * sizeof(int));
    }

    /* Derive the 'local_root' which is the equivalent rank for 'root' of
       'comm' in 'local_comm' */
    for (i = 0; i < local_size; i++) {
        if (ranks[i] == root) {
            *is_root_node = 1;
            *local_root = i;
            break;
        }
    }

    err = ompi_group_free(&grp);
    err = ompi_group_free(&local_grp);
    free(ranks);
    free(local_ranks);

    return err;
}

static inline int mca_coll_acoll_create_base_comm(ompi_communicator_t **parent_comm,
                                                  coll_acoll_subcomms_t *subc, int color, int rank,
                                                  int *root, int base_lyr)
{
    int i;
    int err;

    for (i = 0; i < MCA_COLL_ACOLL_NUM_LAYERS; i++) {
        int is_root_node = 0;

        /* Create base comm */
        err = ompi_comm_split(parent_comm[i], color, rank, &subc->base_comm[base_lyr][i], false);
        if (MPI_SUCCESS != err)
            return err;

        /* Find out local rank of root in base comm */
        err = comm_grp_ranks_local(parent_comm[i], subc->base_comm[base_lyr][i], &is_root_node,
                                   &subc->base_root[base_lyr][i], NULL, root[i]);
    }
    return err;
}

static inline int mca_coll_acoll_comm_split_init(ompi_communicator_t *comm,
                                                 mca_coll_acoll_module_t *acoll_module,
                                                 coll_acoll_subcomms_t *subc,
                                                 int root)
{
    opal_info_t comm_info;
    mca_coll_base_module_allreduce_fn_t coll_allreduce_org = (comm)->c_coll->coll_allreduce;
    mca_coll_base_module_allgather_fn_t coll_allgather_org = (comm)->c_coll->coll_allgather;
    mca_coll_base_module_bcast_fn_t coll_bcast_org = (comm)->c_coll->coll_bcast;
    mca_coll_base_module_allreduce_fn_t coll_allreduce_loc, coll_allreduce_soc;
    mca_coll_base_module_allgather_fn_t coll_allgather_loc, coll_allgather_soc;
    mca_coll_base_module_bcast_fn_t coll_bcast_loc, coll_bcast_soc;
    int err;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    (comm)->c_coll->coll_allgather = ompi_coll_base_allgather_intra_ring;
    (comm)->c_coll->coll_allreduce = ompi_coll_base_allreduce_intra_recursivedoubling;
    (comm)->c_coll->coll_bcast = ompi_coll_base_bcast_intra_basic_linear;
    if (!subc->initialized) {
        OBJ_CONSTRUCT(&comm_info, opal_info_t);
        opal_info_set(&comm_info, "ompi_comm_coll_preference", "libnbc,basic,^acoll");
        /* Create node-level subcommunicator */
        err = ompi_comm_split_type(comm, MPI_COMM_TYPE_SHARED, 0, &comm_info, &(subc->local_comm));
        if (MPI_SUCCESS != err) {
            return err;
        }
        /* Create socket-level subcommunicator */
        err = ompi_comm_split_type(comm, OMPI_COMM_TYPE_SOCKET, 0, &comm_info,
                                   &(subc->socket_comm));
        if (MPI_SUCCESS != err) {
            return err;
        }
        OBJ_DESTRUCT(&comm_info);
        OBJ_CONSTRUCT(&comm_info, opal_info_t);
        opal_info_set(&comm_info, "ompi_comm_coll_preference", "libnbc,basic,^acoll");
        /* Create subgroup-level subcommunicator */
        err = ompi_comm_split_type(comm, OMPI_COMM_TYPE_L3CACHE, 0, &comm_info,
                                   &(subc->subgrp_comm));
        if (MPI_SUCCESS != err) {
            return err;
        }
        err = ompi_comm_split_type(comm, OMPI_COMM_TYPE_NUMA, 0, &comm_info, &(subc->numa_comm));
        if (MPI_SUCCESS != err) {
            return err;
        }
        subc->subgrp_size = ompi_comm_size(subc->subgrp_comm);
        OBJ_DESTRUCT(&comm_info);

        /* Derive the no. of nodes */
        if (size == ompi_comm_size(subc->local_comm)) {
            subc->num_nodes = 1;
        } else {
            int *size_list_buf = (int *) malloc(size * sizeof(int));
            int num_nodes = 0;
            int local_size = ompi_comm_size(subc->local_comm);
            /* Perform allgather so that all ranks know the sizes of the nodes
               to which all other ranks belong */
            err = (comm)->c_coll->coll_allgather(&local_size, 1, MPI_INT, size_list_buf, 1, MPI_INT,
                                                 comm, &acoll_module->super);
            if (MPI_SUCCESS != err) {
                free(size_list_buf);
                return err;
            }
            /* Find the no. of nodes by counting each node only once.
             * E.g., if there are 3 nodes with 2, 3 and 4 ranks on each node,
             * first sort the size array so that the array elements are
             * {2,2,3,3,3,4,4,4,4}. Read the value at the start of the array,
             * offset the array by the read value, increment the counter,
             * and repeat the process till end of array is reached. */
            qsort(size_list_buf, size, sizeof(int), compare_values);
            for (int i = 0; i < size;) {
                int ofst = size_list_buf[i];
                num_nodes++;
                i += ofst;
            }
            subc->num_nodes = num_nodes;
            free(size_list_buf);
        }
    }
    /* Common initializations */
    {
        subc->outer_grp_root = -1;
        subc->subgrp_root = 0;
        subc->is_root_sg = 0;
        subc->is_root_numa = 0;
        subc->numa_root = 0;
        subc->is_root_socket = 0;
        subc->socket_ldr_root = -1;

        if (subc->initialized) {
            if (subc->num_nodes > 1) {
                ompi_comm_free(&(subc->leader_comm));
                subc->leader_comm = NULL;
            }
            ompi_comm_free(&(subc->socket_ldr_comm));
            subc->socket_ldr_comm = NULL;
        }
        for (int i = 0; i < MCA_COLL_ACOLL_NUM_LAYERS; i++) {
            if (subc->initialized) {
                ompi_comm_free(&(subc->base_comm[MCA_COLL_ACOLL_L3CACHE][i]));
                subc->base_comm[MCA_COLL_ACOLL_L3CACHE][i] = NULL;
                ompi_comm_free(&(subc->base_comm[MCA_COLL_ACOLL_NUMA][i]));
                subc->base_comm[MCA_COLL_ACOLL_NUMA][i] = NULL;
            }
            subc->base_root[MCA_COLL_ACOLL_L3CACHE][i] = -1;
            subc->base_root[MCA_COLL_ACOLL_NUMA][i] = -1;
        }
        /* Store original collectives for local and socket comms */
        coll_allreduce_loc = (subc->local_comm)->c_coll->coll_allreduce;
        coll_allgather_loc = (subc->local_comm)->c_coll->coll_allgather;
        coll_bcast_loc = (subc->local_comm)->c_coll->coll_bcast;
        (subc->local_comm)->c_coll->coll_allgather = ompi_coll_base_allgather_intra_ring;
        (subc->local_comm)->c_coll->coll_allreduce
            = ompi_coll_base_allreduce_intra_recursivedoubling;
        (subc->local_comm)->c_coll->coll_bcast = ompi_coll_base_bcast_intra_basic_linear;
        coll_allreduce_soc = (subc->socket_comm)->c_coll->coll_allreduce;
        coll_allgather_soc = (subc->socket_comm)->c_coll->coll_allgather;
        coll_bcast_soc = (subc->socket_comm)->c_coll->coll_bcast;
        (subc->socket_comm)->c_coll->coll_allgather = ompi_coll_base_allgather_intra_ring;
        (subc->socket_comm)->c_coll->coll_allreduce
            = ompi_coll_base_allreduce_intra_recursivedoubling;
        (subc->socket_comm)->c_coll->coll_bcast = ompi_coll_base_bcast_intra_basic_linear;
    }

    /* Further subcommunicators based on root */
    if (subc->num_nodes > 1) {
        int local_rank = ompi_comm_rank(subc->local_comm);
        int color = MPI_UNDEFINED;
        int is_root_node = 0, is_root_socket = 0;
        int local_root = 0;
        int *subgrp_ranks = NULL, *numa_ranks = NULL, *socket_ranks = NULL;
        ompi_communicator_t *parent_comm[MCA_COLL_ACOLL_NUM_LAYERS];

        /* Initializations */
        subc->local_root[MCA_COLL_ACOLL_LYR_NODE] = 0;
        subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] = 0;

        /* Find out the local rank of root */
        err = comm_grp_ranks_local(comm, subc->local_comm, &subc->is_root_node,
                                   &subc->local_root[MCA_COLL_ACOLL_LYR_NODE], NULL, root);

        /* Create subcommunicator with leader ranks */
        color = 1;
        if (!subc->is_root_node && (local_rank == 0)) {
            color = 0;
        }
        if (rank == root) {
            color = 0;
        }
        err = ompi_comm_split(comm, color, rank, &subc->leader_comm, false);
        if (MPI_SUCCESS != err) {
            return err;
        }

        /* Find out local rank of root in leader comm */
        err = comm_grp_ranks_local(comm, subc->leader_comm, &is_root_node, &subc->outer_grp_root,
                                   NULL, root);

        /* Find out local rank of root in socket comm */
        if (subc->is_root_node) {
            local_root = subc->local_root[MCA_COLL_ACOLL_LYR_NODE];
        }
        err = comm_grp_ranks_local(subc->local_comm, subc->socket_comm, &subc->is_root_socket,
                                   &subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET], &socket_ranks,
                                   local_root);

        /* Create subcommunicator with socket leaders */
        subc->socket_rank = subc->is_root_socket == 1 ? local_root : socket_ranks[0];
        color = local_rank == subc->socket_rank ? 0 : 1;
        err = ompi_comm_split(subc->local_comm, color, local_rank, &subc->socket_ldr_comm, false);
        if (MPI_SUCCESS != err)
            return err;

        /* Find out local rank of root in socket leader comm */
        err = comm_grp_ranks_local(subc->local_comm, subc->socket_ldr_comm, &is_root_socket,
                                   &subc->socket_ldr_root, NULL, local_root);

        /* Find out local rank of root in subgroup comm */
        err = comm_grp_ranks_local(subc->local_comm, subc->subgrp_comm, &subc->is_root_sg,
                                   &subc->subgrp_root, &subgrp_ranks, local_root);

        /* Create subcommunicator with base ranks */
        subc->base_rank[MCA_COLL_ACOLL_L3CACHE] = subc->is_root_sg == 1 ? local_root
                                                                        : subgrp_ranks[0];
        color = local_rank == subc->base_rank[MCA_COLL_ACOLL_L3CACHE] ? 0 : 1;
        parent_comm[MCA_COLL_ACOLL_LYR_NODE] = subc->local_comm;
        parent_comm[MCA_COLL_ACOLL_LYR_SOCKET] = subc->socket_comm;
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, local_rank,
                                              subc->local_root, MCA_COLL_ACOLL_L3CACHE);

        /* Find out local rank of root in numa comm */
        err = comm_grp_ranks_local(subc->local_comm, subc->numa_comm, &subc->is_root_numa,
                                   &subc->numa_root, &numa_ranks, local_root);

        subc->base_rank[MCA_COLL_ACOLL_NUMA] = subc->is_root_numa == 1 ? local_root : numa_ranks[0];
        color = local_rank == subc->base_rank[MCA_COLL_ACOLL_NUMA] ? 0 : 1;
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, local_rank,
                                              subc->local_root, MCA_COLL_ACOLL_NUMA);

        if (socket_ranks != NULL) {
            free(socket_ranks);
            socket_ranks = NULL;
        }
        if (subgrp_ranks != NULL) {
            free(subgrp_ranks);
            subgrp_ranks = NULL;
        }
        if (numa_ranks != NULL) {
            free(numa_ranks);
            numa_ranks = NULL;
        }
    } else {
        /* Intra node case */
        int color;
        int is_root_socket = 0;
        int *subgrp_ranks = NULL, *numa_ranks = NULL, *socket_ranks = NULL;
        ompi_communicator_t *parent_comm[MCA_COLL_ACOLL_NUM_LAYERS];

        /* Initializations */
        subc->local_root[MCA_COLL_ACOLL_LYR_NODE] = root;
        subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] = 0;

        /* Find out local rank of root in socket comm */
        err = comm_grp_ranks_local(comm, subc->socket_comm, &subc->is_root_socket,
                                   &subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET], &socket_ranks,
                                   root);

        /* Create subcommunicator with socket leaders */
        subc->socket_rank = subc->is_root_socket == 1 ? root : socket_ranks[0];
        color = rank == subc->socket_rank ? 0 : 1;
        err = ompi_comm_split(comm, color, rank, &subc->socket_ldr_comm, false);
        if (MPI_SUCCESS != err) {
            return err;
        }

        /* Find out local rank of root in socket leader comm */
        err = comm_grp_ranks_local(comm, subc->socket_ldr_comm, &is_root_socket,
                                   &subc->socket_ldr_root, NULL, root);

        /* Find out local rank of root in subgroup comm */
        err = comm_grp_ranks_local(comm, subc->subgrp_comm, &subc->is_root_sg, &subc->subgrp_root,
                                   &subgrp_ranks, root);

        /* Create subcommunicator with base ranks */
        subc->base_rank[MCA_COLL_ACOLL_L3CACHE] = subc->is_root_sg == 1 ? root : subgrp_ranks[0];
        color = rank == subc->base_rank[MCA_COLL_ACOLL_L3CACHE] ? 0 : 1;
        parent_comm[MCA_COLL_ACOLL_LYR_NODE] = subc->local_comm;
        parent_comm[MCA_COLL_ACOLL_LYR_SOCKET] = subc->socket_comm;
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, rank, subc->local_root,
                                              MCA_COLL_ACOLL_L3CACHE);

        int numa_rank;
        numa_rank = ompi_comm_rank(subc->numa_comm);
        color = (numa_rank == 0) ? 0 : 1;
        err = ompi_comm_split(subc->local_comm, color, rank, &subc->numa_comm_ldrs, false);

        /* Find out local rank of root in numa comm */
        err = comm_grp_ranks_local(comm, subc->numa_comm, &subc->is_root_numa, &subc->numa_root,
                                   &numa_ranks, root);

        subc->base_rank[MCA_COLL_ACOLL_NUMA] = subc->is_root_numa == 1 ? root : numa_ranks[0];
        color = rank == subc->base_rank[MCA_COLL_ACOLL_NUMA] ? 0 : 1;
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, rank, subc->local_root,
                                              MCA_COLL_ACOLL_NUMA);

        if (socket_ranks != NULL) {
            free(socket_ranks);
            socket_ranks = NULL;
        }
        if (subgrp_ranks != NULL) {
            free(subgrp_ranks);
            subgrp_ranks = NULL;
        }
        if (numa_ranks != NULL) {
            free(numa_ranks);
            numa_ranks = NULL;
        }
    }

    /* Restore originals for local and socket comms */
    (subc->local_comm)->c_coll->coll_allreduce = coll_allreduce_loc;
    (subc->local_comm)->c_coll->coll_allgather = coll_allgather_loc;
    (subc->local_comm)->c_coll->coll_bcast = coll_bcast_loc;
    (subc->socket_comm)->c_coll->coll_allreduce = coll_allreduce_soc;
    (subc->socket_comm)->c_coll->coll_allgather = coll_allgather_soc;
    (subc->socket_comm)->c_coll->coll_bcast = coll_bcast_soc;

    /* For collectives where order is important (like gather, allgather),
     * split based on ranks. This is optimal for global communicators with
     * equal split among nodes, but suboptimal for other cases.
     */
    if (!subc->initialized) {
        if (subc->num_nodes > 1) {
            int node_size = (size + subc->num_nodes - 1) / subc->num_nodes;
            int color = rank / node_size;
            err = ompi_comm_split(comm, color, rank, &subc->local_r_comm, false);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        subc->derived_node_size = (size + subc->num_nodes - 1) / subc->num_nodes;
    }

    /* Restore originals */
    (comm)->c_coll->coll_allreduce = coll_allreduce_org;
    (comm)->c_coll->coll_allgather = coll_allgather_org;
    (comm)->c_coll->coll_bcast = coll_bcast_org;

    /* Init done */
    subc->initialized = 1;
    if (root != subc->prev_init_root) {
        subc->num_root_change++;
    }
    subc->prev_init_root = root;

    return err;
}

#ifdef HAVE_XPMEM_H
static inline int mca_coll_acoll_xpmem_register(void *xpmem_apid, void *base, size_t size,
                                                mca_rcache_base_registration_t *reg)
{
    struct xpmem_addr xpmem_addr;
    xpmem_addr.apid = *((xpmem_apid_t *) xpmem_apid);
    xpmem_addr.offset = (uintptr_t) base;
    struct acoll_xpmem_rcache_reg_t *xpmem_reg = (struct acoll_xpmem_rcache_reg_t *) reg;
    xpmem_reg->xpmem_vaddr = xpmem_attach(xpmem_addr, size, NULL);

    if ((void *) -1 == xpmem_reg->xpmem_vaddr) {
        return -1;
    }
    return 0;
}

static inline int mca_coll_acoll_xpmem_deregister(void *xpmem_apid,
                                                  mca_rcache_base_registration_t *reg)
{
    int status = xpmem_detach(((struct acoll_xpmem_rcache_reg_t *) reg)->xpmem_vaddr);
    return status;
}
#endif

static inline int coll_acoll_init(mca_coll_base_module_t *module, ompi_communicator_t *comm,
                                  coll_acoll_data_t *data, coll_acoll_subcomms_t *subc)
{
    int size, ret = 0, rank, line;

    int cid = ompi_comm_get_local_cid(comm);
    if (subc->initialized_data) {
        return ret;
    }
    data = (coll_acoll_data_t *) malloc(sizeof(coll_acoll_data_t));
    if (NULL == data) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    data->comm_size = size;

#ifdef HAVE_XPMEM_H
    if (subc->xpmem_use_sr_buf == 0) {
        data->scratch = (char *) malloc(subc->xpmem_buf_size);
        if (NULL == data->scratch) {
            line = __LINE__;
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto error_hndl;
        }
    } else {
        data->scratch = NULL;
    }

    xpmem_segid_t seg_id;
    data->allseg_id = (xpmem_segid_t *) malloc(sizeof(xpmem_segid_t) * size);
    if (NULL == data->allseg_id) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->all_apid = (xpmem_apid_t *) malloc(sizeof(xpmem_apid_t) * size);
    if (NULL == data->all_apid) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->allshm_sbuf = (void **) malloc(sizeof(void *) * size);
    if (NULL == data->allshm_sbuf) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->allshm_rbuf = (void **) malloc(sizeof(void *) * size);
    if (NULL == data->allshm_rbuf) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->xpmem_saddr = (void **) malloc(sizeof(void *) * size);
    if (NULL == data->xpmem_saddr) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->xpmem_raddr = (void **) malloc(sizeof(void *) * size);
    if (NULL == data->xpmem_raddr) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->rcache = (mca_rcache_base_module_t **) malloc(sizeof(mca_rcache_base_module_t *) * size);
    if (NULL == data->rcache) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    seg_id = xpmem_make(0, XPMEM_MAXADDR_SIZE, XPMEM_PERMIT_MODE, (void *) 0666);
    if (seg_id == -1) {
        line = __LINE__;
        ret = -1;
        goto error_hndl;
    }

    ret = comm->c_coll->coll_allgather(&seg_id, sizeof(xpmem_segid_t), MPI_BYTE, data->allseg_id,
                                       sizeof(xpmem_segid_t), MPI_BYTE, comm,
                                       comm->c_coll->coll_allgather_module);

    /* Assuming the length of rcache name is less than 50 characters */
    char rc_name[50];
    for (int i = 0; i < size; i++) {
        if (rank != i) {
            data->all_apid[i] = xpmem_get(data->allseg_id[i], XPMEM_RDWR, XPMEM_PERMIT_MODE,
                                          (void *) 0666);
            if (data->all_apid[i] == -1) {
                line = __LINE__;
                ret = -1;
                goto error_hndl;
            }
            if (data->all_apid[i] == -1) {
                line = __LINE__;
                ret = -1;
                goto error_hndl;
            }
            sprintf(rc_name, "acoll_%d_%d_%d", cid, rank, i);
            mca_rcache_base_resources_t rcache_element
                = {.cache_name = rc_name,
                   .reg_data = &data->all_apid[i],
                   .sizeof_reg = sizeof(struct acoll_xpmem_rcache_reg_t),
                   .register_mem = mca_coll_acoll_xpmem_register,
                   .deregister_mem = mca_coll_acoll_xpmem_deregister};

            data->rcache[i] = mca_rcache_base_module_create("grdma", NULL, &rcache_element);
            if (data->rcache[i] == NULL) {
                ret = -1;
                line = __LINE__;
                goto error_hndl;
            }
        }
    }
#endif

    /* temporary variables */
    int tmp1, tmp2, tmp3 = 0;
    comm_grp_ranks_local(comm, subc->numa_comm, &tmp1, &tmp2, &data->l1_gp, tmp3);
    data->l1_gp_size = ompi_comm_size(subc->numa_comm);
    data->l1_local_rank = ompi_comm_rank(subc->numa_comm);

    comm_grp_ranks_local(comm, subc->numa_comm_ldrs, &tmp1, &tmp2, &data->l2_gp, tmp3);
    data->l2_gp_size = ompi_comm_size(subc->numa_comm_ldrs);
    data->l2_local_rank = ompi_comm_rank(subc->numa_comm_ldrs);
    data->offset[0] = LEADER_SHM_SIZE;
    data->offset[1] = data->offset[0] + size * CACHE_LINE_SIZE;
    data->offset[2] = data->offset[1] + size * CACHE_LINE_SIZE;
    data->offset[3] = data->offset[2] + rank * PER_RANK_SHM_SIZE;
    data->allshmseg_id = (opal_shmem_ds_t *) malloc(sizeof(opal_shmem_ds_t) * size);
    data->allshmmmap_sbuf = (void **) malloc(sizeof(void *) * size);
    data->sync[0] = 0;
    data->sync[1] = 0;
    char *shfn;

    /* Only the leaders need to allocate shared memory */
    /* remaining ranks move their data into their leader's shm */
    if (data->l1_gp[0] == rank) {
        subc->initialized_shm_data = true;
        ret = asprintf(&shfn, "/dev/shm/acoll_coll_shmem_seg.%u.%x.%d:%d-%d", geteuid(),
                       OPAL_PROC_MY_NAME.jobid, ompi_comm_rank(MPI_COMM_WORLD),
                       ompi_comm_get_local_cid(comm), ompi_comm_size(comm));
    }

    if (ret < 0) {
        line = __LINE__;
        goto error_hndl;
    }

    opal_shmem_ds_t seg_ds;
    if (data->l1_gp[0] == rank) {
        /* Assuming cacheline size is 64 */
        long memsize
            = (LEADER_SHM_SIZE /* scratch leader */ + CACHE_LINE_SIZE * size /* sync variables l1 group*/
               + CACHE_LINE_SIZE * size /* sync variables l2 group*/ + PER_RANK_SHM_SIZE * size /*data from ranks*/);
        ret = opal_shmem_segment_create(&seg_ds, shfn, memsize);
        free(shfn);
    }

    if (ret != OPAL_SUCCESS) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_coll_base_framework.framework_output,
                            "coll:acoll: Error: Could not create shared memory segment");
        line = __LINE__;
        goto error_hndl;
    }

    ret = comm->c_coll->coll_allgather(&seg_ds, sizeof(opal_shmem_ds_t), MPI_BYTE,
                                       data->allshmseg_id, sizeof(opal_shmem_ds_t), MPI_BYTE, comm,
                                       comm->c_coll->coll_allgather_module);

    if (data->l1_gp[0] != rank) {
        data->allshmmmap_sbuf[data->l1_gp[0]] = opal_shmem_segment_attach(
            &data->allshmseg_id[data->l1_gp[0]]);
    } else {
        for (int i = 0; i < data->l2_gp_size; i++) {
            data->allshmmmap_sbuf[data->l2_gp[i]] = opal_shmem_segment_attach(
                &data->allshmseg_id[data->l2_gp[i]]);
        }
    }

    int offset = LEADER_SHM_SIZE;
    memset(((char *) data->allshmmmap_sbuf[data->l1_gp[0]]) + offset + CACHE_LINE_SIZE * rank, 0, CACHE_LINE_SIZE);
    if (data->l1_gp[0] == rank) {
        memset(((char *) data->allshmmmap_sbuf[data->l2_gp[0]]) + (offset + CACHE_LINE_SIZE * size) + CACHE_LINE_SIZE * rank,
               0, CACHE_LINE_SIZE);
    }

    subc->initialized_data = true;
    subc->data = data;
    ompi_coll_base_barrier_intra_tree(comm, module);

    return MPI_SUCCESS;
error_hndl:
    (void) line;
    if (NULL != data) {
#ifdef HAVE_XPMEM_H
        free(data->allseg_id);
        data->allseg_id = NULL;
        free(data->all_apid);
        data->all_apid = NULL;
        free(data->allshm_sbuf);
        data->allshm_sbuf = NULL;
        free(data->allshm_rbuf);
        data->allshm_rbuf = NULL;
        free(data->xpmem_saddr);
        data->xpmem_saddr = NULL;
        free(data->xpmem_raddr);
        data->xpmem_raddr = NULL;
        free(data->rcache);
        data->rcache = NULL;
        free(data->scratch);
        data->scratch = NULL;
#endif
        free(data->allshmseg_id);
        data->allshmseg_id = NULL;
        free(data->allshmmmap_sbuf);
        data->allshmmmap_sbuf = NULL;
        free(data->l1_gp);
        data->l1_gp = NULL;
        free(data->l2_gp);
        data->l2_gp = NULL;
        free(data);
        data = NULL;
    }
    return ret;
}

#ifdef HAVE_XPMEM_H
static inline void register_and_cache(int size, size_t total_dsize, int rank,
                                      coll_acoll_data_t *data)
{
    uintptr_t base, bound;
    for (int i = 0; i < size; i++) {
        if (rank != i) {
            mca_rcache_base_module_t *rcache_i = data->rcache[i];
            int access_flags = 0;
            struct acoll_xpmem_rcache_reg_t *sbuf_reg = NULL, *rbuf_reg = NULL;
            base = OPAL_DOWN_ALIGN((uintptr_t) data->allshm_sbuf[i], 4096, uintptr_t);
            bound = OPAL_ALIGN((uintptr_t) data->allshm_sbuf[i] + total_dsize, 4096, uintptr_t);
            int ret = rcache_i->rcache_register(rcache_i, (void *) base, bound - base, access_flags,
                                                MCA_RCACHE_ACCESS_ANY,
                                                (mca_rcache_base_registration_t **) &sbuf_reg);

            if (ret != 0) {
                sbuf_reg = NULL;
                return;
            }
            data->xpmem_saddr[i] = (void *) ((uintptr_t) sbuf_reg->xpmem_vaddr
                                             + ((uintptr_t) data->allshm_sbuf[i]
                                                - (uintptr_t) sbuf_reg->base.base));

            base = OPAL_DOWN_ALIGN((uintptr_t) data->allshm_rbuf[i], 4096, uintptr_t);
            bound = OPAL_ALIGN((uintptr_t) data->allshm_rbuf[i] + total_dsize, 4096, uintptr_t);
            ret = rcache_i->rcache_register(rcache_i, (void *) base, bound - base, access_flags,
                                            MCA_RCACHE_ACCESS_ANY,
                                            (mca_rcache_base_registration_t **) &rbuf_reg);

            if (ret != 0) {
                rbuf_reg = NULL;
                return;
            }
            data->xpmem_raddr[i] = (void *) ((uintptr_t) rbuf_reg->xpmem_vaddr
                                             + ((uintptr_t) data->allshm_rbuf[i]
                                                - (uintptr_t) rbuf_reg->base.base));
        } else {
            data->xpmem_saddr[i] = data->allshm_sbuf[i];
            data->xpmem_raddr[i] = data->allshm_rbuf[i];
        }
    }
}
#endif

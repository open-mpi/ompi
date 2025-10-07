/* -*- Mode: C; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 - 2025 Advanced Micro Devices, Inc. All rights reserved.
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
#include "opal/mca/rcache/base/base.h"



/* shared memory structure */
/* first 16 * 1024 bytes (16KB) are used for the leader */
/* next 2* 64 * comm_size bytes are used for  sync variables */
/* next 8 * 1024 * comm_size are used for per_rank data (8KB per rank) */
/* offsets for the shared memory region */
#define CACHE_LINE_SIZE 64
#define LEADER_SHM_SIZE 16384
#define PER_RANK_SHM_SIZE 8192

extern uint64_t mca_coll_acoll_smsc_buffer_size;
extern int mca_coll_acoll_without_smsc;
extern int mca_coll_acoll_smsc_use_sr_buf;
extern int mca_coll_acoll_barrier_algo;


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
    } else {
        /* Check if subcomms structure is already created for the communicator */
        for (int i = 0; i < num_subc; i++) {
            if (NULL != acoll_module->subc[i]) {
                if (acoll_module->subc[i]->cid == cid) {
                    *subc_ptr = acoll_module->subc[i];
                    return MPI_SUCCESS;
                }
            }
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

    for (int k = 0; k < MCA_COLL_ACOLL_SPLIT_FACTOR_LIST_LEN; ++k) {
        subc->split_comm[k] = NULL;
    }

    subc->numa_comm = NULL;
    subc->numa_comm_ldrs = NULL;
    subc->node_comm = NULL;
    subc->inter_comm = NULL;
    subc->initialized_data = false;
    subc->initialized_shm_data = false;
    subc->data = NULL;
    subc->barrier_algo = mca_coll_acoll_barrier_algo;

    if (acoll_module->has_smsc) {
        subc->smsc_buf_size = mca_coll_acoll_smsc_buffer_size;
        subc->smsc_use_sr_buf = mca_coll_acoll_smsc_use_sr_buf;
        subc->without_smsc = mca_coll_acoll_without_smsc;
    } else {
        subc->smsc_buf_size = 0;
        subc->smsc_use_sr_buf = 0;
        subc->without_smsc = 1;
    }
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

    free(ranks);
    free(local_ranks);

    return err;
}

static inline int mca_coll_acoll_create_base_comm(ompi_communicator_t **parent_comm,
                                                  coll_acoll_subcomms_t *subc, int color, int *rank,
                                                  int *root, int base_lyr)
{
    int i;
    int err;

    for (i = 0; i < MCA_COLL_ACOLL_NUM_LAYERS; i++) {
        int is_root_node = 0;

        /* Create base comm */
        err = ompi_comm_split(parent_comm[i], color, rank[i], &subc->base_comm[base_lyr][i], false);
        if (MPI_SUCCESS != err)
            return err;

        /* Find out local rank of root in base comm */
        err = comm_grp_ranks_local(parent_comm[i], subc->base_comm[base_lyr][i], &is_root_node,
                                   &subc->base_root[base_lyr][i], NULL, root[i]);
    }
    return err;
}

static inline int mca_coll_acoll_is_adj_rank_same_sub_comm
                            (ompi_communicator_t* sub_comm,
                             int sub_comm_size, int* sub_comm_ranks,
                             ompi_group_t* parent_grp,
                             int parent_comm_size, int* parent_comm_ranks,
                             int par_comm_rank, bool* is_adj)
{
    (*is_adj) = false;

    ompi_group_t *sub_comm_grp;
    int error = ompi_comm_group(sub_comm, &sub_comm_grp);
    if (MPI_SUCCESS != error) {
        return error;
    }

    for (int i = 0; i < sub_comm_size; ++i) {
        sub_comm_ranks[i] = i;
    }

    /* Current rank is guaranteed to be in all the accessed subcomms. */
    error = ompi_group_translate_ranks(sub_comm_grp, sub_comm_size, sub_comm_ranks,
                                       parent_grp, parent_comm_ranks);
    if (MPI_SUCCESS != error) {
        return error;
    }

    for (int ii = 0; ii < sub_comm_size; ++ii)
    {
        if (((par_comm_rank + 1) % parent_comm_size) == parent_comm_ranks[ii])
        {
            (*is_adj) = true;
            break;
        }
    }

    return MPI_SUCCESS;
}

static inline int mca_coll_acoll_derive_r2r_latency
                                (ompi_communicator_t *comm,
                                 coll_acoll_subcomms_t *subc,
                                 mca_coll_acoll_module_t *acoll_module)
{
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    subc->r2r_dist = DIST_NODE;

    coll_acoll_reserve_mem_t *rsv_mem = &(acoll_module->reserve_mem_s);
    int* workbuf = (int *) coll_acoll_buf_alloc(rsv_mem,
                                                2 * size * sizeof(int));
    if (NULL == workbuf) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    int* comm_ranks = workbuf + size;
    ompi_group_t *comm_grp;
    int error = ompi_comm_group(comm, &comm_grp);
    if (MPI_SUCCESS != error) { goto error_handler; }

    bool is_same_l3 = false;
    int distance = DIST_CORE; /* map-by core distance. */
    int l3_comm_size = subc->subgrp_size;
    error = mca_coll_acoll_is_adj_rank_same_sub_comm(subc->subgrp_comm,
                                l3_comm_size, workbuf, comm_grp,
                                size, comm_ranks, rank, &is_same_l3);
    if (MPI_SUCCESS != error) { goto error_handler; }

    bool is_same_numa = false;
    if (!is_same_l3) {
        distance = DIST_L3CACHE; /* map-by l3 distance. */
        int numa_comm_size = ompi_comm_size(subc->numa_comm);
        error = mca_coll_acoll_is_adj_rank_same_sub_comm(subc->numa_comm,
                                    numa_comm_size, workbuf, comm_grp,
                                    size, comm_ranks, rank, &is_same_numa);
        if (MPI_SUCCESS != error) { goto error_handler; }
    }

    bool is_same_socket = false;
    if ((!is_same_l3) && (!is_same_numa)) {
        distance = DIST_NUMA; /* map-by numa distance. */
        int socket_comm_size = ompi_comm_size(subc->socket_comm);
        error = mca_coll_acoll_is_adj_rank_same_sub_comm(subc->socket_comm,
                                    socket_comm_size, workbuf, comm_grp,
                                    size, comm_ranks, rank, &is_same_socket);
        if (MPI_SUCCESS != error) { goto error_handler; }
    }

    bool is_same_node = false;
    if ((!is_same_l3) && (!is_same_numa) && (!is_same_socket)) {
        distance = DIST_SOCKET; /* map-by socket distance. */
        int local_comm_size = ompi_comm_size(subc->local_comm);
        error = mca_coll_acoll_is_adj_rank_same_sub_comm(subc->local_comm,
                                    local_comm_size, workbuf, comm_grp,
                                    size, comm_ranks, rank, &is_same_node);
        if (MPI_SUCCESS != error) { goto error_handler; }
    }

    if ((!is_same_l3) && (!is_same_numa) && (!is_same_socket) && (!is_same_node)) {
        distance = DIST_NODE; /* map-by node distance. */
    }

    error = (comm)->c_coll->coll_allgather(&distance, 1, MPI_INT,
                                           workbuf, 1, MPI_INT,
                                           comm, &acoll_module->super);
    if (MPI_SUCCESS != error) { goto error_handler; }

    int dist_count_array[DIST_END] = {0};
    for (int ii = 0; ii < size; ++ii) {
        dist_count_array[workbuf[ii]] += 1;
    }

    int max_idx = DIST_CORE;
    for (int ii = (max_idx + 1); ii < DIST_END; ++ii) {
        if (dist_count_array[ii] > dist_count_array[max_idx]) {
            max_idx = ii;
        }
    }
    subc->r2r_dist = max_idx;

    coll_acoll_buf_free(rsv_mem, workbuf);
    return MPI_SUCCESS;

error_handler:
    coll_acoll_buf_free(rsv_mem, workbuf);
    return error;
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
        subc->is_root_node = 0;

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
    int *subgrp_ranks = NULL, *numa_ranks = NULL, *socket_ranks = NULL;
    ompi_communicator_t *parent_comm[MCA_COLL_ACOLL_NUM_LAYERS];
    int parent_rank[MCA_COLL_ACOLL_NUM_LAYERS];
    if (subc->num_nodes > 1) { /* Multinode case */
        int local_rank = ompi_comm_rank(subc->local_comm);
        int color = MPI_UNDEFINED;
        int is_root_node = 0, is_root_socket = 0;
        int local_root = 0;

        /* Initializations */
        subc->local_root[MCA_COLL_ACOLL_LYR_NODE] = 0;
        subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] = 0;

        /* Find out the local rank of root */
        err = comm_grp_ranks_local(comm, subc->local_comm, &subc->is_root_node,
                                   &subc->local_root[MCA_COLL_ACOLL_LYR_NODE], NULL, root);

        /* Create subcommunicator with leader ranks */
        color = 1;
        if (!subc->is_root_node && (0 == local_rank)) {
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
        subc->socket_rank = 1 == subc->is_root_socket ? local_root : socket_ranks[0];
        color = local_rank == subc->socket_rank ? 0 : 1;
        err = ompi_comm_split(comm, color, rank, &subc->socket_ldr_comm, false);
        if (MPI_SUCCESS != err)
            return err;

        /* Find out local rank of root in socket leader comm */
        err = comm_grp_ranks_local(comm, subc->socket_ldr_comm, &is_root_socket,
                                   &subc->socket_ldr_root, NULL, root);

        /* Find out local rank of root in subgroup comm */
        err = comm_grp_ranks_local(subc->local_comm, subc->subgrp_comm, &subc->is_root_sg,
                                   &subc->subgrp_root, &subgrp_ranks, local_root);

        subc->base_rank[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE] =
                1 == subc->is_root_sg ? local_root : subgrp_ranks[0];
        /* Find out socket rank of root in subgroup comm */
        int tmp_root;
        err = comm_grp_ranks_local(subc->socket_comm, subc->subgrp_comm, &subc->is_root_sg,
                                   &tmp_root, &subgrp_ranks,
                                   subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET]);
        subc->base_rank[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_SOCKET] =
            1 == subc->is_root_sg ? subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] : subgrp_ranks[0];

        /* Create subcommunicator with base ranks */
        color = local_rank == subc->base_rank[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE] ? 0 : 1;
        parent_comm[MCA_COLL_ACOLL_LYR_NODE] = subc->local_comm;
        parent_comm[MCA_COLL_ACOLL_LYR_SOCKET] = subc->socket_comm;
        parent_rank[MCA_COLL_ACOLL_LYR_NODE] = local_rank;
        parent_rank[MCA_COLL_ACOLL_LYR_SOCKET] = ompi_comm_rank(subc->socket_comm);
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, parent_rank,
                                              subc->local_root, MCA_COLL_ACOLL_L3CACHE);

        /* Find out local rank of root in numa comm */
        err = comm_grp_ranks_local(subc->local_comm, subc->numa_comm, &subc->is_root_numa,
                                   &subc->numa_root, &numa_ranks, local_root);

        subc->base_rank[MCA_COLL_ACOLL_NUMA][MCA_COLL_ACOLL_LYR_NODE] =
                1 == subc->is_root_numa ? local_root : numa_ranks[0];
        /* Find out socket rank of root in numa comm */
        err = comm_grp_ranks_local(subc->socket_comm, subc->numa_comm, &subc->is_root_numa,
                                   &tmp_root, &numa_ranks,
                                   subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET]);
        subc->base_rank[MCA_COLL_ACOLL_NUMA][MCA_COLL_ACOLL_LYR_SOCKET] =
            1 == subc->is_root_numa ? subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] : numa_ranks[0];

        color = local_rank == subc->base_rank[MCA_COLL_ACOLL_NUMA][MCA_COLL_ACOLL_LYR_NODE] ? 0 : 1;
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, parent_rank,
                                              subc->local_root, MCA_COLL_ACOLL_NUMA);
    } else {
        /* Intra node case */
        int color;
        int is_root_socket = 0;

        /* Initializations */
        subc->local_root[MCA_COLL_ACOLL_LYR_NODE] = root;
        subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] = 0;

        /* Find out local rank of root in socket comm */
        err = comm_grp_ranks_local(comm, subc->socket_comm, &subc->is_root_socket,
                                   &subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET], &socket_ranks,
                                   root);

        /* Create subcommunicator with socket leaders */
        subc->socket_rank = 1 == subc->is_root_socket ? root : socket_ranks[0];
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

        subc->base_rank[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE] =
                        1 == subc->is_root_sg ? root : subgrp_ranks[0];
        /* Find out socket rank of root in subgroup comm */
        int tmp_root;
        err = comm_grp_ranks_local(subc->socket_comm, subc->subgrp_comm, &subc->is_root_sg,
                                   &tmp_root, &subgrp_ranks,
                                   subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET]);
        subc->base_rank[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_SOCKET] =
            1 == subc->is_root_sg ? subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] : subgrp_ranks[0];

        /* Create subcommunicator with base ranks */
        color = rank == subc->base_rank[MCA_COLL_ACOLL_L3CACHE][MCA_COLL_ACOLL_LYR_NODE] ? 0 : 1;
        parent_comm[MCA_COLL_ACOLL_LYR_NODE] = subc->local_comm;
        parent_comm[MCA_COLL_ACOLL_LYR_SOCKET] = subc->socket_comm;
        parent_rank[MCA_COLL_ACOLL_LYR_NODE] = rank;
        parent_rank[MCA_COLL_ACOLL_LYR_SOCKET] = ompi_comm_rank(subc->socket_comm);
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, parent_rank, subc->local_root,
                                              MCA_COLL_ACOLL_L3CACHE);

        int numa_rank;
        numa_rank = ompi_comm_rank(subc->numa_comm);
        color = (0 == numa_rank) ? 0 : 1;
        err = ompi_comm_split(subc->local_comm, color, rank, &subc->numa_comm_ldrs, false);

        /* Find out local rank of root in numa comm */
        err = comm_grp_ranks_local(comm, subc->numa_comm, &subc->is_root_numa, &subc->numa_root,
                                   &numa_ranks, root);

        subc->base_rank[MCA_COLL_ACOLL_NUMA][MCA_COLL_ACOLL_LYR_NODE] =
                            1 == subc->is_root_numa ? root : numa_ranks[0];
        /* Find out socket rank of root in numa comm */
        err = comm_grp_ranks_local(subc->socket_comm, subc->numa_comm, &subc->is_root_numa,
                                   &tmp_root, &numa_ranks,
                                   subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET]);
        subc->base_rank[MCA_COLL_ACOLL_NUMA][MCA_COLL_ACOLL_LYR_SOCKET] =
            1 == subc->is_root_numa ? subc->local_root[MCA_COLL_ACOLL_LYR_SOCKET] : numa_ranks[0];

        color = rank == subc->base_rank[MCA_COLL_ACOLL_NUMA][MCA_COLL_ACOLL_LYR_NODE] ? 0 : 1;
        err = mca_coll_acoll_create_base_comm(parent_comm, subc, color, parent_rank, subc->local_root,
                                              MCA_COLL_ACOLL_NUMA);
    }

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

        err = mca_coll_acoll_derive_r2r_latency(comm, subc, acoll_module);
        if (MPI_SUCCESS != err) {
            return err;
        }

        const int split_factor_list_len = MCA_COLL_ACOLL_SPLIT_FACTOR_LIST_LEN;
        const int split_factor_list[MCA_COLL_ACOLL_SPLIT_FACTOR_LIST_LEN] =
                    MCA_COLL_ACOLL_SPLIT_FACTOR_LIST;
        for (int ii = 0; ii < split_factor_list_len; ++ii) {
            int split_comm_color = rank % split_factor_list[ii];

            /* If comm size is not a perfect multiple of split factor, then
             * unless comm size % split factor <= 1, the split_comm
             * for split factor 2 is used.*/
            if ((0 != (size % split_factor_list[ii])) &&
                (rank >= (size - (size % split_factor_list[ii])))) {
                split_comm_color = split_factor_list[ii];
            }
            err = ompi_comm_split(comm, split_comm_color, rank,
                            &subc->split_comm[ii], false);
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

static inline int coll_acoll_init(mca_coll_base_module_t *module, ompi_communicator_t *comm,
                                  coll_acoll_data_t *data, coll_acoll_subcomms_t *subc, int root)
{
    int size, ret = 0, rank, line;

    if (subc->initialized_data) {
        return ret;
    }
    data = (coll_acoll_data_t *) malloc(sizeof(coll_acoll_data_t));
    if (NULL == data) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }

    // initialize data
    data->allshm_rbuf = NULL;
    data->allshm_sbuf = NULL;
    data->allshmmmap_sbuf = NULL;
    data->scratch = NULL;
    data->smsc_info.sreg = NULL;
    data->smsc_info.rreg = NULL;
    data->smsc_info.ep = NULL;
    data->smsc_saddr = NULL;
    data->smsc_raddr = NULL;
    data->l1_gp = NULL;
    data->l2_gp = NULL;
    data->allshmseg_id = NULL;


    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    data->comm_size = size;

    data->smsc_info.sreg = (void **) malloc(sizeof(void *) * size);
    data->smsc_info.rreg = (void **) malloc(sizeof(void *) * size);
    if (NULL == data->smsc_info.sreg || NULL == data->smsc_info.rreg) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->smsc_info.ep = (mca_smsc_endpoint_t **) malloc(sizeof(mca_smsc_endpoint_t *) * size);
    if (NULL == data->smsc_info.ep) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    for (int i = 0 ; i < size; i++) {
        data->smsc_info.ep[i] = NULL;
        data->smsc_info.sreg[i] = NULL;
        data->smsc_info.rreg[i] = NULL;
    }

    if (0 == subc->smsc_use_sr_buf) {
        data->scratch = (char *) malloc(subc->smsc_buf_size);
        if (NULL == data->scratch) {
            line = __LINE__;
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto error_hndl;
        }
    } else {
        data->scratch = NULL;
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

    data->smsc_saddr = (void **) malloc(sizeof(void *) * size);
    if (NULL == data->smsc_saddr) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }
    data->smsc_raddr = (void **) malloc(sizeof(void *) * size);
    if (NULL == data->smsc_raddr) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }

    /* temporary variables */
    int tmp1, tmp2, tmp3 = root;
    comm_grp_ranks_local(comm, subc->numa_comm, &tmp1, &tmp2, &data->l1_gp, tmp3);
    if (NULL == data->l1_gp) {
        line = __LINE__;
        ret = OMPI_ERROR;
        goto error_hndl;
    }
    data->l1_gp_size = ompi_comm_size(subc->numa_comm);
    data->l1_local_rank = ompi_comm_rank(subc->numa_comm);

    comm_grp_ranks_local(comm, subc->numa_comm_ldrs, &tmp1, &tmp2, &data->l2_gp, tmp3);
    if (NULL == data->l2_gp) {
        line = __LINE__;
        ret = OMPI_ERROR;
        goto error_hndl;
    }

    data->l2_gp_size = ompi_comm_size(subc->numa_comm_ldrs);
    data->l2_local_rank = ompi_comm_rank(subc->numa_comm_ldrs);
    data->offset[0] = LEADER_SHM_SIZE;
    data->offset[1] = data->offset[0] + size * CACHE_LINE_SIZE;
    data->offset[2] = data->offset[1] + size * CACHE_LINE_SIZE;
    data->offset[3] = data->offset[2] + rank * PER_RANK_SHM_SIZE;
    data->allshmseg_id = (opal_shmem_ds_t *) malloc(sizeof(opal_shmem_ds_t) * size);
    data->allshmmmap_sbuf = (void **) malloc(sizeof(void *) * size);

    if (NULL == data->allshmseg_id || NULL == data->allshmmmap_sbuf) {
        line = __LINE__;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error_hndl;
    }

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
               + CACHE_LINE_SIZE * size /* sync variables l2 group*/ + PER_RANK_SHM_SIZE * size /*data from ranks*/ + 2 * CACHE_LINE_SIZE * size /* sync variables for bcast and barrier*/);
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

    data->allshmmmap_sbuf[root] = opal_shmem_segment_attach(&data->allshmseg_id[0]);

    int offset = LEADER_SHM_SIZE;
    memset(((char *) data->allshmmmap_sbuf[data->l1_gp[0]]) + offset + CACHE_LINE_SIZE * rank, 0,
           CACHE_LINE_SIZE);
    int offset_bcast = LEADER_SHM_SIZE + 2 * CACHE_LINE_SIZE * size + PER_RANK_SHM_SIZE * size;
    int offset_barrier = offset_bcast + CACHE_LINE_SIZE * size;
    memset(((char *) data->allshmmmap_sbuf[data->l1_gp[0]])
               + offset_bcast /*16K + 16k + 16k + 2M */ + CACHE_LINE_SIZE * rank,
           0, CACHE_LINE_SIZE);
    memset(((char *) data->allshmmmap_sbuf[data->l1_gp[0]])
               + offset_barrier /*16K + 16k + 16k + 2M + 16k*/ + CACHE_LINE_SIZE * rank,
           0, CACHE_LINE_SIZE);
    memset(((char *) data->allshmmmap_sbuf[root])
               + offset_barrier /*16K + 16k + 16k + 2M + 16k*/ + CACHE_LINE_SIZE * rank,
           0, CACHE_LINE_SIZE);
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
        free(data->allshm_sbuf);
        data->allshm_sbuf = NULL;
        free(data->allshm_rbuf);
        data->allshm_rbuf = NULL;
        free(data->smsc_saddr);
        data->smsc_saddr = NULL;
        free(data->smsc_raddr);
        data->smsc_raddr = NULL;
        free(data->scratch);
        data->scratch = NULL;
        free(data->smsc_info.ep);
        data->smsc_info.ep = NULL;
        free(data->smsc_info.sreg);
        data->smsc_info.sreg = NULL;
        free(data->smsc_info.rreg);
        data->smsc_info.rreg = NULL;
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

static inline int register_mem_with_smsc(int rank, int size, size_t total_dsize, coll_acoll_data_t *data, struct ompi_communicator_t *comm) {
    ompi_proc_t *proc = NULL;
    mca_smsc_endpoint_t *smsc_ep = NULL;
    if (NULL == data->smsc_info.ep) {
        return MPI_ERR_OTHER;
    }

    for (int i = 0; i < size; i++) {
        if (rank != i) {
            if (NULL == data->smsc_info.ep[i]) {
                proc = ompi_comm_peer_lookup(comm, i);
                data->smsc_info.ep[i] = MCA_SMSC_CALL(get_endpoint, &proc->super);
            }
            if (NULL == data->smsc_info.ep[i]) {
                 opal_output_verbose(MCA_BASE_VERBOSE_ERROR, ompi_coll_base_framework.framework_output,
                     "coll:acoll: SMSC endpoint not available for processes.\n");
                return MPI_ERR_OTHER;
            }
            smsc_ep = data->smsc_info.ep[i];
            data->smsc_info.sreg[i] = MCA_SMSC_CALL(map_peer_region, smsc_ep,
                MCA_RCACHE_FLAGS_PERSIST, data->allshm_sbuf[i], total_dsize, &data->smsc_saddr[i]);
            data->smsc_info.rreg[i] = MCA_SMSC_CALL(map_peer_region, smsc_ep,
                MCA_RCACHE_FLAGS_PERSIST, data->allshm_rbuf[i], total_dsize, &data->smsc_raddr[i]);

        } else {
            data->smsc_saddr[i] = data->allshm_sbuf[i];
            data->smsc_raddr[i] = data->allshm_rbuf[i];
        }
    }
    return MPI_SUCCESS;
}

static inline void unmap_mem_with_smsc(int rank, int size, coll_acoll_data_t *data)
{
    for (int i = 0; i < size; i++) {
        if (rank != i && NULL != data->smsc_info.sreg[i]  && NULL != data->smsc_info.rreg[i]) {
            MCA_SMSC_CALL(unmap_peer_region, data->smsc_info.sreg[i]);
            MCA_SMSC_CALL(unmap_peer_region, data->smsc_info.rreg[i]);
        }
    }
}

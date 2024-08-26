/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_ACOLL_EXPORT_H
#define MCA_COLL_ACOLL_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/mca.h"
#include "ompi/request/request.h"

#ifdef HAVE_XPMEM_H
#include "opal/mca/rcache/base/base.h"
#include <xpmem.h>
#endif

#include "opal/mca/shmem/base/base.h"
#include "opal/mca/shmem/shmem.h"

BEGIN_C_DECLS

/* Globally exported variables */
OMPI_DECLSPEC extern const mca_coll_base_component_3_0_0_t mca_coll_acoll_component;
extern int mca_coll_acoll_priority;
extern int mca_coll_acoll_max_comms;
extern int mca_coll_acoll_sg_size;
extern int mca_coll_acoll_sg_scale;
extern int mca_coll_acoll_node_size;
extern int mca_coll_acoll_use_dynamic_rules;
extern int mca_coll_acoll_mnode_enable;
extern int mca_coll_acoll_bcast_lin0;
extern int mca_coll_acoll_bcast_lin1;
extern int mca_coll_acoll_bcast_lin2;
extern int mca_coll_acoll_bcast_nonsg;
extern int mca_coll_acoll_allgather_lin;
extern int mca_coll_acoll_allgather_ring_1;

/* API functions */
int mca_coll_acoll_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t *mca_coll_acoll_comm_query(struct ompi_communicator_t *comm, int *priority);

int mca_coll_acoll_module_enable(mca_coll_base_module_t *module, struct ompi_communicator_t *comm);

int mca_coll_acoll_allgather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                             void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm, mca_coll_base_module_t *module);

int mca_coll_acoll_bcast(void *buff, size_t count, struct ompi_datatype_t *datatype, int root,
                         struct ompi_communicator_t *comm, mca_coll_base_module_t *module);

int mca_coll_acoll_gather_intra(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype, int root,
                                struct ompi_communicator_t *comm, mca_coll_base_module_t *module);

int mca_coll_acoll_reduce_intra(const void *sbuf, void *rbuf, size_t count,
                                struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                struct ompi_communicator_t *comm, mca_coll_base_module_t *module);

int mca_coll_acoll_allreduce_intra(const void *sbuf, void *rbuf, size_t count,
                                   struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);

int mca_coll_acoll_barrier_intra(struct ompi_communicator_t *comm, mca_coll_base_module_t *module);

END_C_DECLS

#define MCA_COLL_ACOLL_ROOT_CHANGE_THRESH 10

typedef enum MCA_COLL_ACOLL_SG_SIZES {
    MCA_COLL_ACOLL_SG_SIZE_1 = 8,
    MCA_COLL_ACOLL_SG_SIZE_2 = 16
} MCA_COLL_ACOLL_SG_SIZES;

typedef enum MCA_COLL_ACOLL_SG_SCALES {
    MCA_COLL_ACOLL_SG_SCALE_1 = 1,
    MCA_COLL_ACOLL_SG_SCALE_2 = 2,
    MCA_COLL_ACOLL_SG_SCALE_3 = 4,
    MCA_COLL_ACOLL_SG_SCALE_4 = 8,
    MCA_COLL_ACOLL_SG_SCALE_5 = 16
} MCA_COLL_ACOLL_SG_SCALES;

typedef enum MCA_COLL_ACOLL_SUBCOMMS {
    MCA_COLL_ACOLL_NODE_L = 0,
    MCA_COLL_ACOLL_INTRA,
    MCA_COLL_ACOLL_SOCK_L,
    MCA_COLL_ACOLL_NUMA_L,
    MCA_COLL_ACOLL_L3_L,
    MCA_COLL_ACOLL_LEAF,
    MCA_COLL_ACOLL_NUM_SC
} MCA_COLL_ACOLL_SUBCOMMS;

typedef enum MCA_COLL_ACOLL_LAYERS {
    MCA_COLL_ACOLL_LYR_NODE = 0,
    MCA_COLL_ACOLL_LYR_SOCKET,
    MCA_COLL_ACOLL_NUM_LAYERS
} MCA_COLL_ACOLL_LAYERS;

typedef enum MCA_COLL_ACOLL_BASE_LYRS {
    MCA_COLL_ACOLL_L3CACHE = 0,
    MCA_COLL_ACOLL_NUMA,
    MCA_COLL_ACOLL_NUM_BASE_LYRS
} MCA_COLL_ACOLL_BASE_LYRS;

typedef struct coll_acoll_data {
#ifdef HAVE_XPMEM_H
    xpmem_segid_t *allseg_id;
    xpmem_apid_t *all_apid;
    void **allshm_sbuf;
    void **allshm_rbuf;
    void **xpmem_saddr;
    void **xpmem_raddr;
    mca_rcache_base_module_t **rcache;
    void *scratch;
#endif
    opal_shmem_ds_t *allshmseg_id;
    void **allshmmmap_sbuf;

    int comm_size;
    int l1_local_rank;
    int l2_local_rank;
    int l1_gp_size;
    int *l1_gp;
    int *l2_gp;
    int l2_gp_size;
    int offset[4];
    int sync[2];
} coll_acoll_data_t;

typedef struct coll_acoll_subcomms {
    ompi_communicator_t *local_comm;
    ompi_communicator_t *local_r_comm;
    ompi_communicator_t *leader_comm;
    ompi_communicator_t *subgrp_comm;
    ompi_communicator_t *numa_comm;
    ompi_communicator_t *base_comm[MCA_COLL_ACOLL_NUM_BASE_LYRS][MCA_COLL_ACOLL_NUM_LAYERS];
    ompi_communicator_t *orig_comm;
    ompi_communicator_t *socket_comm;
    ompi_communicator_t *socket_ldr_comm;
    int num_nodes;
    int derived_node_size;
    int is_root_node;
    int is_root_sg;
    int is_root_numa;
    int is_root_socket;
    int local_root[MCA_COLL_ACOLL_NUM_LAYERS];
    int outer_grp_root;
    int subgrp_root;
    int numa_root;
    int socket_ldr_root;
    int base_root[MCA_COLL_ACOLL_NUM_BASE_LYRS][MCA_COLL_ACOLL_NUM_LAYERS];
    int base_rank[MCA_COLL_ACOLL_NUM_BASE_LYRS];
    int socket_rank;
    int subgrp_size;
    int initialized;
    int prev_init_root;
    int num_root_change;

    ompi_communicator_t *numa_comm_ldrs;
    ompi_communicator_t *node_comm;
    ompi_communicator_t *inter_comm;
    int cid;
    coll_acoll_data_t *data;
    bool initialized_data;
    bool initialized_shm_data;
#ifdef HAVE_XPMEM_H
    uint64_t xpmem_buf_size;
    int without_xpmem;
    int xpmem_use_sr_buf;
#endif

} coll_acoll_subcomms_t;

typedef struct coll_acoll_reserve_mem {
    void *reserve_mem;
    uint64_t reserve_mem_size;
    bool reserve_mem_allocate;
    bool reserve_mem_in_use;
} coll_acoll_reserve_mem_t;

struct mca_coll_acoll_module_t {
    mca_coll_base_module_t super;
    MCA_COLL_ACOLL_SG_SIZES sg_size;
    MCA_COLL_ACOLL_SG_SCALES sg_scale;
    int sg_cnt;
    // Todo: Remove log2 variables
    int log2_sg_cnt;
    int node_cnt;
    int log2_node_cnt;
    int use_dyn_rules;
    // Todo: Use substructure for every API related ones
    int use_mnode;
    int use_lin0;
    int use_lin1;
    int use_lin2;
    int mnode_sg_size;
    int mnode_log2_sg_size;
    int allg_lin;
    int allg_ring;
    int max_comms;
    coll_acoll_subcomms_t **subc;
    coll_acoll_reserve_mem_t reserve_mem_s;
    int num_subc;
};

#ifdef HAVE_XPMEM_H
struct acoll_xpmem_rcache_reg_t {
    mca_rcache_base_registration_t base;
    void *xpmem_vaddr;
};
#endif

typedef struct mca_coll_acoll_module_t mca_coll_acoll_module_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_acoll_module_t);

#endif /* MCA_COLL_ACOLL_EXPORT_H */

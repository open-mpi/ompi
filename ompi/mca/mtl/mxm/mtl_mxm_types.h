/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_MXM_TYPES_H_HAS_BEEN_INCLUDED
#define MTL_MXM_TYPES_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "mtl_mxm.h"

#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/communicator/communicator.h"
#include "mtl_mxm_endpoint.h" 


BEGIN_C_DECLS

/** 
 * MTL Module Interface
 */
typedef struct mca_mtl_mxm_module_t {
    mca_mtl_base_module_t super; /**< base MTL interface */
    int		          verbose;
    mxm_h                 mxm_context;
    mxm_ep_h              ep;
} mca_mtl_mxm_module_t;


typedef struct ompi_mtl_mxm_ep_conn_info_t {
    struct sockaddr_storage  ptl_addr[MXM_PTL_LAST];
} ompi_mtl_mxm_ep_conn_info_t;

extern mca_mtl_mxm_module_t ompi_mtl_mxm;

typedef struct mca_mtl_mxm_component_t {
    mca_mtl_base_component_2_0_0_t super; /**< base MTL component */
} mca_mtl_mxm_component_t;


OMPI_DECLSPEC mca_mtl_mxm_component_t mca_mtl_mxm_component;


static inline mxm_conn_h ompi_mtl_mxm_conn_lookup(struct ompi_communicator_t* comm, int rank) {
    ompi_proc_t* ompi_proc = ompi_comm_peer_lookup(comm, rank);
    mca_mtl_mxm_endpoint_t *endpoint = (mca_mtl_mxm_endpoint_t*) ompi_proc->proc_pml;

    return endpoint->mxm_conn;
}

static inline mxm_mq_h ompi_mtl_mxm_mq_lookup(struct ompi_communicator_t* comm) {
    return (mxm_mq_h)comm->c_pml_comm;
}

static inline int ompi_mtl_mxm_to_mpi_status(mxm_error_t status) {
    if (MXM_OK == status) {
        return OMPI_SUCCESS;
    } else if (MXM_ERR_MESSAGE_TRUNCATED == status) {
        return MPI_ERR_TRUNCATE;
    } else {
        return MPI_ERR_INTERN;
    }
}

END_C_DECLS

#endif


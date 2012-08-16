/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_DEVICE_H
#define MCA_BCOL_IBOFFLOAD_DEVICE_H

#include "ompi_config.h"

#include <infiniband/mqe.h>
#include <infiniband/mverbs.h>

#include <infiniband/verbs.h>

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"

#define BCOL_IBOFFLOAD_DUMMY_MEM_SIZE 1

BEGIN_C_DECLS

/* Device OBJ */
struct mca_bcol_iboffload_device_t {
    opal_list_item_t super;

    bool activated;

    struct ompi_common_ofacm_base_dev_desc_t dev;
    struct ibv_pd *ib_pd;
    struct ibv_device_attr ib_dev_attr;

    int num_act_ports;

    struct mca_bcol_iboffload_port_t *ports;
    struct ibv_cq *ib_cq;

    /* CQ for MQs of all iboffload modules on this device */
    struct ibv_cq *ib_mq_cq;

    /* The free list of registered buffers
     * since the registration depends on PD, it is
     * most resonable place to keep the frags */
    ompi_free_list_t *frags_free;
    mca_mpool_base_module_t *mpool;

    /* netowrk context */
    bcol_base_network_context_t *net_context;

    /* We keep dummy frags for all QPs on each device,
       possibly some of QPs don't need it but anyway we distribute dummy
       for them. All dummies point to a same byte of memory. */
    mca_bcol_iboffload_frag_t dummy_frags[MCA_BCOL_IBOFFLOAD_QP_LAST];

    /* Registred memory for the dummy frags */
    char dummy_mem[BCOL_IBOFFLOAD_DUMMY_MEM_SIZE];

    /* Registration info of the dummy memory */
    mca_bcol_iboffload_reg_t dummy_reg;
};

typedef struct mca_bcol_iboffload_device_t mca_bcol_iboffload_device_t;
OBJ_CLASS_DECLARATION(mca_bcol_iboffload_device_t);

END_C_DECLS

#endif /* MCA_BCOL_IBOFFLOAD_DEVICE_H */


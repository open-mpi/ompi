/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *               2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#if !defined(MCA_OOB_UD_QP_H)
#define MCA_OOB_UD_QP_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "orte/types.h"

#include "opal/mca/base/base.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/threads/threads.h"
#include "opal/mca/timer/base/base.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"

#include <infiniband/verbs.h>


enum mca_oob_ud_qp_type_t {
    MCA_OOB_UD_QP_DATA,
    MCA_OOB_UD_QP_LISTEN
};

struct mca_oob_ud_port_t;

struct mca_oob_ud_qp_t {
    opal_free_list_item_t super;
    enum mca_oob_ud_qp_type_t type;

    struct ibv_qp *ib_qp;
    struct mca_oob_ud_port_t *port;

    struct ibv_cq *ib_send_cq, *ib_recv_cq;
};
typedef struct mca_oob_ud_qp_t mca_oob_ud_qp_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_qp_t);

int mca_oob_ud_qp_init (mca_oob_ud_qp_t *qp, struct mca_oob_ud_port_t *port,
                        struct ibv_comp_channel *recv_channel,
                        struct ibv_comp_channel *send_channel, bool onecq);

int mca_oob_ud_qp_to_reset (mca_oob_ud_qp_t *qp);
int mca_oob_ud_qp_to_rts (mca_oob_ud_qp_t *qp);
int mca_oob_ud_qp_purge (mca_oob_ud_qp_t *qp);

int mca_oob_ud_qp_post_send (mca_oob_ud_qp_t *qp, struct ibv_send_wr *wr, int num_completions);
int mca_oob_ud_qp_post_recv (mca_oob_ud_qp_t *qp, struct ibv_recv_wr *wr);

int mca_oob_ud_qp_data_aquire (struct mca_oob_ud_port_t *port, mca_oob_ud_qp_t **qp_ptr);
int mca_oob_ud_qp_data_release (mca_oob_ud_qp_t *qp);

#endif

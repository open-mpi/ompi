/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#if !defined(MCA_OOB_UD_H)
#define MCA_OOB_UD_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <math.h>
#include <infiniband/verbs.h>

#include "opal/types.h"
#include "orte/types.h"

#include "opal/mca/base/base.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/threads/threads.h"
#include "opal/mca/timer/base/base.h"
#include "opal/include/opal_stdint.h"
#include "opal/mca/memchecker/base/base.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/util/name_fns.h"

#include "orte/runtime/orte_globals.h"

#include "oob_ud_qp.h"
#include "oob_ud_peer.h"
#include "oob_ud_req.h"

/* Use for valgrind checks*/
#ifdef HAVE_VALGRIND
#include <valgrind/memcheck.h>
#else
#define VALGRIND_MAKE_MEM_DEFINED(addr,len)
#endif

BEGIN_C_DECLS

enum {
    MCA_OOB_UD_SEND_WR = 0x10000000,
    MCA_OOB_UD_RECV_WR = 0x20000000
};

enum {
    MCA_OOB_UD_DEBUG_NONE,
    MCA_OOB_UD_DEBUG_ALL
};

static inline void mca_oob_ud_fill_send_wr (struct ibv_send_wr *wr, struct ibv_sge *sge,
                                            int num_sge, const mca_oob_ud_peer_t *peer)
{
    wr->wr_id      = MCA_OOB_UD_SEND_WR;
    wr->next       = NULL;
    wr->sg_list    = sge;
    wr->num_sge    = num_sge;
    wr->opcode     = IBV_WR_SEND;
    wr->send_flags = IBV_SEND_SIGNALED;

    wr->wr.ud.ah          = peer->peer_ah;
    wr->wr.ud.remote_qpn  = peer->peer_qpn;
    wr->wr.ud.remote_qkey = peer->peer_qkey;
}

static inline void mca_oob_ud_fill_recv_wr (struct ibv_recv_wr *wr, struct ibv_sge *sge,
                                            int num_sge)
{
    wr->wr_id   = MCA_OOB_UD_RECV_WR;
    wr->next    = NULL;
    wr->sg_list = sge;
    wr->num_sge = num_sge;
}

static inline void mca_oob_ud_fill_sge (struct ibv_sge *sge, void *addr,
                                        uint32_t length, uint32_t lkey)
{
    sge->addr   = (uint64_t)addr;
    sge->length = length;
    sge->lkey   = lkey;
}


struct mca_oob_ud_device_t {
    opal_list_item_t super;

    struct ibv_context      *ib_context;
    struct ibv_comp_channel *ib_channel;
    struct ibv_pd           *ib_pd;

    opal_event_t event;

    opal_list_t ports;
};

typedef struct mca_oob_ud_device_t mca_oob_ud_device_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_device_t);

/* events */
void mca_oob_ud_event_start_monitor (mca_oob_ud_device_t *device);
void mca_oob_ud_event_stop_monitor (mca_oob_ud_device_t *device);

struct mca_oob_ud_reg_mem_t {
    char          *ptr;
    size_t         len;
    struct ibv_mr *mr;
};
typedef struct mca_oob_ud_reg_mem_t mca_oob_ud_reg_mem_t;

struct mca_oob_ud_port_t {
    opal_list_item_t    super;
    mca_oob_ud_device_t *device;
    mca_oob_ud_qp_t      listen_qp;
    opal_free_list_t     data_qps;
    opal_free_list_t     free_msgs;
    int                  mtu;
    uint16_t             lid;
    uint8_t              port_num;

    mca_oob_ud_reg_mem_t grh_buf;
    mca_oob_ud_reg_mem_t msg_buf;
};

typedef struct mca_oob_ud_port_t mca_oob_ud_port_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_port_t);


int mca_oob_ud_port_post_one_recv (mca_oob_ud_port_t *port, int msg_num);

void mca_oob_ud_port_get_uri (mca_oob_ud_port_t *port, char *uri);

struct mca_oob_ud_component_t {
    mca_oob_base_component_2_0_0_t super;    /**< base OOB component */

    opal_list_t       ud_devices;

    opal_list_t       ud_pending_recvs;
    opal_list_t       ud_active_recvs;
    opal_list_t       ud_active_sends;
    opal_list_t       ud_unexpected_recvs;
    opal_list_t       ud_event_queued_reqs;
    opal_list_t       ud_event_processing_msgs;
    opal_list_t       ud_completed;

    opal_event_t      ud_complete_event;

    opal_mutex_t      ud_lock;

    int               ud_min_qp;
    int               ud_max_qp;

    int               ud_recv_buffer_count;
    int               ud_send_buffer_count;

    opal_mutex_t      ud_match_lock;

    opal_hash_table_t ud_peers;
};

typedef struct mca_oob_ud_component_t mca_oob_ud_component_t;

ORTE_MODULE_DECLSPEC extern mca_oob_ud_component_t mca_oob_ud_component;
ORTE_MODULE_DECLSPEC extern mca_oob_t mca_oob_ud_module;


char *mca_oob_ud_get_addr (void);
int mca_oob_ud_set_addr (const orte_process_name_t *name, const char *uri);

int mca_oob_ud_ping(const orte_process_name_t* name, const char* uri,
                    const struct timeval *timeout);

int mca_oob_ud_send_nb(orte_process_name_t* target, orte_process_name_t* origin, 
                       struct iovec* iov, int count, int tag, int flags, 
                       orte_rml_callback_fn_t cbfunc, void* cbdata);
int mca_oob_ud_send_try (mca_oob_ud_req_t *send_req);
int mca_oob_ud_send_complete (mca_oob_ud_req_t *send_req, int rc);

/* recv */
int mca_oob_ud_recv_nb(orte_process_name_t* peer, struct iovec* iov, int count,
                       int tag, int flags, orte_rml_callback_fn_t cbfunc,
                       void* cbdata);
int mca_oob_ud_recv_cancel(orte_process_name_t* name, int tag);

int mca_oob_ud_recv_complete (mca_oob_ud_req_t *recv_req);
int mca_oob_ud_recv_try (mca_oob_ud_req_t *recv_req);
int mca_oob_ud_recv_match_send (mca_oob_ud_port_t *port, mca_oob_ud_peer_t *peer,
                                mca_oob_ud_msg_hdr_t *msg_hdr, mca_oob_ud_req_t **reqp);
int mca_oob_ud_recv_match (mca_oob_ud_req_t *recv_req);
int mca_oob_ud_get_recv_req (const orte_process_name_t name, const int tag, mca_oob_ud_req_t **reqp);

int mca_oob_ud_ft_event(int state);

int mca_oob_ud_register_iov (struct iovec *iov, int count, struct ibv_mr **ib_mr,
                             struct ibv_pd *ib_pd, unsigned int mtu, int *sge_countp,
                             int *wr_countp, int *data_lenp);

void mca_oob_ud_event_queue_completed (mca_oob_ud_req_t *req);

int mca_oob_ud_module_init(void);
END_C_DECLS

#endif

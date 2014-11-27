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

#if !defined(MCA_OOB_UD_PEER_H)
#define MCA_OOB_UD_PEER_H

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

struct mca_oob_ud_msg_hdr_t;
struct mca_oob_ud_port_t;

struct mca_oob_ud_peer_t {
    opal_object_t    super;

    void            *peer_context;
    struct ibv_ah   *peer_ah;
    uint32_t         peer_qpn;
    uint32_t         peer_qkey;
    uint64_t         peer_next_id;
    uint64_t         peer_expected_id;
    orte_process_name_t peer_name;
    uint16_t         peer_lid;
    uint8_t          peer_port;
    bool             peer_available;
    bool             needs_notification;

    opal_list_t      peer_flying_messages;
    opal_mutex_t     peer_lock;

    struct {
        int            tries;
        opal_event_t   event;
        struct timeval value;
        bool           active;
    } peer_timer;
};
typedef struct mca_oob_ud_peer_t mca_oob_ud_peer_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_peer_t);


int mca_oob_ud_peer_lookup (const orte_process_name_t *name, mca_oob_ud_peer_t **peer);

int mca_oob_ud_peer_update_with_uri (mca_oob_ud_peer_t *peer, const char *uri);

mca_oob_ud_peer_t *mca_oob_ud_peer_from_uri (const char *uri);

mca_oob_ud_peer_t *mca_oob_ud_get_peer (struct mca_oob_ud_port_t *port,
                                        orte_process_name_t *name,
                                        uint32_t qpn, uint32_t qkey,
                                        uint16_t lid, uint8_t port_num);

void mca_oob_ud_peer_lost (mca_oob_ud_peer_t *peer);
void mca_oob_ud_peer_release (mca_oob_ud_peer_t *peer);

struct mca_oob_ud_msg_t;

int mca_oob_ud_peer_post_msg (mca_oob_ud_peer_t *peer, struct mca_oob_ud_msg_t *msg);

void mca_oob_ud_peer_start_timer (mca_oob_ud_peer_t *peer);
void mca_oob_ud_peer_stop_timer (mca_oob_ud_peer_t *peer);
void mca_oob_ud_peer_reset_timer (mca_oob_ud_peer_t *peer);

void mca_oob_ud_peer_post_all (mca_oob_ud_peer_t *peer);
void mca_oob_ud_peer_handle_end (mca_oob_ud_peer_t *peer);

#endif


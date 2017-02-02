/*
 * Copyright (c) 2013-2017 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Statistics for the usnic BTL component.
 */

#ifndef OPAL_BTL_USNIC_STATS_H
#define OPAL_BTL_USNIC_STATS_H

#include <sys/time.h>

#include "opal/mca/event/event.h"


/**
 * Struct containing all the statistics that are trackedx
 */
typedef struct opal_btl_usnic_module_stats_t {
    bool final_stats;
    uint64_t report_num;

    uint64_t num_total_sends;
    uint64_t num_resends;
    uint64_t num_timeout_retrans;
    uint64_t num_fast_retrans;
    uint64_t num_chunk_sends;
    uint64_t num_frag_sends;
    uint64_t num_ack_sends;

    uint64_t num_total_recvs;
    uint64_t num_unk_recvs;
    uint64_t num_dup_recvs;
    uint64_t num_oow_low_recvs;
    uint64_t num_oow_high_recvs;
    uint64_t num_frag_recvs;
    uint64_t num_chunk_recvs;
    uint64_t num_badfrag_recvs;
    uint64_t num_ack_recvs;
    uint64_t num_old_dup_acks;
    uint64_t num_dup_acks;
    uint64_t num_recv_reposts;
    uint64_t num_crc_errors;

    uint64_t max_sent_window_size;
    uint64_t max_rcvd_window_size;

    uint64_t pml_module_sends;
    uint64_t pml_send_callbacks;

    uint64_t num_seg_total_completions;
    uint64_t num_seg_ack_completions;
    uint64_t num_seg_frag_completions;
    uint64_t num_seg_chunk_completions;
    uint64_t num_seg_recv_completions;

    opal_event_t timer_event;
    struct timeval timeout;
} opal_btl_usnic_module_stats_t;


/**
 * Initialize the stats on a module.  Must use "struct
 * opal_btl_usnic_module_t*" here to avoid an #include cycle.
 */
int opal_btl_usnic_stats_init(struct opal_btl_usnic_module_t *module);

/**
 * Finalize the stats on a module.  Must use "struct
 * opal_btl_usnic_module_t*" here to avoid an #include cycle.
 */
int opal_btl_usnic_stats_finalize(struct opal_btl_usnic_module_t *module);

/**
 * Initialize the MPI_T performance variables (for all modules)
 */
int opal_btl_usnic_setup_mpit_pvars(void);

#endif

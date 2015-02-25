/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2011-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OPAL_BTL_USNIC_H
#define OPAL_BTL_USNIC_H

#include "opal_config.h"
#include <sys/types.h>

#include "opal_stdint.h"
#include "opal/util/alfg.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/event/event.h"

#if BTL_IN_OPAL
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/btl_base_error.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/mpool/grdma/mpool_grdma.h"
#else
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/grdma/mpool_grdma.h"
#endif

#include "btl_usnic_compat.h"

BEGIN_C_DECLS

/*
 * We're simulating a clock as best we can without resorting to the
 * system.  The clock is used to defer ACKs, and ticks will be incremented
 * when progression gets called.  It could be incremented by different amounts
 * at other times as needed or as tuning dictates.
 */
extern uint64_t opal_btl_usnic_ticks;
static inline uint64_t
get_nsec(void)
{
    return opal_btl_usnic_ticks;
}

/* RNG buffer declaration */
extern opal_rng_buff_t opal_btl_usnic_rand_buff;

#ifndef container_of
#define container_of(ptr, type, member) ( \
        (type *)( ((char *)(ptr)) - offsetof(type,member) ))
#endif

#ifndef max
#define max(a, b) (((a) > (b)) ? (a) : (b))
#endif

/* MSGDEBUG2 prints 1 line at each BTL entry point */
#define MSGDEBUG2 (MSGDEBUG1||0)
/* MSGDEBUG1 prints more info about arguments and internal functions */
#define MSGDEBUG1 0

/* output macros to declutter source */
#if MSGDEBUG1
#define MSGDEBUG1_OUT(...) opal_output(0, __VA_ARGS__)
#else
#define MSGDEBUG1_OUT(...) do {} while (0)
#endif
#if MSGDEBUG2
#define MSGDEBUG2_OUT(...) opal_output(0, __VA_ARGS__)
#else
#define MSGDEBUG2_OUT(...) do {} while (0)
#endif

/* Set to >0 to randomly drop received frags.  The higher the number,
   the more frequent the drops. */
#define WANT_RECV_FRAG_DROPS 0
/* Set to >0 to randomly fail to send an ACK, mimicing a lost ACK.
   The higher the number, the more frequent the failed-to-send-ACK. */
#define WANT_FAIL_TO_SEND_ACK 0
/* Set to >0 to randomly fail to resend a frag (causing it to be
   requed to be sent later).  The higher the number, the more frequent
   the failed-to-resend-frag. */
#define WANT_FAIL_TO_RESEND_FRAG 0

#if WANT_RECV_FRAG_DROPS > 0
#define FAKE_RECV_FRAG_DROP (opal_rand(&opal_btl_usnic_rand_buff) < WANT_RECV_FRAG_DROPS)
#else
#define FAKE_RECV_FRAG_DROP 0
#endif

#if WANT_FAIL_TO_SEND_ACK > 0
#define FAKE_FAIL_TO_SEND_ACK (opal_rand(&opal_btl_usnic_rand_buff) < WANT_FAIL_TO_SEND_ACK)
#else
#define FAKE_FAIL_TO_SEND_ACK 0
#endif

#if WANT_FAIL_TO_RESEND_FRAG > 0
#define FAKE_FAIL_TO_RESEND_FRAG (opal_rand(&opal_btl_usnic_rand_buff) < WANT_FAIL_TO_RESEND_FRAG)
#else
#define FAKE_FAIL_TO_RESEND_FRAG 0
#endif


/**
 * usnic BTL component
 */
typedef struct opal_btl_usnic_component_t {
    /** base BTL component */
    mca_btl_base_component_2_0_0_t super;

    /* in the v1.6 series, sizeof(super) is 256, leading to good alignment for
     * subsequent fastpath fields */

    /** Maximum number of BTL modules */
    int max_modules;
    /** Number of available/initialized BTL modules */
    int num_modules;

    /* Cached hashed version of my RTE proc name (to stuff in
       protocol headers) */
    uint64_t my_hashed_rte_name;

    /** array of possible BTLs (>= num_modules elements) */
    struct opal_btl_usnic_module_t* usnic_all_modules;
    /** array of pointers to active BTLs (num_modules elements) */
    struct opal_btl_usnic_module_t** usnic_active_modules;

    /** convertor packing threshold */
    int pack_lazy_threshold;

    /* vvvvvvvvvv non-fastpath fields go below vvvvvvvvvv */

    /** list of usnic proc structures */
    opal_list_t usnic_procs;

    /** name of memory pool */
    char* usnic_mpool_name;

    char *if_include;
    char *if_exclude;

    /** Want stats? */
    bool stats_enabled;
    bool stats_relative;
    int stats_frequency;

    /** Whether we want to use NUMA distances to choose which usNIC
        devices to use for short messages */
    bool want_numa_device_assignment;

    /** max send descriptors to post per module */
    int32_t sd_num;

    /** max receive descriptors per module */
    int32_t rd_num;

    /** max send/receive desriptors for priority channel */
    int32_t prio_sd_num;
    int32_t prio_rd_num;

    /** max completion queue entries per module */
    int32_t cq_num;

    /** retrans characteristics */
    int retrans_timeout;

    /** transport header length for all usNIC devices on this server
        (it is guaranteed that all usNIC devices on a single server
        will have the same underlying transport, and therefore the
        same transport header length) */
    int transport_header_len;
    uint32_t transport_protocol;

    /* what UDP port do we want to use?  If 0, the system will pick.
       If nonzero, it is used as the base -- the final number will be
       (base+my_local_rank). */
    int udp_port_base;

    /** disable the "cannot find route" warnings (for network setups
        where this is known/acceptable) */
    bool show_route_failures;

    /** connectivity verification: ACK timeout, number of retries
        before issue an error/abort the job */
    bool connectivity_enabled;
    int connectivity_ack_timeout;
    int connectivity_num_retries;

    /** how many short packets have to be received before outputting
        the "received short packets" warning? */
    uint32_t max_short_packets;

    /* Prefix for the connectivity map filename (map will be output if
       the prefix is non-NULL) */
    char *connectivity_map_prefix;
} opal_btl_usnic_component_t;

OPAL_MODULE_DECLSPEC extern opal_btl_usnic_component_t mca_btl_usnic_component;

typedef mca_btl_base_recv_reg_t opal_btl_usnic_recv_reg_t;

/**
 * Size for sequence numbers (just to ensure we use the same size
 * everywhere)
 */
typedef uint16_t opal_btl_usnic_seq_t;
#define UDSEQ PRIu16

/* sequence number comparison macros that allow for rollover.
 * Relies on the fact that sequence numbers should be relatively close
 * together as compared to (1<<31)
 */
#define SEQ_DIFF(A,B) ((int16_t)((A)-(B)))
#define SEQ_LT(A,B) (SEQ_DIFF(A,B) < 0)
#define SEQ_LE(A,B) (SEQ_DIFF(A,B) <= 0)
#define SEQ_GT(A,B) (SEQ_DIFF(A,B) > 0)
#define SEQ_GE(A,B) (SEQ_DIFF(A,B) >= 0)

/**
 * Register the usnic BTL MCA params
 */
int opal_btl_usnic_component_register(void);

/**
 * Routine which can be called from a debugger to print module, endpoint,
 * fragment, and segment state to standard output. */
void opal_btl_usnic_component_debug(void);

/**
 * Called to output the connectivity map
 */
void opal_btl_usnic_connectivity_map(void);

END_C_DECLS
#endif

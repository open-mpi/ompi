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
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OMPI_BTL_USNIC_H
#define OMPI_BTL_USNIC_H

#include "ompi_config.h"
#include <sys/types.h>
#include <infiniband/verbs.h>

#include "opal_stdint.h"
#include "opal/util/alfg.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/event/event.h"

#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/grdma/mpool_grdma.h"

#include "btl_usnic_libnl_utils.h"
#include "btl_usnic_compat.h"

BEGIN_C_DECLS

/*
 * We're simulating a clock as best we can without resorting to the
 * system.  The clock is used to defer ACKs, and ticks will be incremented
 * when progression gets called.  It could be incremented by different amounts
 * at other times as needed or as tuning dictates.
 */
extern uint64_t ompi_btl_usnic_ticks;
static inline uint64_t
get_nsec(void)
{
    return ompi_btl_usnic_ticks;
}

/* RNG buffer declaration */
extern opal_rng_buff_t ompi_btl_usnic_rand_buff;

#ifndef container_of
#define container_of(ptr, type, member) ( \
        (type *)( ((char *)(ptr)) - offsetof(type,member) ))
#endif

/* particularly old versions of verbs do not have this function, which will
 * cause unnecessary build failures on other platforms */
#if !HAVE_DECL_IBV_EVENT_TYPE_STR
#define ibv_event_type_str(ev_type) "(ibv_event_type_str unavailable)"
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
#define FAKE_RECV_FRAG_DROP (opal_rand(&ompi_btl_usnic_rand_buff) < WANT_RECV_FRAG_DROPS)
#else
#define FAKE_RECV_FRAG_DROP 0
#endif

#if WANT_FAIL_TO_SEND_ACK > 0
#define FAKE_FAIL_TO_SEND_ACK (opal_rand(&ompi_btl_usnic_rand_buff) < WANT_FAIL_TO_SEND_ACK)
#else
#define FAKE_FAIL_TO_SEND_ACK 0
#endif

#if WANT_FAIL_TO_RESEND_FRAG > 0
#define FAKE_FAIL_TO_RESEND_FRAG (opal_rand(&ompi_btl_usnic_rand_buff) < WANT_FAIL_TO_RESEND_FRAG)
#else
#define FAKE_FAIL_TO_RESEND_FRAG 0
#endif


/**
 * Verbs UD BTL component.
 */
typedef struct ompi_btl_usnic_component_t {
    /** base BTL component */
    mca_btl_base_component_2_0_0_t super;

    /* in the v1.6 series, sizeof(super) is 256, leading to good alignment for
     * subsequent fastpath fields */

    /** Maximum number of BTL modules */
    uint32_t max_modules;
    /** Number of available/initialized BTL modules */
    uint32_t num_modules;

    /* Cached hashed version of my RTE proc name (to stuff in
       protocol headers) */
    uint64_t my_hashed_rte_name;

    /** array of possible BTLs (>= num_modules elements) */
    struct ompi_btl_usnic_module_t* usnic_all_modules;
    /** array of pointers to active BTLs (num_modules elements) */
    struct ompi_btl_usnic_module_t** usnic_active_modules;

    /** convertor packing threshold */
    int pack_lazy_threshold;

    /** does the stack below us speak UDP or custom-L2? */
    bool use_udp;

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

    /** GID index to use */
    int gid_index;

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

    struct usnic_rtnl_sk *unlsk;

    /** disable the "cannot find route" warnings (for network setups
        where this is known/acceptable) */
    bool show_route_failures;

    /** connectivity verification: ACK timeout, number of retries
        before issue an error/abort the job */
    bool connectivity_enabled;
    int connectivity_ack_timeout;
    int connectivity_num_retries;

    /* ibv_create_ah() (i.e., ARP) timeout */
    int arp_timeout;

    /** how many short packets have to be received before outputting
        the "received short packets" warning? */
    uint32_t max_short_packets;

    /* Prefix for the connectivity map filename (map will be output if
       the prefix is non-NULL) */
    char *connectivity_map_prefix;
} ompi_btl_usnic_component_t;

OMPI_MODULE_DECLSPEC extern ompi_btl_usnic_component_t mca_btl_usnic_component;

typedef mca_btl_base_recv_reg_t ompi_btl_usnic_recv_reg_t;

/**
 * Size for sequence numbers (just to ensure we use the same size
 * everywhere)
 */
typedef uint16_t ompi_btl_usnic_seq_t;
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
int ompi_btl_usnic_component_register(void); 

/**
 * Routine which can be called from a debugger to print module, endpoint,
 * fragment, and segment state to standard output. */
void ompi_btl_usnic_component_debug(void);

/**
 * Called to output the connectivity map
 */
void ompi_btl_usnic_connectivity_map(void);

END_C_DECLS
#endif

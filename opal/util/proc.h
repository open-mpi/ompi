/*
 * Copyright (c) 2013-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013      Inria.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_PROC_H
#define OPAL_PROC_H

#include "opal_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/hwloc/hwloc-internal.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/sys/atomic.h"
#include "opal/types.h"

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
#    include <arpa/inet.h>
#endif

/**
 * This is a transparent handle proposed to the upper layer as a mean
 * to store whatever information it needs in order to efficiently
 * retrieve the RTE process naming scheme, and get access to the RTE
 * information associated with it. The only direct usage of this type
 * is to be copied from one structure to another, otherwise it should
 * only be used via the accessors defined below.
 */
#define OPAL_JOBID_T        OPAL_UINT32
#define OPAL_JOBID_MAX      UINT32_MAX - 2
#define OPAL_JOBID_MIN      0
#define OPAL_JOBID_INVALID  (OPAL_JOBID_MAX + 2)
#define OPAL_JOBID_WILDCARD (OPAL_JOBID_MAX + 1)

#define OPAL_VPID_T        OPAL_UINT32
#define OPAL_VPID_MAX      UINT32_MAX - 2
#define OPAL_VPID_MIN      0
#define OPAL_VPID_INVALID  (OPAL_VPID_MAX + 2)
#define OPAL_VPID_WILDCARD (OPAL_VPID_MAX + 1)

#define OPAL_PROC_MY_NAME     (opal_proc_local_get()->proc_name)
#define OPAL_PROC_MY_HOSTNAME (opal_process_info.nodename)

#define OPAL_NAME_WILDCARD (&opal_name_wildcard)
OPAL_DECLSPEC extern opal_process_name_t opal_name_wildcard;
#define OPAL_NAME_INVALID (&opal_name_invalid)
OPAL_DECLSPEC extern opal_process_name_t opal_name_invalid;

#define OPAL_NAME_ARGS(n)                                                \
    (unsigned long) ((NULL == n) ? (unsigned long) OPAL_JOBID_INVALID    \
                                 : (unsigned long) (n)->jobid),          \
        (unsigned long) ((NULL == n) ? (unsigned long) OPAL_VPID_INVALID \
                                     : (unsigned long) (n)->vpid)

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && !defined(WORDS_BIGENDIAN)
#    define OPAL_PROCESS_NAME_NTOH(guid) opal_process_name_ntoh_intr(&(guid))
static inline __opal_attribute_always_inline__ void
opal_process_name_ntoh_intr(opal_process_name_t *name)
{
    name->jobid = ntohl(name->jobid);
    name->vpid = ntohl(name->vpid);
}
#    define OPAL_PROCESS_NAME_HTON(guid) opal_process_name_hton_intr(&(guid))
static inline __opal_attribute_always_inline__ void
opal_process_name_hton_intr(opal_process_name_t *name)
{
    name->jobid = htonl(name->jobid);
    name->vpid = htonl(name->vpid);
}
#else
#    define OPAL_PROCESS_NAME_NTOH(guid)
#    define OPAL_PROCESS_NAME_HTON(guid)
#endif

/**
 * What is known about the peer this proc stands for.
 *
 * A proc can be created from nothing but a name -- a wild receive, a
 * sentinel resolution, a first send to somebody nobody has talked to yet
 * -- and is filled in as its peer's data arrives. A state of zero is that
 * bare proc; each flag is one thing that has since been learned.
 *
 * Flags rather than one value counting up through stages. They read like
 * a sequence, and in the common case they arrive in this order, but they
 * are not one: a single value can only say "at least this far", and no
 * ordering of these makes that true. Take the architecture and the
 * peer's published data. In a build without heterogeneous support the
 * architecture is the local one, known the moment the proc exists, while
 * nothing of the peer's may ever be fetched -- so being INITIALIZED
 * cannot imply being AVAILABLE. Reverse the two and a heterogeneous build
 * breaks the other way: there the architecture is read out of the peer's
 * data, so it arrives second, and a peer whose data is local but whose
 * convertor has not been built yet is exactly the case the receive path
 * has to catch. Neither implication holds, so each is asked by name.
 *
 * This lives at the bottom because the fields it qualifies do: proc_arch
 * and proc_convertor are read by this layer, and this is what says whether
 * they are the peer's or still copies of our own. Setting them belongs to
 * whichever layer learns the thing in question.
 */
enum {
    /** A fetch of what this peer published has been started. Set by
     *  whoever starts it, so exactly one caller does. */
    OPAL_PROC_FLAG_FETCHING = 0x01,
    /** What it published is local: a Get for one of its keys can be
     *  issued, and a key missing from the answer is missing for good
     *  rather than merely late. */
    OPAL_PROC_FLAG_AVAILABLE = 0x02,
    /** proc_arch and proc_convertor are the peer's own. Nothing may be
     *  packed for, or unpacked from, a peer without this. */
    OPAL_PROC_FLAG_INITIALIZED = 0x04,
    /** The messaging layer has wired it, so it can be reached. */
    OPAL_PROC_FLAG_WIRED = 0x08,
    /** The fetch of what this peer published came back a failure, so
     *  AVAILABLE above is a decision rather than a fact: nothing was
     *  fetched, and a key missing from what was never fetched is being
     *  read as a key the peer never published because that is the only
     *  reading left. Set with AVAILABLE and never alone, so a reader
     *  that acts on the one cannot miss the other.
     *
     *  Kept because of what the reading costs: no btl claims a peer
     *  whose keys all read as absent, and a peer no btl claims is
     *  declared unreachable. This is what lets the process that says so
     *  say why, rather than reporting a runtime failure as a job with no
     *  usable network. */
    OPAL_PROC_FLAG_FETCH_FAILED = 0x10,
};

typedef struct opal_proc_t {
    /** allow proc to be placed on a list */
    opal_list_item_t super;
    /** this process' name */
    opal_process_name_t proc_name;
    /** architecture of this process */
    uint32_t proc_arch;
    /** what is known about this proc -- see the OPAL_PROC_FLAG_* above.
     * Atomic because these are learned concurrently and now share one
     * word: a plain read-modify-write here would un-say what another
     * thread has just said, where separate flags could only ever lose
     * their own. */
    opal_atomic_int32_t proc_state;
    /** flags for this proc */
    opal_hwloc_locality_t proc_flags;
    /** Base convertor for the proc described by this process */
    struct opal_convertor_t *proc_convertor;
} opal_proc_t;
OBJ_CLASS_DECLARATION(opal_proc_t);

/**
 * Is all of this known about the proc?
 *
 * The load is relaxed on purpose: in a heterogeneous build this is asked
 * on every matched receive, and what a flag announces is ordered by the
 * side that set it.
 */
static inline bool opal_proc_known(const opal_proc_t *proc, int32_t flags)
{
#if OPAL_USE_C11_ATOMICS
    /* Cast for the compilers whose atomic_load_explicit() predates taking
     * a const pointer. */
    int32_t state = atomic_load_explicit((opal_atomic_int32_t *) &proc->proc_state,
                                         memory_order_relaxed);
#else
    int32_t state = proc->proc_state;
#endif

    return (flags == (state & flags));
}

/**
 * Record that these are now known. Idempotent, and safe against anything
 * else being learned about the same proc at the same time.
 */
static inline void opal_proc_learned(opal_proc_t *proc, int32_t flags)
{
    (void) opal_atomic_fetch_or_32(&proc->proc_state, flags);
}

/**
 * Record it, and report whether this call is the one that did. For the
 * caller that must then do something exactly once -- start the fetch that
 * OPAL_PROC_FLAG_FETCHING stands for -- where a second caller arriving in
 * the meantime must not do it again.
 */
static inline bool opal_proc_learned_first(opal_proc_t *proc, int32_t flag)
{
    return (0 == (opal_atomic_fetch_or_32(&proc->proc_state, flag) & flag));
}

/**
 * Unlearn: what this stood for has been taken apart again. The mirror of
 * opal_proc_learned(), and like it safe against anything else being
 * learned about the same proc at the same time.
 *
 * Callers unlearn one thing at a time -- del_procs unwires, and that says
 * nothing about the rest. A caller that means "all of it" wants the reset
 * below instead of a list.
 */
static inline void opal_proc_forget(opal_proc_t *proc, int32_t flags)
{
    (void) opal_atomic_fetch_and_32(&proc->proc_state, ~flags);
}

/**
 * Back to a bare proc, nothing known. For a proc being built or rebuilt
 * from scratch, by a caller that has excluded everybody else: this
 * overwrites the word rather than clearing bits in it.
 */
static inline void opal_proc_forget_all(opal_proc_t *proc)
{
#if OPAL_USE_C11_ATOMICS
    atomic_store_explicit(&proc->proc_state, 0, memory_order_relaxed);
#else
    proc->proc_state = 0;
#endif
}

typedef struct {
    opal_list_item_t super;
    opal_process_name_t name;
} opal_namelist_t;
OBJ_CLASS_DECLARATION(opal_namelist_t);

typedef struct opal_process_info_t {
    opal_process_name_t my_name;
    pmix_proc_t myprocid;
    bool nativelaunch;        /**< launched by mpirun */
    char *nodename;           /**< string name for this node */
    char *top_session_dir;    /**< Top-level session directory */
    char *job_session_dir;    /**< Session directory for job */
    char *proc_session_dir;   /**< Session directory for the process */
    uint32_t num_local_peers; /**< number of procs from my job that share my node with me */
    uint16_t my_local_rank;   /**< local rank on this node within my job */
    uint16_t my_node_rank;
    uint16_t my_numa_rank;              /**< rank on this processes NUMA node. A value of UINT16_MAX indicates unavailable numa_rank */
    char *cpuset;                       /**< String-representation of bitmap where we are bound */
    char *locality;                     /**< String-representation of process locality */
    pid_t pid;
    uint32_t num_procs;
    uint32_t app_num;
    uint32_t univ_size;
    char *app_sizes;
    char *app_ldrs;
    char *command;
    uint32_t num_apps;
    char *initial_wdir;
    uint32_t reincarnation;
    bool proc_is_bound;
    char *initial_errhandler;
    bool is_singleton;         /**<note this value can transition from false to true in some cases */
} opal_process_info_t;
OPAL_DECLSPEC extern opal_process_info_t opal_process_info;

OPAL_DECLSPEC extern opal_proc_t *opal_proc_local_get(void);
OPAL_DECLSPEC extern int opal_proc_local_set(opal_proc_t *proc);
OPAL_DECLSPEC extern void opal_proc_set_name(opal_process_name_t *name);

/**
 * Compare two processor name and return an integer greater than,
 * equal to, or less than 0, according as the proc_name of proc1
 * is greater than, equal to, or less than the proc_name of proc2.
 */
typedef int (*opal_compare_proc_fct_t)(const opal_process_name_t, const opal_process_name_t);
OPAL_DECLSPEC extern opal_compare_proc_fct_t opal_compare_proc;

/* Provide print functions that will be overwritten by the RTE layer */
OPAL_DECLSPEC extern char *(*opal_process_name_print)(const opal_process_name_t);
OPAL_DECLSPEC extern int (*opal_convert_string_to_process_name)(opal_process_name_t *name,
                                                                const char *name_string);
OPAL_DECLSPEC extern int (*opal_convert_process_name_to_string)(char **name_string,
                                                                const opal_process_name_t *name);
OPAL_DECLSPEC extern char *(*opal_vpid_print)(const opal_vpid_t);
OPAL_DECLSPEC extern char *(*opal_jobid_print)(const opal_jobid_t);
OPAL_DECLSPEC extern int (*opal_snprintf_jobid)(char *name_string, size_t size, opal_jobid_t jobid);
OPAL_DECLSPEC extern int (*opal_convert_string_to_jobid)(opal_jobid_t *jobid,
                                                         const char *jobid_string);

/**
 * Lookup an opal_proc_t by name
 *
 * @param name (IN) name to lookup
 */
OPAL_DECLSPEC extern struct opal_proc_t *(*opal_proc_for_name)(const opal_process_name_t name);

#define OPAL_NAME_PRINT(OPAL_PN)  opal_process_name_print(OPAL_PN)
#define OPAL_JOBID_PRINT(OPAL_PN) opal_jobid_print(OPAL_PN)
#define OPAL_VPID_PRINT(OPAL_PN)  opal_vpid_print(OPAL_PN)

/* provide a safe way to retrieve the hostname of a proc, including
 * our own. This is to be used by all BTLs so we don't retrieve hostnames
 * unless needed. The returned value MUST NOT be free'd as it is
 * owned by the proc_t */
OPAL_DECLSPEC extern char *(*opal_get_proc_hostname)(const opal_proc_t *proc);

#endif /* OPAL_PROC_H */

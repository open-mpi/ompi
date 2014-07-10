/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The OpenRTE Group Communications
 *
 * The OpenRTE Group Comm framework provides communication services that
 * span entire jobs or collections of processes. It is not intended to be
 * used for point-to-point communications (the RML does that), nor should
 * it be viewed as a high-performance communication channel for large-scale
 * data transfers.
 */

#ifndef MCA_GRPCOMM_TYPES_H
#define MCA_GRPCOMM_TYPES_H

/*
 * includes
 */

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

BEGIN_C_DECLS

/* Define a collective callback function - this will
 * be called upon completion of collective ops such
 * as modex and barrier.
 */
typedef void (*orte_grpcomm_collective_cbfunc_t)(opal_buffer_t *data, void *cbdata);

/* define a collective_id_t. In order to allow scalable
 * generation of collective id's, they are formed as:
 *
 * top 32-bits are the jobid of the procs involved in
 * the collective. For collectives across multiple jobs
 * (e.g., in a connect_accept), the daemon jobid will
 * be used as the id will be issued by mpirun. This
 * won't cause problems because daemons don't use the
 * collective_id
 *
 * bottom 32-bits are a rolling counter that recycles
 * when the max is hit. The daemon will cleanup each
 * collective upon completion, so this means a job can
 * never have more than 2**32 collectives going on at
 * a time. If someone needs more than that - they've got
 * a problem.
 *
 * Note that this means (for now) that RTE-level collectives
 * cannot be done by individual threads - they must be
 * done at the overall process level. This is required as
 * there is no guaranteed ordering for the collective id's,
 * and all the participants must agree on the id of the
 * collective they are executing. So if thread A on one
 * process asks for a collective id before thread B does,
 * but B asks before A on another process, the collectives will
 * be mixed and not result in the expected behavior. We may
 * find a way to relax this requirement in the future by
 * adding a thread context id to the jobid field (maybe taking the
 * lower 16-bits of that field).
 */
typedef uint64_t orte_grpcomm_coll_id_t;
#define ORTE_GRPCOMM_COLL_ID_T   OPAL_UINT64
#define ORTE_GRPCOMM_COLL_ID_INVALID  UINT64_MAX

typedef int8_t orte_grpcomm_coll_t;
#define ORTE_GRPCOMM_XCAST         1
#define ORTE_GRPCOMM_COLL_RELAY    2
#define ORTE_GRPCOMM_COLL_COMPLETE 3
#define ORTE_GRPCOMM_COLL_PEERS    4

typedef enum {
    ORTE_GRPCOMM_INTERNAL_STG_LOCAL,
    ORTE_GRPCOMM_INTERNAL_STG_GLOBAL,
    ORTE_GRPCOMM_INTERNAL_STG_APP
} orte_grpcomm_internal_stage_t;

/* structure for tracking collective operations */
typedef struct {
    opal_list_item_t super;
    orte_grpcomm_coll_id_t id;
    /* flag that user can poll on to know when collective 
     * has completed - set to false just prior to
     * calling user callback function, if non-NULL
     */
    bool active;
    /* number of local contributors */
    orte_vpid_t num_local_recvd;
    /* bucket to collect local contributions */
    opal_buffer_t local_bucket;
    /* number of buckets collected from peers */
    orte_vpid_t num_peer_buckets;
    /* total number of contributors */
    orte_vpid_t num_global_recvd;
    /* flag to mark that the collective is locally complete - i.e.,
     * all local contributions have been recvd and the local
     * data has been entered into the global collective
     */
    bool locally_complete;
    /* list of names of those participating in the collective - an
     * entry with vpid=WILDCARD implies that all members of that
     * job must participate in the collective
     */
    opal_list_t participants;
    /* user callback function to be executed when collective
     * is completed
     */
    orte_grpcomm_collective_cbfunc_t cbfunc;
    void *cbdata;
    /* buffer collecting data to be delivered to user */
    opal_buffer_t buffer;
    /* list of names of procs to receive the next step
     * in executing the collective - this is obtained from
     * the routed framework to minimize hops
     */
    opal_list_t targets;
    /* some collectives wrap around and call internal
     * steps before completing - e.g., modex. This
     * points the collective to the next step in the procedure
     */
    orte_grpcomm_collective_cbfunc_t next_cb;
    void *next_cbdata;
} orte_grpcomm_collective_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_grpcomm_collective_t);

END_C_DECLS

#endif

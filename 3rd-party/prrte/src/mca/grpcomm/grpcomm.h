/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The PRTE Group Communications
 *
 * The PRTE Group Comm framework provides communication services that
 * span entire jobs or collections of processes. It is not intended to be
 * used for point-to-point communications (the RML does that), nor should
 * it be viewed as a high-performance communication channel for large-scale
 * data transfers.
 */

#ifndef MCA_GRPCOMM_H
#define MCA_GRPCOMM_H

/*
 * includes
 */

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include "src/class/pmix_bitmap.h"
#include "src/class/pmix_list.h"
#include "src/pmix/pmix-internal.h"
#include "src/mca/mca.h"
#include "src/rml/rml_types.h"

BEGIN_C_DECLS

/* define a callback function to be invoked upon
 * collective completion */
typedef void (*prte_grpcomm_cbfunc_t)(int status, pmix_data_buffer_t *buf, void *cbdata);

typedef int (*prte_grpcomm_rbcast_cb_t)(pmix_data_buffer_t *buffer);

/* Define a collective signature so we don't need to
 * track global collective id's */
typedef struct {
    pmix_object_t super;
    pmix_proc_t *signature;
    size_t sz;
} prte_grpcomm_signature_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_grpcomm_signature_t);

/* Internal component object for tracking ongoing
 * allgather operations */
typedef struct {
    pmix_list_item_t super;
    /* collective's signature */
    prte_grpcomm_signature_t *sig;
    pmix_status_t status;
    /* collection bucket */
    pmix_data_buffer_t bucket;
    /* participating daemons */
    pmix_rank_t *dmns;
    /** number of participating daemons */
    size_t ndmns;
    /** my index in the dmns array */
    unsigned long my_rank;
    /* number of buckets expected */
    size_t nexpected;
    /* number reported in */
    size_t nreported;
    /* controls values */
    bool assignID;
    int timeout;
    size_t memsize;
    pmix_list_t addmembers;
    /* distance masks for receive */
    pmix_bitmap_t distance_mask_recv;
    /* received buckets */
    pmix_data_buffer_t **buffers;
    /* callback function */
    prte_grpcomm_cbfunc_t cbfunc;
    /* user-provided callback data */
    void *cbdata;
} prte_grpcomm_coll_t;
PMIX_CLASS_DECLARATION(prte_grpcomm_coll_t);

typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    prte_grpcomm_signature_t *sig;
    pmix_group_operation_t op;
    char *grpid;
    pmix_data_buffer_t *buf;
    pmix_byte_object_t ctrls;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_info_t *info;
    size_t ninfo;
    prte_grpcomm_cbfunc_t grpcbfunc;
    pmix_modex_cbfunc_t mdxcbfunc;
    pmix_info_cbfunc_t infocbfunc;
    pmix_op_cbfunc_t opcbfunc;
    void *cbdata;
    void *relcbdata;
} prte_pmix_mdx_caddy_t;
PMIX_CLASS_DECLARATION(prte_pmix_mdx_caddy_t);

/*
 * Component functions - all MUST be provided!
 */

/* initialize the selected module */
typedef int (*prte_grpcomm_base_module_init_fn_t)(void);

/* finalize the selected module */
typedef void (*prte_grpcomm_base_module_finalize_fn_t)(void);

/* Scalably send a message. Caller will provide an array
 * of daemon vpids that are to receive the message. A NULL
 * pointer indicates that all daemons are participating. */
typedef int (*prte_grpcomm_base_module_xcast_fn_t)(pmix_rank_t *vpids, size_t nprocs,
                                                   pmix_data_buffer_t *msg);

/* allgather - gather data from all specified daemons. Barrier operations
 * will provide a zero-byte buffer. Caller will provide an array
 * of daemon vpids that are participating in the allgather via the
 * prte_grpcomm_coll_t object. A NULL pointer indicates that all daemons
 * are participating.
 *
 * NOTE: this is a non-blocking call. The callback function cached in
 * the prte_grpcomm_coll_t will be invoked upon completion. */
typedef int (*prte_grpcomm_base_module_allgather_fn_t)(prte_grpcomm_coll_t *coll,
                                                       prte_pmix_mdx_caddy_t *cd);

/* Reliable broadcast a message thru BMG.
 * only need to provide a message buffer, dont need create dmns
 */
typedef int (*prte_grpcomm_base_module_rbcast_fn_t)(pmix_data_buffer_t *msg);

typedef int (*prte_grpcomm_base_module_rbcast_register_cb_fn_t)(prte_grpcomm_rbcast_cb_t callback);

typedef int (*prte_grpcomm_base_module_rbcast_unregister_cb_fn_t)(int type);

/*
 * Ver 3.0 - internal modules
 */
typedef struct {
    prte_grpcomm_base_module_init_fn_t init;
    prte_grpcomm_base_module_finalize_fn_t finalize;
    /* collective operations */
    prte_grpcomm_base_module_xcast_fn_t xcast;
    prte_grpcomm_base_module_allgather_fn_t allgather;
    prte_grpcomm_base_module_rbcast_fn_t rbcast;
    prte_grpcomm_base_module_rbcast_register_cb_fn_t register_cb;
    prte_grpcomm_base_module_rbcast_unregister_cb_fn_t unregister_cb;
} prte_grpcomm_base_module_t;

/* the Public APIs */
/* Scalably send a message. Caller will provide an array
 * of process names that are to receive the message. A NULL
 * pointer indicates that all known procs are to receive
 * the message. A pointer to a name that includes PRTE_VPID_WILDCARD
 * will send the message to all procs in the specified jobid.
 * The message will be sent to the daemons hosting the specified
 * procs for processing and relay. */
typedef int (*prte_grpcomm_base_API_xcast_fn_t)(prte_grpcomm_signature_t *sig, prte_rml_tag_t tag,
                                                pmix_data_buffer_t *msg);

/* allgather - gather data from all specified procs. Barrier operations
 * will provide a zero-byte buffer. Caller will provide an array
 * of application proc vpids that are participating in the allgather. A NULL
 * pointer indicates that all known procs are participating. A pointer
 * to a name that includes PRTE_VPID_WILDCARD indicates that all procs
 * in the specified jobid are contributing.
 *
 * NOTE: this is a non-blocking call. The provided callback function
 * will be invoked upon completion. */
typedef int (*prte_grpcomm_base_API_allgather_fn_t)(prte_pmix_mdx_caddy_t *cd);

/* Reliable broadcast a message. Caller will provide an array
 * of daemon. A NULL pointer indicates that all known daemons are in the BMG.
 * A pointer to a name that includes ORTE_VPID_WILDCARD
 * all daemons in the specified jobid.*/
typedef int (*prte_grpcomm_base_API_rbcast_fn_t)(prte_grpcomm_signature_t *sig, prte_rml_tag_t tag,
                                                 pmix_data_buffer_t *msg);

typedef int (*prte_grpcomm_base_API_rbcast_register_cb_fn_t)(prte_grpcomm_rbcast_cb_t callback);

typedef int (*prte_grpcomm_base_API_rbcast_unregister_cb_fn_t)(int type);

typedef struct {
    /* collective operations */
    prte_grpcomm_base_API_xcast_fn_t xcast;
    prte_grpcomm_base_API_allgather_fn_t allgather;
    prte_grpcomm_base_API_rbcast_fn_t rbcast;
    prte_grpcomm_base_API_rbcast_register_cb_fn_t register_cb;
    prte_grpcomm_base_API_rbcast_unregister_cb_fn_t unregister_cb;
} prte_grpcomm_API_module_t;

/*
 * the standard component data structure
 */
typedef pmix_mca_base_component_t prte_grpcomm_base_component_t;

/*
 * Macro for use in components that are of type grpcomm v3.0.0
 */
#define PRTE_GRPCOMM_BASE_VERSION_3_0_0       \
    /* grpcomm v3.0 is chained to MCA v2.0 */ \
    PRTE_MCA_BASE_VERSION_3_0_0("grpcomm", 3, 0, 0)

/* Global structure for accessing grpcomm functions */
PRTE_EXPORT extern prte_grpcomm_API_module_t prte_grpcomm;

END_C_DECLS

#endif

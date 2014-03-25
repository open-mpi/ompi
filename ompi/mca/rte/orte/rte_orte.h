/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * When this component is used, this file is included in the rest of
 * the OPAL/ORTE/OMPI code base via ompi/mca/rte/rte.h.  As such,
 * this header represents the public interface to this static component.
 */

#ifndef MCA_OMPI_RTE_ORTE_H
#define MCA_OMPI_RTE_ORTE_H

#include "ompi_config.h"
#include "ompi/constants.h"

#include "ompi/info/info.h"
struct ompi_proc_t;

#include "opal/threads/threads.h"

#include "orte/types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_data_server.h"
#include "orte/runtime/runtime.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"

BEGIN_C_DECLS

/* Process name objects and operations */
typedef orte_process_name_t ompi_process_name_t;
typedef orte_jobid_t ompi_jobid_t;
typedef orte_vpid_t ompi_vpid_t;
typedef orte_ns_cmp_bitmask_t ompi_rte_cmp_bitmask_t;
#define OMPI_PROC_MY_NAME ORTE_PROC_MY_NAME
#define OMPI_NAME_PRINT(a) ORTE_NAME_PRINT(a)
#define ompi_rte_compare_name_fields(a, b, c) orte_util_compare_name_fields(a, b, c)
#define OMPI_NAME_WILDCARD  ORTE_NAME_WILDCARD
#define OMPI_NODE_RANK_INVALID ORTE_NODE_RANK_INVALID
#define OMPI_LOCAL_RANK_INVALID ORTE_LOCAL_RANK_INVALID
#define OMPI_RTE_CMP_JOBID  ORTE_NS_CMP_JOBID
#define OMPI_RTE_CMP_VPID   ORTE_NS_CMP_VPID
#define OMPI_RTE_CMP_ALL    ORTE_NS_CMP_ALL
#define ompi_rte_hash_name(a) orte_util_hash_name(a)
/* This is the DSS tag to serialize a proc name */
#define OMPI_NAME ORTE_NAME
#define OMPI_PROCESS_NAME_HTON ORTE_PROCESS_NAME_HTON
#define OMPI_PROCESS_NAME_NTOH ORTE_PROCESS_NAME_NTOH
#define OMPI_RTE_NODE_ID  ORTE_DB_DAEMON_VPID
#define OMPI_RTE_MY_NODEID ORTE_PROC_MY_DAEMON->vpid
#define OMPI_RTE_HOST_ID ORTE_DB_HOSTID

/* Collective objects and operations */
#define ompi_rte_collective_t orte_grpcomm_collective_t
typedef orte_grpcomm_coll_id_t ompi_rte_collective_id_t;
OMPI_DECLSPEC int ompi_rte_modex(ompi_rte_collective_t *coll);
#define ompi_rte_barrier(a) orte_grpcomm.barrier(a)

/* Process info struct and values */
typedef orte_node_rank_t ompi_node_rank_t;
typedef orte_local_rank_t ompi_local_rank_t;
#define ompi_process_info orte_process_info
#define ompi_rte_proc_is_bound orte_proc_is_bound

/* Error handling objects and operations */
OMPI_DECLSPEC void __opal_attribute_noreturn__ 
  ompi_rte_abort(int error_code, char *fmt, ...);
#define ompi_rte_abort_peers(a, b, c) orte_errmgr.abort_peers(a, b, c)
#define OMPI_RTE_ERRHANDLER_FIRST ORTE_ERRMGR_CALLBACK_FIRST
#define OMPI_RTE_ERRHANDLER_LAST ORTE_ERRMGR_CALLBACK_LAST
#define OMPI_RTE_ERRHANDLER_PREPEND ORTE_ERRMGR_CALLBACK_PREPEND
#define OMPI_RTE_ERRHANDLER_APPEND ORTE_ERRMGR_CALLBACK_APPEND
typedef orte_error_t ompi_rte_error_report_t;
#define ompi_rte_register_errhandler(a, b) orte_errmgr.register_error_callback(a, b)
#define OMPI_ERROR_LOG ORTE_ERROR_LOG

/* Init and finalize objects and operations */
#define ompi_rte_init(a, b) orte_init(a, b, ORTE_PROC_MPI)
#define ompi_rte_finalize() orte_finalize()
OMPI_DECLSPEC void ompi_rte_wait_for_debugger(void);

/* Database operations */
OMPI_DECLSPEC int ompi_rte_db_store(const ompi_process_name_t *nm, const char* key,
                                    const void *data, opal_data_type_t type);
OMPI_DECLSPEC int ompi_rte_db_fetch(const struct ompi_proc_t *proc,
                                    const char *key,
                                    void **data, opal_data_type_t type);
OMPI_DECLSPEC int ompi_rte_db_fetch_pointer(const struct ompi_proc_t *proc,
                                            const char *key,
                                            void **data, opal_data_type_t type);
OMPI_DECLSPEC int ompi_rte_db_fetch_multiple(const struct ompi_proc_t *proc,
                                             const char *key,
                                             opal_list_t *kvs);
OMPI_DECLSPEC int ompi_rte_db_remove(const ompi_process_name_t *nm,
                                     const char *key);
#define OMPI_DB_HOSTNAME ORTE_DB_HOSTNAME
#define OMPI_DB_LOCALITY ORTE_DB_LOCALITY
#define OMPI_DB_GLOBAL_RANK ORTE_DB_GLOBAL_RANK

/* Communications */
typedef orte_rml_tag_t ompi_rml_tag_t;
#define ompi_rte_send_buffer_nb(a, b, c, d, e) orte_rml.send_buffer_nb(a, b, c, d, e)
#define ompi_rte_recv_buffer_nb(a, b, c, d, e) orte_rml.recv_buffer_nb(a, b, c, d, e)
#define ompi_rte_recv_cancel(a, b) orte_rml.recv_cancel(a, b)
#define ompi_rte_parse_uris(a, b, c) orte_rml_base_parse_uris(a, b, c)
#define ompi_rte_send_cbfunc orte_rml_send_callback

/* Communication tags */
/* carry over the INVALID def */
#define OMPI_RML_TAG_INVALID ORTE_RML_TAG_INVALID
/* define a starting point to avoid conflicts */
#define OMPI_RML_TAG_BASE    ORTE_RML_TAG_MAX

#define OMPI_RML_PERSISTENT      ORTE_RML_PERSISTENT
#define OMPI_RML_NON_PERSISTENT  ORTE_RML_NON_PERSISTENT

typedef struct {
    ompi_rte_component_t super;
    bool direct_modex;
    opal_mutex_t lock;
    opal_list_t modx_reqs;
} ompi_rte_orte_component_t;

typedef struct {
    opal_list_item_t super;
    opal_mutex_t lock;
    opal_condition_t cond;
    bool active;
    orte_process_name_t peer;
} ompi_orte_tracker_t;
OBJ_CLASS_DECLARATION(ompi_orte_tracker_t);

END_C_DECLS

#endif /* MCA_OMPI_RTE_ORTE_H */

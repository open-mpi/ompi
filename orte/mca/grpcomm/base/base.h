/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_GRPCOMM_BASE_H
#define MCA_GRPCOMM_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/class/opal_list.h"
#include "opal/dss/dss_types.h"
#include "opal/mca/mca.h"
#include "opal/mca/hwloc/hwloc.h"

#include "orte/mca/odls/odls_types.h"

#include "orte/mca/grpcomm/grpcomm.h"


/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/*
 * MCA framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_grpcomm_base_framework;
/*
 * Select an available component.
 */
ORTE_DECLSPEC int orte_grpcomm_base_select(void);

/*
 * globals that might be needed
 */
typedef struct {
    orte_grpcomm_coll_id_t coll_id;
    opal_list_t active_colls;
#if OPAL_HAVE_HWLOC
    hwloc_cpuset_t working_cpuset;
#endif
    bool modex_ready;
    opal_list_t modex_requests;
} orte_grpcomm_base_t;

typedef struct {
    opal_list_item_t super;
    orte_process_name_t peer;
    opal_scope_t scope;
} orte_grpcomm_modex_req_t;
OBJ_CLASS_DECLARATION(orte_grpcomm_modex_req_t);

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_grpcomm_collective_t *op;
} orte_grpcomm_caddy_t;
OBJ_CLASS_DECLARATION(orte_grpcomm_caddy_t);

#define ORTE_GRPCOMM_ACTIVATE(o, cb)                                    \
    do {                                                                \
        orte_grpcomm_caddy_t *caddy;                                    \
        OPAL_OUTPUT_VERBOSE((5, orte_grpcomm_base_framework.framework_output, \
                             "%s ACTIVATING GRCPCOMM OP %d at %s:%d",   \
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),        \
                             (o)->id, __FILE__, __LINE__));             \
        caddy = OBJ_NEW(orte_grpcomm_caddy_t);                          \
        caddy->op = (o);                                                \
        opal_event_set(orte_event_base, &caddy->ev, -1,                 \
                       OPAL_EV_WRITE, (cb), caddy);                     \
        opal_event_set_priority(&caddy->ev, ORTE_MSG_PRI);              \
        opal_event_active(&caddy->ev, OPAL_EV_WRITE, 1);                \
    } while(0);

ORTE_DECLSPEC extern orte_grpcomm_base_t orte_grpcomm_base;

ORTE_DECLSPEC orte_grpcomm_collective_t* orte_grpcomm_base_setup_collective(orte_grpcomm_coll_id_t id);
ORTE_DECLSPEC void orte_grpcomm_base_progress_collectives(void);
ORTE_DECLSPEC orte_grpcomm_coll_id_t orte_grpcomm_base_get_coll_id(void);
ORTE_DECLSPEC void orte_grpcomm_base_pack_collective(opal_buffer_t *relay,
                                                     orte_jobid_t jobid,
                                                     orte_grpcomm_collective_t *coll,
                                                     orte_grpcomm_internal_stage_t stg);
ORTE_DECLSPEC void orte_grpcomm_base_rollup_recv(int status, orte_process_name_t* sender,
                                                 opal_buffer_t* buffer, orte_rml_tag_t tag,
                                                 void* cbdata);

/* modex support */
ORTE_DECLSPEC   void orte_grpcomm_base_store_modex(opal_buffer_t *rbuf, void *cbdata);
ORTE_DECLSPEC   void orte_grpcomm_base_modex(int fd, short args, void *cbdata);
ORTE_DECLSPEC   int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf, opal_scope_t scope);

/* comm support */
ORTE_DECLSPEC int orte_grpcomm_base_comm_start(void);
ORTE_DECLSPEC void orte_grpcomm_base_comm_stop(void);
ORTE_DECLSPEC void orte_grpcomm_base_xcast_recv(int status, orte_process_name_t* sender,
                                                opal_buffer_t* buffer, orte_rml_tag_t tag,
                                                void* cbdata);
ORTE_DECLSPEC int orte_grpcomm_base_pack_xcast(orte_jobid_t job,
                                               opal_buffer_t *buffer,
                                               opal_buffer_t *message,
                                               orte_rml_tag_t tag);
ORTE_DECLSPEC void orte_grpcomm_base_process_modex(int fd, short args, void *cbdata);

END_C_DECLS
#endif

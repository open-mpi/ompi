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
#include "opal/mca/mca.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/mca/odls/odls_types.h"

#include "orte/mca/grpcomm/grpcomm.h"


/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/*
 * function definitions
 */
ORTE_DECLSPEC    int orte_grpcomm_base_open(void);
ORTE_DECLSPEC    int orte_grpcomm_base_select(void);
ORTE_DECLSPEC    int orte_grpcomm_base_close(void);

/* daemon collective function */
typedef void (*orte_grpcomm_daemon_collective_fn_t)(orte_process_name_t *sender,
                                                    opal_buffer_t *data);
/*
 * globals that might be needed
 */
typedef struct {
    int output;
    bool selected;
    opal_list_t components_available;
    orte_grpcomm_base_component_t selected_component;
    int profile_fd;
    orte_grpcomm_daemon_collective_fn_t daemon_coll;
} orte_grpcomm_base_t;

ORTE_DECLSPEC extern orte_grpcomm_base_t orte_grpcomm_base;

/* structure for tracking collective operations */
typedef struct {
    opal_object_t super;
    opal_mutex_t lock;
    opal_condition_t cond;
    orte_vpid_t recvd;
    opal_buffer_t results;
} orte_grpcomm_collective_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_grpcomm_collective_t);

/*
 * Base functions
 */
ORTE_DECLSPEC   int orte_grpcomm_base_allgather_list(opal_list_t *names,
                                                     opal_buffer_t *sbuf,
                                                     opal_buffer_t *rbuf);
ORTE_DECLSPEC   int orte_grpcomm_base_set_proc_attr(const char *attr_name,
                                                    const void *data,
                                                    size_t size);
ORTE_DECLSPEC   int orte_grpcomm_base_get_proc_attr(const orte_process_name_t proc,
                                                    const char * attribute_name, void **val, 
                                                    size_t *size);
ORTE_DECLSPEC   int orte_grpcomm_base_peer_modex(void);
ORTE_DECLSPEC   int orte_grpcomm_base_modex_unpack( opal_buffer_t* rbuf);
ORTE_DECLSPEC   int orte_grpcomm_base_full_modex(opal_list_t *procs);
ORTE_DECLSPEC   int orte_grpcomm_base_purge_proc_attrs(void);
ORTE_DECLSPEC   int orte_grpcomm_base_modex_init(void);
ORTE_DECLSPEC   void orte_grpcomm_base_modex_finalize(void);
ORTE_DECLSPEC   int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf);
ORTE_DECLSPEC   int orte_grpcomm_base_update_modex_entries(orte_process_name_t *proc_name,
                                                           opal_buffer_t *rbuf);
ORTE_DECLSPEC   int orte_grpcomm_base_load_modex_data(orte_process_name_t *proc, char *attribute_name,
                                                      void *data, int num_bytes);

/* app functions */
ORTE_DECLSPEC   int orte_grpcomm_base_app_barrier(orte_process_name_t *recipient,
                                                  orte_grpcomm_collective_t *coll);
ORTE_DECLSPEC   int orte_grpcomm_base_app_allgather(orte_process_name_t *recipient,
                                                    orte_grpcomm_collective_t *coll,
                                                    opal_buffer_t *sbuf,
                                                    opal_buffer_t *rbuf);
ORTE_DECLSPEC   int orte_grpcomm_base_app_pack_xcast(orte_daemon_cmd_flag_t cmd,
                                                     orte_jobid_t job,
                                                     opal_buffer_t *buffer,
                                                     opal_buffer_t *message,
                                                     orte_rml_tag_t tag);

/* Tuned collectives */
ORTE_DECLSPEC void orte_grpcomm_base_coll_recv(int status, orte_process_name_t* sender,
                                               opal_buffer_t* buffer, orte_rml_tag_t tag,
                                               void* cbdata);
ORTE_DECLSPEC int orte_grpcomm_base_allgather(opal_buffer_t *sendbuf, opal_buffer_t *recvbuf, int32_t num_entries,
                                              orte_jobid_t jobid, orte_vpid_t np, orte_vpid_t *vpids);
ORTE_DECLSPEC void orte_grpcomm_base_daemon_coll_recv(int status, orte_process_name_t* sender,
                                                      opal_buffer_t* buffer, orte_rml_tag_t tag,
                                                      void* cbdata);
ORTE_DECLSPEC void orte_grpcomm_base_daemon_collective(orte_process_name_t *sender,
                                                       opal_buffer_t *data);

END_C_DECLS
#endif

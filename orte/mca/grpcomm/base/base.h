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
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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
#include "opal/mca/hwloc/hwloc.h"

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

/*
 * globals that might be needed
 */
typedef struct {
    int output;
    bool selected;
    opal_list_t components_available;
    orte_grpcomm_base_component_t selected_component;
    orte_grpcomm_coll_id_t coll_id;
    opal_list_t active_colls;
#if OPAL_HAVE_HWLOC
    hwloc_cpuset_t working_cpuset;
#endif
} orte_grpcomm_base_t;

ORTE_DECLSPEC extern orte_grpcomm_base_t orte_grpcomm_base;

ORTE_DECLSPEC orte_grpcomm_collective_t* orte_grpcomm_base_setup_collective(orte_grpcomm_coll_id_t id);
ORTE_DECLSPEC void orte_grpcomm_base_progress_collectives(void);
ORTE_DECLSPEC orte_grpcomm_coll_id_t orte_grpcomm_base_get_coll_id(void);
ORTE_DECLSPEC void orte_grpcomm_base_pack_collective(opal_buffer_t *relay,
                                                     orte_grpcomm_collective_t *coll,
                                                     orte_grpcomm_internal_stage_t stg);
ORTE_DECLSPEC void orte_grpcomm_base_rollup_recv(int status, orte_process_name_t* sender,
                                                 opal_buffer_t* buffer, orte_rml_tag_t tag,
                                                 void* cbdata);

/* modex support */
ORTE_DECLSPEC   int orte_grpcomm_base_set_proc_attr(const char *attr_name,
                                                    const void *data,
                                                    size_t size);
ORTE_DECLSPEC   int orte_grpcomm_base_get_proc_attr(const orte_process_name_t proc,
                                                    const char * attribute_name, void **val, 
                                                    size_t *size);
ORTE_DECLSPEC   void orte_grpcomm_base_store_peer_modex(opal_buffer_t *rbuf, void *cbdata);
ORTE_DECLSPEC   void orte_grpcomm_base_store_modex(opal_buffer_t *rbuf, void *cbdata);
ORTE_DECLSPEC   int orte_grpcomm_base_modex(orte_grpcomm_collective_t *modex);
ORTE_DECLSPEC   int orte_grpcomm_base_purge_proc_attrs(void);
ORTE_DECLSPEC   int orte_grpcomm_base_modex_init(void);
ORTE_DECLSPEC   void orte_grpcomm_base_modex_finalize(void);
ORTE_DECLSPEC   int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf);
ORTE_DECLSPEC   int orte_grpcomm_base_update_modex_entries(orte_process_name_t *proc_name,
                                                           opal_buffer_t *rbuf);
ORTE_DECLSPEC   int orte_grpcomm_base_load_modex_data(orte_process_name_t *proc, char *attribute_name,
                                                      void *data, int num_bytes);

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

END_C_DECLS
#endif

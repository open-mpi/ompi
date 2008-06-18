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

ORTE_DECLSPEC extern int orte_grpcomm_base_output;
ORTE_DECLSPEC extern bool mca_grpcomm_base_selected;
ORTE_DECLSPEC extern opal_list_t mca_grpcomm_base_components_available;
ORTE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_base_selected_component;

#if !ORTE_DISABLE_FULL_SUPPORT

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
ORTE_DECLSPEC   int orte_grpcomm_base_modex(opal_list_t *procs);
ORTE_DECLSPEC   int orte_grpcomm_base_purge_proc_attrs(void);
ORTE_DECLSPEC   int orte_grpcomm_base_modex_init(void);
ORTE_DECLSPEC   void orte_grpcomm_base_modex_finalize(void);
ORTE_DECLSPEC   int orte_grpcomm_base_pack_modex_entries(opal_buffer_t *buf, bool *modex_reqd);
ORTE_DECLSPEC   int orte_grpcomm_base_update_modex_entries(orte_process_name_t *proc_name,
                                                           opal_buffer_t *rbuf);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif

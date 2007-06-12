/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_ODLS_PROCESS_EXPORT_H
#define ORTE_ODLS_PROCESS_EXPORT_H

#include "orte_config.h"

#include "opal/threads/condition.h"
#include "opal/mca/mca.h"

#include "orte/mca/rmgr/rmgr_types.h"

#include "orte/mca/odls/odls.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Module open / close
 */
int orte_odls_process_component_open(void);
int orte_odls_process_component_close(void);
orte_odls_base_module_t* orte_odls_process_component_init(int *priority);

/*
 * Startup / Shutdown
 */
int orte_odls_process_component_finalize(void);

/**
 * ODLS Process globals
 */
typedef struct orte_odls_process_globals_t {
    opal_mutex_t mutex;
    opal_condition_t cond;
    opal_list_t children;
} orte_odls_process_globals_t;

extern orte_odls_process_globals_t orte_odls_process;

/*
 * List object to locally store app_contexts returned by the
 * registry subscription. Since we don't know how many app_contexts will
 * be returned, we need to store them on a list.
 */
typedef struct odls_process_app_context_t {
    opal_list_item_t super;      /* required to place this on a list */
    orte_app_context_t *app_context;
} odls_process_app_context_t;
OBJ_CLASS_DECLARATION(odls_process_app_context_t);

/*
 * ODLS Process module
 */
extern orte_odls_base_module_t orte_odls_process_module;
ORTE_MODULE_DECLSPEC extern orte_odls_base_component_t mca_odls_process_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_ODLS_PROCESS_EXPORT_H */

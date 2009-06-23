/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file:
 * Part of the rshd launcher. See plm_rshd.h for an overview of how it works.
 */

#ifndef ORTE_PLM_RSHD_EXPORT_H
#define ORTE_PLM_RSHD_EXPORT_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/threads/condition.h"
#include "opal/mca/mca.h"

#include "orte/mca/plm/plm.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_plm_rshd_component_open(void);
int orte_plm_rshd_component_close(void);
int orte_plm_rshd_component_query(mca_base_module_t **module, int *priority);

/*
 * Startup / Shutdown
 */
int orte_plm_rshd_finalize(void);

/*
 * Interface
 */
int orte_plm_rshd_init(void);
int orte_plm_rshd_launch(orte_job_t *jdata);
int orte_plm_rshd_terminate_job(orte_jobid_t);
int orte_plm_rshd_terminate_orteds(void);
int orte_plm_rshd_signal_job(orte_jobid_t, int32_t);

/**
 * PLS Component
 */
struct orte_plm_rshd_component_t {
    orte_plm_base_component_t super;
    bool force_rsh;
    opal_list_t children;
    int num_children;
    int num_concurrent;
    opal_mutex_t lock;
    opal_condition_t cond;
};
typedef struct orte_plm_rshd_component_t orte_plm_rshd_component_t;

ORTE_MODULE_DECLSPEC extern orte_plm_rshd_component_t mca_plm_rshd_component;
extern orte_plm_base_module_t orte_plm_rshd_module;

END_C_DECLS

#endif /* ORTE_PLS_RSHD_EXPORT_H */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_PLS_TM_EXPORT_H
#define ORTE_PLS_TM_EXPORT_H

#include "orte_config.h"

#include "mca/mca.h"
#include "mca/pls/pls.h"

#include "tm.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /* Globally exported variables */
    
    OMPI_COMP_EXPORT extern orte_pls_base_component_1_0_0_t 
        orte_pls_tm_component;
    OMPI_COMP_EXPORT extern orte_pls_base_module_1_0_0_t
        orte_pls_tm_module;

    /* Global, but not exported variables */

    extern bool orte_pls_tm_connected;

    /* Internal struct */

    typedef struct pls_tm_proc_state_t {
        tm_task_id tid;
        uint32_t state;
    } pls_tm_proc_state_t;

    /* Local functions */

    int orte_pls_tm_put_tid(const orte_process_name_t* name, 
                            tm_task_id tid, int state);
    int orte_pls_tm_get_tids(orte_jobid_t jobid, tm_task_id **tids, 
                             orte_process_name_t **names, size_t *size);

    /* Child process functions */

    int orte_pls_tm_child_init(void);
    int orte_pls_tm_child_launch(orte_jobid_t jobid);
    int orte_pls_tm_child_wait(orte_jobid_t jobid);
    int orte_pls_tm_child_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* ORTE_PLS_TM_EXPORT_H */

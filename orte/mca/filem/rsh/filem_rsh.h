/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

/**
 * @file
 * 
 * RSH FILEM component
 *
 */

#ifndef MCA_FILEM_RSH_EXPORT_H
#define MCA_FILEM_RSH_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/filem/filem.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * Local Component structures
     */
    struct orte_filem_rsh_component_t {
        /** Base FILEM component */
        orte_filem_base_component_t super;

        /** RSH cp command: rsh = rcp, ssh = scp */
        char * cp_command;

        /** SSH remote login command */	
        char * remote_sh_command;
    };
    typedef struct orte_filem_rsh_component_t orte_filem_rsh_component_t;
    extern orte_filem_rsh_component_t mca_filem_rsh_component;

    /*
     * Module functions
     */
    orte_filem_base_module_1_0_0_t *
        orte_filem_rsh_component_query(int *priority);
    int orte_filem_rsh_module_init(void);
    int orte_filem_rsh_module_finalize(void);

    int orte_filem_base_rsh_put(orte_filem_base_request_t *request);
    int orte_filem_base_rsh_get(orte_filem_base_request_t *request);
    int orte_filem_base_rsh_rm( orte_filem_base_request_t *request);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_FILEM_RSH_EXPORT_H */

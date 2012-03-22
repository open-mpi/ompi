/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c)      2010-2011 Alex Brick <bricka@ccs.neu.edu>.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * 
 * DMTCP CRS component
 *
 */

#ifndef MCA_CRS_DMTCP_EXPORT_H
#define MCA_CRS_DMTCP_EXPORT_H

#include "opal_config.h"


#include "opal/mca/mca.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/base/base.h"

/* JJH NOTE: Include your library header here */
/* #include <libmtcp.h> */
#include <mtcp.h>

BEGIN_C_DECLS

    /*
     * Local Component Structure
     */
    struct opal_crs_dmtcp_component_t {
        /** Base CRS component */
        opal_crs_base_component_t super;

        /** JJH: Add additional items here as needed internally */
    };
    typedef struct opal_crs_dmtcp_component_t opal_crs_dmtcp_component_t;
    OPAL_MODULE_DECLSPEC extern opal_crs_dmtcp_component_t mca_crs_dmtcp_component;

    /*
     * Component query command
     *  - Called during opal_init() to determine if this component should be selected.
     */
    int opal_crs_dmtcp_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int opal_crs_dmtcp_module_init(void);
    int opal_crs_dmtcp_module_finalize(void);

    /*
     * Actual CRS funcationality
     */
    int opal_crs_dmtcp_checkpoint( pid_t pid, 
                                  opal_crs_base_snapshot_t *snapshot,
                                  opal_crs_base_ckpt_options_t *options,
                                  opal_crs_state_type_t    *state);

    int opal_crs_dmtcp_restart(    opal_crs_base_snapshot_t *snapshot, 
                                  bool spawn_child, 
                                  pid_t *child_pid);

    int opal_crs_dmtcp_disable_checkpoint(void);
    int opal_crs_dmtcp_enable_checkpoint(void);

    int opal_crs_dmtcp_prelaunch(int32_t rank,
                                char *base_snapshot_dir,
                                char **app,
                                char **cwd,
                                char ***argv,
                                char ***env);

    int opal_crs_dmtcp_reg_thread(void);

END_C_DECLS

#endif /* MCA_CRS_DMTCP_EXPORT_H */

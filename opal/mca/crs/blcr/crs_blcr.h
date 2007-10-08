
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
 * BLCR CRS component
 *
 */

#ifndef MCA_CRS_BLCR_EXPORT_H
#define MCA_CRS_BLCR_EXPORT_H

#include "opal_config.h"

#include "opal/util/output.h"

#include "opal/mca/mca.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal_cr.h"

#include <libcr.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /*
     * Local Component structures
     */
    struct opal_crs_blcr_component_t {
        /** Base CRS component */
        opal_crs_base_component_t super;
    };
    typedef struct opal_crs_blcr_component_t opal_crs_blcr_component_t;
    OPAL_MODULE_DECLSPEC extern opal_crs_blcr_component_t mca_crs_blcr_component;

    /*
     * Module functions
     */
    opal_crs_base_module_1_0_0_t *
          opal_crs_blcr_component_query(int *priority);
    int opal_crs_blcr_module_init(void);
    int opal_crs_blcr_module_finalize(void);

    /*
     * Actual funcationality
     */
    int opal_crs_blcr_checkpoint( pid_t pid, 
                                  opal_crs_base_snapshot_t *snapshot, 
                                  opal_crs_state_type_t    *state);

    int opal_crs_blcr_restart(    opal_crs_base_snapshot_t *snapshot, 
                                  bool spawn_child, 
                                  pid_t *child_pid);

    int opal_crs_blcr_disable_checkpoint(void);
    int opal_crs_blcr_enable_checkpoint(void);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_CRS_BLCR_EXPORT_H */

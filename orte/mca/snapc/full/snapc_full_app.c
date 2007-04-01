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

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/runtime/opal_cr.h"
#include "opal/util/output.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/orte_constants.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

/************************************
 * Locally Global vars & functions :)
 ************************************/

/************************
 * Function Definitions
 ************************/

int app_coord_init() {
    int exit_status  = ORTE_SUCCESS;

    /*
     * Setup GPR callback that indicates if we should checkpoint ourselves
     * Currently this is relayed via the Local Snapshot Coordinator
     */

    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "app) Initalized for Application (%d.%d.%d)\n", 
                        orte_process_info.my_name->cellid,
                        orte_process_info.my_name->jobid,
                        orte_process_info.my_name->vpid);

    return exit_status;
}

int app_coord_finalize() {

    /*
     * Cleanup GPR callbacks
     */

    return ORTE_SUCCESS;
}

/******************
 * Local functions
 ******************/

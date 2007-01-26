/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

/** @file **/

#include "orte_config.h"

#include "orte/orte_constants.h"

#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/os_path.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/rml/base/base.h"
#include "orte/dss/dss.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/rds/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/schema/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/odls/base/base.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/univ_info.h"

/**
 * Leave ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 */
int orte_system_finalize(void)
{
    char *contact_path;
    
    /* if I'm the seed, remove the universe contact info file */
    if (orte_process_info.seed) {
        contact_path = opal_os_path(false, orte_process_info.universe_session_dir,
                    "universe-setup.txt", NULL);
        unlink(contact_path);
        free(contact_path);
    }
    
    /* rmgr and odls close depend on wait/iof */
    orte_rmgr_base_close();
    orte_odls_base_close();
    orte_wait_finalize();
    orte_iof_base_close();

    orte_ns_base_close();
    orte_gpr_base_close();
    orte_schema_base_close();
    
    /* finalize selected modules so they can de-register
     * their receives
     */
    orte_rds_base_close();
    orte_ras_base_close();
    orte_rmaps_base_close();
    orte_pls_base_close();
    /* the errmgr close function retains the base
     * module so that error logging can continue
     */
    orte_errmgr_base_close();
    
    /* now can close the rml */
    orte_rml_base_close();
    orte_dss_close();
    
    opal_progress_finalize();

    opal_event_fini();

    orte_session_dir_finalize(orte_process_info.my_name);

    /* clean out the global structures */
    orte_sys_info_finalize();
    orte_proc_info_finalize();
    orte_univ_info_finalize();
    
    return ORTE_SUCCESS;
}

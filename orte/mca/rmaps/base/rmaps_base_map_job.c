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

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/base/rmaps_private.h"


/*
 * Function for selecting one component from all those that are
 * available.
 */
int orte_rmaps_base_map_job(orte_job_t *jdata)
{
    orte_job_map_t *map=NULL;
    int rc;
    
    /* NOTE: NO PROXY COMPONENT REQUIRED - REMOTE PROCS ARE NOT
     * ALLOWED TO CALL RMAPS INDEPENDENTLY. ONLY THE PLM CAN
     * DO SO, AND ALL PLM COMMANDS ARE RELAYED TO HNP
     */
    
    /* NOTE: CHECK FOR JDATA->MAP == NULL. IF IT IS, THEN USE
     * THE VALUES THAT WERE READ BY THE LOCAL MCA PARAMS. THE
     * PLM PROXY WILL SEND A JOB-OBJECT THAT WILL INCLUDE ANY
     * MAPPING DIRECTIVES - OTHERWISE, THAT OBJECT WILL HAVE A
     * NULL MAP FIELD
     * LONE EXCEPTION - WE COPY DISPLAY MAP ACROSS IF THEY
     * DIDN'T SET IT
     */
    
    if (NULL == jdata->map) {
        /* a map has not been defined yet for this job, so set one
         * up here
         */
        /* create a map object where we will store the results */
        map = OBJ_NEW(orte_job_map_t);
        if (NULL == map) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* load it with the system defaults */
        map->policy = orte_rmaps_base.policy;
        map->pernode = orte_rmaps_base.pernode;
        map->npernode = orte_rmaps_base.npernode;
        map->oversubscribe = orte_rmaps_base.oversubscribe;
        map->display_map = orte_rmaps_base.display_map;
        /* assign the map object to this job */
        jdata->map = map;
    } else {
        if (!map->display_map) {
            map->display_map = orte_rmaps_base.display_map;
        }
    }

    /* go ahead and map the job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base.active_module->map_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if we wanted to display the map, now is the time to do it */
    if (jdata->map->display_map) {
        opal_output(0, "***   JOB MAP FOR JOB %s   ***", ORTE_JOBID_PRINT(jdata->jobid));
        opal_dss.dump(0, jdata->map, ORTE_JOB_MAP);
    }
    
    return ORTE_SUCCESS;
    
CLEANUP:
    if (NULL != map) OBJ_RELEASE(map);
    return rc;
}

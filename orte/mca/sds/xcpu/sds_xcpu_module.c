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
 *
 */

#include "orte_config.h"
//#include <sys/xcpu.h>

#include "orte/orte_constants.h"
#include "orte/util/sys_info.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/xcpu/sds_xcpu.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/errmgr/base/base.h"

orte_sds_base_module_t orte_sds_xcpu_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_xcpu_set_name,
    orte_sds_xcpu_finalize,
};

/**
 * Sets up the process name from the information put into the environment
 * by the xcpu launcher and orte_ns_nds_xcpu_put.
 * @retval ORTE_SUCCESS
 * @retval error
 */
int orte_sds_xcpu_set_name(void)
{
    int rc;
    int id;
    char* name_string = NULL;

    id = mca_base_param_register_string("ns", "nds", "name", NULL, NULL);
    mca_base_param_lookup_string(id, &name_string);
    if(name_string != NULL) {
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(
           &(orte_process_info.my_name),
           name_string))) {
           ORTE_ERROR_LOG(rc);
           free(name_string);
           return rc;
        }
        free(name_string);

    } else {

        orte_jobid_t jobid;
        orte_vpid_t vpid_start;
        char* jobid_string;
        char* vpid_string;
        char *xcpu_rank_string;
        int xcpu_rank;
        int stride;
      
        id = mca_base_param_register_string("ns", "nds", "jobid", NULL, NULL);
        mca_base_param_lookup_string(id, &jobid_string);
        if (NULL == jobid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_jobid(&jobid, jobid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }

        /* XCPUID is set by xcpu when we do a parallel launch */
        xcpu_rank_string = getenv("XCPUID");
        if (NULL == xcpu_rank_string) {
            opal_output(0, "orte_ns_nds_xcpu_get: Error: Environment variable "
                           "XCPUID not found.\n");
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
	xcpu_rank_string = strstr(xcpu_rank_string, "/");

        xcpu_rank = (int)strtol(xcpu_rank_string+1, NULL, 10);

        /* get the non-name common environmental variables */
        if (ORTE_SUCCESS != (rc = orte_sds_env_get())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* RHC: Hmmm...I don't see where the process name actually
         * gets created here....does it always come in as a complete name?
         * If so, then why have the above computation?
         */
        
#if 0
        id = mca_base_param_register_string("ns", "nds", "global_vpid_start", NULL, NULL);
        mca_base_param_lookup_string(id, &vpid_string);
        if (NULL == vpid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        rc = orte_ns.convert_string_to_vpid(&orte_process_info.vpid_start, vpid_string);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }

        if(NULL != orte_system_info.nodename)
            free(orte_system_info.nodename);
        //asprintf(&orte_system_info.nodename, "%d", xcpu_currnode());
#endif
    }
    return ORTE_SUCCESS;
}


int 
orte_sds_xcpu_finalize(void)
{
    return ORTE_SUCCESS;
}

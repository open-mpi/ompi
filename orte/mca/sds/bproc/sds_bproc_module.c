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
#include <sys/bproc.h>

#include "orte/orte_constants.h"
#include "orte/util/sys_info.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/bproc/sds_bproc.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/util/session_dir.h"

orte_sds_base_module_t orte_sds_bproc_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_bproc_set_name,
    orte_sds_bproc_finalize,
};

/**
 * Sets up the process name from the information put into the environment
 * by the bproc launcher and orte_ns_nds_bproc_put.
 * @retval ORTE_SUCCESS
 * @retval error
 */
int orte_sds_bproc_set_name(void)
{
    int rc;
    int id;
    char *name_string = NULL;
    char *jobid_string;
    char *vpid_string;
    char orted_uri[1024];
    bool cleanup_jobid_string, cleanup_vpid_string;
    char *session_dir;
    char *uri_file;
    FILE *fp;
    int local_rank;
    int num_local_procs;
    
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

        /* get the jobid and vpid strings for use later */
        if (ORTE_SUCCESS != (rc = orte_ns.get_jobid_string(&jobid_string, ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_string(&vpid_string, ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        cleanup_jobid_string = true;
        cleanup_vpid_string = true;
        
    } else {

        orte_cellid_t cellid;
        orte_jobid_t jobid;
        orte_vpid_t vpid;
        orte_vpid_t vpid_start;
        char* cellid_string;
        int num_procs;
        char *bproc_rank_string;
        int bproc_rank;
        int stride;
      
        id = mca_base_param_register_string("ns", "nds", "cellid", NULL, NULL);
        mca_base_param_lookup_string(id, &cellid_string);
        if (NULL == cellid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_cellid(&cellid, cellid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
            
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
        cleanup_jobid_string = false;

        /* BPROC_RANK is set by bproc when we do a parallel launch */
        bproc_rank_string = getenv("BPROC_RANK");
        if (NULL == bproc_rank_string) {
            opal_output(0, "orte_ns_nds_bproc_get: Error: Environment variable "
                           "BPROC_RANK not found.\n");
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        bproc_rank = (int)strtol(bproc_rank_string, NULL, 10);

        /* to compute our process name, we need to know two other things: the
         * stride (i.e., the size of the step between vpids in this launch
         * wave) and the starting vpid of this launch. Get those values here
         */
        id = mca_base_param_register_int("pls", "bproc", "stride", NULL, -1);
        mca_base_param_lookup_int(id, &stride);
        if (stride < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        
        id = mca_base_param_register_string("ns", "nds", "vpid_start", NULL, NULL);
        mca_base_param_lookup_string(id, &vpid_string);
        if (NULL == vpid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        rc = orte_ns.convert_string_to_vpid(&vpid_start, vpid_string);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        
        /* compute our vpid */
        vpid = vpid_start + (bproc_rank * stride);
        
        /* create our name */
        if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(
           &(orte_process_info.my_name),
           cellid,
           jobid,
           vpid))) {
           ORTE_ERROR_LOG(rc);
           return rc;
        }

        id = mca_base_param_register_int("ns", "nds", "num_procs", NULL, -1);
        mca_base_param_lookup_int(id, &num_procs);
        if (num_procs < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        orte_process_info.num_procs = (orte_std_cntr_t)num_procs;

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
        asprintf(&orte_system_info.nodename, "%d", bproc_currnode());

        /* ensure the vpid is in the vpid_string in case we need it later */
        if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_string(&vpid_string, ORTE_PROC_MY_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        cleanup_vpid_string = true;
    }
    
    /* if we are NOT a daemon, then lookup our local daemon's contact info
     * and setup that link
     */
    if (!orte_process_info.daemon) {
        /* get the session dir name so we can find the file there */
        if (ORTE_SUCCESS != (rc = orte_session_dir_get_name(&session_dir, NULL, NULL, NULL,
                                                            NULL, NULL, NULL, jobid_string, vpid_string))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* find the file and get the local orted's uri from it */
        uri_file = opal_os_path(false, session_dir, "orted-uri.txt", NULL);
        free(session_dir);
        
        fp = fopen(uri_file, "r");
        if (NULL == fp) {
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
        fgets(orted_uri, 1024, fp);
        orted_uri[strlen(orted_uri)-1] = '\0';
        /* now get the local rank */
        fscanf(fp, "%d", &local_rank);
        orte_process_info.local_rank = (orte_vpid_t)local_rank;
        /* and the number of local procs */
        fscanf(fp, "%d", &num_local_procs);
        orte_process_info.num_local_procs = (orte_std_cntr_t)num_local_procs;
        fclose(fp);
        /* setup the link to the local orted */
        if (ORTE_SUCCESS != (rc = orte_sds_base_contact_orted(orted_uri))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }
        free(uri_file);
    }
    
    if (cleanup_jobid_string) free(jobid_string);
    if (cleanup_vpid_string) free(vpid_string);
    
    return ORTE_SUCCESS;
}


int 
orte_sds_bproc_finalize(void)
{
    return ORTE_SUCCESS;
}

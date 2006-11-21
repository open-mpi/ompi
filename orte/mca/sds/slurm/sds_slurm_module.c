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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <ctype.h>

#include "orte/orte_constants.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/slurm/sds_slurm.h"
#include "orte/util/proc_info.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/base/base.h"
#include "orte/util/sys_info.h"
#include "opal/util/argv.h"

orte_sds_base_module_t orte_sds_slurm_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_slurm_set_name,
    orte_sds_slurm_finalize,
};

static char *get_slurm_nodename(int nodeid);


int
orte_sds_slurm_set_name(void)
{
    int rc;
    int id;
    int vpid_start;
    int num_procs;
    char* name_string = NULL;
    int slurm_nodeid;

    /* start by getting our cellid, jobid, and vpid (which is the
       starting vpid for the list of daemons) */
    id = mca_base_param_register_string("ns", "nds", "name", NULL, NULL);
    mca_base_param_lookup_string(id, &name_string);

    if(name_string != NULL) {
        if (ORTE_SUCCESS != 
            (rc = orte_ns.convert_string_to_process_name(&(orte_process_info.my_name),
                                                              name_string))) {
            ORTE_ERROR_LOG(rc);
            free(name_string);
            return rc;
        }
        free(name_string);

    } else {
        orte_cellid_t cellid;
        orte_jobid_t jobid;
        orte_vpid_t vpid;
        char* cellid_string;
        char* jobid_string;
        char* vpid_string;
      
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
        
        id = mca_base_param_register_string("ns", "nds", "vpid", NULL, NULL);
        mca_base_param_lookup_string(id, &vpid_string);
        if (NULL == vpid_string) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_vpid(&vpid, vpid_string))) {
            ORTE_ERROR_LOG(rc);
            return(rc);
        }

        if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(orte_process_info.my_name),
                                                              cellid,
                                                              jobid,
                                                              vpid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* fix up the base name and make it the "real" name */
    slurm_nodeid = atoi(getenv("SLURM_NODEID"));
    orte_process_info.my_name->vpid += slurm_nodeid;

    /* fix up the system info nodename to match exactly what slurm returned */
    if (NULL != orte_system_info.nodename) {
        free(orte_system_info.nodename);
    }
    orte_system_info.nodename = get_slurm_nodename(slurm_nodeid);

    id = mca_base_param_register_int("ns", "nds", "vpid_start", NULL, -1);
    mca_base_param_lookup_int(id, &vpid_start);
    if (vpid_start < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    id = mca_base_param_register_int("ns", "nds", "num_procs", NULL, -1);
    mca_base_param_lookup_int(id, &num_procs);
    if (num_procs < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    orte_process_info.vpid_start = (orte_vpid_t)vpid_start;
    orte_process_info.num_procs = (size_t)num_procs;
    return ORTE_SUCCESS;
}


int 
orte_sds_slurm_finalize(void)
{
    return ORTE_SUCCESS;
}


static char *
get_slurm_nodename(int nodeid)
{
    char **names = NULL;
    char *slurm_nodelist;
    char *ret;

    slurm_nodelist = getenv("OMPI_MCA_orte_slurm_nodelist");
    
    if (NULL == slurm_nodelist) {
        return NULL;
    }
    
    /* split the node list into an argv array */
    names = opal_argv_split(slurm_nodelist, ',');
    if (NULL == names) {  /* got an error */
        return NULL;
    }

    /* check to see if there are enough entries */
    if (nodeid > opal_argv_count(names)) {
        return NULL;
    }

    ret = strdup(names[nodeid]);

    opal_argv_free(names);

    /* All done */
    return ret;
}

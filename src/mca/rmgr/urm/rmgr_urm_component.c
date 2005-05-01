/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
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
#include "include/orte_constants.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "mca/dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "mca/rds/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/ras/base/base.h"
#include "mca/rmaps/base/base.h"
#include "mca/pls/base/base.h"
#include "mca/rmgr/base/base.h"
#include "mca/rml/rml.h"
#include "rmgr_urm.h"

/*
 * Local functions
 */

static int orte_rmgr_urm_open(void);
static int orte_rmgr_urm_close(void);
static orte_rmgr_base_module_t* orte_rmgr_urm_init(int *priority);


orte_rmgr_urm_component_t mca_rmgr_urm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RMGR_BASE_VERSION_1_0_0,

        "urm", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        orte_rmgr_urm_open,  /* component open  */
        orte_rmgr_urm_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_rmgr_urm_init
    }
};


/**
  * component open/close/init function
  */
static int orte_rmgr_urm_open(void)
{
    int rc;

    /**
     * Open Resource Discovery Subsystem (RDS)
     */
    if (ORTE_SUCCESS != (rc = orte_rds_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Open Resource Allocation Subsystem (RAS)
     */
    if (ORTE_SUCCESS != (rc = orte_ras_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Open Resource Mapping Subsystem (RMAPS)
     */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Open Process Launch Subsystem (PLS)
     */
    if (ORTE_SUCCESS != (rc = orte_pls_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}


static void orte_rmgr_urm_recv(
    int status,
    orte_process_name_t* peer,
    orte_buffer_t* req,
    orte_rml_tag_t tag,
    void* cbdata)
{
    int rc;
    orte_buffer_t rsp;
    OBJ_CONSTRUCT(&rsp, orte_buffer_t);
                                                                                                                          
    if (ORTE_SUCCESS != (rc = orte_rmgr_base_cmd_dispatch(req,&rsp))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
                                                                                                                          
    rc = orte_rml.send_buffer(peer, &rsp, ORTE_RML_TAG_RMGR_CLNT, 0);
    if (rc < 0) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
                                                                                                                          
cleanup:
                                                                                                                          
    rc = orte_rml.recv_buffer_nb(
        ORTE_RML_NAME_ANY,
        ORTE_RML_TAG_RMGR_SVC,
        0,
        orte_rmgr_urm_recv,
        NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&rsp);
}


static orte_rmgr_base_module_t *orte_rmgr_urm_init(int* priority)
{
    int rc;
    char* pls = NULL;
    if(orte_process_info.seed == false) {
        /* if we are bootproxy - need to be selected */
        int id = mca_base_param_register_int("rmgr","bootproxy","jobid",NULL,0);
        int jobid = 0;
        mca_base_param_lookup_int(id,&jobid);
        if(jobid == 0) {
            return NULL;
        }
        /* use fork pls for bootproxy */
        id = mca_base_param_register_string("rmgr","bootproxy","pls",NULL,"fork");
        mca_base_param_lookup_string(id,&pls);
    }

    /**
     * Select RDS components.
     */
    if (ORTE_SUCCESS != (rc = orte_rds_base_select())) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }
    mca_rmgr_urm_component.urm_rds = false;

    /**
     * Select RAS component
     */
    if (NULL == (mca_rmgr_urm_component.urm_ras = orte_ras_base_select(NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }

    /**
     * Select RMAPS component
     */
    if (NULL == (mca_rmgr_urm_component.urm_rmaps = orte_rmaps_base_select(NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }

    /**
     * Select PLS component
     */
    if (NULL == (mca_rmgr_urm_component.urm_pls = orte_pls_base_select(pls))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }

    /* Post non-blocking receive */

    if (0 > (rc = orte_rml.recv_buffer_nb(
        ORTE_RML_NAME_ANY,
        ORTE_RML_TAG_RMGR_SVC,
        0,
        orte_rmgr_urm_recv,
        NULL))) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }

    *priority = 100;
    return &orte_rmgr_urm_module;
}


/**
 *  Close all subsystems.
 */
static int orte_rmgr_urm_close(void)
{
    int rc;

    /**
     * Close Process Launch Subsystem (PLS)
     */
    if (ORTE_SUCCESS != (rc = orte_pls_base_close())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Close Resource Mapping Subsystem (RMAPS)
     */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_close())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Close Resource Allocation Subsystem (RAS)
     */
    if (ORTE_SUCCESS != (rc = orte_ras_base_close())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /**
     * Close Resource Discovery Subsystem (RDS)
     */
    if (ORTE_SUCCESS != (rc = orte_rds_base_close())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

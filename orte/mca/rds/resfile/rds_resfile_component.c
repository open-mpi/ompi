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
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "mca/rds/resfile/rds_resfile.h"

/*
 * Local functions
 */

static int orte_rds_resfile_open(void);
static int orte_rds_resfile_close(void);
static orte_rds_base_module_t* orte_rds_resfile_init(void);


orte_rds_resfile_component_t mca_rds_resfile_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RDS_BASE_VERSION_1_0_0,

        "resfile", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        orte_rds_resfile_open,  /* component open  */
        orte_rds_resfile_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_rds_resfile_init
    }
};


orte_rds_base_module_t orte_rds_resfile_module = {
    orte_rds_resfile_query,
    orte_rds_resfile_finalize
};

/*
 * Instantiate component variables
 */
bool orte_rds_resfile_queried;

/**
 *  Convience functions to lookup MCA parameter values.
 */

static  int orte_rds_resfile_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("rds","resfile",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                                                                       
static char* orte_rds_resfile_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("rds","resfile",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int orte_rds_resfile_open(void)
{
    OBJ_CONSTRUCT(&mca_rds_resfile_component.lock, opal_mutex_t);
    mca_rds_resfile_component.debug = orte_rds_resfile_param_register_int("debug",1);
    mca_rds_resfile_component.filename = orte_rds_resfile_param_register_string("file", NULL);
    orte_rds_resfile_queried = false;
    
    return ORTE_SUCCESS;
}


static orte_rds_base_module_t *orte_rds_resfile_init(void)
{
    OBJ_DESTRUCT(&mca_rds_resfile_component.lock);
    return &orte_rds_resfile_module;
}

/**
 *  Close all subsystems.
 */

static int orte_rds_resfile_close(void)
{
    return ORTE_SUCCESS;
}



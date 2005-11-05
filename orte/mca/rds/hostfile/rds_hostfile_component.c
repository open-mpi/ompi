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
#include "include/orte_constants.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/proc_info.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "mca/rds/hostfile/rds_hostfile.h"

/*
 * Local functions
 */

static int orte_rds_hostfile_open(void);
static int orte_rds_hostfile_close(void);
static orte_rds_base_module_t* orte_rds_hostfile_init(void);


orte_rds_hostfile_component_t mca_rds_hostfile_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RDS_BASE_VERSION_1_0_0,

        "hostfile", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rds_hostfile_open,  /* component open  */
        orte_rds_hostfile_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_rds_hostfile_init
    }
};


/**
  * component open/close/init function
  */
static int orte_rds_hostfile_open(void)
{
#ifdef WIN32
   printf("Unimplemented feature for windows\n");
   return ORTE_ERROR;
#else
    char *path = opal_os_path(false, ORTE_SYSCONFDIR, "openmpi-default-hostfile", NULL);
    OBJ_CONSTRUCT(&mca_rds_hostfile_component.lock, opal_mutex_t);

    mca_base_param_reg_int(&mca_rds_hostfile_component.super.rds_version, "debug",
                           "Toggle debug output for hostfile RDS component",
                           false, false, (int)false,
                           &mca_rds_hostfile_component.debug);
    mca_base_param_reg_string(&mca_rds_hostfile_component.super.rds_version, "path",
                              "ORTE Host filename",
                              false, false, path,
                              &mca_rds_hostfile_component.path);
    
    mca_rds_hostfile_component.default_hostfile = (strcmp(mca_rds_hostfile_component.path,path) == 0);
    free(path);

    return ORTE_SUCCESS;
#endif
}


static orte_rds_base_module_t *orte_rds_hostfile_init(void)
{
    return &orte_rds_hostfile_module;
}

/**
 *  Close all subsystems.
 */

static int orte_rds_hostfile_close(void)
{
    OBJ_DESTRUCT(&mca_rds_hostfile_component.lock);
    if (NULL != mca_rds_hostfile_component.path) {
        free(mca_rds_hostfile_component.path);
    }

    return ORTE_SUCCESS;
}



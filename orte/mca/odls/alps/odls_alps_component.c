/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include <sys/syscall.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/alps/odls_alps.h"

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_odls_base_component_t mca_odls_alps_component = {
    /* First, the mca_component_t struct containing meta information
    about the component itself */
    {
        ORTE_ODLS_BASE_VERSION_2_0_0,
        /* Component name and version */
        "alps",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_odls_alps_component_open,
        orte_odls_alps_component_close,
        orte_odls_alps_component_query,
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


int orte_odls_alps_component_open(void)
{
    return ORTE_SUCCESS;
}

int orte_odls_alps_component_query(mca_base_module_t **module, int *priority)
{
    int rc = ORTE_SUCCESS;
    const char proc_job_file[]="/proc/job";
    FILE *fd = NULL, *fd_task_is_app = NULL;
    char task_is_app_fname[PATH_MAX];

    /*
     * make sure we're in a daemon process
     */

    if (!ORTE_PROC_IS_DAEMON) {
        *priority = 0;
        *module = NULL;
        rc = ORTE_ERROR;
    }

    /*
     * make sure we're in a Cray PAGG container, and that we are also on
     * a compute node (i.e. we are thought of as a application task by
     * the cray job kernel module  - the thing that creates the PAGG
     */

    /* disqualify ourselves if not running in a Cray PAGG container */
    fd = fopen(proc_job_file, "r");
    if (fd == NULL) {
        *priority = 0;
        *module = NULL;
        rc = ORTE_ERROR;
    } else {
        snprintf(task_is_app_fname,sizeof(task_is_app_fname),
                 "/proc/self/task/%ld/task_is_app",syscall(SYS_gettid));
        fd_task_is_app = fopen(task_is_app_fname, "r");
        if (fd_task_is_app != NULL) {   /* okay we're in a PAGG container, 
                                           and we are an app task (not just a process
                                           running on a mom node, for example),
                                           so we should give cray pmi a shot. */
            *priority = 10; /* take precendence over base */
            *module = (mca_base_module_t *) &orte_odls_alps_module;
            fclose(fd_task_is_app);
            rc = orte_odls_alps_get_rdma_creds();
        }
        fclose(fd);
    }

    return rc;
}


int orte_odls_alps_component_close(void)
{
    return ORTE_SUCCESS;
}



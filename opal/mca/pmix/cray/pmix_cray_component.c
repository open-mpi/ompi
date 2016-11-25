/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
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

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/show_help.h"
#include "pmix_cray.h"
#include <sys/syscall.h>
#include <pmi.h>

/*
 * Public string showing the pmix cray component version number
 */
const char *opal_pmix_cray_component_version_string =
    "OPAL cray pmix MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int pmix_cray_component_open(void);
static int pmix_cray_component_query(mca_base_module_t **module, int *priority);
static int pmix_cray_component_close(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_pmix_cray_component_t mca_pmix_cray_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

        .base_version = {
            /* Indicate that we are a pmix v1.1.0 component (which also
               implies a specific MCA version) */

            OPAL_PMIX_BASE_VERSION_2_0_0,

            /* Component name and version */

            .mca_component_name = "cray",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */

            .mca_open_component = pmix_cray_component_open,
            .mca_close_component = pmix_cray_component_close,
            .mca_query_component = pmix_cray_component_query,
        },
        /* Next the MCA v1.0.0 component meta data */
        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },
    .cache_local = NULL,
    .cache_global = NULL,
};

static int pmix_cray_component_open(void)
{
    /*
     * Turns out that there's a lot of reliance on libevent
     * and the default behavior of Cray PMI to fork
     * in a constructor breaks libevent.
     *
     * Open MPI will not launch correctly on Cray XE/XC systems
     * under these conditions:
     *
     * 1) direct launch using aprun, and
     * 2) PMI_NO_FORK env. variable is not set, nor was
     * 3) --disable-dlopen used as part of configury
     *
     * Under SLURM, PMI_NO_FORK is always set, so we can combine
     * the check for conditions 1) and 2) together
     */

#if OPAL_ENABLE_DLOPEN_SUPPORT
    if (NULL == getenv("PMI_NO_FORK")) {
        opal_show_help("help-pmix-cray.txt", "aprun-not-supported", true);
        exit(-1);
    }
#endif
    return OPAL_SUCCESS;
}

static int pmix_cray_component_query(mca_base_module_t **module, int *priority)
{
    int rc;
    const char proc_job_file[]="/proc/job";
    FILE *fd = NULL, *fd_task_is_app = NULL;
    char task_is_app_fname[PATH_MAX];

    /* disqualify ourselves if not running in a Cray PAGG container, or we
       were launched by the orte/mpirun launcher */
    fd = fopen(proc_job_file, "r");
    if ((fd == NULL) || (getenv("OMPI_NO_USE_CRAY_PMI") != NULL)) {
        *priority = 0;
        *module = NULL;
        rc = OPAL_ERROR;
    } else {
        snprintf(task_is_app_fname,sizeof(task_is_app_fname),
                 "/proc/self/task/%ld/task_is_app",syscall(SYS_gettid));
        fd_task_is_app = fopen(task_is_app_fname, "r");
        if (fd_task_is_app != NULL) {   /* okay we're in a PAGG container,
                                           and we are an app task (not just a process
                                           running on a mom node, for example),
                                           so we should give cray pmi a shot. */
            *priority = 90;
            *module = (mca_base_module_t *)&opal_pmix_cray_module;
            fclose(fd_task_is_app);
            rc = OPAL_SUCCESS;
        }
        fclose(fd);
    }

    return rc;
}

static int pmix_cray_component_close(void)
{
    return OPAL_SUCCESS;
}

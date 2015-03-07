/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
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

        {
            /* Indicate that we are a pmix v1.1.0 component (which also
               implies a specific MCA version) */
        
            OPAL_PMIX_BASE_VERSION_2_0_0,

            /* Component name and version */

            "cray",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component open and close functions */

            pmix_cray_component_open,
            pmix_cray_component_close,
            pmix_cray_component_query,
            NULL
        },
        /* Next the MCA v1.0.0 component meta data */
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },
    .cache_local = NULL,
    .cache_global = NULL,
};

static int pmix_cray_component_open(void)
{
    return OPAL_SUCCESS;
}

static int pmix_cray_component_query(mca_base_module_t **module, int *priority)
{
    int rc;
    const char proc_job_file[]="/proc/job";
    FILE *fd = NULL, *fd_task_is_app = NULL;
    char task_is_app_fname[PATH_MAX];

    /* disqualify ourselves if not running in a Cray PAGG container */
    fd = fopen(proc_job_file, "r");
    if (fd == NULL) {
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


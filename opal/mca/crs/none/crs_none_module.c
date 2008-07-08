/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <sys/types.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/runtime/opal_cr.h"

#include "crs_none.h"

int opal_crs_none_module_init(void)
{
    /*
     * If not a tool, and requesting C/R support print a warning.
     */
    if( opal_crs_none_select_warning &&
        !opal_cr_is_tool && opal_cr_is_enabled ) {
        opal_show_help("help-opal-crs-none.txt", "none:select-warning",
                       true);
    }

    return OPAL_SUCCESS;
}

int opal_crs_none_module_finalize(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_none_checkpoint(pid_t pid, opal_crs_base_snapshot_t *snapshot, opal_crs_state_type_t *state)
{
    int ret;

    *state = OPAL_CRS_CONTINUE;
    
    snapshot->component_name  = strdup("none");
    snapshot->reference_name  = strdup("none");
    snapshot->local_location  = strdup("");
    snapshot->remote_location = strdup("");
    snapshot->cold_start      = false;

    /*
     * Update the snapshot metadata
     */
    if( OPAL_SUCCESS != (ret = opal_crs_base_metadata_write_token(NULL, CRS_METADATA_COMP, "none") ) ) {
        opal_output(0,
                    "crs:none: checkpoint(): Error: Unable to write component name to the directory for (%s).",
                    snapshot->reference_name);
        return ret;
    }

    return OPAL_SUCCESS;
}

int opal_crs_none_restart(opal_crs_base_snapshot_t *base_snapshot, bool spawn_child, pid_t *child_pid)
{
    *child_pid = getpid();

    return OPAL_SUCCESS;
}

int opal_crs_none_disable_checkpoint(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_none_enable_checkpoint(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_none_prelaunch(int32_t rank,
                            char *base_snapshot_dir,
                            char **app,
                            char **cwd,
                            char ***argv,
                            char ***env)
{
    char * tmp_env_var = NULL;

    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "0", true, env);
    free(tmp_env_var);
    tmp_env_var = NULL;

    return OPAL_SUCCESS;
}

int opal_crs_none_reg_thread(void)
{
    return OPAL_SUCCESS;
}

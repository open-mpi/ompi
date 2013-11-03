/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 *
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/event/event.h"

#include "orte/constants.h"
#include "orte/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/snapc/snapc.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "sstore_stage.h"

/**********
 * Local Function and Variable Declarations
 **********/

/*
 * stage module
 */
static orte_sstore_base_module_t loc_module = {
    /** Initialization Function */
    orte_sstore_stage_module_init,
    /** Finalization Function */
    orte_sstore_stage_module_finalize,

    orte_sstore_stage_request_checkpoint_handle,
    orte_sstore_stage_request_restart_handle,
    orte_sstore_stage_request_global_snapshot_data,
    orte_sstore_stage_register,
    orte_sstore_stage_get_attr,
    orte_sstore_stage_set_attr,
    orte_sstore_stage_sync,
    orte_sstore_stage_remove,

    orte_sstore_stage_pack,
    orte_sstore_stage_unpack,
    orte_sstore_stage_fetch_app_deps,
    orte_sstore_stage_wait_all_deps
};

/*
 * MCA Functions
 */
int orte_sstore_stage_component_query(mca_base_module_t **module, int *priority)
{
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage: component_query()"));

    /*
     * If the user failed to specify a directory, then skip this component
     */
    if( NULL != orte_sstore_stage_local_snapshot_dir &&
        0 < strlen(orte_sstore_stage_local_snapshot_dir) ) {
        *priority = mca_sstore_stage_component.super.priority;
        *module = (mca_base_module_t *)&loc_module;
    } else {
        *priority = -1;
        *module = NULL;
    }

    return ORTE_SUCCESS;
}

int orte_sstore_stage_module_init(void)
{
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage: module_init()"));

    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_module_init();
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_module_init();
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_module_init();
    }

    return ORTE_SUCCESS;
}

int orte_sstore_stage_module_finalize(void)
{
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage: module_finalize()"));

    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_module_finalize();
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_module_finalize();
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_module_finalize();
    }

    return ORTE_SUCCESS;
}

/******************
 * Local functions
 ******************/
int orte_sstore_stage_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid)
{
    if( orte_sstore_context & ORTE_SSTORE_TOOL_TYPE ) {
        opal_output(0, "sstore:stage:(tool): request_checkpoint_handle() Not supported!");
    }
    else if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_request_checkpoint_handle(handle, seq, jobid);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_request_checkpoint_handle(handle, seq, jobid);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_request_checkpoint_handle(handle, seq, jobid);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_request_restart_handle(orte_sstore_base_handle_t *handle, char *basedir, char *ref, int seq,
                                               orte_sstore_base_global_snapshot_info_t *snapshot)
{
    if( orte_sstore_context & ORTE_SSTORE_TOOL_TYPE ) {
        return orte_sstore_base_tool_request_restart_handle(handle, basedir, ref, seq, snapshot);
    }
    else if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        opal_output(0, "sstore:stage:(global): request_restart_handle() Not supported!");
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        opal_output(0, "sstore:stage:(local): request_restart_handle() Not supported!");
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        opal_output(0, "sstore:stage:(app): request_restart_handle() Not supported!");
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_request_global_snapshot_data(orte_sstore_base_handle_t *handle,
                                                   orte_sstore_base_global_snapshot_info_t *snapshot)
{
    if( orte_sstore_context & ORTE_SSTORE_TOOL_TYPE ) {
        opal_output(0, "sstore:stage:(tool): request_global_snapshot_data() Not supported!");
    }
    else if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_request_global_snapshot_data(handle, snapshot);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        opal_output(0, "sstore:stage:(local): request_global_snapshot_data() Not supported!");
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        opal_output(0, "sstore:stage:(app): request_global_snapshot_data() Not supported!");
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_register(orte_sstore_base_handle_t handle)
{
    if( ORTE_SSTORE_HANDLE_INVALID == handle ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_register(handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_register(handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_register(handle);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value)
{
    if( ORTE_SSTORE_HANDLE_INVALID == handle ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    if( orte_sstore_context & ORTE_SSTORE_TOOL_TYPE ) {
        return orte_sstore_base_tool_get_attr(handle, key, value);
    }
    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_get_attr(handle, key, value);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_get_attr(handle, key, value);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_get_attr(handle, key, value);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value)
{
    if( ORTE_SSTORE_HANDLE_INVALID == handle ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_set_attr(handle, key, value);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_set_attr(handle, key, value);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_set_attr(handle, key, value);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_sync(orte_sstore_base_handle_t handle)
{
    if( ORTE_SSTORE_HANDLE_INVALID == handle ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_sync(handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_sync(handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_sync(handle);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_remove(orte_sstore_base_handle_t handle)
{
    if( ORTE_SSTORE_HANDLE_INVALID == handle ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_remove(handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_remove(handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_remove(handle);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle)
{
    if( ORTE_SSTORE_HANDLE_INVALID == handle ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_pack(peer, buffer, handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_pack(peer, buffer, handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_pack(peer, buffer, handle);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle)
{
    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        return orte_sstore_stage_global_unpack(peer, buffer, handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_unpack(peer, buffer, handle);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        return orte_sstore_stage_app_unpack(peer, buffer, handle);
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_fetch_app_deps(orte_app_context_t *app)
{
    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        opal_output(0, "sstore:stage:(Global): fetch_app_deps() Not supported!");
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_fetch_app_deps(app);
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        opal_output(0, "sstore:stage:(App): fetch_app_deps() Not supported!");
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_sstore_stage_wait_all_deps(void)
{
    if( orte_sstore_context & ORTE_SSTORE_GLOBAL_TYPE ) {
        opal_output(0, "sstore:stage:(Global): wait_all_deps() Not supported!");
    }
    else if( orte_sstore_context & ORTE_SSTORE_LOCAL_TYPE ) {
        return orte_sstore_stage_local_wait_all_deps();
    }
    else if( orte_sstore_context & ORTE_SSTORE_APP_TYPE ) {
        opal_output(0, "sstore:stage:(App): wait_all_deps() Not supported!");
    }

    return ORTE_ERR_NOT_SUPPORTED;
}

/**************************
 * Local functions
 **************************/

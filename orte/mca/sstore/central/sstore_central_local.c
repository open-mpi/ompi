/*
 * Copyright (c)      2011 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "sstore_central.h"

/**********
 * Object stuff
 **********/
#define SSTORE_LOCAL_NONE   0
#define SSTORE_LOCAL_ERROR  1
#define SSTORE_LOCAL_INIT   2
#define SSTORE_LOCAL_READY  3
#define SSTORE_LOCAL_SYNCED 4

struct  orte_sstore_central_local_snapshot_info_t {
    /** List super object */
    opal_list_item_t super;

    /** */
    orte_sstore_base_handle_t id;

    /** Status */
    int status;

    /** Sequence Number */
    int seq_num;

    /** Global Reference Name */
    char * global_ref_name;

    /** Local Location Format String */
    char * location_fmt;

    /* Application info handles*/
    opal_list_t *app_info_handle;
};
typedef struct orte_sstore_central_local_snapshot_info_t orte_sstore_central_local_snapshot_info_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_sstore_central_local_snapshot_info_t);

void orte_sstore_central_local_snapshot_info_construct(orte_sstore_central_local_snapshot_info_t *info);
void orte_sstore_central_local_snapshot_info_destruct( orte_sstore_central_local_snapshot_info_t *info);

OBJ_CLASS_INSTANCE(orte_sstore_central_local_snapshot_info_t,
                   opal_list_item_t,
                   orte_sstore_central_local_snapshot_info_construct,
                   orte_sstore_central_local_snapshot_info_destruct);

struct  orte_sstore_central_local_app_snapshot_info_t {
    /** List super object */
    opal_list_item_t super;

    /** Process Name associated with this entry */
    orte_process_name_t name;

    /** Local Location (Absolute Path) */
    char * local_location;

    /** Metadata File Name (Absolute Path) */
    char * metadata_filename;

    /** CRS Component used */
    char * crs_comp;

    /** If this app. skipped the checkpoint - usually for non-migrating procs */
    bool ckpt_skipped;
};
typedef struct orte_sstore_central_local_app_snapshot_info_t orte_sstore_central_local_app_snapshot_info_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_sstore_central_local_app_snapshot_info_t);

void orte_sstore_central_local_app_snapshot_info_construct(orte_sstore_central_local_app_snapshot_info_t *info);
void orte_sstore_central_local_app_snapshot_info_destruct( orte_sstore_central_local_app_snapshot_info_t *info);

OBJ_CLASS_INSTANCE(orte_sstore_central_local_app_snapshot_info_t,
                   opal_list_item_t,
                   orte_sstore_central_local_app_snapshot_info_construct,
                   orte_sstore_central_local_app_snapshot_info_destruct);



/**********
 * Local Function and Variable Declarations
 **********/
static bool is_global_listener_active = false;
static int sstore_central_local_start_listener(void);
static int sstore_central_local_stop_listener(void);

static int process_global_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info);
static int process_global_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info);
static int process_app_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info);
static int process_app_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info);

static orte_sstore_central_local_snapshot_info_t *create_new_handle_info(orte_sstore_base_handle_t handle);
static orte_sstore_central_local_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle);

static int append_new_app_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info,
                                      orte_process_name_t *name);
static orte_sstore_central_local_app_snapshot_info_t *find_app_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info,
                                                                           orte_process_name_t *name);

static int pull_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info );
static int push_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info );

static int wait_all_apps_updated(orte_sstore_central_local_snapshot_info_t *handle_info);


static opal_list_t *active_handles = NULL;

/**********
 * Object stuff
 **********/
void orte_sstore_central_local_snapshot_info_construct(orte_sstore_central_local_snapshot_info_t *info)
{
    info->id      = 0;

    info->status = SSTORE_LOCAL_NONE;

    info->seq_num = -1;

    info->global_ref_name = NULL;

    info->location_fmt    = NULL;

    info->app_info_handle = OBJ_NEW(opal_list_t);
}

void orte_sstore_central_local_snapshot_info_destruct( orte_sstore_central_local_snapshot_info_t *info)
{
    info->id      = 0;

    info->status = SSTORE_LOCAL_NONE;

    info->seq_num = -1;

    if( NULL != info->global_ref_name ) {
        free( info->global_ref_name );
        info->global_ref_name  = NULL;
    }

    if( NULL != info->location_fmt ) {
        free( info->location_fmt );
        info->location_fmt = NULL;
    }

    if( NULL != info->app_info_handle ) {
        OBJ_RELEASE(info->app_info_handle);
        info->app_info_handle = NULL;
    }
}

void orte_sstore_central_local_app_snapshot_info_construct(orte_sstore_central_local_app_snapshot_info_t *info)
{
    info->name.jobid = ORTE_JOBID_INVALID;
    info->name.vpid  = ORTE_VPID_INVALID;

    info->local_location = NULL;
    info->metadata_filename = NULL;
    info->crs_comp = NULL;
    info->ckpt_skipped = false;
}

void orte_sstore_central_local_app_snapshot_info_destruct( orte_sstore_central_local_app_snapshot_info_t *info)
{
    info->name.jobid = ORTE_JOBID_INVALID;
    info->name.vpid  = ORTE_VPID_INVALID;

    if( NULL != info->local_location ) {
        free(info->local_location);
        info->local_location = NULL;
    }

    if( NULL != info->metadata_filename ) {
        free(info->metadata_filename);
        info->metadata_filename = NULL;
    }

    if( NULL != info->crs_comp ) {
        free(info->crs_comp);
        info->crs_comp = NULL;
    }

    info->ckpt_skipped = false;
}

/******************
 * Local functions
 ******************/
int orte_sstore_central_local_module_init(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): init()"));

    if( NULL == active_handles ) {
        active_handles = OBJ_NEW(opal_list_t);
    }

    /*
     * Setup a listener for the HNP/Apps
     * We could be the HNP, in which case the listener is already registered.
     */
    if( !ORTE_PROC_IS_HNP ) {
        if( ORTE_SUCCESS != (ret = sstore_central_local_start_listener()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_local_module_finalize(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): finalize()"));

    if( NULL != active_handles ) {
        OBJ_RELEASE(active_handles);
    }

    /*
     * Shutdown the listener for the HNP/Apps
     * We could be the HNP, in which case the listener is already deregistered.
     */
    if( !ORTE_PROC_IS_HNP ) {
        if( ORTE_SUCCESS != (ret = sstore_central_local_stop_listener()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_local_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid)
{
    opal_output(0, "sstore:central:(local): request_checkpoint_handle() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_central_local_register(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_local_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): register()"));

    /*
     * Create a handle
     */
    if( NULL == (handle_info = find_handle_info(handle)) ) {
        handle_info = create_new_handle_info(handle);
    }

    /*
     * Get basic information from Global SStore
     */
    if( ORTE_SUCCESS != (ret = pull_handle_info(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Wait here until the pull request has been satisfied
     */
    while(SSTORE_LOCAL_READY != handle_info->status &&
          SSTORE_LOCAL_ERROR != handle_info->status ) {
        opal_progress();
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_local_get_attr(orte_sstore_base_handle_t handle,  orte_sstore_base_key_t key, char **value)
{
    opal_output(0, "sstore:central:(local): get_attr() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_central_local_set_attr(orte_sstore_base_handle_t handle,  orte_sstore_base_key_t key, char *value)
{
    opal_output(0, "sstore:central:(local): set_attr() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_central_local_sync(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_local_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): sync()"));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);

    /*
     * Wait for all of the applications to update their metadata
     */
    if( ORTE_SUCCESS != (ret = wait_all_apps_updated(handle_info))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Push information to the Global coordinator
     */
    if( ORTE_SUCCESS != (ret = push_handle_info(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    handle_info->status = SSTORE_LOCAL_SYNCED;

 cleanup:
    return exit_status;
}

int orte_sstore_central_local_remove(orte_sstore_base_handle_t handle)
{
    opal_output(0, "sstore:central:(local): remove() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_central_local_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): pack()"));

    /*
     * Lookup the handle
     */


    /*
     * Pack the handle ID
     */
    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &handle, 1, ORTE_SSTORE_HANDLE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Pack any metadata
     */

 cleanup:
    return exit_status;
}

int orte_sstore_central_local_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_local_snapshot_info_t *handle_info = NULL;
    orte_std_cntr_t count;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): unpack()"));

    /*
     * Unpack the handle id
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, handle, &count, ORTE_SSTORE_HANDLE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Lookup the handle
     */
    if( NULL == (handle_info = find_handle_info(*handle)) ) {
        handle_info = create_new_handle_info(*handle);
    }

    /*
     * Unpack the metadata piggybacked on this message
     */
    if( ORTE_SUCCESS != (ret = process_global_push(peer, buffer, handle_info))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): unpack(%d, %d, %s)",
                         handle_info->id,
                         handle_info->seq_num,
                         handle_info->global_ref_name));

 cleanup:
    return exit_status;
}

void orte_sstore_central_local_recv(int status,
                                    orte_process_name_t* sender,
                                    opal_buffer_t* buffer,
                                    orte_rml_tag_t tag,
                                    void* cbdata)
{
    int ret;
    orte_sstore_central_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_sstore_base_handle_t loc_id;
    orte_sstore_central_local_snapshot_info_t *handle_info = NULL;

    if( ORTE_RML_TAG_SSTORE_INTERNAL != tag ) {
        return;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): process_cmd(%s)",
                         ORTE_NAME_PRINT(sender)));

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SSTORE_CENTRAL_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &loc_id, &count, ORTE_SSTORE_HANDLE )) ) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /*
     * Find the referenced handle (Create if it does not exist)
     */
    if(NULL == (handle_info = find_handle_info(loc_id)) ) {
        handle_info = create_new_handle_info(loc_id);
    }

    /*
     * Process the command
     */
    if( ORTE_SSTORE_CENTRAL_PULL == command ) {
        if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, sender)) {
            process_global_pull(sender, buffer, handle_info);
        } else {
            process_app_pull(sender, buffer, handle_info);
        }
    }
    else if( ORTE_SSTORE_CENTRAL_PUSH == command ) {
        if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, sender)) {
            process_global_push(sender, buffer, handle_info);
        } else {
            process_app_push(sender, buffer, handle_info);
        }
    }

 cleanup:
    return;
}

/**************************
 * Local functions
 **************************/
static orte_sstore_central_local_snapshot_info_t *create_new_handle_info(orte_sstore_base_handle_t handle)
{
    orte_sstore_central_local_snapshot_info_t *handle_info = NULL;
    int i;
    orte_proc_t *child = NULL;

    if( NULL == active_handles ) {
        active_handles = OBJ_NEW(opal_list_t);
    }

    handle_info = OBJ_NEW(orte_sstore_central_local_snapshot_info_t);

    handle_info->id = handle;

    opal_list_append(active_handles, &(handle_info->super));

    /*
     * Create a sub structure for each child
     */
    for (i=0; i < orte_local_children->size; i++) {
	    if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        append_new_app_handle_info(handle_info, &child->name);
    }

    handle_info->status = SSTORE_LOCAL_INIT;

    return handle_info;
}

static orte_sstore_central_local_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle)
{
    orte_sstore_central_local_snapshot_info_t *handle_info = NULL;
    opal_list_item_t* item = NULL;

    if( NULL == active_handles ) {
        return NULL;
    }

    for(item  = opal_list_get_first(active_handles);
        item != opal_list_get_end(active_handles);
        item  = opal_list_get_next(item) ) {
        handle_info = (orte_sstore_central_local_snapshot_info_t*)item;

        if( handle_info->id == handle ) {
            return handle_info;
        }
    }

    return NULL;
}

static int append_new_app_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info,
                                      orte_process_name_t *name)
{
    orte_sstore_central_local_app_snapshot_info_t *app_info = NULL;

    app_info = OBJ_NEW(orte_sstore_central_local_app_snapshot_info_t);

    app_info->name.jobid = name->jobid;
    app_info->name.vpid  = name->vpid;

    opal_list_append(handle_info->app_info_handle, &(app_info->super));

    return ORTE_SUCCESS;
}

static orte_sstore_central_local_app_snapshot_info_t *find_app_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info,
                                                                           orte_process_name_t *name)
{
    orte_sstore_central_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t* item = NULL;
    orte_ns_cmp_bitmask_t mask;

    for(item  = opal_list_get_first(handle_info->app_info_handle);
        item != opal_list_get_end(handle_info->app_info_handle);
        item  = opal_list_get_next(item) ) {
        app_info = (orte_sstore_central_local_app_snapshot_info_t*)item;

        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &app_info->name, name)) {
            return app_info;
        }
    }

    return NULL;
}

static int sstore_central_local_start_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if( is_global_listener_active ) {
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_SSTORE_INTERNAL,
                                                       ORTE_RML_PERSISTENT,
                                                       orte_sstore_central_local_recv,
                                                       NULL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    is_global_listener_active = true;
    
 cleanup:
    return exit_status;
}

static int sstore_central_local_stop_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if (ORTE_SUCCESS != (ret = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                    ORTE_RML_TAG_SSTORE_INTERNAL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    is_global_listener_active = false;
    
 cleanup:
    return exit_status;
}

static int process_global_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info)
{
    /* JJH should be as simple as calling push_handle_info() */
    opal_output(0, "sstore:central:(local): process_global_pull() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int process_global_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_sstore_central_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t* item = NULL;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(handle_info->seq_num), &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(handle_info->global_ref_name), &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(handle_info->location_fmt), &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * For each process we are working with
     */
    for(item  = opal_list_get_first(handle_info->app_info_handle);
        item != opal_list_get_end(handle_info->app_info_handle);
        item  = opal_list_get_next(item) ) {
        app_info = (orte_sstore_central_local_app_snapshot_info_t*)item;

        if( NULL != app_info->local_location ) {
            free(app_info->local_location);
            app_info->local_location = NULL;
        }
        asprintf(&(app_info->local_location), handle_info->location_fmt, app_info->name.vpid);

        if( NULL != app_info->metadata_filename ) {
            free(app_info->metadata_filename);
            app_info->metadata_filename = NULL;
        }
        asprintf(&(app_info->metadata_filename), "%s/%s",
                 app_info->local_location,
                 orte_sstore_base_local_metadata_filename);
    }

 cleanup:
    if( ORTE_SUCCESS == exit_status ) {
        handle_info->status = SSTORE_LOCAL_READY;
    } else {
        handle_info->status = SSTORE_LOCAL_ERROR;
    }

    return exit_status;
}

static int process_app_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t loc_buffer;
    orte_sstore_central_cmd_flag_t command;
    orte_sstore_central_local_app_snapshot_info_t *app_info = NULL;

    /*
     * Find this app's data
     */
    app_info = find_app_handle_info(handle_info, peer);

    /*
     * Push back the requested information
     */
    OBJ_CONSTRUCT(&loc_buffer, opal_buffer_t);

    command = ORTE_SSTORE_CENTRAL_PUSH;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &command, 1, ORTE_SSTORE_CENTRAL_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(handle_info->id), 1, ORTE_SSTORE_HANDLE )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(handle_info->seq_num), 1, OPAL_INT )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(handle_info->global_ref_name), 1, OPAL_STRING )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(app_info->local_location), 1, OPAL_STRING )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(app_info->metadata_filename), 1, OPAL_STRING )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, &loc_buffer, ORTE_RML_TAG_SSTORE_INTERNAL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&loc_buffer);

    return exit_status;
}

static int process_app_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_sstore_central_local_app_snapshot_info_t *app_info = NULL;

    /*
     * Find this app's data
     */
    app_info = find_app_handle_info(handle_info, peer);

    /*
     * Unpack the data
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(app_info->ckpt_skipped), &count, OPAL_BOOL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( !app_info->ckpt_skipped ) {
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(app_info->crs_comp), &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(local): app_push(%s, skip=%s, %s)",
                         ORTE_NAME_PRINT(&(app_info->name)),
                         (app_info->ckpt_skipped ? "T" : "F"),
                         app_info->crs_comp));

 cleanup:
    return exit_status;
}

static int wait_all_apps_updated(orte_sstore_central_local_snapshot_info_t *handle_info)
{
    orte_sstore_central_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t *item = NULL;
    bool is_done = true;

    do {
        is_done = true;
        for(item  = opal_list_get_first(handle_info->app_info_handle);
            item != opal_list_get_end(handle_info->app_info_handle);
            item  = opal_list_get_next(item) ) {
            app_info = (orte_sstore_central_local_app_snapshot_info_t*)item;

            if( NULL == app_info->crs_comp && !app_info->ckpt_skipped ) {
                is_done = false;
                break;
            }
        }

        if( !is_done ) {
            OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                                 "sstore:central:(local): Waiting for appliccation %s",
                                 ORTE_NAME_PRINT(&(app_info->name)) ));
            opal_progress();
        }
    } while(!is_done);

    return ORTE_SUCCESS;
}

static int pull_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info )
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_sstore_central_cmd_flag_t command;

    /*
     * Check to see if this is necessary
     * (Did we get all of the info from the handle unpack?)
     */
    if( 0 <= handle_info->seq_num &&
        NULL != handle_info->global_ref_name &&
        NULL != handle_info->location_fmt ) {
        handle_info->status = SSTORE_LOCAL_READY;
        return ORTE_SUCCESS;
    }

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    /*
     * Ask the daemon to send us the info that we need
     */
    command = ORTE_SSTORE_CENTRAL_PULL;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SSTORE_CENTRAL_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(handle_info->id), 1, ORTE_SSTORE_HANDLE )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SSTORE_INTERNAL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

static int push_handle_info(orte_sstore_central_local_snapshot_info_t *handle_info )
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_sstore_central_cmd_flag_t command;
    orte_sstore_central_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t *item = NULL;
    size_t list_size;

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    command = ORTE_SSTORE_CENTRAL_PUSH;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SSTORE_CENTRAL_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(handle_info->id), 1, ORTE_SSTORE_HANDLE )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    list_size = opal_list_get_size(handle_info->app_info_handle);
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &list_size, 1, OPAL_SIZE )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * For each process we are working with
     */
    for(item  = opal_list_get_first(handle_info->app_info_handle);
        item != opal_list_get_end(handle_info->app_info_handle);
        item  = opal_list_get_next(item) ) {
        app_info = (orte_sstore_central_local_app_snapshot_info_t*)item;

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(app_info->name), 1, ORTE_NAME )) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(app_info->ckpt_skipped), 1, OPAL_BOOL )) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if( !app_info->ckpt_skipped ) {
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(app_info->crs_comp), 1, OPAL_STRING )) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SSTORE_INTERNAL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

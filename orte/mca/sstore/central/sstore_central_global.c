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
#include "opal/util/os_dirpath.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "sstore_central.h"

#define SSTORE_HANDLE_TYPE_NONE    0
#define SSTORE_HANDLE_TYPE_CKPT    1
#define SSTORE_HANDLE_TYPE_RESTART 2

#define SSTORE_GLOBAL_NONE    0
#define SSTORE_GLOBAL_ERROR   1
#define SSTORE_GLOBAL_INIT    2
#define SSTORE_GLOBAL_REG     3
#define SSTORE_GLOBAL_SYNCING 4
#define SSTORE_GLOBAL_SYNCED  5

/**********
 * Object Stuff
 **********/
struct  orte_sstore_central_global_snapshot_info_t {
    /** List super object */
    opal_list_item_t super;

    /** */
    orte_sstore_base_handle_t id;

    /** Job ID */
    orte_jobid_t jobid;

    /** State */
    int state;

    /** Handle type */
    int handle_type;

    /** Sequence Number */
    int seq_num;

    /** Reference Name */
    char * ref_name;

    /** Local Location (Relative Path to base_location) */
    char * local_location;

    /** Application location format */
    char * app_location_fmt;

    /** Base location */
    char * base_location;

    /** Metadata File Name */
    char *metadata_filename;

    /** Metadata File Descriptor */
    FILE *metadata;

    /** Num procs in job */
    int num_procs_total;

    /** Num procs synced */
    int num_procs_synced;

    /** Is this checkpoint representing a migration? */
    bool migrating;
};
typedef struct orte_sstore_central_global_snapshot_info_t orte_sstore_central_global_snapshot_info_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_sstore_central_global_snapshot_info_t);

void orte_sstore_central_global_snapshot_info_construct(orte_sstore_central_global_snapshot_info_t *info);
void orte_sstore_central_global_snapshot_info_destruct( orte_sstore_central_global_snapshot_info_t *info);

OBJ_CLASS_INSTANCE(orte_sstore_central_global_snapshot_info_t,
                   opal_list_item_t,
                   orte_sstore_central_global_snapshot_info_construct,
                   orte_sstore_central_global_snapshot_info_destruct);


/**********
 * Local Function and Variable Declarations
 **********/
static bool is_global_listener_active = false;
static int sstore_central_global_start_listener(void);
static int sstore_central_global_stop_listener(void);
static void sstore_central_global_recv(int status,
                                       orte_process_name_t* sender,
                                       opal_buffer_t* buffer,
                                       orte_rml_tag_t tag,
                                       void* cbdata);
static void sstore_central_global_process_cmd(int fd,
                                              short event,
                                              void *cbdata);
static int process_local_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_global_snapshot_info_t *handle_info);
static int process_local_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_global_snapshot_info_t *handle_info);

static orte_sstore_central_global_snapshot_info_t *create_new_handle_info(int seq, int type, orte_jobid_t jobid);
static orte_sstore_central_global_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle);
static orte_sstore_central_global_snapshot_info_t *find_handle_info_from_ref(char *ref, int seq);

static int metadata_open(orte_sstore_central_global_snapshot_info_t * handle_info);
static int metadata_close(orte_sstore_central_global_snapshot_info_t * handle_info);
static int metadata_write_int(orte_sstore_central_global_snapshot_info_t * handle_info, char * key, int value);
static int metadata_write_str(orte_sstore_central_global_snapshot_info_t * handle_info, char * key, char *value);
static int metadata_write_timestamp(orte_sstore_central_global_snapshot_info_t * handle_info);

static int init_global_snapshot_directory(orte_sstore_central_global_snapshot_info_t *handle_info);
static int central_snapshot_sort_compare_fn(opal_list_item_t **a,
                                            opal_list_item_t **b);
static int orte_sstore_central_extract_global_metadata(orte_sstore_central_global_snapshot_info_t * handle_info,
                                                       orte_sstore_base_global_snapshot_info_t *global_snapshot);

static int next_handle_id = 1;

static opal_list_t *active_handles = NULL;

/**********
 * Object stuff
 **********/
void orte_sstore_central_global_snapshot_info_construct(orte_sstore_central_global_snapshot_info_t *info)
{
    info->id      = next_handle_id;
    next_handle_id++;

    info->jobid = ORTE_JOBID_INVALID;

    info->state = SSTORE_GLOBAL_NONE;

    info->handle_type = SSTORE_HANDLE_TYPE_NONE;

    info->seq_num = -1;

    info->base_location  = strdup(orte_sstore_base_global_snapshot_dir);

    info->ref_name       = NULL;
    info->local_location = NULL;
    info->app_location_fmt = NULL;

    info->metadata_filename = NULL;
    info->metadata = NULL;

    info->num_procs_total = 0;
    info->num_procs_synced = 0;

    info->migrating = false;
}

void orte_sstore_central_global_snapshot_info_destruct( orte_sstore_central_global_snapshot_info_t *info)
{
    info->id      = 0;
    info->seq_num = -1;

    info->jobid = ORTE_JOBID_INVALID;

    info->state = SSTORE_GLOBAL_NONE;

    info->handle_type = SSTORE_HANDLE_TYPE_NONE;

    if( NULL != info->ref_name ) {
        free( info->ref_name );
        info->ref_name  = NULL;
    }

    if( NULL != info->local_location ) {
        free( info->local_location );
        info->local_location = NULL;
    }

    if( NULL != info->app_location_fmt ) {
        free( info->app_location_fmt );
        info->app_location_fmt = NULL;
    }

    if( NULL != info->base_location ) {
        free( info->base_location );
        info->base_location = NULL;
    }

    if( NULL != info->metadata_filename ) {
        free( info->metadata_filename ) ;
        info->metadata_filename = NULL;
    }

    if( NULL != info->metadata ) {
        fclose(info->metadata);
        info->metadata = NULL;
    }

    info->num_procs_total = 0;
    info->num_procs_synced = 0;

    info->migrating = false;
}

/******************
 * Local functions
 ******************/
int orte_sstore_central_global_module_init(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): init()"));

    if( NULL == active_handles ) {
        active_handles = OBJ_NEW(opal_list_t);
    }

    /*
     * Setup a listener for the HNP/Apps
     */
    if( ORTE_SUCCESS != (ret = sstore_central_global_start_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    exit_status = orte_sstore_central_local_module_init();

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_module_finalize(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): finalize()"));

    exit_status = orte_sstore_central_local_module_finalize();

    if( NULL != active_handles ) {
        OBJ_RELEASE(active_handles);
    }

    /*
     * Shutdown the listener for the HNP/Apps
     */
    if( ORTE_SUCCESS != (ret = sstore_central_global_stop_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): request_checkpoint_handle()"));

    /*
     * Construct a handle
     *  - Associate all of the necessary information
     */
    handle_info = create_new_handle_info(seq, SSTORE_HANDLE_TYPE_CKPT, jobid);

    /*
     * Create the global checkpoint directory
     */
    if( ORTE_SUCCESS != (ret = init_global_snapshot_directory(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Return the handle
     */
    *handle = handle_info->id;

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_request_restart_handle(orte_sstore_base_handle_t *handle, char *basedir,
                                                      char *ref, int seq,
                                                      orte_sstore_base_global_snapshot_info_t *snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    handle_info = find_handle_info_from_ref(ref, seq);
    if( NULL == handle_info ) {
        ret = ORTE_ERROR;
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    *handle = handle_info->id;

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_request_global_snapshot_data(orte_sstore_base_handle_t *handle,
                                                            orte_sstore_base_global_snapshot_info_t *snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): request_global_snapshot_data()"));

    /*
     * Lookup the handle (if NULL, use last stable)
     */
    if( NULL != handle ) {
        handle_info = find_handle_info(*handle);
        snapshot->ss_handle = *handle;
    } else {
        handle_info = find_handle_info(orte_sstore_handle_last_stable);
        snapshot->ss_handle = orte_sstore_handle_last_stable;
    }

    /*
     * Construct the snapshot from local data, and metadata file
     */
    snapshot->seq_num   = handle_info->seq_num;
    snapshot->reference = strdup(handle_info->ref_name);
    snapshot->basedir   = strdup(handle_info->base_location); 
    snapshot->metadata_filename = strdup(handle_info->metadata_filename);

    /* If this is the current checkpoint, pull data from local cache */
    if( orte_sstore_handle_current == snapshot->ss_handle ) {
        if( ORTE_SUCCESS != (ret = orte_sstore_central_extract_global_metadata(handle_info, snapshot)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    /* Otherwise, pull from metadata */
    else {
        if( ORTE_SUCCESS != (ret = orte_sstore_base_extract_global_metadata(snapshot)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    opal_list_sort(&snapshot->local_snapshots, central_snapshot_sort_compare_fn);

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_register(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): register(%d) - Global", handle));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);
    if( SSTORE_GLOBAL_REG != handle_info->state ) {
        handle_info->state = SSTORE_GLOBAL_REG;
    } else {
        return orte_sstore_central_local_register(handle);
    }

    orte_sstore_handle_current = handle;

    /*
     * Associate the metadata
     */
    if( handle_info->migrating ) {
        if( ORTE_SUCCESS != (ret = metadata_write_int(handle_info,
                                                      SSTORE_METADATA_INTERNAL_MIG_SEQ_STR,
                                                      handle_info->seq_num)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    } else {
        if( ORTE_SUCCESS != (ret = metadata_write_int(handle_info,
                                                      SSTORE_METADATA_GLOBAL_SNAP_SEQ_STR,
                                                      handle_info->seq_num)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    if( ORTE_SUCCESS != (ret = metadata_write_str(handle_info,
                                                  SSTORE_METADATA_LOCAL_SNAP_REF_FMT_STR,
                                                  orte_sstore_base_local_snapshot_fmt)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = metadata_write_timestamp(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value)
{
    int exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): get_attr()"));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);

    /*
     * Access metadata
     */
    if( SSTORE_METADATA_GLOBAL_SNAP_REF == key ) {
        *value = strdup(handle_info->ref_name);
    }
    else if( SSTORE_METADATA_GLOBAL_SNAP_SEQ == key ) {
        asprintf(value, "%d", handle_info->seq_num);
    }
    else if( SSTORE_METADATA_LOCAL_SNAP_REF_FMT == key ) {
        *value = strdup(orte_sstore_base_local_snapshot_fmt);
    }
    /* 'central' does not cache, so these are the same */
    else if( SSTORE_METADATA_LOCAL_SNAP_LOC       == key ) {
        asprintf(value, "%s/%s/%d",
                 handle_info->base_location,
                 handle_info->ref_name,
                 handle_info->seq_num);
    }
    else if( SSTORE_METADATA_LOCAL_SNAP_REF_LOC_FMT == key ) {
        asprintf(value, "%s/%s/%d/%s",
                 handle_info->base_location,
                 handle_info->ref_name,
                 handle_info->seq_num,
                 orte_sstore_base_local_snapshot_fmt);
    }
    else {
        exit_status = ORTE_ERR_NOT_SUPPORTED;
    }

    return exit_status;
}

int orte_sstore_central_global_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;
    char *key_str = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): set_attr()"));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);

    /*
     * Process key (Access metadata)
     */
    if( key == SSTORE_METADATA_GLOBAL_MIGRATING ) {
        handle_info->migrating = true;
    }
    else {
        orte_sstore_base_convert_key_to_string(key, &key_str);
        if( NULL == key_str ) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }

        if( ORTE_SUCCESS != (ret = metadata_write_str(handle_info, key_str, value))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    if( NULL != key_str ) {
        free(key_str);
        key_str = NULL;
    }

    return exit_status;
}

int orte_sstore_central_global_sync(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): sync()"));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);
    if( SSTORE_GLOBAL_SYNCING != handle_info->state ) {
        handle_info->state = SSTORE_GLOBAL_SYNCING;
        if( ORTE_SNAPC_LOCAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_LOCAL_COORD_TYPE) ) {
            return orte_sstore_central_local_sync(handle);
        }
    }

    /*
     * Synchronize all of the files
     */
    while(handle_info->num_procs_synced < handle_info->num_procs_total) {
        opal_progress();
    }

    /*
     * Finalize and close the metadata
     */
    if( ORTE_SUCCESS != (ret = metadata_write_timestamp(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( handle_info->migrating ) {
        if( ORTE_SUCCESS != (ret = metadata_write_int(handle_info,
                                                      SSTORE_METADATA_INTERNAL_DONE_MIG_SEQ_STR,
                                                      handle_info->seq_num)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    } else {
        if( ORTE_SUCCESS != (ret = metadata_write_int(handle_info,
                                                      SSTORE_METADATA_INTERNAL_DONE_SEQ_STR,
                                                      handle_info->seq_num)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    if( ORTE_SUCCESS != (ret = metadata_close(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /* JJH: We should lock this var! */
    if( !handle_info->migrating ) {
        orte_sstore_base_is_checkpoint_available = true;
        orte_sstore_handle_last_stable = orte_sstore_handle_current;
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_remove(orte_sstore_base_handle_t handle)
{
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): remove()"));

    /*
     * Lookup the handle
     */

    return ORTE_SUCCESS;
}

int orte_sstore_central_global_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): pack()"));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);

    /*
     * Pack the handle ID
     */
    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &handle, 1, ORTE_SSTORE_HANDLE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): pack(%d, %d, %s)",
                         handle_info->id,
                         handle_info->seq_num,
                         handle_info->ref_name));

    /*
     * Pack any metadata
     */
    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(handle_info->seq_num), 1, OPAL_INT )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(handle_info->ref_name), 1, OPAL_STRING )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(handle_info->app_location_fmt), 1, OPAL_STRING )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_global_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): unpack()"));

    /*
     * Unpack the handle id
     */
    if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_JOBID,
                                                   ORTE_PROC_MY_NAME,
                                                   peer)) {
        /*
         * Differ to the orted version, so if we have application then they get updated too
         */
        if( ORTE_SUCCESS != (ret = orte_sstore_central_local_unpack(peer, buffer, handle)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}

/**************************
 * Local functions
 **************************/
static orte_sstore_central_global_snapshot_info_t *create_new_handle_info(int seq, int type, orte_jobid_t jobid)
{
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;
    orte_job_t *jdata = NULL;

    handle_info = OBJ_NEW(orte_sstore_central_global_snapshot_info_t);

    handle_info->jobid = jobid;

    handle_info->state = SSTORE_GLOBAL_INIT;

    handle_info->handle_type = type;

    handle_info->seq_num = seq;

    orte_sstore_base_get_global_snapshot_ref(&(handle_info->ref_name), getpid());

    asprintf(&(handle_info->local_location), "%s/%d",
             handle_info->ref_name, handle_info->seq_num);

    asprintf(&(handle_info->app_location_fmt), "%s/%s/%s",
             handle_info->base_location,
             handle_info->local_location,
             orte_sstore_base_local_snapshot_fmt);

    asprintf(&(handle_info->metadata_filename), "%s/%s/%s",
             handle_info->base_location,
             handle_info->ref_name,
             orte_sstore_base_global_metadata_filename);

    jdata = orte_get_job_data_object(handle_info->jobid);
    handle_info->num_procs_total = (int)jdata->num_procs;

    opal_list_append(active_handles, &(handle_info->super));

    return handle_info;
}

static orte_sstore_central_global_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle)
{
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(active_handles);
        item != opal_list_get_end(active_handles);
        item  = opal_list_get_next(item) ) {
        handle_info = (orte_sstore_central_global_snapshot_info_t*)item;

        if( handle_info->id == handle ) {
            return handle_info;
        }
    }

    return NULL;
}

static orte_sstore_central_global_snapshot_info_t *find_handle_info_from_ref(char *ref, int seq)
{
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(active_handles);
        item != opal_list_get_end(active_handles);
        item  = opal_list_get_next(item) ) {
        handle_info = (orte_sstore_central_global_snapshot_info_t*)item;

        if( handle_info->seq_num == seq ) {
            if( NULL != ref &&
                strncmp(handle_info->ref_name, ref, strlen(ref)) ) {
                return handle_info;
            } else {
                return handle_info;
            }
        }
    }

    return NULL;
}

static int sstore_central_global_start_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if( is_global_listener_active ) {
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_SSTORE_INTERNAL,
                                                       ORTE_RML_PERSISTENT,
                                                       sstore_central_global_recv,
                                                       NULL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    is_global_listener_active = true;
    
 cleanup:
    return exit_status;
}

static int sstore_central_global_stop_listener(void)
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

static void sstore_central_global_recv(int status,
                                       orte_process_name_t* sender,
                                       opal_buffer_t* buffer,
                                       orte_rml_tag_t tag,
                                       void* cbdata)
{
    int ret;
    orte_sstore_central_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_sstore_base_handle_t loc_id;
    orte_sstore_central_global_snapshot_info_t *handle_info = NULL;

    if( ORTE_RML_TAG_SSTORE_INTERNAL != tag ) {
        return;
    }

    /*
     * If this was an application process contacting us, then act like an orted
     * instead of an HNP
     */
    if(OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_JOBID,
                                                   ORTE_PROC_MY_NAME,
                                                   sender)) {
        orte_sstore_central_local_recv(status, sender, buffer, tag, cbdata);
        return;
    }


    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(global): process_cmd(%s)",
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
     * Find the referenced handle
     */
    if(NULL == (handle_info = find_handle_info(loc_id)) ) {
        ; /* JJH big problem */
    }

    /*
     * Process the command
     */
    if( ORTE_SSTORE_CENTRAL_PULL == command ) {
        process_local_pull(sender, buffer, handle_info);
    }
    else if( ORTE_SSTORE_CENTRAL_PUSH == command ) {
        process_local_push(sender, buffer, handle_info);
    }

 cleanup:
    return;
}

static int process_local_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_global_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t loc_buffer;
    orte_sstore_central_cmd_flag_t command;

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

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(handle_info->ref_name), 1, OPAL_STRING )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(handle_info->app_location_fmt), 1, OPAL_STRING )) ) {
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

static int process_local_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_central_global_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    size_t num_entries, i;
    orte_process_name_t name;
    bool ckpt_skipped = false;
    char * crs_comp = NULL;
    char * proc_name = NULL;

    /*
     * Unpack the data
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &num_entries, &count, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    for(i = 0; i < num_entries; ++i ) {
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &name, &count, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &ckpt_skipped, &count, OPAL_BOOL))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if( !ckpt_skipped ) {
            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &crs_comp, &count, OPAL_STRING))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            /*
             * Write this information to the global metadata
             */
            orte_util_convert_process_name_to_string(&proc_name, &name);

            metadata_write_str(handle_info,
                               SSTORE_METADATA_INTERNAL_PROCESS_STR,
                               proc_name);
            metadata_write_str(handle_info,
                               SSTORE_METADATA_LOCAL_CRS_COMP_STR,
                               crs_comp);
        }

        if( NULL != crs_comp ) {
            free(crs_comp);
            crs_comp = NULL;
        }
        if( NULL != proc_name ) {
            free(proc_name);
            proc_name = NULL;
        }

        (handle_info->num_procs_synced)++;
    }

 cleanup:
    if( NULL != crs_comp ) {
        free(crs_comp);
        crs_comp = NULL;
    }
    if( NULL != proc_name ) {
        free(proc_name);
        proc_name = NULL;
    }

    return exit_status;
}

static int init_global_snapshot_directory(orte_sstore_central_global_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    char * dir_name = NULL;
    mode_t my_mode = S_IRWXU;

    /*
     * Make the snapshot directory from the uniq_global_snapshot_name
     */
    asprintf(&dir_name, "%s/%s",
             handle_info->base_location,
             handle_info->local_location);
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(dir_name, my_mode)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Open up the metadata file
     */
    if( ORTE_SUCCESS != (ret = metadata_open(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(NULL != dir_name) {
        free(dir_name);
        dir_name = NULL;
    }

    return exit_status;
}

/**************************
 * Metadata functions
 **************************/
static int metadata_open(orte_sstore_central_global_snapshot_info_t * handle_info)
{
    /* If already open, then just return */
    if( NULL != handle_info->metadata ) {
        return ORTE_SUCCESS;
    }

    if (NULL == (handle_info->metadata = fopen(handle_info->metadata_filename, "a")) ) {
        opal_output(orte_sstore_base_framework.framework_output,
                    "sstore:central:(global):init_dir() Unable to open the file (%s)\n",
                    handle_info->metadata_filename);
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
   }

   return ORTE_SUCCESS;
}

static int metadata_close(orte_sstore_central_global_snapshot_info_t * handle_info)
{
    /* If already closed, then just return */
    if( NULL == handle_info->metadata ) {
        return ORTE_SUCCESS;
    }

    fclose(handle_info->metadata);
    handle_info->metadata = NULL;

    return ORTE_SUCCESS;
}

static int metadata_write_int(orte_sstore_central_global_snapshot_info_t * handle_info, char *key, int value)
{
    int ret, exit_status = ORTE_SUCCESS;

    /* Make sure the metadata file is open */
    if( NULL == handle_info->metadata ) {
        if( ORTE_SUCCESS != (ret = metadata_open(handle_info)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    fprintf(handle_info->metadata, "%s%d\n", key, value);

 cleanup:
    return exit_status;
}

static int metadata_write_str(orte_sstore_central_global_snapshot_info_t * handle_info, char *key, char *value)
{
    int ret, exit_status = ORTE_SUCCESS;

    /* Make sure the metadata file is open */
    if( NULL == handle_info->metadata ) {
        if( ORTE_SUCCESS != (ret = metadata_open(handle_info)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    fprintf(handle_info->metadata, "%s%s\n", key, value);

 cleanup:
    return exit_status;
}

static int metadata_write_timestamp(orte_sstore_central_global_snapshot_info_t * handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    time_t timestamp;

    /* Make sure the metadata file is open */
    if( NULL == handle_info->metadata ) {
        if( ORTE_SUCCESS != (ret = metadata_open(handle_info)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    timestamp = time(NULL);
    fprintf(handle_info->metadata, "%s%s",
            SSTORE_METADATA_INTERNAL_TIME_STR,
            ctime(&timestamp));

 cleanup:
    return exit_status;
}

static int orte_sstore_central_extract_global_metadata(orte_sstore_central_global_snapshot_info_t * handle_info,
                                                       orte_sstore_base_global_snapshot_info_t *global_snapshot)
{
    int exit_status = ORTE_SUCCESS;
    orte_sstore_base_local_snapshot_info_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;
    int i = 0;

    /*
     * Cleanup the structure a bit, so we can refresh it below
     */
    while (NULL != (item = opal_list_remove_first(&global_snapshot->local_snapshots))) {
        OBJ_RELEASE(item);
    }

    if( NULL != global_snapshot->start_time ) {
        free( global_snapshot->start_time );
        global_snapshot->start_time = NULL;
    }

    if( NULL != global_snapshot->end_time ) {
        free( global_snapshot->end_time );
        global_snapshot->end_time = NULL;
    }

    /*
     * Create a structure for each application process
     */
    for(i = 0; i < handle_info->num_procs_total; ++i) {
        vpid_snapshot = OBJ_NEW(orte_sstore_base_local_snapshot_info_t);
        vpid_snapshot->ss_handle = handle_info->id;

        vpid_snapshot->process_name.jobid  = handle_info->jobid;
        vpid_snapshot->process_name.vpid   = i;

        vpid_snapshot->crs_comp     = NULL;
        global_snapshot->start_time = NULL;
        global_snapshot->end_time   = NULL;

        opal_list_append(&global_snapshot->local_snapshots, &(vpid_snapshot->super));
    }

    return exit_status;
}

static int central_snapshot_sort_compare_fn(opal_list_item_t **a,
                                            opal_list_item_t **b)
{
    orte_sstore_base_local_snapshot_info_t *snap_a, *snap_b;

    snap_a = (orte_sstore_base_local_snapshot_info_t*)(*a);
    snap_b = (orte_sstore_base_local_snapshot_info_t*)(*b);

    if( snap_a->process_name.vpid > snap_b->process_name.vpid ) {
        return 1;
    }
    else if( snap_a->process_name.vpid == snap_b->process_name.vpid ) {
        return 0;
    }
    else {
        return -1;
    }
}

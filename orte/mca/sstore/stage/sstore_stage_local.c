/*
 * Copyright (c)      2010 The Trustees of Indiana University.
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

#include "opal/mca/compress/compress.h"
#include "opal/mca/compress/base/base.h"

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
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "sstore_stage.h"

/**********
 * Object stuff
 **********/
#define SSTORE_LOCAL_NONE   0
#define SSTORE_LOCAL_ERROR  1
#define SSTORE_LOCAL_INIT   2
#define SSTORE_LOCAL_READY  3
#define SSTORE_LOCAL_SYNCED 4
#define SSTORE_LOCAL_DONE   5

struct  orte_sstore_stage_local_snapshot_info_t {
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

    /** Local Cache Location Format String */
    char * cache_location_fmt;

    /* Application info handles*/
    opal_list_t *app_info_handle;

    /** Compress Component used */
    char * compress_comp;

    /** Compress Component postfix */
    char * compress_postfix;

    /** Is this checkpoint representing a migration? */
    bool migrating;
};
typedef struct orte_sstore_stage_local_snapshot_info_t orte_sstore_stage_local_snapshot_info_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_sstore_stage_local_snapshot_info_t);

void orte_sstore_stage_local_snapshot_info_construct(orte_sstore_stage_local_snapshot_info_t *info);
void orte_sstore_stage_local_snapshot_info_destruct( orte_sstore_stage_local_snapshot_info_t *info);

OBJ_CLASS_INSTANCE(orte_sstore_stage_local_snapshot_info_t,
                   opal_list_item_t,
                   orte_sstore_stage_local_snapshot_info_construct,
                   orte_sstore_stage_local_snapshot_info_destruct);

struct  orte_sstore_stage_local_app_snapshot_info_t {
    /** List super object */
    opal_list_item_t super;

    /** Process Name associated with this entry */
    orte_process_name_t name;

    /** Local Location (Absolute Path) */
    char * local_location;

    /** Compressed Local Location (Absolute Path) */
    char * compressed_local_location;

    /** Local Cache Location (Absolute Path) */
    char * local_cache_location;

    /** Metadata File Name (Absolute Path) */
    char * metadata_filename;

    /** CRS Component used */
    char * crs_comp;

    /** If this app. skipped the checkpoint - usually for non-migrating procs */
    bool ckpt_skipped;

    /** Compression PID to wait on */
    pid_t compress_pid;
};
typedef struct orte_sstore_stage_local_app_snapshot_info_t orte_sstore_stage_local_app_snapshot_info_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_sstore_stage_local_app_snapshot_info_t);

void orte_sstore_stage_local_app_snapshot_info_construct(orte_sstore_stage_local_app_snapshot_info_t *info);
void orte_sstore_stage_local_app_snapshot_info_destruct( orte_sstore_stage_local_app_snapshot_info_t *info);

OBJ_CLASS_INSTANCE(orte_sstore_stage_local_app_snapshot_info_t,
                   opal_list_item_t,
                   orte_sstore_stage_local_app_snapshot_info_construct,
                   orte_sstore_stage_local_app_snapshot_info_destruct);



/**********
 * Local Function and Variable Declarations
 **********/
static bool is_global_listener_active = false;
static int sstore_stage_local_start_listener(void);
static int sstore_stage_local_stop_listener(void);
static void sstore_stage_local_recv(int status,
                                      orte_process_name_t* sender,
                                      opal_buffer_t* buffer,
                                      orte_rml_tag_t tag,
                                      void* cbdata);

static int process_global_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info);
static int process_global_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info);
static int process_global_remove(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info);
static int process_app_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info);
static int process_app_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info);

static orte_sstore_stage_local_snapshot_info_t *create_new_handle_info(orte_sstore_base_handle_t handle);
static orte_sstore_stage_local_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle);
static orte_sstore_stage_local_snapshot_info_t *find_handle_info_ref(char * ref, int seq);

static int append_new_app_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info,
                                      orte_process_name_t *name);
static orte_sstore_stage_local_app_snapshot_info_t *find_app_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info,
                                                                           orte_process_name_t *name);

static int pull_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info );
static int push_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info );

static int wait_all_apps_updated(orte_sstore_stage_local_snapshot_info_t *handle_info);

static int start_compression(orte_sstore_stage_local_snapshot_info_t *handle_info,
                             orte_sstore_stage_local_app_snapshot_info_t *app_info);
static void sstore_stage_local_compress_waitpid_cb(pid_t pid, int status, void* cbdata);
static int wait_all_compressed(orte_sstore_stage_local_snapshot_info_t *handle_info);

static int orte_sstore_stage_local_preload_files(char **local_location, bool *skip_xfer,
                                                 char *global_loc, char *ref, char *postfix, int seq);

static int sstore_stage_create_local_dir(void);
static int sstore_stage_destroy_local_dir(void);

static int sstore_stage_create_cache(void);
static int sstore_stage_update_cache(orte_sstore_stage_local_snapshot_info_t *handle_info);
static int sstore_stage_destroy_cache(void);

static opal_list_t *active_handles = NULL;
static char * sstore_stage_local_basedir = NULL;

static char * sstore_stage_cache_basedir = NULL;

static char * sstore_stage_cache_current_dir = NULL;
static char * sstore_stage_cache_last_dir    = NULL;

static opal_list_t * preload_filem_requests = NULL;

/**********
 * Object stuff
 **********/
void orte_sstore_stage_local_snapshot_info_construct(orte_sstore_stage_local_snapshot_info_t *info)
{
    info->id      = 0;

    info->status = SSTORE_LOCAL_NONE;

    info->seq_num = -1;

    info->global_ref_name = NULL;

    info->location_fmt    = NULL;

    info->cache_location_fmt    = NULL;

    info->app_info_handle = OBJ_NEW(opal_list_t);

    info->compress_comp = NULL;

    info->compress_postfix = NULL;

    info->migrating = false;
}

void orte_sstore_stage_local_snapshot_info_destruct( orte_sstore_stage_local_snapshot_info_t *info)
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

    if( NULL != info->cache_location_fmt ) {
        free( info->cache_location_fmt );
        info->cache_location_fmt = NULL;
    }

    if( NULL != info->app_info_handle ) {
        OBJ_RELEASE(info->app_info_handle);
        info->app_info_handle = NULL;
    }

    if( NULL != info->compress_comp ) {
        free(info->compress_comp);
        info->compress_comp = NULL;
    }

    if( NULL != info->compress_postfix ) {
        free(info->compress_postfix);
        info->compress_postfix = NULL;
    }

    info->migrating = false;
}

void orte_sstore_stage_local_app_snapshot_info_construct(orte_sstore_stage_local_app_snapshot_info_t *info)
{
    info->name.jobid = ORTE_JOBID_INVALID;
    info->name.vpid  = ORTE_VPID_INVALID;

    info->local_location = NULL;
    info->compressed_local_location = NULL;
    info->local_cache_location = NULL;
    info->metadata_filename = NULL;
    info->crs_comp = NULL;
    info->ckpt_skipped = false;
    info->compress_pid = 0;
}

void orte_sstore_stage_local_app_snapshot_info_destruct( orte_sstore_stage_local_app_snapshot_info_t *info)
{
    info->name.jobid = ORTE_JOBID_INVALID;
    info->name.vpid  = ORTE_VPID_INVALID;

    if( NULL != info->local_location ) {
        free(info->local_location);
        info->local_location = NULL;
    }

    if( NULL != info->compressed_local_location ) {
        free(info->compressed_local_location);
        info->compressed_local_location = NULL;
    }

    if( NULL != info->local_cache_location ) {
        free(info->local_cache_location);
        info->local_cache_location = NULL;
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

    info->compress_pid = 0;
}

/******************
 * Local functions
 ******************/
int orte_sstore_stage_local_module_init(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): init()"));

    if( NULL == active_handles ) {
        active_handles = OBJ_NEW(opal_list_t);
    }

    if( NULL == preload_filem_requests ) {
        preload_filem_requests = OBJ_NEW(opal_list_t);
    }

    /*
     * Create the local storage directory
     */
    asprintf(&sstore_stage_local_basedir, "%s/%s/%s",
             orte_sstore_stage_local_snapshot_dir,
             ORTE_SSTORE_LOCAL_SNAPSHOT_DIR_NAME,
             ORTE_SSTORE_LOCAL_SNAPSHOT_STAGE_DIR_NAME);
    if( ORTE_SUCCESS != (ret = sstore_stage_create_local_dir()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Create the local cache
     */
    if( orte_sstore_stage_enabled_caching ) {
        asprintf(&sstore_stage_cache_basedir, "%s/%s/%s",
                 orte_sstore_stage_local_snapshot_dir,
                 ORTE_SSTORE_LOCAL_SNAPSHOT_DIR_NAME,
                 ORTE_SSTORE_LOCAL_SNAPSHOT_CACHE_DIR_NAME);

        if( ORTE_SUCCESS != (ret = sstore_stage_create_cache()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Setup a listener for the HNP/Apps
     * We could be the HNP, in which case the listener is already registered.
     */
    if( !ORTE_PROC_IS_HNP ) {
        if( ORTE_SUCCESS != (ret = sstore_stage_local_start_listener()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}

int orte_sstore_stage_local_module_finalize(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;
    opal_list_item_t* item = NULL;
    bool done = false;
    int cur_time = 0, max_time = 120;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): finalize()"));

    /*
     * Wait for all active transfers to finish
     */
    if( !ORTE_PROC_IS_HNP ) {
        done = false;
        while( 0 < opal_list_get_size(active_handles) && !done ) {
            done = true;
            for(item  = opal_list_get_first(active_handles);
                item != opal_list_get_end(active_handles);
                item  = opal_list_get_next(item) ) {
                handle_info = (orte_sstore_stage_local_snapshot_info_t*)item;
                if( SSTORE_LOCAL_DONE  != handle_info->status &&
                    SSTORE_LOCAL_NONE  != handle_info->status &&
                    SSTORE_LOCAL_ERROR != handle_info->status ) {
                    done = false;
                    break;
                }
            }
            if( done ) {
                break;
            }
            else {
                if( cur_time != 0 && cur_time % 30 == 0 ) {
                    opal_output(0, "---> Waiting for fin(): %3d / %3d\n",
                                cur_time, max_time);
                }

                opal_progress();
                if( cur_time >= max_time ) {
                    break;
                } else {
                    sleep(1);
                }
                cur_time++;
            }
        }
    }

    if( NULL != active_handles ) {
        OBJ_RELEASE(active_handles);
    }

    if( NULL != preload_filem_requests ) {
        OBJ_RELEASE(preload_filem_requests);
    }

    /*
     * Shutdown the listener for the HNP/Apps
     * We could be the HNP, in which case the listener is already deregistered.
     */
    if( !ORTE_PROC_IS_HNP ) {
        if( ORTE_SUCCESS != (ret = sstore_stage_local_stop_listener()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Destroy the local cache
     */
    if( orte_sstore_stage_enabled_caching ) {
        if( ORTE_SUCCESS != (ret = sstore_stage_destroy_cache()) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Destroy the local storage directory
     */
    if( ORTE_SUCCESS != (ret = sstore_stage_destroy_local_dir()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( orte_sstore_stage_enabled_caching ) {
        if( NULL != sstore_stage_cache_basedir ) {
            free(sstore_stage_cache_basedir);
            sstore_stage_cache_basedir = NULL;
        }
    }

    if( NULL != sstore_stage_local_basedir ) {
        free(sstore_stage_local_basedir);
        sstore_stage_local_basedir = NULL;
    }

    return exit_status;
}

int orte_sstore_stage_local_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid)
{
    opal_output(0, "sstore:stage:(local): request_checkpoint_handle() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_stage_local_register(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): register()"));

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

int orte_sstore_stage_local_get_attr(orte_sstore_base_handle_t handle,  orte_sstore_base_key_t key, char **value)
{
    opal_output(0, "sstore:stage:(local): get_attr() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_stage_local_set_attr(orte_sstore_base_handle_t handle,  orte_sstore_base_key_t key, char *value)
{
    opal_output(0, "sstore:stage:(local): set_attr() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_stage_local_sync(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): sync()"));

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
     * Wait for compression to finish
     */
    if( orte_sstore_stage_enabled_compression ) {
        if( ORTE_SUCCESS != (ret = wait_all_compressed(handle_info))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
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

int orte_sstore_stage_local_remove(orte_sstore_base_handle_t handle)
{
    opal_output(0, "sstore:stage:(local): remove() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_stage_local_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): pack()"));

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

int orte_sstore_stage_local_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;
    orte_std_cntr_t count;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): unpack()"));

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

 cleanup:
    return exit_status;
}

int orte_sstore_stage_local_fetch_app_deps(orte_app_context_t *app)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
    char **sstore_args = NULL;
    char * req_snap_loc = NULL;
    char * req_snap_global_ref = NULL;
    char * req_snap_ref = NULL;
    char * req_snap_postfix = NULL;
    char * local_location = NULL;
    char * req_snap_compress = NULL;
    char * compress_local_location = NULL;
    char * compress_ref = NULL;
    char * tmp_str = NULL;
    int req_snap_seq = 0;
    int i;
    orte_proc_t *child = NULL;
    int loc_argc = 0;
    bool skip_xfer = false;

    if( !app->used_on_node || NULL == app->sstore_load ) {
        OPAL_OUTPUT_VERBOSE((30, mca_sstore_stage_component.super.output_handle,
                             "sstore:stage:(local): fetch_app_deps(%3d): Not for this daemon (%s, %d, %s)",
                             app->idx,
                             (app->used_on_node ? "T" : "F"),
                             (int)app->num_procs,
                             app->sstore_load));
        /* Nothing to do */
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): fetch_app_deps(%3d): %s",
                         app->idx, app->sstore_load));

    /*
     * Extract the 'ref:seq' parameter
     */
    sstore_args = opal_argv_split(app->sstore_load, ':');
    req_snap_loc        = strdup(sstore_args[0]);
    req_snap_global_ref = strdup(sstore_args[1]);
    req_snap_ref        = strdup(sstore_args[2]);
    if( NULL == sstore_args[4] ) { /* Not compressed */
        req_snap_seq        = atoi(  sstore_args[3]);
    } else {
        req_snap_compress   = strdup(sstore_args[3]);
        req_snap_postfix    = strdup(sstore_args[4]);
        req_snap_seq        = atoi(  sstore_args[5]);
    }

    handle_info = find_handle_info_ref(req_snap_global_ref, req_snap_seq);
    if( NULL == handle_info ) {
        /* No checkpoints known, just preload the checkpoint */
        OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                             "sstore:stage:(local): fetch_app_deps(%3d): No known checkpoint [%s, %d]",
                             app->idx,
                             req_snap_ref,
                             req_snap_seq));
        goto filem_preload;
    }

    /*
     * If caching enabled, then look to see if we have this snapshot cached
     * Do not cache if migrating, since checkpoints taken while migrating are
     * not guaranteed to be globally taken.
     */
    if( orte_sstore_stage_enabled_caching && !handle_info->migrating ) {
        /*
         * Find the process
         */
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }

            if( app->idx == child->app_idx ) {
                /*
                 * Find the app snapshot ref
                 */
                app_info = find_app_handle_info(handle_info, &child->name);
                break;
            }
        }

        if( NULL == app_info ) {
            OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                                 "sstore:stage:(local): fetch_app_deps(%3d): No processes known for this app context",
                                 app->idx));
            goto filem_preload;
        }

        /*
         * Do we have a cached version of this file?
         */
        if( NULL != app_info->local_cache_location &&
            0 == (ret = access(app_info->local_cache_location, F_OK)) ) {
            OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                                 "sstore:stage:(local): fetch_app_deps(%3d): Using local cache. (%s)",
                                 app->idx,
                                 app_info->local_cache_location));

            opal_argv_append(&loc_argc, &(app->argv), "-c");
            opal_argv_append(&loc_argc, &(app->argv), app_info->local_cache_location);
            goto cleanup;
        } else {
            OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                                 "sstore:stage:(local): fetch_app_deps(%3d): No cache available for %s. (%s)",
                                 app->idx,
                                 ORTE_NAME_PRINT(&app_info->name),
                                 app_info->local_cache_location));
        }
    }

 filem_preload:
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): fetch_app_deps(%3d): Fetch files from Central storage",
                         app->idx));

    /*
     * If we got here, then there is no cached directory, so just preload the
     * files, update the argument set, and carry on.
     */
    if( ORTE_SUCCESS != (ret = orte_sstore_stage_local_preload_files(&local_location,
                                                                     &skip_xfer,
                                                                     req_snap_loc,
                                                                     req_snap_ref,
                                                                     req_snap_postfix,
                                                                     req_snap_seq)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    opal_argv_append(&loc_argc, &(app->argv), "-l");
    opal_argv_append(&loc_argc, &(app->argv), local_location);

    /*
     * Decompress files:
     *  opal-restart will do this for us on launch
     */
    if( !skip_xfer ) {
        if( NULL != req_snap_compress && 0 < strlen(req_snap_compress) ) {
            opal_argv_append(&loc_argc, &(app->argv), "-d");
            opal_argv_append(&loc_argc, &(app->argv), req_snap_compress);
        }
        if( NULL != req_snap_postfix && 0 < strlen(req_snap_postfix) ) {
            opal_argv_append(&loc_argc, &(app->argv), "-p");
            opal_argv_append(&loc_argc, &(app->argv), req_snap_postfix);
        }
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): fetch_app_deps(%3d): Fetching to (%s)",
                         app->idx,
                         local_location));

 cleanup:
    if( NULL != req_snap_compress ) {
        free(req_snap_compress);
        req_snap_compress = NULL;
    }

    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    if( NULL != compress_local_location ) {
        free(compress_local_location);
        compress_local_location = NULL;
    }

    if( NULL != compress_ref ) {
        free(compress_ref);
        compress_ref = NULL;
    }

    if( NULL != sstore_args ) {
        opal_argv_free(sstore_args);
        sstore_args = NULL;
    }

    if( NULL != req_snap_ref ) {
        free(req_snap_ref);
        req_snap_ref = NULL;
    }

    if( NULL != req_snap_postfix ) {
        free(req_snap_postfix);
        req_snap_postfix = NULL;
    }

    if( NULL != req_snap_loc ) {
        free(req_snap_loc);
        req_snap_loc = NULL;
    }

    if( NULL != req_snap_global_ref ) {
        free(req_snap_global_ref);
        req_snap_global_ref = NULL;
    }

    return exit_status;
}

int orte_sstore_stage_local_wait_all_deps(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;

    /* Nothing being preloaded, so just move on */
    if( 0 >= opal_list_get_size(preload_filem_requests) ) {
        return ORTE_SUCCESS;
    }

    /*
     * Wait for all files to move
     */
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): wait_all_deps(): Waiting on %d requests",
                         (int)opal_list_get_size(preload_filem_requests)));

    if(ORTE_SUCCESS != (ret = orte_filem.wait_all(preload_filem_requests)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Cache the restart files locally, so we can restart faster next time
     * JJH: We already check the restart directory for a local copy before
     *      starting the transfer. So this feels unnecessary since the
     *      restart directory is always used as a cache, whether or not
     *      caching is enabled. The extra copy to the cache directory
     *      does not buy us anything.
     */

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): wait_all_deps(): Finished waiting on %d requests!",
                         (int)opal_list_get_size(preload_filem_requests)));

 cleanup:
    while (NULL != (item = opal_list_remove_first(preload_filem_requests) ) ) {
        OBJ_RELEASE(item);
    }

    return exit_status;
}

/**************************
 * Local functions
 **************************/
static orte_sstore_stage_local_snapshot_info_t *create_new_handle_info(orte_sstore_base_handle_t handle)
{
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;
    int i;
    orte_proc_t *child = NULL;

    if( NULL == active_handles ) {
        active_handles = OBJ_NEW(opal_list_t);
    }

    handle_info = OBJ_NEW(orte_sstore_stage_local_snapshot_info_t);

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

static orte_sstore_stage_local_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle)
{
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;
    opal_list_item_t* item = NULL;

    if( NULL == active_handles ) {
        return NULL;
    }

    for(item  = opal_list_get_first(active_handles);
        item != opal_list_get_end(active_handles);
        item  = opal_list_get_next(item) ) {
        handle_info = (orte_sstore_stage_local_snapshot_info_t*)item;

        if( handle_info->id == handle ) {
            return handle_info;
        }
    }

    return NULL;
}

static orte_sstore_stage_local_snapshot_info_t *find_handle_info_ref(char * ref, int seq)
{
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;
    opal_list_item_t* item = NULL;

    if( NULL == active_handles ) {
        return NULL;
    }

    for(item  = opal_list_get_first(active_handles);
        item != opal_list_get_end(active_handles);
        item  = opal_list_get_next(item) ) {
        handle_info = (orte_sstore_stage_local_snapshot_info_t*)item;

        if( 0 == strncmp(handle_info->global_ref_name, ref, strlen(ref)) &&
            handle_info->seq_num == seq ) {
            return handle_info;
        }
    }

    return NULL;
}

static int append_new_app_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info,
                                      orte_process_name_t *name)
{
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;

    app_info = OBJ_NEW(orte_sstore_stage_local_app_snapshot_info_t);

    app_info->name.jobid = name->jobid;
    app_info->name.vpid  = name->vpid;

    opal_list_append(handle_info->app_info_handle, &(app_info->super));

    return ORTE_SUCCESS;
}

static orte_sstore_stage_local_app_snapshot_info_t *find_app_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info,
                                                                           orte_process_name_t *name)
{
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t* item = NULL;
    orte_ns_cmp_bitmask_t mask;

    for(item  = opal_list_get_first(handle_info->app_info_handle);
        item != opal_list_get_end(handle_info->app_info_handle);
        item  = opal_list_get_next(item) ) {
        app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &app_info->name, name)) {
            return app_info;
        }
    }

    return NULL;
}

static int sstore_stage_local_start_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if( is_global_listener_active ) {
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_SSTORE_INTERNAL,
                                                       ORTE_RML_PERSISTENT,
                                                       sstore_stage_local_recv,
                                                       NULL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    is_global_listener_active = true;
    
 cleanup:
    return exit_status;
}

static int sstore_stage_local_stop_listener(void)
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

static void sstore_stage_local_recv(int status,
                                    orte_process_name_t* sender,
                                    opal_buffer_t* buffer,
                                    orte_rml_tag_t tag,
                                    void* cbdata)
{
    int ret;
    orte_sstore_stage_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_sstore_base_handle_t loc_id;

    if( ORTE_RML_TAG_SSTORE_INTERNAL != tag ) {
        return;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): process_cmd(%s)",
                         ORTE_NAME_PRINT(sender)));

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SSTORE_STAGE_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &loc_id, &count, ORTE_SSTORE_HANDLE )) ) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    orte_sstore_stage_local_process_cmd_action(sender, command, loc_id, buffer);

 cleanup:
    return;
}

int orte_sstore_stage_local_process_cmd_action(orte_process_name_t *sender,
                                               orte_sstore_stage_cmd_flag_t command,
                                               orte_sstore_base_handle_t loc_id,
                                               opal_buffer_t* buffer)
{
    orte_sstore_stage_local_snapshot_info_t *handle_info = NULL;
           
    /*
     * Find the referenced handle (Create if it does not exist)
     */
    if(NULL == (handle_info = find_handle_info(loc_id)) ) {
        handle_info = create_new_handle_info(loc_id);
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): process_cmd(%s) - Command = %s",
                         ORTE_NAME_PRINT(sender),
                         (ORTE_SSTORE_STAGE_PULL == command ? "Pull" :
                          (ORTE_SSTORE_STAGE_PUSH == command ? "Push" :
                           (ORTE_SSTORE_STAGE_REMOVE == command ? "Remove" :
                            (ORTE_SSTORE_STAGE_DONE == command ? "Done" : "Unknown")))) ));

    /*
     * Process the command
     */
    if( ORTE_SSTORE_STAGE_PULL == command ) {
        if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, sender)) {
            process_global_pull(sender, buffer, handle_info);
        } else {
            process_app_pull(sender, buffer, handle_info);
        }
    }
    else if( ORTE_SSTORE_STAGE_PUSH == command ) {
        if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, sender)) {
            process_global_push(sender, buffer, handle_info);
        } else {
            process_app_push(sender, buffer, handle_info);
        }
    }
    else if( ORTE_SSTORE_STAGE_REMOVE == command ) {
        /* The xcast from the root makes the 'sender' equal to this process :/
         * so we know it is the HNP, so just use that name */
        process_global_remove(ORTE_PROC_MY_HNP, buffer, handle_info);
    }

    return ORTE_SUCCESS;
}

static int process_global_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    /* JJH should be as simple as calling push_handle_info() */
    opal_output(0, "sstore:stage:(local): process_global_pull() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int process_global_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
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

    if( orte_sstore_stage_enabled_caching ) {
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(handle_info->cache_location_fmt), &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(handle_info->migrating), &count, OPAL_BOOL))) {
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
        app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

        if( NULL != app_info->local_location ) {
            free(app_info->local_location);
            app_info->local_location = NULL;
        }
        asprintf(&(app_info->local_location), handle_info->location_fmt, app_info->name.vpid);

        if( orte_sstore_stage_enabled_caching ) {
            if( NULL != app_info->local_cache_location ) {
                free(app_info->local_cache_location);
                app_info->local_cache_location = NULL;
            }
            asprintf(&(app_info->local_cache_location), handle_info->cache_location_fmt, app_info->name.vpid);
        }

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

static int process_global_remove(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t* item = NULL;
    opal_buffer_t loc_buffer;
    orte_sstore_stage_cmd_flag_t command;
    size_t list_size;
    char * cmd = NULL;

    /*
     * If not caching, then just remove the local copy
     * Or if migrating, since we do not cache checkpoints generated while
     * migrating.
     */
    if( !orte_sstore_stage_enabled_caching || handle_info->migrating ) {
        for(item  = opal_list_get_first(handle_info->app_info_handle);
            item != opal_list_get_end(handle_info->app_info_handle);
            item  = opal_list_get_next(item) ) {
            app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

            asprintf(&cmd, "rm -rf %s", app_info->local_location);
            OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                                 "sstore:stage:(local): update_cache(): Removing with command (%s)",
                                 cmd));
            system(cmd);

            if( orte_sstore_stage_enabled_compression && NULL != app_info->compressed_local_location) {
                free(cmd);
                cmd = NULL;

                asprintf(&cmd, "rm -rf %s", app_info->compressed_local_location);
                OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                                     "sstore:stage:(local): update_cache(): Removing with command (%s)",
                                     cmd));
                system(cmd);
            }
        }
    }
    else {
          /*
           * Update the local cache
           */
          if( ORTE_SUCCESS != (ret = sstore_stage_update_cache(handle_info)) ) {
              ORTE_ERROR_LOG(ret);
              exit_status = ret;
              goto cleanup;
          }
    }

    OBJ_CONSTRUCT(&loc_buffer, opal_buffer_t);

    command = ORTE_SSTORE_STAGE_DONE;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &command, 1, ORTE_SSTORE_STAGE_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &(handle_info->id), 1, ORTE_SSTORE_HANDLE )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    list_size = opal_list_get_size(handle_info->app_info_handle);
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &list_size, 1, OPAL_SIZE )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, &loc_buffer, ORTE_RML_TAG_SSTORE_INTERNAL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): remove(): Sent done for %d files to %s",
                         (int)list_size,
                         ORTE_NAME_PRINT(peer)));

    handle_info->status = SSTORE_LOCAL_DONE;

 cleanup:
    if( NULL != cmd ) {
        free(cmd);
        cmd = NULL;
    }

    OBJ_DESTRUCT(&loc_buffer);

    return exit_status;
}

static int process_app_pull(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t loc_buffer;
    orte_sstore_stage_cmd_flag_t command;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;

    /*
     * Find this app's data
     */
    app_info = find_app_handle_info(handle_info, peer);

    /*
     * Push back the requested information
     */
    OBJ_CONSTRUCT(&loc_buffer, opal_buffer_t);

    command = ORTE_SSTORE_STAGE_PUSH;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&loc_buffer, &command, 1, ORTE_SSTORE_STAGE_CMD )) ) {
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

static int process_app_push(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;

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

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): app_push(%s, skip=%s, %s)",
                         ORTE_NAME_PRINT(&(app_info->name)),
                         (app_info->ckpt_skipped ? "T" : "F"),
                         app_info->crs_comp));

    /* Compression started on sync() */

 cleanup:
    return exit_status;
}

static int wait_all_apps_updated(orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t *item = NULL;
    bool is_done = true;

    do {
        is_done = true;
        for(item  = opal_list_get_first(handle_info->app_info_handle);
            item != opal_list_get_end(handle_info->app_info_handle);
            item  = opal_list_get_next(item) ) {
            app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

            if( NULL == app_info->crs_comp && !app_info->ckpt_skipped ) {
                is_done = false;
                break;
            }
        }

        if( !is_done ) {
            OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                                 "sstore:stage:(local): Waiting for appliccation %s",
                                 ORTE_NAME_PRINT(&(app_info->name)) ));
            opal_progress();
        }
    } while(!is_done);

    return ORTE_SUCCESS;
}

static int start_compression(orte_sstore_stage_local_snapshot_info_t *handle_info,
                             orte_sstore_stage_local_app_snapshot_info_t *app_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    char * postfix = NULL;

    /* Sanity Check */
    if( !orte_sstore_stage_enabled_compression ) {
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): start_compression() Starting compression for process %s of (%s)",
                         ORTE_NAME_PRINT(&(app_info->name)),
                         app_info->local_location ));

    /*
     * Start compression (nonblocking)
     */
    if( ORTE_SUCCESS != (ret = opal_compress.compress_nb(app_info->local_location,
                                                         &(app_info->compressed_local_location),
                                                         &(postfix),
                                                         &(app_info->compress_pid))) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    if( app_info->compress_pid <= 0 ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if( NULL == handle_info->compress_comp ) {
        handle_info->compress_comp = strdup(opal_compress_base_selected_component.base_version.mca_component_name);
        handle_info->compress_postfix = strdup(postfix);
    }

    /*
     * Setup a callback for when it is finished
     */
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): start_compression() Waiting for compression (%d) for process %s",
                         app_info->compress_pid,
                         ORTE_NAME_PRINT(&(app_info->name)) ));

    if( ORTE_SUCCESS != (ret = orte_wait_cb(app_info->compress_pid, sstore_stage_local_compress_waitpid_cb, app_info) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != postfix ) {
        free(postfix);
        postfix = NULL;
    }

    return exit_status;
}

static void sstore_stage_local_compress_waitpid_cb(pid_t pid, int status, void* cbdata)
{
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;

    app_info = (orte_sstore_stage_local_app_snapshot_info_t*)cbdata;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): waitpid(%6d) Compression finished for Process %s",
                         (int)pid,
                         ORTE_NAME_PRINT(&(app_info->name)) ));

    app_info->compress_pid = 0;
}

static int wait_all_compressed(orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t *item = NULL;
    bool is_done = true;
    int usleep_time = 1000;
    int s_time = 0, max_wait_time;

    /* Sanity Check */
    if( !orte_sstore_stage_enabled_compression ) {
        return ORTE_SUCCESS;
    }

    /*
     * Start all compression
     */
    if( orte_sstore_stage_compress_delay > 0 ) {
        OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                             "sstore:stage:(local): Delaying %d second before starting compression...",
                             orte_sstore_stage_compress_delay));
        max_wait_time = orte_sstore_stage_compress_delay * (1000000/usleep_time);
        for( s_time = 0; s_time < max_wait_time; ++s_time) {
            opal_progress();
            usleep(1000);
        }
    }

    for(item  = opal_list_get_first(handle_info->app_info_handle);
        item != opal_list_get_end(handle_info->app_info_handle);
        item  = opal_list_get_next(item) ) {
        app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

        if( ORTE_SUCCESS != (ret = start_compression(handle_info, app_info)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Wait for compression to finish
     */
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): Waiting for compression to finish..."));
    do {
        is_done = true;
        for(item  = opal_list_get_first(handle_info->app_info_handle);
            item != opal_list_get_end(handle_info->app_info_handle);
            item  = opal_list_get_next(item) ) {
            app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

            if( 0 < app_info->compress_pid ) {
                is_done = false;
                break;
            }
        }

        if( !is_done ) {
            OPAL_OUTPUT_VERBOSE((30, mca_sstore_stage_component.super.output_handle,
                                 "sstore:stage:(local): Waiting for compression to finish for appliccation %s",
                                 ORTE_NAME_PRINT(&(app_info->name)) ));
            opal_progress();
        }
    } while(!is_done);

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): Compression finished!"));
 cleanup:
    return exit_status;
}

static int pull_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info )
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_sstore_stage_cmd_flag_t command;

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
    command = ORTE_SSTORE_STAGE_PULL;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SSTORE_STAGE_CMD )) ) {
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

static int push_handle_info(orte_sstore_stage_local_snapshot_info_t *handle_info )
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_sstore_stage_cmd_flag_t command;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t *item = NULL;
    size_t list_size;

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    command = ORTE_SSTORE_STAGE_PUSH;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SSTORE_STAGE_CMD )) ) {
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
        app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

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

            if( orte_sstore_stage_enabled_compression ) {
                if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(handle_info->compress_comp), 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    goto cleanup;
                }
                if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(handle_info->compress_postfix), 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    goto cleanup;
                }
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

static int sstore_stage_create_local_dir(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    mode_t my_mode = S_IRWXU;

    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(sstore_stage_local_basedir, my_mode)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int sstore_stage_destroy_local_dir(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    char * basedir_root = NULL;

    asprintf(&basedir_root, "%s/%s",
             orte_sstore_stage_local_snapshot_dir,
             ORTE_SSTORE_LOCAL_SNAPSHOT_DIR_NAME);

    if(OPAL_SUCCESS != (ret = opal_os_dirpath_destroy(basedir_root, true, NULL)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != basedir_root ) {
        free(basedir_root);
        basedir_root = NULL;
    }

    return exit_status;
}

static int sstore_stage_create_cache(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    mode_t my_mode = S_IRWXU;

    /* Sanity check */
    if( !orte_sstore_stage_enabled_caching ) {
        goto cleanup;
    }

    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(sstore_stage_cache_basedir, my_mode)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int sstore_stage_destroy_cache(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    /* Sanity check */
    if( !orte_sstore_stage_enabled_caching ) {
        goto cleanup;
    }

    if(OPAL_SUCCESS != (ret = opal_os_dirpath_destroy(sstore_stage_cache_basedir, true, NULL)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int sstore_stage_update_cache(orte_sstore_stage_local_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *cmd = NULL;
    mode_t my_mode = S_IRWXU;
    char *cache_dirname = NULL;
    orte_sstore_stage_local_app_snapshot_info_t *app_info = NULL;
    opal_list_item_t* item = NULL;
    size_t list_size;

    /* Sanity Check */
    if( !orte_sstore_stage_enabled_caching || handle_info->migrating) {
        goto cleanup;
    }

    list_size = opal_list_get_size(handle_info->app_info_handle);
    if( 0 >= list_size ) {
        /* No processes on this node, skip */
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    app_info = (orte_sstore_stage_local_app_snapshot_info_t*)opal_list_get_first(handle_info->app_info_handle);
    if( NULL == app_info ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Create the base cache directory
     */
    cache_dirname = opal_dirname(app_info->local_cache_location);
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(cache_dirname, my_mode)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * For each process, move the current checkpoint to the cache directory
     * Cached snapshots are always stored uncompressed.
     */
    for(item  = opal_list_get_first(handle_info->app_info_handle);
        item != opal_list_get_end(handle_info->app_info_handle);
        item  = opal_list_get_next(item) ) {
        app_info = (orte_sstore_stage_local_app_snapshot_info_t*)item;

        asprintf(&cmd, "mv %s %s", app_info->local_location, cache_dirname);
        OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                             "sstore:stage:(local): update_cache(): Caching snapshot for process %s [%s]",
                             ORTE_NAME_PRINT(&app_info->name),
                             cmd));
        system(cmd);

        /* (JJH) Remove the cached files */
        if( orte_sstore_stage_enabled_compression && NULL != app_info->compressed_local_location) {
            free(cmd);
            cmd = NULL;

            asprintf(&cmd, "rm -rf %s", app_info->compressed_local_location);
            OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                                 "sstore:stage:(local): update_cache(): Removing with command (%s)",
                                 cmd));
            system(cmd);
        }
    }

    /*
     * Remove the previous cached checkpoint
     */
    if( NULL != sstore_stage_cache_last_dir ) {
        asprintf(&cmd, "rm -rf %s", sstore_stage_cache_last_dir);
        OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                             "sstore:stage:(local): update_cache(): Removing old cache dir command (%s)",
                             sstore_stage_cache_last_dir));
        system(cmd);
    }

    /*
     * Update 'last' cache pointer
     */
    if( NULL != sstore_stage_cache_last_dir ) {
        free(sstore_stage_cache_last_dir);
        sstore_stage_cache_last_dir = NULL;
    }
    if( NULL != sstore_stage_cache_current_dir ) {
        sstore_stage_cache_last_dir    = strdup(sstore_stage_cache_current_dir);
    }

    /*
     * Update 'current' cache pointer
     */
    if( NULL != sstore_stage_cache_current_dir ) {
        free(sstore_stage_cache_current_dir);
        sstore_stage_cache_current_dir = NULL;
    }
    sstore_stage_cache_current_dir = strdup(cache_dirname);

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                         "sstore:stage:(local): update_cache(): Cache Pointers cur(%s), last(%s)",
                         sstore_stage_cache_current_dir, sstore_stage_cache_last_dir));

 cleanup:
    if( NULL != cmd ) {
        free(cmd);
        cmd = NULL;
    }

    return exit_status;
}

static int orte_sstore_stage_local_preload_files(char **local_location, bool *skip_xfer,
                                                 char *global_loc, char *ref, char *postfix, int seq)
{
    int ret, exit_status = ORTE_SUCCESS;
    mode_t my_mode = S_IRWXU;
    orte_filem_base_request_t *filem_request;
    orte_filem_base_process_set_t *p_set = NULL;
    orte_filem_base_file_set_t * f_set = NULL;
    char * full_local_location = NULL;

    *skip_xfer = false;

    if( NULL != *local_location) {
        free(*local_location);
        *local_location = NULL;
    }

    /*
     * If the global directory is shared, then just reference directly
     *
     * Skip this optimization if compressing. Since decompressing on the
     * central storage would typically require a transfer to the local
     * disk to decompress, then transfer back. Eliminating all benefits
     * of the optimization.
     */
    /* (JJH) If we are going to use the preloaded restart files for subsequent
     *       restarts then we actually always want to preload the files. This
     *       way if we need to restart from the same checkpoint again, then
     *       we can from the local restart cache.
     */
#if 0
    if( orte_sstore_stage_global_is_shared &&
        (NULL == postfix || 0 >= strlen(postfix) ) ) {
        *local_location = strdup(global_loc);
        *skip_xfer = true;
        goto cleanup;
    }
#endif

    asprintf(local_location, "%s/%s/%s/%d",
             orte_sstore_stage_local_snapshot_dir,
             ORTE_SSTORE_LOCAL_SNAPSHOT_DIR_NAME,
             ORTE_SSTORE_LOCAL_SNAPSHOT_RESTART_DIR_NAME,
             seq);
    asprintf(&full_local_location, "%s/%s",
             *local_location,
             ref);

    /*
     * If the snapshot already exists locally, just reuse it instead of
     * transfering it again.
     */
    if( 0 == (ret = access(full_local_location, F_OK)) ) {
        OPAL_OUTPUT_VERBOSE((10, mca_sstore_stage_component.super.output_handle,
                             "sstore:stage:(local): preload_files() Local snapshot already exists, reuse it (%s)",
                             full_local_location));
        *skip_xfer = true;
        goto cleanup;
    }

    /*
     * Create the local restart directory
     */
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(*local_location, my_mode)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * FileM request to move the checkpoint to the local directory
     */
    filem_request = OBJ_NEW(orte_filem_base_request_t);

    /* Define the process set */
    p_set = OBJ_NEW(orte_filem_base_process_set_t);
    if( ORTE_PROC_IS_HNP ) {
        /* if I am the HNP, then use me as the source */
        p_set->source.jobid = ORTE_PROC_MY_NAME->jobid;
        p_set->source.vpid  = ORTE_PROC_MY_NAME->vpid;
    }
    else {
        /* otherwise, set the HNP as the source */
        p_set->source.jobid = ORTE_PROC_MY_HNP->jobid;
        p_set->source.vpid  = ORTE_PROC_MY_HNP->vpid;
    }
    p_set->sink.jobid   = ORTE_PROC_MY_NAME->jobid;
    p_set->sink.vpid    = ORTE_PROC_MY_NAME->vpid;
    opal_list_append(&(filem_request->process_sets), &(p_set->super) );

    /* Define the file set */
    f_set = OBJ_NEW(orte_filem_base_file_set_t);

    f_set->local_target = strdup(*local_location);
    if( NULL != postfix && 0 < strlen(postfix) ) {
        asprintf(&(f_set->remote_target), "%s/%s%s",
                 global_loc,
                 ref,
                 postfix);
    } else {
        asprintf(&(f_set->remote_target), "%s/%s",
                 global_loc,
                 ref);
    }
    if( NULL != postfix && 0 < strlen(postfix) ) {
        f_set->target_flag = ORTE_FILEM_TYPE_FILE;
    } else {
        f_set->target_flag = ORTE_FILEM_TYPE_DIR;
    }

    opal_list_append(&(filem_request->file_sets), &(f_set->super) );

    /* Start getting the files */
    opal_list_append(preload_filem_requests, &(filem_request->super));
    if(ORTE_SUCCESS != (ret = orte_filem.get_nb(filem_request)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != full_local_location ) {
        free(full_local_location);
        full_local_location = NULL;
    }

    return exit_status;
}

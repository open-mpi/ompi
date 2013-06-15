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
#include "opal/util/show_help.h"
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
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"

#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "sstore_central.h"

/**********
 * Object stuff
 **********/
struct  orte_sstore_central_app_snapshot_info_t {
    /** List super object */
    opal_list_item_t super;

    /** */
    orte_sstore_base_handle_t id;

    /** Global Sequence Number */
    int seq_num;

    /** Global Reference Name */
    char * global_ref_name;

    /** Local Location (Absolute Path) */
    char * local_location;

    /** Metadata File Name (Absolute Path) */
    char *metadata_filename;

    /** Metadata File Descriptor */
    FILE *metadata;

    /** CRS Component used */
    char * crs_comp;

    /** Did this process skip the checkpoint? */
    bool ckpt_skipped;
};
typedef struct orte_sstore_central_app_snapshot_info_t orte_sstore_central_app_snapshot_info_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_sstore_central_app_snapshot_info_t);

void orte_sstore_central_app_snapshot_info_construct(orte_sstore_central_app_snapshot_info_t *info);
void orte_sstore_central_app_snapshot_info_destruct( orte_sstore_central_app_snapshot_info_t *info);

OBJ_CLASS_INSTANCE(orte_sstore_central_app_snapshot_info_t,
                   opal_list_item_t,
                   orte_sstore_central_app_snapshot_info_construct,
                   orte_sstore_central_app_snapshot_info_destruct);


/**********
 * Local Function and Variable Declarations
 **********/
static orte_sstore_central_app_snapshot_info_t *create_new_handle_info(orte_sstore_base_handle_t handle);
static orte_sstore_central_app_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle);

static int init_local_snapshot_directory(orte_sstore_central_app_snapshot_info_t *handle_info);
static int pull_handle_info(orte_sstore_central_app_snapshot_info_t *handle_info );
static int push_handle_info(orte_sstore_central_app_snapshot_info_t *handle_info );

static int metadata_open(orte_sstore_central_app_snapshot_info_t * handle_info);
static int metadata_close(orte_sstore_central_app_snapshot_info_t * handle_info);
static int metadata_write_str(orte_sstore_central_app_snapshot_info_t * handle_info, char * key, char *value);
static int metadata_write_int(orte_sstore_central_app_snapshot_info_t * handle_info, char *key, int value);
static int metadata_write_timestamp(orte_sstore_central_app_snapshot_info_t * handle_info);

static opal_list_t *active_handles = NULL;

/**********
 * Object stuff
 **********/
void orte_sstore_central_app_snapshot_info_construct(orte_sstore_central_app_snapshot_info_t *info)
{
    info->id      = 0;

    info->seq_num = -1;

    info->global_ref_name = NULL;
    info->local_location  = NULL;

    info->metadata_filename = NULL;
    info->metadata = NULL;

    info->crs_comp = NULL;

    info->ckpt_skipped = false;
}

void orte_sstore_central_app_snapshot_info_destruct( orte_sstore_central_app_snapshot_info_t *info)
{
    info->id      = 0;
    info->seq_num = -1;

    if( NULL != info->global_ref_name ) {
        free( info->global_ref_name );
        info->global_ref_name  = NULL;
    }

    if( NULL != info->local_location ) {
        free( info->local_location );
        info->local_location = NULL;
    }

    if( NULL != info->metadata_filename ) {
        free( info->metadata_filename ) ;
        info->metadata_filename = NULL;
    }

    if( NULL != info->metadata ) {
        fclose(info->metadata);
        info->metadata = NULL;
    }

    if( NULL != info->crs_comp ) {
        free( info->crs_comp );
        info->crs_comp = NULL;
    }

    info->ckpt_skipped = false;
}

/******************
 * Local functions
 ******************/
int orte_sstore_central_app_module_init(void)
{
    if( NULL == active_handles ) {
        active_handles = OBJ_NEW(opal_list_t);
    }

    return ORTE_SUCCESS;
}

int orte_sstore_central_app_module_finalize(void)
{
    if( NULL != active_handles ) {
        OBJ_RELEASE(active_handles);
    }

    return ORTE_SUCCESS;
}

int orte_sstore_central_app_request_checkpoint_handle(orte_sstore_base_handle_t *handle, int seq, orte_jobid_t jobid)
{
    opal_output(0, "sstore:central:(app): request_checkpoint_handle() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_central_app_register(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_app_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(app): register(%d)", (int)handle));

    /*
     * Create a handle
     */
    orte_sstore_handle_current = handle;
    handle_info = find_handle_info(handle);
    if( NULL != handle_info ) {
        /* Remove the old, stale handle */
        opal_list_remove_item(active_handles, &(handle_info->super));
    }
    handle_info = create_new_handle_info(handle);

    /*
     * Get basic information from Local SStore
     */
    if( ORTE_SUCCESS != (ret = pull_handle_info(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup the storage directory
     */
    if( ORTE_SUCCESS != (ret = init_local_snapshot_directory(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_sstore_central_app_get_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char **value)
{
    int exit_status = ORTE_SUCCESS;
    orte_sstore_central_app_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(app): get_attr(%d)", key));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);

    /*
     * Access metadata
     */
    if( SSTORE_METADATA_GLOBAL_SNAP_SEQ == key ) {
        asprintf(value, "%d", handle_info->seq_num);
        OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                             "sstore:central:(app): get_attr(%d, %d) Seq = <%s>", key, handle_info->id, *value));
    }
    else if( SSTORE_METADATA_LOCAL_SNAP_LOC == key) {
        *value = strdup(handle_info->local_location);
    }
    else if( SSTORE_METADATA_LOCAL_SNAP_META == key ) {
        *value = strdup(handle_info->metadata_filename);
    }
    else if( SSTORE_METADATA_GLOBAL_SNAP_REF == key ) {
        *value = strdup(handle_info->global_ref_name);
        OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                             "sstore:central:(app): get_attr(%d, %d) Ref = <%s>", key, handle_info->id, *value));
    }
    else {
        exit_status = ORTE_ERR_NOT_SUPPORTED;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(app): get_attr(%d, %d) <%s>", key, handle_info->id, *value));
 cleanup:
    return exit_status;
}

int orte_sstore_central_app_set_attr(orte_sstore_base_handle_t handle, orte_sstore_base_key_t key, char *value)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_app_snapshot_info_t *handle_info = NULL;
    char *key_str = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(app): set_attr(%d = %s)", key, value));

    if( NULL == value ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if( key >= SSTORE_METADATA_MAX ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);

    /*
     * Access metadata
     */
    if( SSTORE_METADATA_LOCAL_CRS_COMP == key ) {
        if( NULL != handle_info->crs_comp ) {
            free(handle_info->crs_comp);
        }
        handle_info->crs_comp = strdup(value);
    }
    else if(SSTORE_METADATA_LOCAL_SKIP_CKPT == key ) {
        handle_info->ckpt_skipped = true;
    }
    else if( SSTORE_METADATA_LOCAL_MKDIR == key ||
             SSTORE_METADATA_LOCAL_TOUCH == key ) {
        orte_sstore_base_convert_key_to_string(key, &key_str);
        if( ORTE_SUCCESS != (ret = metadata_write_str(handle_info, key_str, value))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != key_str ) {
        free(key_str);
        key_str = NULL;
    }

    return exit_status;
}

int orte_sstore_central_app_sync(orte_sstore_base_handle_t handle)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_sstore_central_app_snapshot_info_t *handle_info = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(app): sync()"));

    /*
     * Lookup the handle
     */
    handle_info = find_handle_info(handle);

    /*
     * Finalize and close the metadata
     */
    if( ORTE_SUCCESS != (ret = metadata_write_timestamp(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = metadata_close(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Push information to the Local coordinator
     */
    if( ORTE_SUCCESS != (ret = push_handle_info(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    orte_sstore_handle_current = ORTE_SSTORE_HANDLE_INVALID;

    return exit_status;
}

int orte_sstore_central_app_remove(orte_sstore_base_handle_t handle)
{
    opal_output(0, "sstore:central:(app): remove() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_central_app_pack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t handle)
{
    opal_output(0, "sstore:central:(app): pack() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_sstore_central_app_unpack(orte_process_name_t* peer, opal_buffer_t* buffer, orte_sstore_base_handle_t *handle)
{
    opal_output(0, "sstore:central:(app): unpack() Not implemented!");
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/**************************
 * Local functions
 **************************/
static orte_sstore_central_app_snapshot_info_t *create_new_handle_info(orte_sstore_base_handle_t handle)
{
    orte_sstore_central_app_snapshot_info_t *handle_info = NULL;

    handle_info = OBJ_NEW(orte_sstore_central_app_snapshot_info_t);

    handle_info->id = handle;

    opal_list_append(active_handles, &(handle_info->super));

    return handle_info;
}

static orte_sstore_central_app_snapshot_info_t *find_handle_info(orte_sstore_base_handle_t handle)
{
    orte_sstore_central_app_snapshot_info_t *handle_info = NULL;
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(active_handles);
        item != opal_list_get_end(active_handles);
        item  = opal_list_get_next(item) ) {
        handle_info = (orte_sstore_central_app_snapshot_info_t*)item;

        if( handle_info->id == handle ) {
            return handle_info;
        }
    }

    return NULL;
}

static int pull_handle_info(orte_sstore_central_app_snapshot_info_t *handle_info )
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_sstore_central_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_sstore_base_handle_t loc_id;

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

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_SSTORE_INTERNAL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Receive the response
     */
    OBJ_DESTRUCT(&buffer);
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(app): pull() from %s -> %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_DAEMON)));
    if( ORTE_SUCCESS != (ret = orte_rml.recv_buffer(ORTE_PROC_MY_DAEMON,
                                                    &buffer,
                                                    ORTE_RML_TAG_SSTORE_INTERNAL,
                                                    0)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &command, &count, ORTE_SSTORE_CENTRAL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &loc_id, &count, ORTE_SSTORE_HANDLE )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    if( loc_id != handle_info->id ) {
        ; /* JJH Big problem */
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &(handle_info->seq_num), &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &(handle_info->global_ref_name), &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &(handle_info->local_location), &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &(handle_info->metadata_filename), &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_sstore_central_component.super.output_handle,
                         "sstore:central:(app): pull() from %s -> %s (%d, %d, %s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(ORTE_PROC_MY_DAEMON),
                         handle_info->id, 
                         handle_info->seq_num,
                         handle_info->global_ref_name
                         ));
 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

static int push_handle_info(orte_sstore_central_app_snapshot_info_t *handle_info )
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_sstore_central_cmd_flag_t command;

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

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(handle_info->ckpt_skipped), 1, OPAL_BOOL )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( !handle_info->ckpt_skipped ) {
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(handle_info->crs_comp), 1, OPAL_STRING )) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_SSTORE_INTERNAL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

static int init_local_snapshot_directory(orte_sstore_central_app_snapshot_info_t *handle_info)
{
    int ret, exit_status = ORTE_SUCCESS;
    mode_t my_mode = S_IRWXU;

    /*
     * Make the snapshot directory from the uniq_global_snapshot_name
     */
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(handle_info->local_location, my_mode)) ) {
        opal_show_help("help-orte-sstore-central.txt", "fail_path_create", true,
                       ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                       orte_process_info.nodename,
                       handle_info->local_location);
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

    /*
     * Add a timestamp and the PID of this process
     */
    if( ORTE_SUCCESS != (ret = metadata_write_timestamp(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = metadata_write_int(handle_info, SSTORE_METADATA_LOCAL_PID_STR, (int)getpid())) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = metadata_close(handle_info)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}


/**************************
 * Metadata functions
 **************************/
static int metadata_open(orte_sstore_central_app_snapshot_info_t * handle_info)
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

static int metadata_close(orte_sstore_central_app_snapshot_info_t * handle_info)
{
    /* If already closed, then just return */
    if( NULL == handle_info->metadata ) {
        return ORTE_SUCCESS;
    }

    fclose(handle_info->metadata);
    handle_info->metadata = NULL;

    return ORTE_SUCCESS;
}

static int metadata_write_str(orte_sstore_central_app_snapshot_info_t * handle_info, char *key, char *value)
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
    /* Must close the metadata each time, since if we try to checkpoint the
     * CRS might want to restore the FD, and will likely fail if the snapshot
     * moved */
    if( NULL != handle_info->metadata ) {
        fclose(handle_info->metadata);
        handle_info->metadata = NULL;
    }

    return exit_status;
}

static int metadata_write_int(orte_sstore_central_app_snapshot_info_t * handle_info, char *key, int value)
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

static int metadata_write_timestamp(orte_sstore_central_app_snapshot_info_t * handle_info)
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
    fprintf(handle_info->metadata, "%s%s", SSTORE_METADATA_INTERNAL_TIME_STR, ctime(&timestamp));

 cleanup:
    return exit_status;
}

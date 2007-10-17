/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/include/opal/constants.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

static void opal_crs_base_construct(opal_crs_base_snapshot_t *snapshot)
{
    snapshot->component_name  = NULL;
    snapshot->reference_name  = opal_crs_base_unique_snapshot_name(getpid());
    snapshot->local_location  = opal_crs_base_get_snapshot_directory(snapshot->reference_name);
    snapshot->remote_location = strdup(snapshot->local_location);
    snapshot->cold_start      = false;
}

static void opal_crs_base_destruct( opal_crs_base_snapshot_t *snapshot)
{
    if(NULL != snapshot->reference_name) {
        free(snapshot->reference_name);
        snapshot->reference_name = NULL;
    }
    if(NULL != snapshot->local_location) {
        free(snapshot->local_location);
        snapshot->local_location = NULL;
    }
    if(NULL != snapshot->remote_location) {
       free(snapshot->remote_location);
       snapshot->remote_location = NULL;
    }
    if(NULL != snapshot->component_name) {
        free(snapshot->component_name);
        snapshot->component_name = NULL;
    }
}

OBJ_CLASS_INSTANCE(opal_crs_base_snapshot_t,
                   opal_list_item_t,
                   opal_crs_base_construct,
                   opal_crs_base_destruct);

int opal_crs_base_none_open(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_base_none_close(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_base_none_module_init(void)
{

    return OPAL_SUCCESS;
}

int opal_crs_base_none_module_finalize(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_base_none_checkpoint(pid_t pid, opal_crs_base_snapshot_t *snapshot, opal_crs_state_type_t *state)
{
    *state = OPAL_CRS_CONTINUE;
    
    snapshot->component_name  = strdup("none");
    snapshot->reference_name  = strdup("none");
    snapshot->local_location  = strdup("");
    snapshot->remote_location = strdup("");
    snapshot->cold_start      = false;

#if 0 
    /*  JJH - A more complete alternative if needed */
    opal_crs_none_snapshot_t *snapshot = OBJ_NEW(opal_crs_none_snapshot_t);

    if(NULL != snapshot->super.reference_name)
        free(snapshot->super.reference_name);
    snapshot->super.reference_name = strdup(base_snapshot->reference_name);

    if(NULL != snapshot->super.local_location)
        free(snapshot->super.local_location);
    snapshot->super.local_location  = strdup(base_snapshot->local_location);

    if(NULL != snapshot->super.remote_location)
        free(snapshot->super.remote_location);
    snapshot->super.remote_location  = strdup(base_snapshot->remote_location);

    opal_output_verbose(10, mca_crs_none_component.super.output_handle,
                        "crs:none: checkpoint(%d, ---)", pid);

    /*
     * Create the snapshot directory
     */
    snapshot->super.component_name = strdup(mca_crs_none_component.super.crs_version.mca_component_name);
    if( OPAL_SUCCESS != (ret = opal_crs_base_init_snapshot_directory(&snapshot->super) )) {
        opal_output(mca_crs_none_component.super.output_handle,
                    "crs:none: checkpoint(): Error: Unable to initialize the directory for (%s).", 
                    snapshot->super.reference_name);
        return ret;
    }

    /*
     * Return to the caller
     */
    base_snapshot = &(snapshot->super);
#endif

    return OPAL_SUCCESS;
}

int opal_crs_base_none_restart(opal_crs_base_snapshot_t *snapshot, bool spawn_child, pid_t *child_pid)
{
    *child_pid = getpid();

    return OPAL_SUCCESS;
}

int opal_crs_base_none_disable_checkpoint(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_base_none_enable_checkpoint(void)
{
    return OPAL_SUCCESS;
}

int opal_crs_base_none_prelaunch(int32_t rank,
                                 char *base_snapshot_dir,
                                 char **app,
                                 char **cwd,
                                 char ***argv,
                                 char ***env)
{
    opal_setenv(mca_base_param_env_var("opal_cr_is_tool"),
                "0", true, env);
    return OPAL_SUCCESS;
}

/*
 * Utility functions
 */
char * opal_crs_base_unique_snapshot_name(pid_t pid)
{
    char * loc_str = NULL;
    
    asprintf(&loc_str, "opal_snapshot_%d.ckpt", pid);
    
    return loc_str;
}

FILE * opal_crs_base_open_read_metadata(char * location, char **component, int *prev_pid)
{
    char * dir_name = NULL;
    char * content = NULL;
    char * tmp_str = NULL;
    int len = 0;
    FILE * meta_data = NULL;

    *component = NULL;
    *prev_pid = -1;

    /*
     * Find the snapshot directory, read the metadata file
     */
    asprintf(&dir_name, "%s/%s", location, opal_crs_base_metadata_filename);
    if (NULL == (meta_data = fopen(dir_name, "r")) ) {
        goto cleanup;
    }

    /* 
     * Component Name
     */
    len = 32; /* Max size for a CRS component name */
    content = (char *) malloc(sizeof(char) * len);
    if (NULL == fgets(content, len, meta_data) ) {
        free(content);
        content = NULL;
        goto cleanup;
    }
    /* Strip of newline */
    len = strlen(content);
    content[len - 1] = '\0';

    *component = strdup(content);

    /*
     * Get the PID
     */
    len = 128;
    tmp_str = (char *) malloc(sizeof(char) * len);
    if (NULL == fgets(tmp_str, len, meta_data) ) {
        goto cleanup;
    }
    /* Strip of newline */
    len = strlen(tmp_str);
    if(tmp_str[len - 1] == '\n')
        tmp_str[len - 1] = '\0';
    *prev_pid = atoi(tmp_str);

 cleanup:
    return meta_data;
}

char * opal_crs_base_extract_expected_component(char *snapshot_loc, int *prev_pid)
{
    FILE * meta_data = NULL;
    char * component_name = NULL;
    
    *prev_pid = -1;

    if( NULL == (meta_data = opal_crs_base_open_read_metadata(snapshot_loc, &component_name, prev_pid)) ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: extract_expected_component: Error: Unable to open the file (%s)\n",
                    snapshot_loc);
        goto cleanup;
    }

 cleanup:
    if(NULL != meta_data) 
        fclose(meta_data);

    return component_name;
}

char * opal_crs_base_get_snapshot_directory(char *uniq_snapshot_name)
{
    char * dir_name = NULL;

    asprintf(&dir_name, "%s/%s", opal_crs_base_snapshot_dir, uniq_snapshot_name);

    return dir_name;
}

int    opal_crs_base_init_snapshot_directory(opal_crs_base_snapshot_t *snapshot)
{
    mode_t my_mode = S_IRWXU; 
    int ret, exit_status = OPAL_SUCCESS;
    FILE * meta_data = NULL;

    /*
     * Make the snapshot directory from the uniq_snapshot_name
     */
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(snapshot->local_location, my_mode)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Initialize the metadata file at the top of that directory.
     */
    if (NULL == (meta_data = opal_crs_base_open_metadata(snapshot, 'w') ) ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: init_snapshot_directory: Error: Unable to open the file (%s/%s)\n",
                    snapshot->local_location, opal_crs_base_metadata_filename);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
     
 cleanup:
    if(NULL != meta_data)
        fclose(meta_data);

    return OPAL_SUCCESS;
}

FILE *opal_crs_base_open_metadata(opal_crs_base_snapshot_t *snapshot, char mode )
{
    char *meta_data_fname = NULL;
    FILE * meta_data = NULL;

    /*
     * Construct path
     */
    asprintf(&meta_data_fname, "%s/%s", snapshot->local_location, opal_crs_base_metadata_filename);

    /*
     * Open the metadata file
     */
    if( mode == 'w' ) {
        meta_data = fopen(meta_data_fname, "w");
    }
    else if( mode == 'a' ) {
        meta_data = fopen(meta_data_fname, "a");
    }

    if (NULL == meta_data ) {
        opal_output(opal_crs_base_output,
                    "opal:crs:base: open_metadata (%c): Error: Unable to open the file (%s)\n",
                    mode, meta_data_fname);
        goto cleanup;
    }

    if( mode == 'w' ) {
        /*
         * The first line is the component name, 
         * everything else here is defined by the component
         */
        fprintf(meta_data, "%s\n", snapshot->component_name);
        fprintf(meta_data, "%d\n", getpid());
    }

 cleanup:
    if(NULL != meta_data_fname)
        free(meta_data_fname);

    return meta_data;
}

char * opal_crs_base_state_str(opal_crs_state_type_t state)
{
    char *str = NULL;

    switch(state) {
    case OPAL_CRS_CHECKPOINT:
        str = strdup("Checkpoint");
        break;
    case OPAL_CRS_RESTART:
        str = strdup("Restart");
        break;
    case OPAL_CRS_CONTINUE:
        str = strdup("Continue");
        break;
    case OPAL_CRS_TERM:
        str = strdup("Terminate");
        break;
    case OPAL_CRS_RUNNING:
        str = strdup("Running");
        break;
    case OPAL_CRS_ERROR:
        str = strdup("Error");
        break;
    default:
        str = strdup("Unknown");
        break;
    }
    
    return str;
}

/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/constants.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

opal_crs_base_self_checkpoint_fn_t crs_base_self_checkpoint_fn;
opal_crs_base_self_restart_fn_t    crs_base_self_restart_fn;
opal_crs_base_self_continue_fn_t   crs_base_self_continue_fn;

/******************
 * Local Functions
 ******************/
static int metadata_extract_next_token(FILE *file, char **token, char **value);

static char **cleanup_file_argv = NULL;
static char **cleanup_dir_argv = NULL;

/******************
 * Object stuff
 ******************/
static void opal_crs_base_construct(opal_crs_base_snapshot_t *snapshot)
{
    snapshot->component_name     = NULL;

    snapshot->metadata_filename  = NULL;
    snapshot->metadata           = NULL;
    snapshot->snapshot_directory = NULL;

    snapshot->cold_start      = false;
}

static void opal_crs_base_destruct( opal_crs_base_snapshot_t *snapshot)
{
    if(NULL != snapshot->metadata_filename ) {
        free(snapshot->metadata_filename);
        snapshot->metadata_filename = NULL;
    }

    if(NULL != snapshot->metadata) {
        fclose(snapshot->metadata);
        snapshot->metadata = NULL;
    }

    if(NULL != snapshot->snapshot_directory ) {
       free(snapshot->snapshot_directory);
       snapshot->snapshot_directory = NULL;
    }
}

OBJ_CLASS_INSTANCE(opal_crs_base_snapshot_t,
                   opal_list_item_t,
                   opal_crs_base_construct,
                   opal_crs_base_destruct);

static void opal_crs_base_ckpt_options_construct(opal_crs_base_ckpt_options_t *opts) {
    opal_crs_base_clear_options(opts);
}

static void opal_crs_base_ckpt_options_destruct(opal_crs_base_ckpt_options_t *opts) {
    opal_crs_base_clear_options(opts);
}

OBJ_CLASS_INSTANCE(opal_crs_base_ckpt_options_t,
                   opal_object_t,
                   opal_crs_base_ckpt_options_construct,
                   opal_crs_base_ckpt_options_destruct);

/*
 * Utility functions
 */
int opal_crs_base_metadata_read_token(FILE *metadata, char * token, char ***value) {
    int exit_status = OPAL_SUCCESS;
    char * loc_token = NULL;
    char * loc_value = NULL;
    int argc = 0;

    /* Dummy check */
    if( NULL == token ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    if( NULL == metadata ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Extract each token and make the records
     */
    rewind(metadata);
    do {
        /* Get next token */
        if( OPAL_SUCCESS != metadata_extract_next_token(metadata, &loc_token, &loc_value) ) {
            break;
        }

        /* Check token to see if it matches */
        if(0 == strncmp(token, loc_token, strlen(loc_token)) ) {
            opal_argv_append(&argc, value, loc_value);
        }
    } while(0 == feof(metadata) );
    
 cleanup:
    if (NULL != metadata) {
        rewind(metadata);
    }
    return exit_status;
}

int opal_crs_base_extract_expected_component(FILE *metadata, char ** component_name, int *prev_pid)
{
    int exit_status = OPAL_SUCCESS;
    char **pid_argv = NULL;
    char **name_argv = NULL;

    /* Dummy check */
    if( NULL == metadata ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_crs_base_metadata_read_token(metadata, CRS_METADATA_PID, &pid_argv);
    if( NULL != pid_argv && NULL != pid_argv[0] ) {
        *prev_pid = atoi(pid_argv[0]);
    } else {
        opal_output(0, "Error: expected_component: PID information unavailable!");
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_crs_base_metadata_read_token(metadata, CRS_METADATA_COMP, &name_argv);
    if( NULL != name_argv && NULL != name_argv[0] ) {
        *component_name = strdup(name_argv[0]);
    } else {
        opal_output(0, "Error: expected_component: Component Name information unavailable!");
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != pid_argv ) {
        opal_argv_free(pid_argv);
        pid_argv = NULL;
    }

    if( NULL != name_argv ) {
        opal_argv_free(name_argv);
        name_argv = NULL;
    }

    return exit_status;
}

int opal_crs_base_cleanup_append(char* filename, bool is_dir)
{
    if( NULL == filename ) {
        return OPAL_SUCCESS;
    }

    if( is_dir ) {
        opal_output_verbose(15, opal_crs_base_framework.framework_output,
                            "opal:crs: cleanup_append: Append Dir  <%s>\n",
                            filename);
        opal_argv_append_nosize(&cleanup_dir_argv, filename);
    } else {
        opal_output_verbose(15, opal_crs_base_framework.framework_output,
                            "opal:crs: cleanup_append: Append File <%s>\n",
                            filename);
        opal_argv_append_nosize(&cleanup_file_argv, filename);
    }

    return OPAL_SUCCESS;
}

int opal_crs_base_cleanup_flush(void)
{
    int argc, i;

    /*
     * Cleanup files first
     */
    if( NULL != cleanup_file_argv ) {
        argc = opal_argv_count(cleanup_file_argv);
        for( i = 0; i < argc; ++i) {
            opal_output_verbose(15, opal_crs_base_framework.framework_output,
                                "opal:crs: cleanup_flush: Remove File <%s>\n", cleanup_file_argv[i]);
            unlink(cleanup_file_argv[i]);
        }

        opal_argv_free(cleanup_file_argv);
        cleanup_file_argv = NULL;
    }

    /*
     * Try to cleanup directories next
     */
    if( NULL != cleanup_dir_argv ) {
        argc = opal_argv_count(cleanup_dir_argv);
        for( i = 0; i < argc; ++i) {
            opal_output_verbose(15, opal_crs_base_framework.framework_output,
                                "opal:crs: cleanup_flush: Remove Dir  <%s>\n", cleanup_dir_argv[i]);
            opal_os_dirpath_destroy(cleanup_dir_argv[i], true, NULL);
        }

        opal_argv_free(cleanup_dir_argv);
        cleanup_dir_argv = NULL;
    }

    return OPAL_SUCCESS;
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

int opal_crs_base_copy_options(opal_crs_base_ckpt_options_t *from,
                                 opal_crs_base_ckpt_options_t *to)
{
    if( NULL == from ) {
        opal_output(opal_crs_base_framework.framework_output,
                    "opal:crs:base: copy_options: Error: from value is NULL\n");
        return OPAL_ERROR;
    }

    if( NULL == to ) {
        opal_output(opal_crs_base_framework.framework_output,
                    "opal:crs:base: copy_options: Error: to value is NULL\n");
        return OPAL_ERROR;
    }

    to->term = from->term;
    to->stop = from->stop;

    to->inc_prep_only    = from->inc_prep_only;
    to->inc_recover_only = from->inc_recover_only;

#if OPAL_ENABLE_CRDEBUG == 1
    to->attach_debugger = from->attach_debugger;
    to->detach_debugger = from->detach_debugger;
#endif

    return OPAL_SUCCESS;
}

int opal_crs_base_clear_options(opal_crs_base_ckpt_options_t *target)
{
    if( NULL == target ) {
        opal_output(opal_crs_base_framework.framework_output,
                    "opal:crs:base: copy_options: Error: target value is NULL\n");
        return OPAL_ERROR;
    }

    target->term = false;
    target->stop = false;

    target->inc_prep_only = false;
    target->inc_recover_only = false;

#if OPAL_ENABLE_CRDEBUG == 1
    target->attach_debugger = false;
    target->detach_debugger = false;
#endif

    return OPAL_SUCCESS;
}

int opal_crs_base_self_register_checkpoint_callback(opal_crs_base_self_checkpoint_fn_t  function)
{
    crs_base_self_checkpoint_fn = function;
    return OPAL_SUCCESS;
}

int opal_crs_base_self_register_restart_callback(opal_crs_base_self_restart_fn_t  function)
{
    crs_base_self_restart_fn = function;
    return OPAL_SUCCESS;
}

int opal_crs_base_self_register_continue_callback(opal_crs_base_self_continue_fn_t  function)
{
    crs_base_self_continue_fn = function;
    return OPAL_SUCCESS;
}


/******************
 * Local Functions
 ******************/
static int metadata_extract_next_token(FILE *file, char **token, char **value)
{
    int exit_status = OPAL_SUCCESS;
    int max_len = 256;
    char * line = NULL;
    int line_len = 0;
    int c = 0, s = 0, v = 0;
    char *local_token = NULL;
    char *local_value = NULL;
    bool end_of_line = false;

    line = (char *) malloc(sizeof(char) * max_len);

 try_again:
    /*
     * If we are at the end of the file, then just return
     */
    if(0 != feof(file) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Other wise grab the next token/value pair
     */
    if (NULL == fgets(line, max_len, file) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    line_len = strlen(line);
    /* Strip off the new line if it it there */
    if('\n' == line[line_len-1]) {
        line[line_len-1] = '\0';
        line_len--;
        end_of_line = true;
    }
    else {
        end_of_line = false;
    }

    /* Ignore lines with just '#' too */
    if(2 >= line_len)
        goto try_again;
    
    /*
     * Extract the token from the set
     */
    for(c = 0; 
        line[c] != ':' && 
            c < line_len;
        ++c) {
        ;
    }
    c += 2; /* For the ' ' and the '\0' */
    local_token = (char *)malloc(sizeof(char) * (c + 1));

    for(s = 0; s < c; ++s) {
        local_token[s] = line[s];
    }

    local_token[s] = '\0';
    *token = strdup(local_token);

    if( NULL != local_token) {
        free(local_token);
        local_token = NULL;
    }

    /*
     * Extract the value from the set
     */
    local_value = (char *)malloc(sizeof(char) * (line_len - c + 1));
    for(v = 0, s = c; 
        s < line_len;
        ++s, ++v) {
        local_value[v] = line[s];
    }

    while(!end_of_line) {
        if (NULL == fgets(line, max_len, file) ) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
        line_len = strlen(line);
        /* Strip off the new line if it it there */
        if('\n' == line[line_len-1]) {
            line[line_len-1] = '\0';
            line_len--;
            end_of_line = true;
        }
        else {
            end_of_line = false;
        }
        
        local_value = (char *)realloc(local_value, sizeof(char) * line_len);
        for(s = 0;
            s < line_len;
            ++s, ++v) {
            local_value[v] = line[s];
        }
    }

    local_value[v] = '\0';
    *value = strdup(local_value);

 cleanup:
    if( NULL != local_token) {
        free(local_token);
        local_token = NULL;
    }

    if( NULL != local_value) {
        free(local_value);
        local_value = NULL;
    }

    if( NULL != line) {
        free(line);
        line = NULL;
    }

    return exit_status;
}

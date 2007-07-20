/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>

#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/os_dirpath.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

/******************
 * Local Functions
 ******************/

/******************
 * Object Stuff
 ******************/
ORTE_DECLSPEC OBJ_CLASS_INSTANCE(orte_filem_base_request_t,
                                 opal_list_item_t,
                                 orte_filem_base_construct,
                                 orte_filem_base_destruct);

ORTE_DECLSPEC void orte_filem_base_construct(orte_filem_base_request_t *req) {
    req->num_targets        = 0;

    req->local_targets      = NULL;
    req->remote_targets     = NULL;
    req->target_flags       = NULL;

    req->proc_name        = NULL;
    req->num_procs        = 0;
}

ORTE_DECLSPEC void orte_filem_base_destruct( orte_filem_base_request_t *req) {
    int i;

    for(i = 0; i < req->num_targets; ++i) {
        free(req->local_targets[i]);
        free(req->remote_targets[i]);
    }
    
    free(req->target_flags);
    
    req->num_targets = 0;
    
    free(req->proc_name);
    
    req->num_procs =  0;
}

/***********************
 * None component stuff
 ************************/
int orte_filem_base_none_open(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_close(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_module_init(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_module_finalize(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_put(orte_filem_base_request_t *request )
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_get(orte_filem_base_request_t *request)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_rm(orte_filem_base_request_t *request)
{
    return ORTE_SUCCESS;
}

/********************
 * Utility functions
 ********************/
int orte_filem_base_listener_init(orte_rml_buffer_callback_fn_t rml_cbfunc) {
    int ret;

    if( ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_FILEM,
                                                       0,
                                                       rml_cbfunc,
                                                       NULL)) ) {
        return ret;
    }

    return ORTE_SUCCESS;
}

int orte_filem_base_listener_cancel() {
    int ret;

    if( ORTE_SUCCESS != (ret = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_FILEM) ) ) {
        return ret;
    }

    return ORTE_SUCCESS;
}

int orte_filem_base_get_proc_node_name(orte_process_name_t *proc, char **machine_name) {
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, **tokens, *keys[2];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t num_tokens = 0, num_values = 0, i, j;
    char   *tmp_node_name = NULL;
    
    /*
     * Contact GPR and get the 'orte-node-name' for this process
     */
    /* if it is the root then we need a different key :/ */
    if(proc->jobid  == 0 &&
       proc->vpid   == 0) {
        keys[0] = ORTE_PROC_RML_IP_ADDRESS_KEY;
    }
    else {
        keys[0] = ORTE_NODE_NAME_KEY;
    }
    
    keys[1] = NULL;

    /* 
     * Get the job segment
     */
    if(ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, proc->jobid))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Get the process tokens
     */
    if (ORTE_SUCCESS != (ret = orte_schema.get_proc_tokens(&tokens,
                                                           &num_tokens,
                                                           proc) )) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Get the requested values
     */
    if( ORTE_SUCCESS != (ret = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                            segment,
                                            tokens,
                                            keys,
                                            &num_values,
                                            &values ) ) ) {
        
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Parse the values
     */
    for(i = 0; i < num_values; ++i) {
        orte_gpr_value_t* value = values[i];

        for(j = 0; j < value->cnt; ++j) {
            orte_gpr_keyval_t* keyval = value->keyvals[j];

            if (strcmp(keyval->key, keys[0]) == 0) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_node_name), keyval->value, ORTE_STRING))) {
                    exit_status = ret;
                    goto cleanup;
                }
                *machine_name = strdup(tmp_node_name);
                if(NULL != tmp_node_name) {
                    free(tmp_node_name);
                    tmp_node_name = NULL;
                }
                continue;
            }
        }
    }
    
    if (NULL == *machine_name ){
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != segment)
        free(segment);

    if(NULL != tmp_node_name) {
        free(tmp_node_name);
        tmp_node_name = NULL;
    }

    return exit_status;
}


/*
 * This function is paired with the orte_filem_base_query_callback() function on the remote machine
 */
int orte_filem_base_query_remote_path(char **remote_ref, orte_process_name_t *peer, int *flag) {
    int ret, exit_status = ORTE_SUCCESS;
    char *tmp_ref = NULL;
    orte_std_cntr_t n;
    orte_buffer_t *loc_buffer = NULL;
    int tmp_flag;

    /*
     * Ask for remote file information from the HNP
     */
    if( NULL == (loc_buffer = OBJ_NEW(orte_buffer_t) ) ) {
        exit_status = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_dss.pack(loc_buffer, remote_ref, 1, ORTE_STRING))) {
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, loc_buffer, ORTE_RML_TAG_FILEM, 0))) {
        exit_status = ret;
        goto cleanup;
    }
    
    OBJ_RELEASE(loc_buffer);
    if( NULL == (loc_buffer = OBJ_NEW(orte_buffer_t) ) ) {
        exit_status = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /*
     * Get the response
     */
    if( 0 > (ret = orte_rml.recv_buffer(peer, loc_buffer, ORTE_RML_TAG_FILEM, 0)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * The absolute path for the remote file
     */
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, &tmp_ref, &n, ORTE_STRING)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * The file type on the remote machine
     */
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, &tmp_flag, &n, ORTE_INT)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if(NULL != *remote_ref)
        free(*remote_ref);
    
    *remote_ref = strdup(tmp_ref);
    *flag = tmp_flag;

 cleanup:
    if( NULL != loc_buffer)
        OBJ_RELEASE(loc_buffer);
    if( NULL != tmp_ref)
        free(tmp_ref);

    return exit_status;
}

/*
 * This function is paired with the orte_filem_base_query_remote_path() function on the 
 * requesting machine.
 * This function is responsible for:
 * - Constructing the remote absolute path for the specified file/dir
 * - Verify the existence of the file/dir
 * - Determine if the specified file/dir is in fact a file or dir or unknown if not found.
 * 
 */
void orte_filem_base_query_callback(int status,
                                    orte_process_name_t* peer,
                                    orte_buffer_t *buffer,
                                    orte_rml_tag_t tag,
                                    void* cbdata) {
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t n;
    orte_buffer_t loc_buffer;
    char *filename = NULL;
    char *tmp_name = NULL;
    char cwd[OMPI_PATH_MAX];
    int file_type = ORTE_FILEM_TYPE_UNKNOWN;
    struct stat file_status;

    /*
     * Receive the file/dir name in question
     */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &filename, &n, ORTE_STRING))) {
        exit_status = ret;
        goto cleanup;
    }

    OBJ_CONSTRUCT(&loc_buffer, orte_buffer_t);
    
    /*
     * Determine the absolute path of the file
     */
    if(filename[0] != '/') { /* if it is not an absolute path already */
        getcwd(cwd, sizeof(cwd));
        asprintf(&tmp_name, "%s/%s", cwd, filename);
    }
    else {
        tmp_name = strdup(filename);
    }

    opal_output_verbose(10, orte_filem_base_output,
                        "filem:base: filem_base_query_callback: %s -> %s: Filename Requested (%s) translated to (%s)",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(peer),
                        filename, tmp_name);

    /*
     * Determine if the file/dir exists at that absolute path
     * Determine if the file/dir is a file or a directory
     */
    if(0 != (ret = stat(tmp_name, &file_status) ) ){
        file_type = ORTE_FILEM_TYPE_UNKNOWN;
    }
    else {
        /* Is it a directory? */
        if(S_ISDIR(file_status.st_mode)) {
            file_type = ORTE_FILEM_TYPE_DIR;
        }
        else if(S_ISREG(file_status.st_mode)) {
            file_type = ORTE_FILEM_TYPE_FILE;
        }
    }

    /*
     * Send back the Absolute Path
     */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.pack(&loc_buffer, &tmp_name, n, ORTE_STRING))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Send back the reference type
     * - ORTE_FILEM_TYPE_FILE    = File
     * - ORTE_FILEM_TYPE_DIR     = Directory
     * - ORTE_FILEM_TYPE_UNKNOWN = Could not be determined, or does not exist
     */
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.pack(&loc_buffer, &file_type, n, ORTE_INT))) {
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, &loc_buffer, ORTE_RML_TAG_FILEM, 0))) {
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    OBJ_DESTRUCT(&loc_buffer);

    if( NULL != filename) 
        free(filename);
    if( NULL != tmp_name)
        free(tmp_name);

    return;
}

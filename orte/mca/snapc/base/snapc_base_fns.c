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

#include "orte_config.h"

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <time.h>

#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/basename.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/os_dirpath.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

/******************
 * Local Functions
 ******************/
/* Some local strings to use genericly with the global metadata file */
#define SNAPC_METADATA_SEQ      ("# Seq: ")
#define SNAPC_METADATA_DONE_SEQ ("# Finished Seq: ")
#define SNAPC_METADATA_TIME     ("# Timestamp: ")
#define SNAPC_METADATA_PROCESS  ("# Process: ")
#define SNAPC_METADATA_CRS_COMP ("# OPAL CRS Component: ")
#define SNAPC_METADATA_SNAP_REF ("# Snapshot Reference: ")
#define SNAPC_METADATA_SNAP_LOC ("# Snapshot Location: ")

static int snapc_base_reg_gpr_request( orte_jobid_t jobid, orte_gpr_notify_cb_fn_t gpr_cbfunc, void* gpr_cbdata );
static int get_next_seq_number(FILE *file);
static int get_next_valid_seq_number(FILE *file);
static int metadata_extract_next_token(FILE *file, char **token, char **value);

size_t orte_snapc_base_snapshot_seq_number = 0;

/******************
 * Object stuff
 ******************/
OBJ_CLASS_INSTANCE(orte_snapc_base_snapshot_t,
                   opal_crs_base_snapshot_t,
                   orte_snapc_base_snapshot_construct,
                   orte_snapc_base_snapshot_destruct);

void orte_snapc_base_snapshot_construct(orte_snapc_base_snapshot_t *snapshot)
{
    snapshot->process_name.jobid  = 0;
    snapshot->process_name.vpid   = 0;
    snapshot->process_pid  = 0;
    snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;
    snapshot->term  = false;
}

void orte_snapc_base_snapshot_destruct( orte_snapc_base_snapshot_t *snapshot)
{
    snapshot->process_name.jobid  = 0;
    snapshot->process_name.vpid   = 0;
    snapshot->process_pid  = 0;
    snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;
    snapshot->term  = false;
}

/****/
OBJ_CLASS_INSTANCE(orte_snapc_base_global_snapshot_t,
                   opal_list_item_t,
                   orte_snapc_base_global_snapshot_construct,
                   orte_snapc_base_global_snapshot_destruct);

void orte_snapc_base_global_snapshot_construct(orte_snapc_base_global_snapshot_t *snapshot)
{
    OBJ_CONSTRUCT(&(snapshot->snapshots), opal_list_t);

    snapshot->component_name = NULL;
    snapshot->reference_name = orte_snapc_base_unique_global_snapshot_name(getpid());
    snapshot->local_location = opal_dirname(orte_snapc_base_get_global_snapshot_directory(snapshot->reference_name));
    
    snapshot->seq_num    = 0;
    snapshot->start_time = NULL;
    snapshot->end_time   = NULL;
}

void orte_snapc_base_global_snapshot_destruct( orte_snapc_base_global_snapshot_t *snapshot)
{
    opal_list_item_t* item = NULL;

    while (NULL != (item = opal_list_remove_first(&snapshot->snapshots))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&(snapshot->snapshots));

    if(NULL != snapshot->reference_name) {
        free(snapshot->reference_name);
        snapshot->reference_name = NULL;
    }

    if(NULL != snapshot->component_name) {
        free(snapshot->component_name);
        snapshot->component_name = NULL;
    }

    if(NULL != snapshot->local_location) {
        free(snapshot->local_location);
        snapshot->local_location = NULL;
    }

    if(NULL != snapshot->start_time) {
        free(snapshot->start_time);
        snapshot->start_time = NULL;
    }

    if(NULL != snapshot->end_time) {
        free(snapshot->end_time);
        snapshot->end_time = NULL;
    }

    snapshot->seq_num = 0;
}

/***********************
 * None component stuff
 ************************/
int orte_snapc_base_none_open(void)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_none_close(void)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_module_init(bool seed, bool app)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_module_finalize(void)
{
    return ORTE_SUCCESS;
}

/* None RML response callback */
static void snapc_none_global_recv(int status,
                                   orte_process_name_t* sender,
                                   orte_buffer_t *buffer,
                                   orte_rml_tag_t tag,
                                   void* cbdata);
/* None GPR response callback */
static void snapc_none_job_ckpt_request_callback(orte_gpr_notify_data_t *data, void *cbdata);

int orte_snapc_base_none_setup_job(orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Setup the checkpoint request callback
     * Do this so we can respond with a 'not checkpointable' response
     * to any requesting tools
     */
    orte_snapc_base_snapshot_seq_number = -1;
    if(ORTE_SUCCESS != (ret = orte_snapc_base_global_init_request(jobid,
                                                                  snapc_none_global_recv,    NULL, /* RML */
                                                                  snapc_none_job_ckpt_request_callback, NULL) /* GPR */
                        ) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_snapc_base_none_release_job(orte_jobid_t jobid)
{
    /*
     * Remove the checkpoint request callback
     */

    return ORTE_SUCCESS;
}

/********************
 * Local Functions
 ********************/
/* None RML response callback */
static void snapc_none_global_recv(int status,
                                   orte_process_name_t* sender,
                                   orte_buffer_t *buffer,
                                   orte_rml_tag_t tag,
                                   void* cbdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    size_t command;
    orte_std_cntr_t n = 1;
    bool term = false;
    orte_jobid_t jobid;

    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &command, &n, ORTE_CKPT_CMD))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * orte_checkpoint has requested that a checkpoint be taken
     * Respond that a checkpoint cannot be taken at this time
     */
    if (ORTE_SNAPC_GLOBAL_INIT_CMD == command) {
        /*
         * Do the basic handshake with the orte_checkpoint command
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_init_cmd(sender, &term, &jobid)) ) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Respond with an invalid response
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(sender, NULL, -1, ORTE_SNAPC_CKPT_STATE_NO_CKPT)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    /*
     * Unknown command
     */
    else {
        goto cleanup;
    }
    
 cleanup:

    return;
}

/* None GPR response callback */
static void snapc_none_job_ckpt_request_callback(orte_gpr_notify_data_t *data, void *cbdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_gpr_value_t **values;
    orte_jobid_t jobid;
    orte_std_cntr_t i;
    size_t job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
    size_t *size_ptr;

    /*
     * Get jobid from the segment name in the first value
     */
    values = (orte_gpr_value_t**)(data->values)->addr;
    if (ORTE_SUCCESS != (ret = orte_schema.extract_jobid_from_segment_name(&jobid,
                                                                           values[0]->segment))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Get the state change (ORTE_JOB_CKPT_STATE_KEY)
     */
    for( i = 0; i < values[0]->cnt; ++i) {
        orte_gpr_keyval_t* keyval = values[0]->keyvals[i];
        if(strcmp(keyval->key, ORTE_JOB_CKPT_STATE_KEY) == 0) {
            if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(size_ptr), keyval->value, ORTE_SIZE))) {
                exit_status = ret;
                goto cleanup;
            }
            job_ckpt_state = *size_ptr;
        }
    }
    
    if(ORTE_SNAPC_CKPT_STATE_REQUEST == job_ckpt_state ) {
        /*
         * Replace with an invalid state
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_set_job_ckpt_info(jobid, 
                                                                     ORTE_SNAPC_CKPT_STATE_NO_CKPT,
                                                                     strdup(""),  /* Snapshot ref */
                                                                     strdup("") ) /* Snapshot loc */ 
                             ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return;
}

/********************
 * Utility functions
 ********************/
int orte_snapc_base_global_coord_ckpt_init_cmd(orte_process_name_t* peer, bool *term, orte_jobid_t *jobid)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_buffer_t *loc_buffer;
    orte_std_cntr_t n = 1;
    pid_t my_pid;
    bool ack = true;

    /*
     * Do not send to self, as that is silly.
     */
    if (0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, peer, ORTE_PROC_MY_HNP) ||
        0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, peer, ORTE_PROC_MY_NAME) ) {
        opal_output_verbose(10, orte_snapc_base_output,
                            "snapc:base: ckpt_init_cmd: Error: Do not send to self!\n");
        return ORTE_SUCCESS;
    }

    opal_output_verbose(10, orte_snapc_base_output,
                        "snapc:base: ckpt_init_cmd: Sending commands\n");

    /*
     * Setup the buffer that we may send back
     */
    if( NULL == (loc_buffer = OBJ_NEW(orte_buffer_t) ) ) {
        exit_status = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
    
    /********************
     * Send over our PID for a sanity check
     ********************/
    my_pid = getpid();
    if (ORTE_SUCCESS != (ret = orte_dss.pack(loc_buffer, &my_pid, 1, ORTE_PID))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: DSS Pack (PID) failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    
    if (0 > (ret = orte_rml.send_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: Send Buffer (PID) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    
    /* ACK */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_recv_ack(peer, &ack)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: ACK (PID) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    if( !ack ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: NACK (PID) (LINE = %d)\n",
                    __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /********************
     * Receive Term flag
     ********************/
    OBJ_RELEASE(loc_buffer);
    if( NULL == (loc_buffer = OBJ_NEW(orte_buffer_t) ) ) {
        exit_status = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    if( 0 > (ret = orte_rml.recv_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: Recv (term) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, term, &n, ORTE_BOOL)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: DSS Unpack (term) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    
    /********************
     * Receive the jobid
     ********************/
    OBJ_RELEASE(loc_buffer);
    if( NULL == (loc_buffer = OBJ_NEW(orte_buffer_t) ) ) {
        exit_status = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    if( 0 > (ret = orte_rml.recv_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: Recv (jobid) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, jobid, &n, ORTE_SIZE)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_init_cmd: Error: DSS Unpack (jobid) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_RELEASE(loc_buffer);
    return exit_status;
}

int orte_snapc_base_global_coord_recv_ack(orte_process_name_t* peer, bool *ack)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_buffer_t *loc_buffer;
    orte_std_cntr_t n = 1;

    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if( 0 > (ret = orte_rml.recv_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: recv_ack: Error: Recv Failed: %d\n",
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, ack, &n, ORTE_BOOL)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: recv_ack: Error: Unpack Failed: %d\n",
                    ret);
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    OBJ_RELEASE(loc_buffer);

    return exit_status;
}

int orte_snapc_base_global_coord_send_ack(orte_process_name_t* peer, bool ack)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_buffer_t *buffer;

    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_dss.pack(buffer, &ack, 1, ORTE_BOOL))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: send_ack: Error: Pack Failed: %d\n",
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, buffer, ORTE_RML_TAG_CKPT, 0))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: send_ack: Error: Send Failed: %d\n",
                    ret);
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    OBJ_RELEASE(buffer);

    return exit_status;
}

int orte_snapc_base_global_coord_ckpt_update_cmd(orte_process_name_t* peer, char *global_snapshot_handle,
                                                 int seq_num, int ckpt_status)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_buffer_t *loc_buffer = NULL;
    bool ack = true;
    size_t str_len = 0;

    /*
     * Do not send to self, as that is silly.
     */
    if (0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, peer, ORTE_PROC_MY_HNP) ||
        0 == orte_ns.compare_fields(ORTE_NS_CMP_ALL, peer, ORTE_PROC_MY_NAME) ) {
        opal_output_verbose(10, orte_snapc_base_output,
                            "snapc:base: ckpt_update_cmd: Error: Do not send to self!\n");
        return ORTE_SUCCESS;
    }

    opal_output_verbose(10, orte_snapc_base_output,
                        "snapc:base: ckpt_update_cmd: Sending update command <%s> <%d> <%d>\n",
                        global_snapshot_handle, seq_num, ckpt_status);

    /*
     * Setup the buffer that we may send back
     */
    if( NULL == (loc_buffer = OBJ_NEW(orte_buffer_t) ) ) {
        exit_status = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /********************
     * Send over the status of the checkpoint
     ********************/
    if(NULL != loc_buffer) {
        OBJ_RELEASE(loc_buffer);
        loc_buffer = NULL;
    }
    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_dss.pack(loc_buffer, &ckpt_status, 1, ORTE_INT))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: DSS Pack (ckpt_status) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    if (0 > (ret = orte_rml.send_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: Send (ckpt_status) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }

    /* ACK */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_recv_ack(peer, &ack)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: ACK (ckpt_status) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    if( !ack ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: NACK (ckpt_status) (LINE = %d)\n",
                    __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /* If we cannot checkpoint, then just skip to the end */
    if( ORTE_SNAPC_CKPT_STATE_NO_CKPT == ckpt_status) {
        goto cleanup;
    }

    /********************
     * Send over the size of the global snapshot handle
     ********************/
    if(NULL != loc_buffer) {
        OBJ_RELEASE(loc_buffer);
        loc_buffer = NULL;
    }
    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    str_len = strlen(global_snapshot_handle);
    if (ORTE_SUCCESS != (ret = orte_dss.pack(loc_buffer, &str_len, 1, ORTE_SIZE))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: DSS Pack (snapshot ref length) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    if (0 > (ret = orte_rml.send_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: Send (snapshot ref length) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }

    /* ACK */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_recv_ack(peer, &ack)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: Send (snapshot ref length) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    if( !ack ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: NACK (snapshot ref length) (LINE = %d)\n",
                    __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /********************
     * Send over the global snapshot handle & sequence number
     ********************/
    if(NULL != loc_buffer) {
        OBJ_RELEASE(loc_buffer);
        loc_buffer = NULL;
    }
    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_dss.pack(loc_buffer, &global_snapshot_handle, 1, ORTE_STRING))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: DSS Pack (snapshot handle) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    if (ORTE_SUCCESS != (ret = orte_dss.pack(loc_buffer, &seq_num, 1, ORTE_INT))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: DSS Pack (seq number) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }
    if (0 > (ret = orte_rml.send_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: Send (snapshot handle, seq number) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ret;
        goto cleanup;
    }

    /* ACK */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_recv_ack(peer, &ack)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: ACK (snapshot handle, seq number) Failure (ret = %d) (LINE = %d)\n",
                    ret, __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    if( !ack ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base: ckpt_update_cmd: Error: NACK (snapshot handle, seq number) (LINE = %d)\n",
                    __LINE__);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

 cleanup:
    if(NULL != loc_buffer) {
        OBJ_RELEASE(loc_buffer);
        loc_buffer = NULL;
    }

    return exit_status;
}

char * orte_snapc_base_unique_global_snapshot_name(pid_t pid)
{
    char * uniq_name;

    if( NULL == orte_snapc_base_global_snapshot_ref ) {
        asprintf(&uniq_name, "ompi_global_snapshot_%d.ckpt", pid);
    }
    else {
        uniq_name = strdup(orte_snapc_base_global_snapshot_ref);
    }
    
    return uniq_name;
}

char * orte_snapc_base_get_global_snapshot_metadata_file(char *uniq_snapshot_name)
{
    char * path = NULL;
    
    asprintf(&path, "%s/%s/%s",
             orte_snapc_base_global_snapshot_dir,
             uniq_snapshot_name,
             orte_snapc_base_metadata_filename);
    
    return path;
}

char * orte_snapc_base_get_global_snapshot_directory(char *uniq_snapshot_name)
{
    char * dir_name = NULL;
    
    asprintf(&dir_name, "%s/%s/%d", 
             orte_snapc_base_global_snapshot_dir, 
             uniq_snapshot_name,
             (int)orte_snapc_base_snapshot_seq_number);

    return dir_name;
}

int orte_snapc_base_init_global_snapshot_directory(char *uniq_global_snapshot_name, bool empty_metadata)
{
    char * dir_name = NULL, *meta_data_fname = NULL;
    mode_t my_mode = S_IRWXU;
    int ret;
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;

    /*
     * Make the snapshot directory from the uniq_global_snapshot_name
     */
    dir_name = orte_snapc_base_get_global_snapshot_directory(uniq_global_snapshot_name);
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(dir_name, my_mode)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Initialize the metadata file at the top of that directory.
     */
    meta_data_fname = orte_snapc_base_get_global_snapshot_metadata_file(uniq_global_snapshot_name);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "orte:snapc:base: init_global_snapshot_directory: Error: Unable to open the file (%s)\n",
                    meta_data_fname);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /*
     * Put in the checkpoint sequence number
     */
    if( empty_metadata ) {
        fprintf(meta_data, "#\n");
    }
    else {
        /*
         * Put in the checkpoint sequence number
         */
        fprintf(meta_data, "#\n%s%d\n", SNAPC_METADATA_SEQ, (int)orte_snapc_base_snapshot_seq_number);

        fclose(meta_data);
        meta_data = NULL;

        /* Add timestamp */
        orte_snapc_base_add_timestamp(uniq_global_snapshot_name);
    }

 cleanup:
    if(NULL != meta_data)
        fclose(meta_data);
    if(NULL != dir_name)
        free(dir_name);
    if(NULL != meta_data_fname)
        free(meta_data_fname);

    return OPAL_SUCCESS;
}

int orte_snapc_base_global_init_request(orte_jobid_t jobid,
                                        orte_rml_buffer_callback_fn_t rml_cbfunc, void* rml_cbdata,
                                        orte_gpr_notify_cb_fn_t       gpr_cbfunc, void* gpr_cbdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    
    /*
     * Setup RML Callback in case the user uses orte_checkpoint()
     */
    if( ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, 
                                                       ORTE_RML_TAG_CKPT, 
                                                       0, 
                                                       rml_cbfunc, 
                                                       NULL)) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Setup GPR Callbacks in case the application initiates a global checkpoint
     */
    if( ORTE_SUCCESS != (ret = snapc_base_reg_gpr_request(jobid, gpr_cbfunc, gpr_cbdata))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_base_reg_gpr_request( orte_jobid_t jobid, orte_gpr_notify_cb_fn_t gpr_cbfunc, void* gpr_cbdata )
{
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, *trig_name = NULL, *tokens[2];
    orte_gpr_subscription_id_t id;
    char* keys[] = {
        ORTE_JOB_CKPT_STATE_KEY,
        NULL
    };
    char* trig_names[] = {
        ORTE_JOB_CKPT_STATE_TRIGGER,
        NULL
    };

    /*
     * Setup the tokens
     */
    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    /*
     * Identify the segment for this job
     */
    if( ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, jobid))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Attach to the standard trigger
     */
    if( ORTE_SUCCESS != (ret = orte_schema.get_std_trigger_name(&trig_name, trig_names[0], jobid))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Subscribe to the GPR
     */
    if( ORTE_SUCCESS != (ret = orte_gpr.subscribe_1(&id,
                                                    trig_name,
                                                    NULL,
                                                    ORTE_GPR_NOTIFY_VALUE_CHG,
                                                    ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                                    segment,
                                                    tokens,
                                                    keys[0],
                                                    gpr_cbfunc,
                                                    NULL))) {
        exit_status = ret;
        goto cleanup;
    }
    
    
 cleanup:
    if(NULL != segment)
        free(segment);
    if(NULL != trig_name)
        free(trig_name);
    
    return exit_status;
}

int orte_snapc_base_get_job_ckpt_info( orte_jobid_t jobid, 
                                       size_t *ckpt_state, 
                                       char **ckpt_snapshot_ref, 
                                       char **ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, *tokens[2], *keys[4];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, j;
    orte_std_cntr_t num_values = 0;
    size_t *tmp_state = 0;
    char   *tmp_snap = NULL;

    /*
     * Setup tokens and keys
     */
    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    keys[0] = ORTE_JOB_CKPT_STATE_KEY;
    keys[1] = ORTE_JOB_CKPT_SNAPSHOT_REF_KEY;
    keys[2] = ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY;
    keys[3] = NULL;
    
    /* 
     * Get the job segment
     */
    if(ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, jobid))) {
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

            if(strcmp(keyval->key, ORTE_JOB_CKPT_STATE_KEY) == 0) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_state), keyval->value, ORTE_SIZE))) {
                    exit_status = ret;
                    goto cleanup;
                }
                *ckpt_state = *tmp_state;
                continue;
            }
            else if (strcmp(keyval->key, ORTE_JOB_CKPT_SNAPSHOT_REF_KEY) == 0) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                    exit_status = ret;
                    goto cleanup;
                }
                *ckpt_snapshot_ref = strdup(tmp_snap);
                if(NULL != tmp_snap) {
                    free(tmp_snap);
                    tmp_snap = NULL;
                }
                continue;
            }
            else if (strcmp(keyval->key, ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY) == 0) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                    exit_status = ret;
                    goto cleanup;
                }
                *ckpt_snapshot_loc = strdup(tmp_snap);
                if(NULL != tmp_snap) {
                    free(tmp_snap);
                    tmp_snap = NULL;
                }
                continue;
            }
        }
    }
    
 cleanup:
    if( NULL != segment) 
        free(segment);

    if(NULL != tmp_snap) {
        free(tmp_snap);
        tmp_snap = NULL;
    }

    return exit_status;
}

int orte_snapc_base_set_job_ckpt_info( orte_jobid_t jobid,
                                       size_t ckpt_state, 
                                       char  *ckpt_snapshot_ref,
                                       char  *ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_gpr_value_t **values = NULL;
    char *segment = NULL;

    if(ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, jobid))) {
        exit_status = ret;
        goto cleanup;
    }
    
    values = (orte_gpr_value_t**)malloc(1 * sizeof(orte_gpr_value_t*));
    if(NULL == values) {
        exit_status = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_gpr.create_value(&(values[0]), 
                                                     ORTE_GPR_OVERWRITE,
                                                     segment, 
                                                     3, 
                                                     0))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Get the tokens for the job
     */
    if( ORTE_SUCCESS != (ret = orte_schema.get_job_tokens(&(values[0]->tokens),
                                                          &(values[0]->num_tokens),
                                                          jobid) ) ) {
        exit_status = ret;
        goto cleanup;
    }
    values[0]->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    values[0]->tokens[1] = NULL;

    /*
     * Set the values
     */
    if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[0]),
                                                      ORTE_JOB_CKPT_STATE_KEY,
                                                      ORTE_SIZE,
                                                      &ckpt_state))) {
        exit_status = ret;
        goto cleanup;
    }
    if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[1]),
                                                      ORTE_JOB_CKPT_SNAPSHOT_REF_KEY,
                                                      ORTE_STRING,
                                                      ckpt_snapshot_ref
                                                      ))) {
        exit_status = ret;
        goto cleanup;
    }
    if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[2]),
                                                      ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY,
                                                      ORTE_STRING,
                                                      ckpt_snapshot_loc
                                                      ))) {
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_gpr.put(1, values))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(NULL != segment) {
        free(segment);
        segment = NULL;
    }
    if( NULL != values) {
        OBJ_RELEASE(values[0]);
        free(values);
        values = NULL;
    }

    return exit_status;
}

int orte_snapc_base_get_vpid_ckpt_info( orte_process_name_t proc, 
                                        size_t *ckpt_state,
                                        char **ckpt_snapshot_ref, 
                                        char **ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, **tokens = NULL, *keys[4];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, j, num_values = 0, num_tokens = 0;
    size_t *tmp_state = 0;
    char   *tmp_snap = NULL;

    keys[0] = ORTE_PROC_CKPT_STATE_KEY;
    keys[1] = ORTE_PROC_CKPT_SNAPSHOT_REF_KEY;
    keys[2] = ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY;
    keys[3] = NULL;

    /* 
     * Get the job segment
     */
    if(ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, proc.jobid))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Get the process tokens
     */
    if (ORTE_SUCCESS != (ret = orte_schema.get_proc_tokens(&tokens,
                                                           &num_tokens,
                                                           &proc) )) {
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

            if(strcmp(keyval->key, ORTE_PROC_CKPT_STATE_KEY) == 0) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_state), keyval->value, ORTE_SIZE))) {
                    exit_status = ret;
                    goto cleanup;
                }
                *ckpt_state = *tmp_state;
                continue;
            }
            else if (strcmp(keyval->key, ORTE_PROC_CKPT_SNAPSHOT_REF_KEY) == 0) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                    exit_status = ret;
                    goto cleanup;
                }
                *ckpt_snapshot_ref = strdup(tmp_snap);
                if(NULL != tmp_snap) {
                    free(tmp_snap);
                    tmp_snap = NULL;
                }
                continue;
            }
            else if (strcmp(keyval->key, ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY) == 0) {
                if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                    exit_status = ret;
                    goto cleanup;
                }
                *ckpt_snapshot_loc = strdup(tmp_snap);
                if(NULL != tmp_snap) {
                    free(tmp_snap);
                    tmp_snap = NULL;
                }
                continue;
            }
        }
    } 
   
 cleanup:
    if( NULL != segment) 
        free(segment);

    if(NULL != tmp_snap) {
        free(tmp_snap);
        tmp_snap = NULL;
    }

    return exit_status;
}

int orte_snapc_base_set_vpid_ckpt_info( orte_process_name_t proc, 
                                   size_t ckpt_state, 
                                   char *ckpt_ref, 
                                   char *ckpt_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_gpr_value_t *values[1];
    char *segment = NULL;
    
    /*
     * Get Job segment
     */
    if(ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&segment, proc.jobid))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Create the value structure
     */
    if (ORTE_SUCCESS != (ret = orte_gpr.create_value(&values[0], 
                                                     ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                                     segment, 
                                                     3, 
                                                     0))) {
        
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Get the tokens for the process
     */
    if (ORTE_SUCCESS != (ret = orte_schema.get_proc_tokens(&(values[0]->tokens), 
                                                           &(values[0]->num_tokens), 
                                                           &proc) )) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Set the values
     */
    if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[0]), 
                                                      ORTE_PROC_CKPT_STATE_KEY, 
                                                      ORTE_SIZE, 
                                                      &ckpt_state)) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[1]), 
                                                      ORTE_PROC_CKPT_SNAPSHOT_REF_KEY, 
                                                      ORTE_STRING, 
                                                      ckpt_ref
                                                      ) ) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    if (ORTE_SUCCESS != (ret = orte_gpr.create_keyval(&(values[0]->keyvals[2]), 
                                                      ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY, 
                                                      ORTE_STRING, 
                                                      ckpt_loc
                                                      )) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Put it in the GPR
     */
    if (ORTE_SUCCESS != (ret = orte_gpr.put(1, values)) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != segment)
        free(segment);

    return exit_status;
}

int orte_snapc_base_extract_gpr_vpid_ckpt_info(orte_gpr_notify_data_t *data,
                                               orte_process_name_t **proc,
                                               size_t *ckpt_state,
                                               char **ckpt_snapshot_ref,
                                               char **ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_gpr_value_t **values, *value;
    orte_std_cntr_t i, j, k;
    orte_gpr_keyval_t** keyvals;
    size_t *tmp_state = 0;

    values = (orte_gpr_value_t**)(data->values)->addr;

    /*
     * Extract the process name from the proper token
     */
    if( ORTE_SUCCESS != (ret = orte_ns.convert_string_to_process_name(proc, values[0]->tokens[0]) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Parse the values structure to get the values that changed
     */
    for(i = 0, k = 0; 
        k < data->cnt && 
            i < (data->values)->size; 
        ++i) {

        if( NULL != values[i]) {
            ++k;
            value = values[i];
            keyvals = value->keyvals;

            for(j = 0; j < value->cnt; ++j) {
                orte_gpr_keyval_t* keyval = keyvals[j];
                if(strcmp(keyval->key, ORTE_PROC_CKPT_STATE_KEY) == 0) {
                    if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_state), keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    *ckpt_state = *tmp_state;
                    continue;
                }
                else if(strcmp(keyval->key, ORTE_PROC_CKPT_SNAPSHOT_REF_KEY) == 0) {
                    char * tmp_snap = NULL;
                    if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    *ckpt_snapshot_ref = strdup(tmp_snap);
                    continue;
                }
                else if(strcmp(keyval->key, ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY) == 0) {
                    char * tmp_snap = NULL;
                    if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    *ckpt_snapshot_loc = strdup(tmp_snap);

                    continue;
                }
            }
        }
    }

 cleanup:

    return exit_status;
}

int orte_snapc_base_extract_gpr_job_ckpt_info(orte_gpr_notify_data_t *data,
                                              size_t *ckpt_state,
                                              char **ckpt_snapshot_ref,
                                              char **ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_gpr_value_t **values, *value;
    orte_std_cntr_t i, j, k;
    orte_gpr_keyval_t** keyvals;
    size_t *tmp_state = 0;

    values = (orte_gpr_value_t**)(data->values)->addr;

    /*
     * Parse the values structure to get the values that changed
     */
    for(i = 0, k = 0; 
        k < data->cnt && 
            i < (data->values)->size; 
        ++i) {

        if( NULL != values[i]) {
            ++k;
            value = values[i];
            keyvals = value->keyvals;

            for(j = 0; j < value->cnt; ++j) {
                orte_gpr_keyval_t* keyval = keyvals[j];
                if(strcmp(keyval->key, ORTE_JOB_CKPT_STATE_KEY) == 0) {
                    if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_state), keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    *ckpt_state = *tmp_state;
                    continue;
                }
                else if(strcmp(keyval->key, ORTE_JOB_CKPT_SNAPSHOT_REF_KEY) == 0) {
                    char * tmp_snap = NULL;
                    if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    *ckpt_snapshot_ref = strdup(tmp_snap);
                    continue;
                }
                else if(strcmp(keyval->key, ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY) == 0) {
                    char * tmp_snap = NULL;
                    if (ORTE_SUCCESS != (ret = orte_dss.get((void**)&(tmp_snap), keyval->value, ORTE_STRING))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    *ckpt_snapshot_loc = strdup(tmp_snap);

                    continue;
                }
            }
        }
    }

 cleanup:

    return exit_status;
}

/*
 * Metadata file handling functions
 * File is of the form:
 *
 * #
 * # Checkpoint Sequence #
 * # Begin Timestamp
 * # Process ID
 * # OPAL CRS
 * opal_restart ----mca crs_base_snapshot_dir SNAPSHOT_LOC SNAPSHOT_REF
 * ...
 * # End Timestamp
 *
 * E.g.,
 #
 # Seq: 0
 # Timestamp:          Mon Jun  5 18:32:08 2006
 # Process:            0.1.0
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/0 opal_snapshot_0.ckpt
 # Process:            0.1.1
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/0 opal_snapshot_1.ckpt
 # Timestamp:          Mon Jun  5 18:32:10 2006
 #
 # Seq: 1
 # Timestamp:          Mon Jun  5 18:32:12 2006
 # Process:            0.1.0
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/1 opal_snapshot_0.ckpt
 # Process:            0.1.1
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/1 opal_snapshot_1.ckpt
 # Timestamp:          Mon Jun  5 18:32:13 2006
 *
 */
int orte_snapc_base_add_timestamp(char * global_snapshot_ref)
{
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;
    time_t timestamp;

    meta_data_fname = orte_snapc_base_get_global_snapshot_metadata_file(global_snapshot_ref);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "orte:snapc:base: orte_snapc_base_add_timestamp: Error: Unable to open the file (%s)\n",
                    meta_data_fname);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    timestamp = time(NULL);
    fprintf(meta_data, "%s%s", SNAPC_METADATA_TIME, ctime(&timestamp));

 cleanup:
    if( NULL != meta_data )
        fclose(meta_data);
    if( NULL != meta_data_fname)
        free(meta_data_fname);
    
    return exit_status;
}

int orte_snapc_base_finalize_metadata(char * global_snapshot_ref)
{
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;

    /* Add the final timestamp */
    orte_snapc_base_add_timestamp(global_snapshot_ref);

    meta_data_fname = orte_snapc_base_get_global_snapshot_metadata_file(global_snapshot_ref);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "orte:snapc:base: orte_snapc_base_add_timestamp: Error: Unable to open the file (%s)\n",
                    meta_data_fname);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    fprintf(meta_data, "%s%d\n", SNAPC_METADATA_DONE_SEQ, (int)orte_snapc_base_snapshot_seq_number);

 cleanup:
    if( NULL != meta_data )
        fclose(meta_data);
    if( NULL != meta_data_fname)
        free(meta_data_fname);
    
    return exit_status;
}


int orte_snapc_base_add_vpid_metadata( orte_process_name_t *proc,
                                       char * global_snapshot_ref,
                                       char *snapshot_ref,
                                       char *snapshot_location)
{
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;
    char * proc_str = NULL, *crs_comp = NULL;
    char * local_dir = NULL;
    int prev_pid = 0;

    meta_data_fname = orte_snapc_base_get_global_snapshot_metadata_file(global_snapshot_ref);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "orte:snapc:base: orte_snapc_base_add_metadata: Error: Unable to open the file (%s)\n",
                    meta_data_fname);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /* 
     * Something of the form:
     * 0.1.0 opal_snapshot_0.ckpt /tmp/ompi_global_snapshot_8827.ckpt/1/opal_snapshot_0.ckpt BLCR 
     * or better yet start to create the proper app schema:
     * orte_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_8827.ckpt/1 opal_snapshot_0.ckpt
     */
    /* Get the process string */
    orte_ns.get_proc_name_string(&proc_str, proc);

    /* Extract the checkpointer */
    crs_comp = opal_crs_base_extract_expected_component(snapshot_location, &prev_pid);
    if( NULL == crs_comp ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /* get the base of the location */
    local_dir = strdup(snapshot_location);
    local_dir = opal_dirname(local_dir);

    /* Write the string */
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_PROCESS,   proc_str);
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_CRS_COMP,  crs_comp);
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_SNAP_REF,  snapshot_ref);
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_SNAP_LOC,  local_dir);

 cleanup:
    if( NULL != meta_data )
        fclose(meta_data);
    if( NULL != meta_data_fname)
        free(meta_data_fname);
    
    if( NULL != local_dir)
        free(local_dir);

    return exit_status;
}

int orte_snapc_base_extract_metadata(orte_snapc_base_global_snapshot_t *global_snapshot)
{
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;
    int    next_seq_int;
    char * token = NULL;
    char * value = NULL;
    orte_snapc_base_snapshot_t *vpid_snapshot = NULL;

    /*
     * Open the metadata file
     */
    meta_data_fname = orte_snapc_base_get_global_snapshot_metadata_file(global_snapshot->reference_name);
    if (NULL == (meta_data = fopen(meta_data_fname, "r")) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /* 
     * If we were not given a sequence number, first find the largest valid seq number
     */
    if(0 > global_snapshot->seq_num ) {
        while(0 <= (next_seq_int = get_next_valid_seq_number(meta_data)) ){
            global_snapshot->seq_num = next_seq_int;
        }
        rewind(meta_data);
    }

    /*
     * Find the requested sequence number, 
     */
    while( global_snapshot->seq_num != (next_seq_int = get_next_seq_number(meta_data)) ) {
        /* We didn't find the requested seq */
        if(0 > next_seq_int) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }

    /*
     * Extract each token and make the records
     */
    do {
        if( ORTE_SUCCESS != metadata_extract_next_token(meta_data, &token, &value) ) {
            break;
        }
        
        if(0 == strncmp(SNAPC_METADATA_SEQ, token, strlen(SNAPC_METADATA_SEQ)) ) {
            break;
        }
        else if(0 == strncmp(SNAPC_METADATA_TIME, token, strlen(SNAPC_METADATA_TIME)) ) {
            if( NULL == global_snapshot->start_time) {
                global_snapshot->start_time = strdup(value);
            }
            else {
                global_snapshot->end_time = strdup(value);
            }
        }
        else if(0 == strncmp(SNAPC_METADATA_PROCESS, token, strlen(SNAPC_METADATA_PROCESS)) ) {
            orte_process_name_t *proc;

            orte_ns.convert_string_to_process_name(&proc, value);            

            /* Not the first process, so append it to the list */
            if( NULL != vpid_snapshot) {
                opal_list_append(&global_snapshot->snapshots, &(vpid_snapshot->crs_snapshot_super.super));
            }

            vpid_snapshot = OBJ_NEW(orte_snapc_base_snapshot_t);

            vpid_snapshot->process_name.jobid  = proc->jobid;
            vpid_snapshot->process_name.vpid   = proc->vpid;
        }
        else if(0 == strncmp(SNAPC_METADATA_CRS_COMP, token, strlen(SNAPC_METADATA_CRS_COMP)) ) {
            vpid_snapshot->crs_snapshot_super.component_name = strdup(value);
        }
        else if(0 == strncmp(SNAPC_METADATA_SNAP_REF, token, strlen(SNAPC_METADATA_SNAP_REF)) ) {
            vpid_snapshot->crs_snapshot_super.reference_name = strdup(value);
        }
        else if(0 == strncmp(SNAPC_METADATA_SNAP_LOC, token, strlen(SNAPC_METADATA_SNAP_LOC)) ) {
            vpid_snapshot->crs_snapshot_super.local_location  = strdup(value);
            vpid_snapshot->crs_snapshot_super.remote_location = strdup(value);
        }
    } while(0 == feof(meta_data) );
    
    /* Append the last item */
    if( NULL != vpid_snapshot) {
        opal_list_append(&global_snapshot->snapshots, &(vpid_snapshot->crs_snapshot_super.super));
    }
    
 cleanup:
    if(NULL != meta_data)
        fclose(meta_data);
    if(NULL != meta_data_fname)
        free(meta_data_fname);

    return exit_status;
}

/*
 * Extract the next sequence number from the file
 */
static int get_next_seq_number(FILE *file)
{
    char *token = NULL;
    char *value = NULL;
    int seq_int = -1;

    do {
        if( ORTE_SUCCESS != metadata_extract_next_token(file, &token, &value) ) {
            seq_int = -1;
            goto cleanup;
        }
    } while(0 != strncmp(token, SNAPC_METADATA_SEQ, strlen(SNAPC_METADATA_SEQ)) );

    seq_int = atoi(value);

 cleanup:
    if( NULL != token)
        free(token);
    if( NULL != value)
        free(value);

    return seq_int;
}

/*
 * Extract the next Valid sequence number from the file
 */
static int get_next_valid_seq_number(FILE *file)
{
    char *token = NULL;
    char *value = NULL;
    int seq_int = -1;

    do {
        if( ORTE_SUCCESS != metadata_extract_next_token(file, &token, &value) ) {
            seq_int = -1;
            goto cleanup;
        }
    } while(0 != strncmp(token, SNAPC_METADATA_DONE_SEQ, strlen(SNAPC_METADATA_DONE_SEQ)) );

    seq_int = atoi(value);

 cleanup:
    if( NULL != token)
        free(token);
    if( NULL != value)
        free(value);

    return seq_int;
}

static int metadata_extract_next_token(FILE *file, char **token, char **value)
{
    int exit_status = ORTE_SUCCESS;
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
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Other wise grab the next token/value pair
     */
    if (NULL == fgets(line, max_len, file) ) {
        exit_status = ORTE_ERROR;
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
            exit_status = ORTE_ERROR;
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
    if( NULL != local_token)
        free(local_token);
    if( NULL != local_value)
        free(local_value);
    if( NULL != line)
        free(line);

    return exit_status;
}

char * orte_snapc_ckpt_state_str(size_t state)
{
    switch(state) {
    case ORTE_SNAPC_CKPT_STATE_NONE:
        return strdup(" -- ");
        break;
    case ORTE_SNAPC_CKPT_STATE_REQUEST:
        return strdup("Requested");
        break;
    case ORTE_SNAPC_CKPT_STATE_PENDING_TERM:
        return strdup("Pending (Termination)");
        break;
    case ORTE_SNAPC_CKPT_STATE_PENDING:
        return strdup("Pending");
        break;
    case ORTE_SNAPC_CKPT_STATE_RUNNING:
        return strdup("Running");
        break;
    case ORTE_SNAPC_CKPT_STATE_FILE_XFER:
        return strdup("File Transfer");
        break;
    case ORTE_SNAPC_CKPT_STATE_FINISHED:
        return strdup("Finished");
        break;
    case ORTE_SNAPC_CKPT_STATE_ERROR:
        return strdup("Error");
        break;
    default:
        return strdup("Unknown");
    }
}

/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

/*
 * includes
 */
#include "prte_config.h"

#include <string.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/mca/mca.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"

#include "constants.h"
#include "src/mca/errmgr/errmgr.h"
#include "src/mca/filem/base/base.h"
#include "src/mca/filem/filem.h"
#include "src/rml/rml.h"
#include "src/mca/state/state.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_quit.h"
#include "src/util/proc_info.h"
#include "types.h"

/*
 * Functions to process some FileM specific commands
 */
static void filem_base_process_get_proc_node_name_cmd(pmix_proc_t *sender,
                                                      pmix_data_buffer_t *buffer);
static void filem_base_process_get_remote_path_cmd(pmix_proc_t *sender, pmix_data_buffer_t *buffer);

static bool recv_issued = false;

int prte_filem_base_comm_start(void)
{
    /* Only active in HNP and daemons */
    if (!PRTE_PROC_IS_MASTER && !PRTE_PROC_IS_DAEMON) {
        return PRTE_SUCCESS;
    }
    if (recv_issued) {
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((5, prte_filem_base_framework.framework_output,
                         "%s filem:base: Receive: Start command recv",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_FILEM_BASE,
                  PRTE_RML_PERSISTENT, prte_filem_base_recv, NULL);

    recv_issued = true;

    return PRTE_SUCCESS;
}

int prte_filem_base_comm_stop(void)
{
    /* Only active in HNP and daemons */
    if (!PRTE_PROC_IS_MASTER && !PRTE_PROC_IS_DAEMON) {
        return PRTE_SUCCESS;
    }
    if (recv_issued) {
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((5, prte_filem_base_framework.framework_output,
                         "%s filem:base:receive stop comm", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_FILEM_BASE);
    recv_issued = false;

    return PRTE_SUCCESS;
}

/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is PMIX_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */
void prte_filem_base_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                          prte_rml_tag_t tag, void *cbdata)
{
    prte_filem_cmd_flag_t command;
    int32_t count;
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    PMIX_OUTPUT_VERBOSE((5, prte_filem_base_framework.framework_output,
                         "%s filem:base: Receive a command message.",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    count = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buffer, &command, &count, PRTE_FILEM_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    switch (command) {
    case PRTE_FILEM_GET_PROC_NODE_NAME_CMD:
        PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                             "%s filem:base: Command: Get Proc node name command",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

        filem_base_process_get_proc_node_name_cmd(sender, buffer);
        break;

    case PRTE_FILEM_GET_REMOTE_PATH_CMD:
        PMIX_OUTPUT_VERBOSE((10, prte_filem_base_framework.framework_output,
                             "%s filem:base: Command: Get remote path command",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

        filem_base_process_get_remote_path_cmd(sender, buffer);
        break;

    default:
        PRTE_ERROR_LOG(PRTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
}

static void filem_base_process_get_proc_node_name_cmd(pmix_proc_t *sender,
                                                      pmix_data_buffer_t *buffer)
{
    pmix_data_buffer_t *answer;
    int32_t count;
    prte_job_t *jdata = NULL;
    prte_proc_t *proc = NULL;
    pmix_proc_t name;
    int rc;

    /*
     * Unpack the data
     */
    count = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buffer, &name, &count, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        return;
    }

    /*
     * Process the data
     */
    /* get the job data object for this proc */
    if (NULL == (jdata = prte_get_job_data_object(name.nspace))) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        return;
    }
    /* get the proc object for it */
    proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, name.rank);
    if (NULL == proc || NULL == proc->node) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        return;
    }

    /*
     * Send back the answer
     */
    PMIX_DATA_BUFFER_CREATE(answer);
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, answer, &(proc->node->name), 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return;
    }

    PRTE_RML_SEND(rc, sender->rank, answer, PRTE_RML_TAG_FILEM_BASE_RESP);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return;
    }
}

/*
 * This function is responsible for:
 * - Constructing the remote absolute path for the specified file/dir
 * - Verify the existence of the file/dir
 * - Determine if the specified file/dir is in fact a file or dir or unknown if not found.
 */
static void filem_base_process_get_remote_path_cmd(pmix_proc_t *sender, pmix_data_buffer_t *buffer)
{
    pmix_data_buffer_t *answer;
    int32_t count;
    char *filename = NULL;
    char *tmp_name = NULL;
    char cwd[PRTE_PATH_MAX];
    int file_type = PRTE_FILEM_TYPE_UNKNOWN;
    struct stat file_status;
    int rc;

    count = 1;
    rc = PMIx_Data_unpack(PRTE_PROC_MY_NAME, buffer, &filename, &count, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        goto CLEANUP;
    }

    /*
     * Determine the absolute path of the file
     */
    if (filename[0] != '/') { /* if it is not an absolute path already */
        if (NULL == getcwd(cwd, sizeof(cwd))) {
            return;
        }
        pmix_asprintf(&tmp_name, "%s/%s", cwd, filename);
    } else {
        tmp_name = strdup(filename);
    }

    pmix_output_verbose(10, prte_filem_base_framework.framework_output,
                        "filem:base: process_get_remote_path_cmd: %s -> %s: Filename Requested "
                        "(%s) translated to (%s)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender), filename,
                        tmp_name);

    /*
     * Determine if the file/dir exists at that absolute path
     * Determine if the file/dir is a file or a directory
     */
    if (0 != (rc = stat(tmp_name, &file_status))) {
        file_type = PRTE_FILEM_TYPE_UNKNOWN;
    } else {
        /* Is it a directory? */
        if (S_ISDIR(file_status.st_mode)) {
            file_type = PRTE_FILEM_TYPE_DIR;
        } else if (S_ISREG(file_status.st_mode)) {
            file_type = PRTE_FILEM_TYPE_FILE;
        }
    }

    /*
     * Pack up the response
     * Send back the reference type
     * - PRTE_FILEM_TYPE_FILE    = File
     * - PRTE_FILEM_TYPE_DIR     = Directory
     * - PRTE_FILEM_TYPE_UNKNOWN = Could not be determined, or does not exist
     */
    PMIX_DATA_BUFFER_CREATE(answer);
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, answer, &tmp_name, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_RELEASE(answer);
        goto CLEANUP;
    }
    rc = PMIx_Data_pack(PRTE_PROC_MY_NAME, answer, &file_type, 1, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_RELEASE(answer);
        goto CLEANUP;
    }

    PRTE_RML_SEND(rc, sender->rank, answer, PRTE_RML_TAG_FILEM_BASE_RESP);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_DATA_BUFFER_RELEASE(answer);
    }

CLEANUP:
    if (NULL != filename) {
        free(filename);
        filename = NULL;
    }
    if (NULL != tmp_name) {
        free(tmp_name);
        tmp_name = NULL;
    }
}

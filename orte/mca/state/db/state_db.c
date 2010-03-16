/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#include <sys/types.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <db.h>

#include "opal/dss/dss_types.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/util/basename.h"
#include "opal/mca/pstat/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/sysinfo/base/base.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/runtime/orte_globals.h"

#include "state_db.h"

static int init(void);
static int finalize(void);
static int save(void *object, opal_data_type_t type);
static int set_recover_source(orte_process_name_t *name);
static int recover(void *object, opal_data_type_t type);

orte_state_base_module_t orte_state_db_module = {
    init,
    finalize,
    save,
    set_recover_source,
    recover
};

/* local variables */
static DB *save_db=NULL, *recover_db=NULL;

static int init(void)
{
    char *path, *name;
    
    /* setup the database */
    if (ORTE_SUCCESS != opal_os_dirpath_create(orte_state_db_directory, S_IRWXU)) {
        orte_show_help("help-state-db.txt", "cannot-create-dir", true,
                       orte_state_db_directory);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    orte_util_convert_process_name_to_string(&name, ORTE_PROC_MY_NAME);
    path = opal_os_path(false, orte_state_db_directory, name, NULL);
    free(name);
    if (NULL == (save_db = dbopen(path, O_CREAT | O_RDWR | O_TRUNC, S_IRWXU, DB_HASH, NULL))) {
        orte_show_help("help-state-db.txt", "cannot-create-db", true, path);
        free(path);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    free(path);
    
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    /* if we are normally terminating, remove the recovery file */
    
    return ORTE_SUCCESS;
}

static int save(void *object, opal_data_type_t type)
{
    DBT key, data;
    opal_buffer_t buf;
    orte_job_t *jdata;
    orte_proc_t *proc;
    char *name;
    int rc=ORTE_SUCCESS, size;
    
    /* construct the buffer we will use for packing the data */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    key.data = NULL;
    data.data = NULL;
    
    switch (type) {
        case ORTE_JOB:
            jdata = (orte_job_t*)object;
            opal_dss.pack(&buf, &jdata->state, 1, ORTE_JOB_STATE_T);
            asprintf((char**)&key.data, "JOB:%s", ORTE_JOBID_PRINT(jdata->jobid));
            key.size = strlen(key.data);
            break;
        case ORTE_PROC:
            proc = (orte_proc_t*)object;
            opal_dss.pack(&buf, &proc->state, 1, ORTE_PROC_STATE_T);
            orte_util_convert_process_name_to_string(&name, &proc->name);
            asprintf((char**)&key.data, "PROC:%s", name);
            free(name);
            key.size = strlen(key.data);
            break;
        default:
            orte_show_help("help-state-db.txt", "unrecognized-type", true, type);
            rc = ORTE_ERR_BAD_PARAM;
            goto cleanup;
            break;
    }
    
    /* unload the data */
    opal_dss.unload(&buf, (void**)&data.data, &size);
    data.size = size;
    OBJ_DESTRUCT(&buf);
    
    /* put the info into the db */
    if (0 > save_db->put(save_db, &key, &data, 0)) {
        orte_show_help("help-state-db.txt", "error-writing-db", true, (char*)key.data, strerror(errno));
        rc = ORTE_ERR_FILE_WRITE_FAILURE;
    }
    /* sync it to force it to disk */
    if (0 > save_db->sync(save_db, 0)) {
        orte_show_help("help-state-db.txt", "error-syncing-db", true, (char*)key.data, strerror(errno));
        rc = ORTE_ERR_FILE_WRITE_FAILURE;
    }
    
cleanup:
    /* cleanup */
    if (NULL != key.data) {
        free(key.data);
    }
    if (NULL != data.data) {
        free(data.data);
    }
    return rc;
}

static int set_recover_source(orte_process_name_t *name)
{
    char *path, *pname;
    int rc=ORTE_SUCCESS;
    
    /* setup the database */
    orte_util_convert_process_name_to_string(&pname, name);
    path = opal_os_path(false, orte_state_db_directory, pname, NULL);
    free(pname);
    if (NULL == (recover_db = dbopen(path, O_RDONLY, S_IRWXU, DB_HASH, NULL))) {
        orte_show_help("help-state-db.txt", "cannot-open-db", true, path);
        free(path);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    free(path);
    
    return rc;
}

static int recover(void *object, opal_data_type_t type)
{
    DBT key, data;
    opal_buffer_t buf;
    orte_job_t *jdata;
    orte_proc_t *proc;
    char *name;
    int rc=ORTE_SUCCESS;
    int32_t n;
    orte_job_state_t *jstate;
    orte_proc_state_t *pstate;
    
    if (NULL == recover_db) {
        orte_show_help("help-state-db.txt", "recover-source-undef", true);
        rc = ORTE_ERR_NOT_FOUND;
    }
    
    /* construct the buffer we will use for unpacking the data */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    key.data = NULL;
    data.data = NULL;
    
    switch (type) {
        case ORTE_JOB:
            jdata = (orte_job_t*)object;
            asprintf((char**)&key.data, "JOB:%s", ORTE_JOBID_PRINT(jdata->jobid));
            key.size = strlen(key.data);
            break;
        case ORTE_PROC:
            proc = (orte_proc_t*)object;
            orte_util_convert_process_name_to_string(&name, &proc->name);
            asprintf((char**)&key.data, "PROC:%s", name);
            free(name);
            key.size = strlen(key.data);
            break;
        default:
            orte_show_help("help-state-db.txt", "unrecognized-type", true, type);
            rc = ORTE_ERR_BAD_PARAM;
            goto cleanup;
            break;
    }
    
    /* get the specified data */
    if (0 > recover_db->get(recover_db, &key, &data, 0)) {
        orte_show_help("help-state-db.txt", "error-reading-db", true, (char*)key.data, strerror(errno));
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto cleanup;
    }
    
    /* populate the recovered info */
    opal_dss.load(&buf, data.data, data.size);
    switch (type) {
        case ORTE_JOB:
            n=1;
            opal_dss.unpack(&buf, &jstate, &n, ORTE_JOB_STATE_T);
            jdata->state = *jstate;
            break;
        case ORTE_PROC:
            n=1;
            opal_dss.unpack(&buf, &pstate, &n, ORTE_PROC_STATE_T);
            proc->state = *pstate;
            break;
        default:
            break;
    }
    
cleanup:
    if (NULL != key.data) {
        free(key.data);
    }
    if (NULL != data.data) {
        free(data.data);
    }
    OBJ_DESTRUCT(&buf);
    return rc;
}

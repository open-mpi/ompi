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

#include "db_dbase.h"

static int init(void);
static int finalize(void);
static int insert(char *key, void *object, opal_data_type_t type);
static int set_recover_source(orte_process_name_t *name);
static int select_data(char *key, void *object, opal_data_type_t type);
static int update(char *key, void *object, opal_data_type_t type);
static int delete_data(char *key);

orte_db_base_module_t orte_db_dbase_module = {
    init,
    finalize,
    insert,
    set_recover_source,
    select_data,
    update,
    delete_data
};

/* local variables */
static DB *save_dbase=NULL, *recover_dbase=NULL;

static int init(void)
{
    char *path, *name;
    
    /* setup the database */
    if (ORTE_SUCCESS != opal_os_dirpath_create(orte_db_dbase_directory, S_IRWXU)) {
        orte_show_help("help-db-dbase.txt", "cannot-create-dir", true,
                       orte_db_dbase_directory);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    orte_util_convert_process_name_to_string(&name, ORTE_PROC_MY_NAME);
    path = opal_os_path(false, orte_db_dbase_directory, name, NULL);
    free(name);
    if (NULL == (save_dbase = dbaseopen(path, O_CREAT | O_RDWR | O_TRUNC, S_IRWXU, DB_HASH, NULL))) {
        orte_show_help("help-db-dbase.txt", "cannot-create-dbase", true, path);
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

static int insert(char *inkey, void *object, opal_data_type_t type)
{
    DBT key, data;
    opal_buffer_t buf;
    orte_job_t *jdata;
    orte_proc_t *proc;
    int rc=ORTE_SUCCESS, size;
    
    /* construct the buffer we will use for packing the data */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    key.data = inkey;
    key.size = strlen(key.data);
    
    switch (type) {
        case ORTE_JOB:
            jdata = (orte_job_t*)object;
            opal_dss.pack(&buf, &jdata, 1, ORTE_JOB);
            break;
        case ORTE_PROC:
            proc = (orte_proc_t*)object;
            opal_dss.pack(&buf, &proc, 1, ORTE_PROC);
            break;
        default:
            orte_show_help("help-db-dbase.txt", "unrecognized-type", true, type);
            rc = ORTE_ERR_BAD_PARAM;
            goto cleanup;
            break;
    }
    
    /* unload the data */
    opal_dss.unload(&buf, (void**)&data.data, &size);
    data.size = size;
    OBJ_DESTRUCT(&buf);
    
    /* put the info into the dbase */
    if (0 > save_dbase->put(save_dbase, &key, &data, 0)) {
        orte_show_help("help-db-dbase.txt", "error-writing-dbase", true, (char*)key.data, strerror(errno));
        rc = ORTE_ERR_FILE_WRITE_FAILURE;
    }
    /* sync it to force it to disk */
    if (0 > save_dbase->sync(save_dbase, 0)) {
        orte_show_help("help-db-dbase.txt", "error-syncing-dbase", true, (char*)key.data, strerror(errno));
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
    path = opal_os_path(false, orte_db_dbase_directory, pname, NULL);
    free(pname);
    if (NULL == (recover_dbase = dbaseopen(path, O_RDONLY, S_IRWXU, DB_HASH, NULL))) {
        orte_show_help("help-db-dbase.txt", "cannot-open-dbase", true, path);
        free(path);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    free(path);
    
    return rc;
}

static int select_data(char *inkey, void *object, opal_data_type_t type)
{
    DBT key, data;
    opal_buffer_t buf;
    orte_job_t *jdata;
    orte_proc_t *proc;
    int rc=ORTE_SUCCESS;
    int32_t n;
    
    if (NULL == recover_dbase) {
        orte_show_help("help-db-dbase.txt", "recover-source-undef", true);
        rc = ORTE_ERR_NOT_FOUND;
    }
    
    /* construct the buffer we will use for unpacking the data */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    key.data = inkey;
    key.size = strlen(key.data);
    
    /* get the specified data */
    if (0 > recover_dbase->get(recover_dbase, &key, &data, 0)) {
        orte_show_help("help-db-dbase.txt", "error-reading-dbase", true, (char*)key.data, strerror(errno));
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto cleanup;
    }
    
    /* populate the recovered info */
    opal_dss.load(&buf, data.data, data.size);
    switch (type) {
        case ORTE_JOB:
            n=1;
            opal_dss.unpack(&buf, &jdata, &n, ORTE_JOB);
            break;
        case ORTE_PROC:
            n=1;
            opal_dss.unpack(&buf, &proc, &n, ORTE_PROC);
            break;
        default:
            break;
    }
    
cleanup:
    OBJ_DESTRUCT(&buf);
    return rc;
}

static int update(char *key, void *object, opal_data_type_t type)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int delete_data(char *key)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <stdio.h>
#include <stddef.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#include <sys/stat.h>
#include <sys/types.h>

#include "opal_stdint.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/state/state.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_file.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);
static void file_sample(void);
static void file_log(opal_buffer_t *sample);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_file_module = {
    init,
    finalize,
    start,
    stop,
    file_sample,
    file_log
};

/* define a tracking object */
typedef struct {
    opal_list_item_t super;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    char *file;
    int tick;
    bool check_size;
    bool check_access;
    bool check_mod;
    int32_t file_size;
    time_t last_access;
    time_t last_mod;
    int limit;
} file_tracker_t;
static void ft_constructor(file_tracker_t *ft)
{
    ft->file = NULL;
    ft->tick = 0;
    ft->file_size = 0;
    ft->last_access = 0;
    ft->last_mod = 0;
    ft->limit = 0;
}
static void ft_destructor(file_tracker_t *ft)
{
    if (NULL != ft->file) {
        free(ft->file);
    }
}
OBJ_CLASS_INSTANCE(file_tracker_t,
                   opal_list_item_t,
                   ft_constructor, ft_destructor);

/* local globals */
static opal_list_t jobs; 

static int init(void)
{
    OBJ_CONSTRUCT(&jobs, opal_list_t);
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;
    
    while (NULL != (item = opal_list_remove_first(&jobs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&jobs);
    
    return;
}

static bool find_value(orte_app_context_t *app,
                       char *pattern, char **value)
{
    int i;
    char *ptr;

    for (i=0; NULL != app->env[i]; i++) {
        if (0 == strncmp(app->env[i], pattern, strlen(pattern))) {
            ptr = strchr(app->env[i], '=');
            ptr++;
            if (NULL != value) {
                *value = strdup(ptr);
            }
            return true;
        }
    }
    return false;
}

/*
 * Start monitoring of local processes
 */
static void start(orte_jobid_t jobid)
{
    orte_job_t *jobdat;
    orte_app_context_t *app, *aptr;
    int i;
    char *filename;
    file_tracker_t *ft;
    char *ptr;

    /* cannot monitor my own job */
    if (jobid == ORTE_PROC_MY_NAME->jobid && ORTE_JOBID_WILDCARD != jobid) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                         "%s starting file monitoring for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobid)));
    
    /* get the local jobdat for this job */
    if (NULL == (jobdat = orte_get_job_data_object(jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return;
    }

    /* must be at least one app_context, so use the first one found */
    app = NULL;
    for (i=0; i < jobdat->apps->size; i++) {
        if (NULL != (aptr = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, i))) {
            app = aptr;
            break;
        }
    }
    if (NULL == app) {
        /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return;
    }
            
    /* search the environ to get the filename */
    if (!find_value(app, "OMPI_MCA_sensor_file_filename", &filename)) {
        /* was a default file given */
        if (NULL == mca_sensor_file_component.file) {
            /* can't do anything without a file */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s sensor:file no file for job %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(jobid)));
            return;
        }
        filename = mca_sensor_file_component.file;
    }
            
    /* create the tracking object */
    ft = OBJ_NEW(file_tracker_t);
    ft->jobid = jobid;
    ft->file = strdup(filename);
    
    /* search the environ to see what we are checking */
    if (!find_value(app, "OMPI_MCA_sensor_file_check_size", &ptr)) {
        /* was a default value given */
        if (0 < mca_sensor_file_component.check_size) {
            ft->check_size = OPAL_INT_TO_BOOL(mca_sensor_file_component.check_size);
        }
    } else {
        ft->check_size = OPAL_INT_TO_BOOL(strtol(ptr, NULL, 10));
        free(ptr);
    }

    if (!find_value(app, "OMPI_MCA_sensor_file_check_access", &ptr)) {
        /* was a default value given */
        if (0 < mca_sensor_file_component.check_access) {
            ft->check_access = OPAL_INT_TO_BOOL(mca_sensor_file_component.check_access);
        }
    } else {
        ft->check_access = OPAL_INT_TO_BOOL(strtol(ptr, NULL, 10));
        free(ptr);
    }

    if (!find_value(app, "OMPI_MCA_sensor_file_check_mod", &ptr)) {
        /* was a default value given */
        if (0 < mca_sensor_file_component.check_mod) {
            ft->check_mod = OPAL_INT_TO_BOOL(mca_sensor_file_component.check_mod);
        }
    } else {
        ft->check_mod = OPAL_INT_TO_BOOL(strtol(ptr, NULL, 10));
        free(ptr);
    }

    if (!find_value(app, "OMPI_MCA_sensor_file_limit", &ptr)) {
        ft->limit = mca_sensor_file_component.limit;
    } else {
        ft->limit = strtol(ptr, NULL, 10);
        free(ptr);
    }
    opal_list_append(&jobs, &ft->super);
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                         "%s file %s monitored for %s%s%s with limit %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ft->file, ft->check_size ? "SIZE:" : " ",
                         ft->check_access ? "ACCESS TIME:" : " ",
                         ft->check_mod ? "MOD TIME" : " ", ft->limit));
    return;
}


static void stop(orte_jobid_t jobid)
{
    opal_list_item_t *item;
    file_tracker_t *ft;
    
    /* cannot monitor my own job */
    if (jobid == ORTE_PROC_MY_NAME->jobid && ORTE_JOBID_WILDCARD != jobid) {
        return;
    }
    
    for (item = opal_list_get_first(&jobs);
         item != opal_list_get_end(&jobs);
         item = opal_list_get_next(item)) {
        ft = (file_tracker_t*)item;
        if (jobid == ft->jobid || ORTE_JOBID_WILDCARD == jobid) {
            opal_list_remove_item(&jobs, item);
            OBJ_RELEASE(item);
        }
    }
    return;
}

static void file_sample(void)
{
    struct stat buf;
    opal_list_item_t *item;
    file_tracker_t *ft;
    orte_job_t *jdata;

    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                         "%s sampling files",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    for (item = opal_list_get_first(&jobs);
         item != opal_list_get_end(&jobs);
         item = opal_list_get_next(item)) {
        ft = (file_tracker_t*)item;
        
        /* stat the file and get its size */
        if (0 > stat(ft->file, &buf)) {
            /* cannot stat file */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                                 "%s could not stat %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ft->file));
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                             "%s size %lu access %s\tmod %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (unsigned long)buf.st_size, ctime(&buf.st_atime), ctime(&buf.st_mtime)));

        if (ft->check_size) {
            if (buf.st_size == ft->file_size) {
                ft->tick++;
                goto CHECK;
            } else {
                ft->tick = 0;
                ft->file_size = buf.st_size;
            }
        }
        if (ft->check_access) {
            if (buf.st_atime == ft->last_access) {
                ft->tick++;
                goto CHECK;
            } else {
                ft->tick = 0;
                ft->last_access = buf.st_atime;
            }
        }
        if (ft->check_mod) {
            if (buf.st_mtime == ft->last_mod) {
                ft->tick++;
                goto CHECK;
            } else {
                ft->tick = 0;
                ft->last_mod = buf.st_mtime;
            }
        }
        
    CHECK:
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base_framework.framework_output,
                             "%s sampled file %s tick %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ft->file, ft->tick));

        if (ft->tick == ft->limit) {
            orte_show_help("help-orte-sensor-file.txt", "file-stalled", true,
                           ft->file, ft->file_size, ctime(&ft->last_access), ctime(&ft->last_mod));
            jdata = orte_get_job_data_object(ft->jobid);
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED);
        }
    }
}

static void file_log(opal_buffer_t *sample)
{
}

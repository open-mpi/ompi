/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "opal/mca/event/event.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_file.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t jobid);
static void stop(orte_jobid_t jobid);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_file_module = {
    init,
    finalize,
    start,
    stop
};

/* define a tracking object */
typedef struct {
    opal_list_item_t super;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
#if ORTE_ENABLE_EPOCH
    orte_epoch_t epoch;
#endif
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

/* declare the local functions */
static void sample(int fd, short event, void *arg);

/* local globals */
static opal_event_t *sample_ev = NULL;
static struct timeval sample_time;
static opal_list_t jobs; 

static int init(void)
{
    OBJ_CONSTRUCT(&jobs, opal_list_t);
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;
    
    if (NULL != sample_ev) {
        opal_event_del(sample_ev);
        free(sample_ev);
    }
    while (NULL != (item = opal_list_remove_first(&jobs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&jobs);
    
    return;
}

/*
 * Start monitoring of local processes
 */
static void start(orte_jobid_t jobid)
{
    mca_base_component_t *c = &mca_sensor_file_component.super.base_version;
    opal_list_item_t *item;
    orte_odls_job_t *jobdat;
    orte_app_context_t *app, *aptr;
    int rc, tmp;
    char *filename;
    file_tracker_t *ft;

    /* cannot monitor my own job */
    if (jobid == ORTE_PROC_MY_NAME->jobid && ORTE_JOBID_WILDCARD != jobid) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s starting file monitoring for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobid)));
    
    /* get the local jobdat for this job */
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_end(&orte_local_jobdata)) {
        jobdat = (orte_odls_job_t*)item;
        if (jobid == jobdat->jobid || ORTE_JOBID_WILDCARD == jobid) {
            /* must be at least one app_context, so use the first one found */
            app = NULL;
            for (tmp=0; tmp < jobdat->apps.size; tmp++) {
                if (NULL != (aptr = (orte_app_context_t*)opal_pointer_array_get_item(&jobdat->apps, tmp))) {
                    app = aptr;
                    break;
                }
            }
            if (NULL == app) {
                /* got a problem */
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                continue;
            }
            
            /* search the environ to get the filename */
            if (ORTE_SUCCESS != (rc = mca_base_param_find_string(c, "filename", app->env, &filename))) {
                /* was a default file given */
                if (NULL == mca_sensor_file_component.file) {
                    /* can't do anything without a file */
                    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                         "%s sensor:file no file for job %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_JOBID_PRINT(jobid)));
                    continue;
                }
                filename = mca_sensor_file_component.file;
            }
            
            /* create the tracking object */
            ft = OBJ_NEW(file_tracker_t);
            ft->jobid = jobid;
            ft->file = strdup(filename);
            
            /* search the environ to see what we are checking */
            tmp = 0;
            if (ORTE_SUCCESS != (rc = mca_base_param_find_int(c, "check_size", app->env, &tmp))) {
                /* was a default value given */
                if (0 < mca_sensor_file_component.check_size) {
                    ft->check_size = OPAL_INT_TO_BOOL(mca_sensor_file_component.check_size);
                }
            } else {
                ft->check_size = OPAL_INT_TO_BOOL(tmp);
            }
            tmp = 0;
            if (ORTE_SUCCESS != (rc = mca_base_param_find_int(c, "check_access", app->env, &tmp))) {
                /* was a default value given */
                if (0 < mca_sensor_file_component.check_access) {
                    ft->check_access = OPAL_INT_TO_BOOL(mca_sensor_file_component.check_access);
                }
            } else {
                ft->check_access = OPAL_INT_TO_BOOL(tmp);
            }
            tmp = 0;
            if (ORTE_SUCCESS != (rc = mca_base_param_find_int(c, "check_mod", app->env, &tmp))) {
                /* was a default value given */
                if (0 < mca_sensor_file_component.check_mod) {
                    ft->check_mod = OPAL_INT_TO_BOOL(mca_sensor_file_component.check_mod);
                }
            } else {
                ft->check_mod = OPAL_INT_TO_BOOL(tmp);
            }
            tmp = 0;
            if (ORTE_SUCCESS != (rc = mca_base_param_find_int(c, "limit", app->env, &tmp))) {
                ft->limit = mca_sensor_file_component.limit;
            } else {
                ft->limit = tmp;
            }
            opal_list_append(&jobs, &ft->super);
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s file %s monitored for %s%s%s with limit %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ft->file, ft->check_size ? "SIZE:" : " ",
                                 ft->check_access ? "ACCESS TIME:" : " ",
                                 ft->check_mod ? "MOD TIME" : " ", ft->limit));
        }
    }
    
    /* start sampling */
    if (NULL == sample_ev && !opal_list_is_empty(&jobs)) {
        /* startup a timer to wake us up periodically
         * for a data sample
         */
        sample_ev =  (opal_event_t *) malloc(sizeof(opal_event_t));
        opal_event_evtimer_set(opal_event_base, sample_ev, sample, sample_ev);
        sample_time.tv_sec = mca_sensor_file_component.sample_rate;
        sample_time.tv_usec = 0;
        opal_event_evtimer_add(sample_ev, &sample_time);
    }
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
    /* if no jobs remain, stop the sampling */
    if (opal_list_is_empty(&jobs) && NULL != sample_ev) {
        opal_event_del(sample_ev);
        free(sample_ev);
        sample_ev = NULL;
    }
    return;
}

static void sample(int fd, short event, void *arg)
{
    struct stat buf;
    opal_list_item_t *item;
    file_tracker_t *ft;

    /* if we are not sampling any more, then just return */
    if (NULL == sample_ev) {
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                         "%s sampling files",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    for (item = opal_list_get_first(&jobs);
         item != opal_list_get_end(&jobs);
         item = opal_list_get_next(item)) {
        ft = (file_tracker_t*)item;
        
        /* stat the file and get its size */
        if (0 > stat(ft->file, &buf)) {
            /* cannot stat file */
            OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                                 "%s could not stat %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ft->file));
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
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
        OPAL_OUTPUT_VERBOSE((1, orte_sensor_base.output,
                             "%s sampled file %s tick %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ft->file, ft->tick));

        if (ft->tick == ft->limit) {
            orte_show_help("help-orte-sensor-file.txt", "file-stalled", true,
                           ft->file, ft->file_size, ctime(&ft->last_access), ctime(&ft->last_mod));
            orte_errmgr.update_state(ft->jobid, ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED,
                                     NULL, ORTE_PROC_STATE_UNDEF,
                                     0, ORTE_ERR_PROC_STALLED);
        }
    }
        
    /* restart the timer */
        opal_event_evtimer_add(sample_ev, &sample_time);
}

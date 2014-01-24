/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <stdio.h>
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include "opal_stdint.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"
#include "opal/mca/db/db.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_coretemp.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);
static void coretemp_sample(void);
static void coretemp_log(opal_buffer_t *buf);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_coretemp_module = {
    init,
    finalize,
    start,
    stop,
    coretemp_sample,
    coretemp_log
};

typedef struct {
    opal_list_item_t super;
    char *file;
    int socket;
    char *label;
    float critical_temp;
    float max_temp;
} coretemp_tracker_t;
static void ctr_con(coretemp_tracker_t *trk)
{
    trk->file = NULL;
    trk->label = NULL;
}
static void ctr_des(coretemp_tracker_t *trk)
{
    if (NULL != trk->file) {
        free(trk->file);
    }
    if (NULL != trk->label) {
        free(trk->label);
    }
}
OBJ_CLASS_INSTANCE(coretemp_tracker_t,
                   opal_list_item_t,
                   ctr_con, ctr_des);

static bool log_enabled = true;
static opal_list_t tracking;

static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];

    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
	   input[strlen(input)-1] = '\0';  /* remove newline */
	   buff = strdup(input);
	   return buff;
    }
    
    return NULL;
}

/* FOR FUTURE: extend to read cooling device speeds in
 *     current speed: /sys/class/thermal/cooling_deviceN/cur_state
 *     max speed: /sys/class/thermal/cooling_deviceN/max_state
 *     type: /sys/class/thermal/cooling_deviceN/type
 */
static int init(void)
{
    DIR *cur_dirp = NULL, *tdir;
    struct dirent *dir_entry, *entry;
    char *dirname, *filename, *ptr, *tmp;
    size_t tlen = strlen("temp");
    size_t ilen = strlen("_input");
    FILE *fp;
    coretemp_tracker_t *trk;
    int socket;

    OBJ_CONSTRUCT(&tracking, opal_list_t);

    /*
     * Open up the base directory so we can get a listing
     */
    if (NULL == (cur_dirp = opendir("/sys/bus/platform/devices"))) {
        OBJ_DESTRUCT(&tracking);
        return ORTE_ERROR;
    }

    /*
     * For each directory
     */
    socket = 0;
    while (NULL != (dir_entry = readdir(cur_dirp))) {
        
        /* look for coretemp directories */
        if (0 != strncmp(dir_entry->d_name, "coretemp", strlen("coretemp"))) {
            continue;
        }

        /* open that directory */
        dirname = opal_os_path(false, "/sys/bus/platform/devices", dir_entry->d_name, NULL );
        if (NULL == (tdir = opendir(dirname))) {
            continue;
        }
        while (NULL != (entry = readdir(tdir))) {
            /*
             * Skip the obvious
             */
            if (0 == strncmp(entry->d_name, ".", strlen(".")) ||
                0 == strncmp(entry->d_name, "..", strlen(".."))) {
                continue;
            }
            if (strlen(entry->d_name) < (tlen+ilen)) {
                /* cannot be a core temp file */
                continue;
            }
            /*
             * See if this is a core temp file
             */
            if (0 != strncmp(entry->d_name, "temp", strlen("temp"))) {
                continue;
            }
            if (0 != strcmp(entry->d_name + strlen(entry->d_name) - ilen, "_input")) {
                continue;
            }
            /* track the info for this core */
            trk = OBJ_NEW(coretemp_tracker_t);
            trk->socket = socket;
            trk->file = opal_os_path(false, dirname, entry->d_name, NULL);
            /* take the part up to the first underscore as this will
             * be used as the start of all the related files
             */
            tmp = strdup(entry->d_name);
            if (NULL == (ptr = strchr(tmp, '_'))) {
                /* unrecognized format */
                free(tmp);
                OBJ_RELEASE(trk);
                continue;
            }
            *ptr = '\0';
            /* look for critical, max, and label info */
            asprintf(&filename, "%s/%s_%s", dirname, tmp, "label");
            fp = fopen(filename, "r");
            trk->label = orte_getline(fp);
            fclose(fp);
            free(filename);

            asprintf(&filename, "%s/%s_%s", dirname, tmp, "crit");
            fp = fopen(filename, "r");
            ptr = orte_getline(fp);
            fclose(fp);
            trk->critical_temp = strtol(ptr, NULL, 10)/100.0;
            free(ptr);
            free(filename);

            asprintf(&filename, "%s/%s_%s", dirname, tmp, "max");
            fp = fopen(filename, "r");
            ptr = orte_getline(fp);
            fclose(fp);
            trk->max_temp = strtol(ptr, NULL, 10)/100.0;
            free(ptr);
            free(filename);

            /* add to our list */
            opal_list_append(&tracking, &trk->super);
            /* cleanup */
            free(tmp);
        }
        closedir(tdir);
        socket++;
    }
    closedir(cur_dirp);

    if (0 == opal_list_get_size(&tracking)) {
        /* nothing to read */
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    OPAL_LIST_DESTRUCT(&tracking);
}

/*
 * Start monitoring of local temps
 */
static void start(orte_jobid_t jobid)
{
    return;
}


static void stop(orte_jobid_t jobid)
{
    return;
}

static void coretemp_sample(void)
{
    int ret;
    coretemp_tracker_t *trk;
    FILE *fp;
    char *temp;
    float degc;
    opal_buffer_t data, *bptr;
    int32_t ncores;
    time_t now;
    char time_str[40];
    char *timestamp_str;
    bool packed;

    /* prep to store the results */
    OBJ_CONSTRUCT(&data, opal_buffer_t);
    packed = false;

    /* pack our name */
    temp = strdup("coretemp");
    if (OPAL_SUCCESS != (ret = opal_dss.pack(&data, &temp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&data);
        return;
    }
    free(temp);

    /* store our hostname */
    if (OPAL_SUCCESS != (ret = opal_dss.pack(&data, &orte_process_info.nodename, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* store the number of cores */
    ncores = (int32_t)opal_list_get_size(&tracking);
    if (OPAL_SUCCESS != (ret = opal_dss.pack(&data, &ncores, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* get the sample time */
    now = time(NULL);
    /* pass the time along as a simple string */
    strftime(time_str, sizeof(time_str), "%F %T%z", localtime(&now));
    asprintf(&timestamp_str, "%s", time_str);
    if (OPAL_SUCCESS != (ret = opal_dss.pack(&data, &timestamp_str, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&data);
        free(timestamp_str);
        return;
    }
    free(timestamp_str);

    OPAL_LIST_FOREACH(trk, &tracking, coretemp_tracker_t) {
        /* read the temp */
        fp = fopen(trk->file, "r");
        while (NULL != (temp = orte_getline(fp))) {
            degc = strtoul(temp, NULL, 10) / 100.0;
            opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                                "%s sensor:coretemp: Socket %d %s temp %f max %f critical %f",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                trk->socket, trk->label, degc, trk->max_temp, trk->critical_temp);
            if (OPAL_SUCCESS != (ret = opal_dss.pack(&data, &degc, 1, OPAL_FLOAT))) {
                ORTE_ERROR_LOG(ret);
                OBJ_DESTRUCT(&data);
                free(temp);
                return;
            }
            free(temp);
            packed = true;
            /* check for exceed critical temp */
            if (trk->critical_temp < degc) {
                /* alert the errmgr - this is a critical problem */
                opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                                    "%s sensor:coretemp: Socket %d %s CRITICAL",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    trk->socket, trk->label);
             } else if (trk->max_temp < degc) {
                /* alert the errmgr */
                opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                                    "%s sensor:coretemp: Socket %d %s MAX",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    trk->socket, trk->label);
             }
        }
        fclose(fp);
    }

    /* xfer the data for transmission */
    if (packed) {
        bptr = &data;
        if (OPAL_SUCCESS != (ret = opal_dss.pack(orte_sensor_base.samples, &bptr, 1, OPAL_BUFFER))) {
            ORTE_ERROR_LOG(ret);
            OBJ_DESTRUCT(&data);
            return;
        }
    }
    OBJ_DESTRUCT(&data);
}

static void coretemp_log(opal_buffer_t *sample)
{
    char *hostname=NULL;
    char *sampletime;
    int rc;
    int32_t n, ncores;
    opal_value_t *kv=NULL;
    float fval;
    int i;

    if (!log_enabled) {
        return;
    }

    /* unpack the host this came from */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &hostname, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    /* and the number of cores on that host */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &ncores, &n, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* sample time */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &sampletime, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    opal_output_verbose(3, orte_sensor_base_framework.framework_output,
                        "%s Received log from host %s with %d cores",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == hostname) ? "NULL" : hostname, ncores);

    /* xfr to storage */
    kv = malloc((ncores+2) * sizeof(opal_value_t));

    /* load the sample time at the start */
    OBJ_CONSTRUCT(&kv[0], opal_value_t);
    kv[0].key = strdup("ctime");
    kv[0].type = OPAL_STRING;
    kv[0].data.string = strdup(sampletime);
    free(sampletime);

    /* load the hostname */
    OBJ_CONSTRUCT(&kv[1], opal_value_t);
    kv[1].key = strdup("hostname");
    kv[1].type = OPAL_STRING;
    kv[1].data.string = strdup(hostname);

    /* protect against segfault if we jump to cleanup */
    for (i=0; i < ncores; i++) {
        OBJ_CONSTRUCT(&kv[i+2], opal_value_t);
    }

    for (i=0; i < ncores; i++) {
        asprintf(&kv[i+2].key, "core%d", i);
        kv[i+2].type = OPAL_FLOAT;
        n=1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &fval, &n, OPAL_FLOAT))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        kv[i+2].data.fval = fval;
    }

    /* store it */
    if (ORTE_SUCCESS != (rc = opal_db.add_log("coretemp", kv, ncores+2))) {
        /* don't bark about it - just quietly disable the log */
        log_enabled = false;
    }

 cleanup:
    /* cleanup the xfr storage */
    for (i=0; i < ncores+2; i++) {
        OBJ_DESTRUCT(&kv[i]);
    }
    if (NULL != hostname) {
        free(hostname);
    }

}

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
#include <ctype.h>

#include "opal_stdint.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"
#include "opal/mca/db/db.h"

#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_freq.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);
static void freq_sample(void);
static void freq_log(opal_buffer_t *buf);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_freq_module = {
    init,
    finalize,
    start,
    stop,
    freq_sample,
    freq_log
};

typedef struct {
    opal_list_item_t super;
    char *file;
    int core;
    float max_freq;
    float min_freq;
} corefreq_tracker_t;
static void ctr_con(corefreq_tracker_t *trk)
{
    trk->file = NULL;
}
static void ctr_des(corefreq_tracker_t *trk)
{
    if (NULL != trk->file) {
        free(trk->file);
    }
}
OBJ_CLASS_INSTANCE(corefreq_tracker_t,
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
    int k;
    DIR *cur_dirp = NULL;
    struct dirent *entry;
    char *filename, *tmp;
    FILE *fp;
    corefreq_tracker_t *trk;

    /* always construct this so we don't segfault in finalize */
    OBJ_CONSTRUCT(&tracking, opal_list_t);

    /*
     * Open up the base directory so we can get a listing
     */
    if (NULL == (cur_dirp = opendir("/sys/devices/system/cpu"))) {
        OBJ_DESTRUCT(&tracking);
        orte_show_help("help-orte-sensor-freq.txt", "req-dir-not-found",
                       true, orte_process_info.nodename,
                       "/sys/devices/system/cpu");
        return ORTE_ERROR;
    }

    /*
     * For each directory
     */
    while (NULL != (entry = readdir(cur_dirp))) {
        
        /*
         * Skip the obvious
         */
        if (0 == strncmp(entry->d_name, ".", strlen(".")) ||
            0 == strncmp(entry->d_name, "..", strlen(".."))) {
            continue;
        }

        /* look for cpu directories */
        if (0 != strncmp(entry->d_name, "cpu", strlen("cpu"))) {
            /* cannot be a cpu directory */
            continue;
        }
        /* if it ends in other than a digit, then it isn't a cpu directory */
        if (!isdigit(entry->d_name[strlen(entry->d_name)-1])) {
            continue;
        }

        /* track the info for this core */
        trk = OBJ_NEW(corefreq_tracker_t);
        /* trailing digits are the core id */
        for (k=strlen(entry->d_name)-1; 0 <= k; k--) {
            if (!isdigit(entry->d_name[k])) {
                break;
            }
        }
        trk->core = strtoul(&entry->d_name[k], NULL, 10);
        trk->file = opal_os_path(false, "/sys/devices/system/cpu", entry->d_name, "cpufreq", "cpuinfo_cur_freq", NULL);
        
        /* read the static info */
        filename = opal_os_path(false, "/sys/devices/system/cpu", entry->d_name, "cpufreq", "cpuinfo_max_freq", NULL);
        fp = fopen(filename, "r");
        tmp = orte_getline(fp);
        fclose(fp);
        trk->max_freq = strtoul(tmp, NULL, 10) / 1000000.0;
        free(filename);

        filename = opal_os_path(false, "/sys/devices/system/cpu", entry->d_name, "cpufreq", "cpuinfo_min_freq", NULL);
        fp = fopen(filename, "r");
        tmp = orte_getline(fp);
        fclose(fp);
        trk->min_freq = strtoul(tmp, NULL, 10) / 1000000.0;
        free(filename);

        /* add to our list */
        opal_list_append(&tracking, &trk->super);
        /* cleanup */
        free(tmp);
    }
    closedir(cur_dirp);

    if (0 == opal_list_get_size(&tracking)) {
        /* nothing to read */
        orte_show_help("help-orte-sensor-freq.txt", "no-cores-found",
                       true, orte_process_info.nodename);
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

static void freq_sample(void)
{
    int ret;
    corefreq_tracker_t *trk, *nxt;
    FILE *fp;
    char *freq;
    float ghz;
    opal_buffer_t data, *bptr;
    int32_t ncores;
    time_t now;
    char time_str[40];
    char *timestamp_str;
    bool packed;

    if (0 == opal_list_get_size(&tracking)) {
        return;
    }

    opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                        "%s sampling freq",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* prep to store the results */
    OBJ_CONSTRUCT(&data, opal_buffer_t);
    packed = false;

    /* pack our name */
    freq = strdup("freq");
    if (OPAL_SUCCESS != (ret = opal_dss.pack(&data, &freq, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&data);
        return;
    }
    free(freq);

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

    OPAL_LIST_FOREACH_SAFE(trk, nxt, &tracking, corefreq_tracker_t) {
        opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                            "%s processing freq file %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            trk->file);
        /* read the freq */
        if (NULL == (fp = fopen(trk->file, "r"))) {
            /* we can't be read, so remove it from the list */
            opal_output_verbose(2, orte_sensor_base_framework.framework_output,
                                "%s access denied to freq file %s - removing it",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                trk->file);
            opal_list_remove_item(&tracking, &trk->super);
            OBJ_RELEASE(trk);
            continue;
        }
        while (NULL != (freq = orte_getline(fp))) {
            ghz = strtoul(freq, NULL, 10) / 1000000.0;
            opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                                "%s sensor:freq: Core %d freq %f max %f min %f",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                trk->core, ghz, trk->max_freq, trk->min_freq);
            if (OPAL_SUCCESS != (ret = opal_dss.pack(&data, &ghz, 1, OPAL_FLOAT))) {
                ORTE_ERROR_LOG(ret);
                OBJ_DESTRUCT(&data);
                free(freq);
                return;
            }
            packed = true;
            free(freq);
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

static void freq_log(opal_buffer_t *sample)
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
                        "%s Received freq log from host %s with %d cores",
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
    if (ORTE_SUCCESS != (rc = opal_db.add_log("freq", kv, ncores+2))) {
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

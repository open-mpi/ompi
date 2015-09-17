/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include <string.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */
#include <ctype.h>

#include "opal/class/opal_list.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"

#include "orte/util/show_help.h"
#include "orte/util/error_strings.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps_types.h"

#include "orte/mca/rtc/base/base.h"
#include "rtc_freq.h"

static int init(void);
static void finalize(void);
static void assign(orte_job_t *jdata);
static void set(orte_job_t *jdata,
                orte_proc_t *proc,
                char ***environ_copy,
                int write_fd);
static void getvals(opal_list_t *vals);

orte_rtc_base_module_t orte_rtc_freq_module = {
    init,
    finalize,
    assign,
    set,
    getvals
};

typedef struct {
    opal_list_item_t super;
    int core;
    char *directory;
    /* save the system settings so we can restore them when we die */
    char *system_governor;
    float system_max_freq;
    float system_min_freq;
    /* save the current settings so we only change them when required */
    char *current_governor;
    float current_max_freq;
    float current_min_freq;
    /* keep a list of allowed values */
    opal_list_t governors;
    opal_list_t frequencies;
    /* mark if setspeed is supported */
    bool setspeed;
} rtefreq_tracker_t;
static void ctr_con(rtefreq_tracker_t *trk)
{
    trk->directory = NULL;
    trk->system_governor = NULL;
    trk->current_governor = NULL;
    OBJ_CONSTRUCT(&trk->governors, opal_list_t);
    OBJ_CONSTRUCT(&trk->frequencies, opal_list_t);
    trk->setspeed = false;
}
static void ctr_des(rtefreq_tracker_t *trk)
{
    if (NULL != trk->directory) {
        free(trk->directory);
    }
    if (NULL != trk->system_governor) {
        free(trk->system_governor);
    }
    if (NULL != trk->current_governor) {
        free(trk->current_governor);
    }
    OPAL_LIST_DESTRUCT(&trk->governors);
    OPAL_LIST_DESTRUCT(&trk->frequencies);
}
OBJ_CLASS_INSTANCE(rtefreq_tracker_t,
                   opal_list_item_t,
                   ctr_con, ctr_des);

static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];
    int k;

    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
        /* trim the end of the line */
        for (k=strlen(input)-1; 0 < k && isspace(input[k]); k--) {
            input[k] = '\0';
        }
        buff = strdup(input);
        return buff;
    }

    return NULL;
}

static opal_list_t tracking;

static int init(void)
{
    int k;
    DIR *cur_dirp = NULL;
    struct dirent *entry;
    char *filename, *tmp, **vals;
    FILE *fp;
    rtefreq_tracker_t *trk;
    opal_value_t *kv;

    /* always construct this so we don't segfault in finalize */
    OBJ_CONSTRUCT(&tracking, opal_list_t);

    /*
     * Open up the base directory so we can get a listing
     */
    if (NULL == (cur_dirp = opendir("/sys/devices/system/cpu"))) {
        OBJ_DESTRUCT(&tracking);
        if (4 < opal_output_get_verbosity(orte_rtc_base_framework.framework_output)) {
            orte_show_help("help-rtc-freq.txt", "req-dir-not-found",
                           true, orte_process_info.nodename,
                           "/sys/devices/system/cpu");
        }
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
        trk = OBJ_NEW(rtefreq_tracker_t);
        /* trailing digits are the core id */
        for (k=strlen(entry->d_name)-1; 0 <= k; k--) {
            if (!isdigit(entry->d_name[k])) {
                break;
            }
        }
        trk->core = strtoul(&entry->d_name[k], NULL, 10);
        trk->directory = opal_os_path(false, "/sys/devices/system/cpu", entry->d_name, "cpufreq", NULL);

        /* read/save the current settings */
        filename = opal_os_path(false, trk->directory, "scaling_governor", NULL);
        if (NULL == (fp = fopen(filename, "rw"))) {
            free(filename);
            OBJ_RELEASE(trk);
            continue;
        }
        trk->system_governor = orte_getline(fp);
        trk->current_governor = strdup(trk->system_governor);
        fclose(fp);
        free(filename);

        filename = opal_os_path(false, trk->directory, "scaling_max_freq", NULL);
        if (NULL == (fp = fopen(filename, "rw"))) {
            free(filename);
            OBJ_RELEASE(trk);
            continue;
        }
        tmp = orte_getline(fp);
        fclose(fp);
        trk->system_max_freq = strtoul(tmp, NULL, 10) / 1000000.0;
        trk->current_max_freq = trk->system_max_freq;
        free(filename);
        free(tmp);

        filename = opal_os_path(false, trk->directory, "scaling_min_freq", NULL);
        if (NULL == (fp = fopen(filename, "rw"))) {
            free(filename);
            OBJ_RELEASE(trk);
            continue;
        }
        tmp = orte_getline(fp);
        fclose(fp);
        trk->system_min_freq = strtoul(tmp, NULL, 10) / 1000000.0;
        trk->current_min_freq = trk->system_min_freq;
        free(filename);
        free(tmp);

        /* get the list of available governors */
        filename = opal_os_path(false, trk->directory, "scaling_available_governors", NULL);
        if (NULL == (fp = fopen(filename, "r"))) {
            free(filename);
            OBJ_RELEASE(trk);
            continue;
        }
        tmp = orte_getline(fp);
        fclose(fp);
        free(filename);
        if (NULL != tmp) {
            vals = opal_argv_split(tmp, ' ');
            free(tmp);
            for (k=0; NULL != vals[k]; k++) {
                kv = OBJ_NEW(opal_value_t);
                kv->type = OPAL_STRING;
                kv->data.string = strdup(vals[k]);
                opal_list_append(&trk->governors, &kv->super);
            }
            opal_argv_free(vals);
        }

        /* get the list of available frequencies */
        filename = opal_os_path(false, trk->directory, "scaling_available_frequencies", NULL);
        if (NULL == (fp = fopen(filename, "r"))) {
            free(filename);
            OBJ_RELEASE(trk);
            continue;
        }
        tmp = orte_getline(fp);
        fclose(fp);
        free(filename);
        if (NULL != tmp) {
            vals = opal_argv_split(tmp, ' ');
            free(tmp);
            for (k=0; NULL != vals[k]; k++) {
                kv = OBJ_NEW(opal_value_t);
                kv->type = OPAL_FLOAT;
                kv->data.fval = strtoul(vals[k], NULL, 10) / 1000000.0;
                opal_list_append(&trk->frequencies, &kv->super);
            }
            opal_argv_free(vals);
        }

        /* see if setspeed is supported */
        filename = opal_os_path(false, trk->directory, "scaling_setspeed", NULL);
        if (NULL != (fp = fopen(filename, "rw"))) {
            trk->setspeed = true;
            fclose(fp);
        }
        free(filename);

        /* add to our list */
        opal_list_append(&tracking, &trk->super);
    }
    closedir(cur_dirp);

    if (0 == opal_list_get_size(&tracking)) {
        /* nothing to read */
        if (0 < opal_output_get_verbosity(orte_rtc_base_framework.framework_output)) {
            orte_show_help("help-rtc-freq.txt", "no-cores-found",
                           true, orte_process_info.nodename);
        }
        OPAL_LIST_DESTRUCT(&tracking);
        return ORTE_ERROR;
    }

    /* report out the results, if requested */
    if (9 < opal_output_get_verbosity(orte_rtc_base_framework.framework_output)) {
        OPAL_LIST_FOREACH(trk, &tracking, rtefreq_tracker_t) {
            opal_output(0, "%s\tCore: %d  Governor: %s MaxFreq: %f MinFreq: %f\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), trk->core,
                        trk->system_governor, trk->system_max_freq, trk->system_min_freq);
            OPAL_LIST_FOREACH(kv, &trk->governors, opal_value_t) {
                opal_output(0, "%s\t\tGovernor: %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), kv->data.string);
            }
            OPAL_LIST_FOREACH(kv, &trk->frequencies, opal_value_t) {
                opal_output(0, "%s\t\tFrequency: %f",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), kv->data.fval);
            }
        }
    }

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    OPAL_LIST_DESTRUCT(&tracking);

    return;
}

static void assign(orte_job_t *jdata)
{
    bool freq_given = false;

    opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                        "%s Assigning freq controls to job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jdata->jobid));

    /* see if the job already has the max freq attribute set */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_MAX_FREQ, NULL, OPAL_STRING)) {
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s Assigning max freq given for job %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_JOBID_PRINT(jdata->jobid));
        freq_given = true;
    } else if (NULL != mca_rtc_freq_component.max_freq) {
        /* if not, set the default value if provided */
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s Assigning default max freq control to job %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_JOBID_PRINT(jdata->jobid));
        orte_set_attribute(&jdata->attributes, ORTE_JOB_MAX_FREQ, ORTE_ATTR_GLOBAL,
                           mca_rtc_freq_component.max_freq, OPAL_STRING);
        freq_given = true;
    }

    /* see if the job already has the min freq attribute set */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_MIN_FREQ, NULL, OPAL_STRING)) {
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s Assigning min freq controls to job %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_JOBID_PRINT(jdata->jobid));
        freq_given = true;
    } else if (NULL != mca_rtc_freq_component.min_freq) {
        /* if not, set the default value if provided */
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s Assigning default minfreq controls to job %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_JOBID_PRINT(jdata->jobid));
        orte_set_attribute(&jdata->attributes, ORTE_JOB_MIN_FREQ, ORTE_ATTR_GLOBAL,
                           mca_rtc_freq_component.min_freq, OPAL_STRING);
        freq_given = true;
    }

    /* see if the job has a governor attribute set */
    if (!orte_get_attribute(&jdata->attributes, ORTE_JOB_GOVERNOR, NULL, OPAL_STRING)) {
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s Assigning freq governor to job %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_JOBID_PRINT(jdata->jobid));
        /* if not, was a default value provided? */
        if (NULL != mca_rtc_freq_component.governor) {
            /* set it */
            opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                                "%s Assigning default freq governor to job %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_JOBID_PRINT(jdata->jobid));
            orte_set_attribute(&jdata->attributes, ORTE_JOB_GOVERNOR, ORTE_ATTR_GLOBAL,
                               mca_rtc_freq_component.governor, OPAL_STRING);
        } else if (freq_given) {
            /* if the user specified a frequency, then we should default
             * to the userspace governor to ensure we can set it */
            opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                                "%s Assigning default userspace governor to job %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_JOBID_PRINT(jdata->jobid));
            orte_set_attribute(&jdata->attributes, ORTE_JOB_GOVERNOR, ORTE_ATTR_GLOBAL,
                               "userspace", OPAL_STRING);
        }
    }
}

static void set(orte_job_t *jdata,
                orte_proc_t *child,
                char ***environ_copy,
                int write_fd)
{
    char *governor, *tmp, **vals;
    rtefreq_tracker_t *trk;
    opal_value_t *kv;
    float freq, *fptr, minfreq;
    bool setspeed_used = false;
    bool allowed;
    char *filename;
    FILE *fp;

    opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                        "%s Setting freq controls for job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jdata->jobid));

    /* see if the job has the governor attribute set */
    governor = NULL;
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_GOVERNOR, (void**)&governor, OPAL_STRING)) {
        /* loop thru all the cpus on this node */
        OPAL_LIST_FOREACH(trk, &tracking, rtefreq_tracker_t) {
            /* does the requested value match the current setting? */
            if (0 == strcmp(trk->current_governor, governor)) {
                continue;
            }
            /* is the specified governor among those allowed? */
            allowed = false;
            OPAL_LIST_FOREACH(kv, &trk->governors, opal_value_t) {
                if (0 == strcmp(kv->data.string, governor)) {
                    allowed = true;
                    break;
                }
            }
            if (!allowed) {
                vals = NULL;
                OPAL_LIST_FOREACH(kv, &trk->governors, opal_value_t) {
                    opal_argv_append_nosize(&vals, kv->data.string);
                }
                tmp = opal_argv_join(vals, ',');
                opal_argv_free(vals);
                orte_show_help("help-rtc-freq.txt", "unsupported-governor", true,
                               orte_process_info.nodename, governor, tmp);
                free(tmp);
                /* generate an error so the errmgr can resolve it */
                return;
            }
            /* attempt to set the value */
            filename = opal_os_path(false, trk->directory, "scaling_governor", NULL);
            if (NULL == (fp = fopen(filename, "w"))) {
                /* not allowed - report the error */
                orte_show_help("help-rtc-freq.txt", "permission-denied", true,
                               "governor", orte_process_info.nodename, filename);
                free(filename);
                return;
            }
            opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                                "%s Setting governor %s for job %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), governor,
                                ORTE_JOBID_PRINT(jdata->jobid));
            fprintf(fp, "%s\n", governor);
            fclose(fp);
            free(filename);
        }
    }

    /* see if the job has the min freq attribute set */
    fptr = &minfreq;
    if (!orte_get_attribute(&jdata->attributes, ORTE_JOB_MIN_FREQ, (void**)&fptr, OPAL_FLOAT)) {
        minfreq = -1.0;
    }

    /* see if the job has the max freq attribute set */
    fptr = &freq;
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_MAX_FREQ, (void**)&fptr, OPAL_FLOAT)) {
        /* loop thru all the cpus on this node */
        OPAL_LIST_FOREACH(trk, &tracking, rtefreq_tracker_t) {
            /* does the requested value match the current setting? */
            if (trk->current_max_freq == freq) {
                continue;
            }
            /* is the specified frequency among those allowed? */
            allowed = false;
            OPAL_LIST_FOREACH(kv, &trk->frequencies, opal_value_t) {
                if (kv->data.fval == freq) {
                    allowed = true;
                    break;
                }
            }
            if (!allowed) {
                vals = NULL;
                OPAL_LIST_FOREACH(kv, &trk->frequencies, opal_value_t) {
                    asprintf(&tmp, "%f", kv->data.fval);
                    opal_argv_append_nosize(&vals, tmp);
                    free(tmp);
                }
                tmp = opal_argv_join(vals, ',');
                opal_argv_free(vals);
                orte_show_help("help-rtc-freq.txt", "unsupported-freq", true, freq, tmp);
                free(tmp);
                /* generate an error so the errmgr can resolve it */
                return;
            }
            /* if we got a min freq and the two are the same, then use setspeed if supported */
            if (minfreq == freq && trk->setspeed) {
                filename = opal_os_path(false, trk->directory, "scaling_setspeed", NULL);
                setspeed_used = true;
            } else {
                filename = opal_os_path(false, trk->directory, "scaling_max_freq", NULL);
            }
            /* attempt to set the value */
            if (NULL == (fp = fopen(filename, "w"))) {
                /* not allowed - report the error */
                orte_show_help("help-rtc-freq.txt", "permission-denied", true,
                               "max freq", orte_process_info.nodename, filename);
                free(filename);
                return;
            }
            opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                                "%s Setting %s freq controls to %ld for job %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                setspeed_used ? "cpu" : "max",
                                (unsigned long)(freq * 1000000.0),
                                ORTE_JOBID_PRINT(jdata->jobid));
            fprintf(fp, "%ld\n", (unsigned long)(freq * 1000000.0));
            fclose(fp);
            free(filename);
        }
    }

    if (!setspeed_used && 0.0 < minfreq) {
        /* need to process the min freq value - loop thru all the cpus on this node */
        OPAL_LIST_FOREACH(trk, &tracking, rtefreq_tracker_t) {
            /* does the requested value match the current setting? */
            if (trk->current_min_freq == minfreq) {
                continue;
            }
            /* is the specified frequency among those allowed? */
            allowed = false;
            OPAL_LIST_FOREACH(kv, &trk->frequencies, opal_value_t) {
                if (kv->data.fval == minfreq) {
                    allowed = true;
                    break;
                }
            }
            if (!allowed) {
                vals = NULL;
                OPAL_LIST_FOREACH(kv, &trk->frequencies, opal_value_t) {
                    asprintf(&tmp, "%f", kv->data.fval);
                    opal_argv_append_nosize(&vals, tmp);
                    free(tmp);
                }
                tmp = opal_argv_join(vals, ',');
                opal_argv_free(vals);
                orte_show_help("help-rtc-freq.txt", "unsupported-freq", true, minfreq, tmp);
                free(tmp);
                /* generate an error so the errmgr can resolve it */
                return;
            }
            filename = opal_os_path(false, trk->directory, "scaling_min_freq", NULL);
            /* attempt to set the value */
            if (NULL == (fp = fopen(filename, "w"))) {
                /* not allowed - report the error */
                orte_show_help("help-rtc-freq.txt", "permission-denied", true,
                               "min freq", orte_process_info.nodename, filename);
                free(filename);
                return;
            }
            opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                                "%s Setting min freq controls to %ld for job %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (unsigned long)(minfreq * 1000000.0),
                                ORTE_JOBID_PRINT(jdata->jobid));
            fprintf(fp, "%ld\n", (unsigned long)(minfreq * 1000000.0));
            fclose(fp);
            free(filename);
        }
    }
}

static void getvals(opal_list_t *vals)
{
    rtefreq_tracker_t *trk;
    orte_rtc_resource_t *res;
    opal_value_t *kv;
    char *tmp, **args;

    res = OBJ_NEW(orte_rtc_resource_t);

    OPAL_LIST_FOREACH(trk, &tracking, rtefreq_tracker_t) {
        res = OBJ_NEW(orte_rtc_resource_t);
        res->component = strdup(mca_rtc_freq_component.super.base_version.mca_component_name);
        asprintf(&res->category, "core-%d", trk->core);
        opal_list_append(vals, &res->super);
        args = NULL;
        OPAL_LIST_FOREACH(kv, &trk->governors, opal_value_t) {
            opal_argv_append_nosize(&args, kv->data.string);
        }
        res->control.key = strdup("governors");
        res->control.type = OPAL_STRING;
        res->control.data.string = opal_argv_join(args, ',');
        opal_argv_free(args);

        res = OBJ_NEW(orte_rtc_resource_t);
        res->component = strdup(mca_rtc_freq_component.super.base_version.mca_component_name);
        asprintf(&res->category, "core-%d", trk->core);
        opal_list_append(vals, &res->super);
        args = NULL;
        OPAL_LIST_FOREACH(kv, &trk->frequencies, opal_value_t) {
            asprintf(&tmp, "%f", kv->data.fval);
            opal_argv_append_nosize(&args, tmp);
            free(tmp);
        }
        res->control.key = strdup("frequencies");
        res->control.type = OPAL_STRING;
        res->control.data.string = opal_argv_join(args, ',');
        opal_argv_free(args);
    }
}

/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"
#include "pmix_common.h"

/* This component will only be compiled on Linux, where we are
   guaranteed to have <unistd.h> and friends */
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif

#include <sys/param.h> /* for HZ to convert jiffies to actual time */

#include "pstat_linux.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_printf.h"

/*
 * API functions
 */
static int linux_module_init(void);
static int query(pid_t pid, pmix_proc_stats_t *stats, pmix_node_stats_t *nstats);
static int linux_module_fini(void);

/*
 * Linux pstat module
 */
const pmix_pstat_base_module_t pmix_pstat_linux_module = {
    /* Initialization function */
    linux_module_init, query, linux_module_fini};

#define PMIX_STAT_MAX_LENGTH 1024

/* Local structs */
typedef struct {
    pmix_list_item_t super;
    pmix_disk_stats_t dstat;
} dstats_t;
static PMIX_CLASS_INSTANCE(dstats_t, pmix_list_item_t, NULL, NULL);

typedef struct {
    pmix_list_item_t super;
    pmix_net_stats_t nstat;
} ndstats_t;
static PMIX_CLASS_INSTANCE(ndstats_t, pmix_list_item_t, NULL, NULL);

/* Local functions */
static char *local_getline(FILE *fp);
static char *local_stripper(char *data);
static void local_getfields(char *data, char ***fields);

/* Local data */
static char input[PMIX_STAT_MAX_LENGTH];

static int linux_module_init(void)
{
    return PMIX_SUCCESS;
}

static int linux_module_fini(void)
{
    return PMIX_SUCCESS;
}

static char *next_field(char *ptr, int barrier)
{
    int i = 0;

    /* we are probably pointing to the last char
     * of the current field, so look for whitespace
     */
    while (!isspace(*ptr) && i < barrier) {
        ptr++; /* step over the current char */
        i++;
    }

    /* now look for the next field */
    while (isspace(*ptr) && i < barrier) {
        ptr++;
        i++;
    }

    return ptr;
}

static float convert_value(char *value)
{
    char *ptr;
    float fval;

    /* compute base value */
    fval = (float) strtoul(value, &ptr, 10);
    /* get the unit multiplier */
    if (NULL != ptr && NULL != strstr(ptr, "kB")) {
        fval /= 1024.0;
    }
    return fval;
}

static int query(pid_t pid, pmix_proc_stats_t *stats, pmix_node_stats_t *nstats)
{
    char data[4096];
    int fd;
    size_t numchars;
    char *ptr, *eptr;
    int i;
    int len, itime;
    double dtime;
    FILE *fp;
    char *dptr, *value;
    char **fields;
    pmix_list_t cache;
    dstats_t *ds;
    ndstats_t *ns;

    if (NULL != stats) {
        /* record the time of this sample */
        gettimeofday(&stats->sample_time, NULL);
        /* check the nstats - don't do gettimeofday twice
         * as it is expensive
         */
        if (NULL != nstats) {
            nstats->sample_time.tv_sec = stats->sample_time.tv_sec;
            nstats->sample_time.tv_usec = stats->sample_time.tv_usec;
        }
    } else if (NULL != nstats) {
        /* record the time of this sample */
        gettimeofday(&nstats->sample_time, NULL);
    }

    if (NULL != stats) {
        stats->node = strdup(pmix_globals.hostname);

        /* create the stat filename for this proc */
        numchars = pmix_snprintf(data, sizeof(data), "/proc/%d/stat", pid);
        if (numchars >= sizeof(data)) {
            return PMIX_ERROR;
        }

        if (0 > (fd = open(data, O_RDONLY))) {
            /* can't access this file - most likely, this means we
             * aren't really on a supported system, or the proc no
             * longer exists. Just return an error
             */
            return PMIX_ERROR;
        }

        /* absorb all of the file's contents in one gulp - we'll process
         * it once it is in memory for speed
         */
        memset(data, 0, sizeof(data));
        len = read(fd, data, sizeof(data) - 1);
        if (len < 0) {
            /* This shouldn't happen! */
            close(fd);
            return PMIX_ERROR;
        }
        close(fd);

        /* remove newline at end */
        data[len] = '\0';

        /* the stat file consists of a single line in a carefully formatted
         * form. Parse it field by field as per proc(3) to get the ones we want
         */

        /* we don't need to read the pid from the file - we already know it! */
        stats->pid = pid;

        /* the cmd is surrounded by parentheses - find the start */
        if (NULL == (ptr = strchr(data, '('))) {
            /* no cmd => something wrong with data, return error */
            return PMIX_ERR_BAD_PARAM;
        }
        /* step over the paren */
        ptr++;

        /* find the ending paren */
        if (NULL == (eptr = strchr(ptr, ')'))) {
            /* no end to cmd => something wrong with data, return error */
            return PMIX_ERR_BAD_PARAM;
        }

        /* save the cmd name, up to the limit of the array */
        *eptr = '\0';
        stats->cmd = strdup(ptr);
        *eptr = ')';

        /* move to the next field in the data */
        ptr = next_field(eptr, len);

        /* next is the process state - a single character */
        stats->state = *ptr;
        /* move to next field */
        ptr = next_field(ptr, len);

        /* skip fields until we get to the times */
        ptr = next_field(ptr, len); /* ppid */
        ptr = next_field(ptr, len); /* pgrp */
        ptr = next_field(ptr, len); /* session */
        ptr = next_field(ptr, len); /* tty_nr */
        ptr = next_field(ptr, len); /* tpgid */
        ptr = next_field(ptr, len); /* flags */
        ptr = next_field(ptr, len); /* minflt */
        ptr = next_field(ptr, len); /* cminflt */
        ptr = next_field(ptr, len); /* majflt */
        ptr = next_field(ptr, len); /* cmajflt */

        /* grab the process time usage fields */
        itime = strtoul(ptr, &ptr, 10);  /* utime */
        itime += strtoul(ptr, &ptr, 10); /* add the stime */
        /* convert to time in seconds */
        dtime = (double) itime / (double) HZ;
        stats->time.tv_sec = (int) dtime;
        stats->time.tv_usec = (int) (1000000.0 * (dtime - stats->time.tv_sec));
        /* move to next field */
        ptr = next_field(ptr, len);

        /* skip fields until we get to priority */
        ptr = next_field(ptr, len); /* cutime */
        ptr = next_field(ptr, len); /* cstime */

        /* save the priority */
        stats->priority = strtol(ptr, &ptr, 10);
        /* move to next field */
        ptr = next_field(ptr, len);

        /* skip nice */
        ptr = next_field(ptr, len);

        /* get number of threads */
        stats->num_threads = strtoul(ptr, &ptr, 10);
        /* move to next field */
        ptr = next_field(ptr, len);

        /* skip fields until we get to processor id */
        ptr = next_field(ptr, len); /* itrealvalue */
        ptr = next_field(ptr, len); /* starttime */
        ptr = next_field(ptr, len); /* vsize */
        ptr = next_field(ptr, len); /* rss */
        ptr = next_field(ptr, len); /* rss limit */
        ptr = next_field(ptr, len); /* startcode */
        ptr = next_field(ptr, len); /* endcode */
        ptr = next_field(ptr, len); /* startstack */
        ptr = next_field(ptr, len); /* kstkesp */
        ptr = next_field(ptr, len); /* kstkeip */
        ptr = next_field(ptr, len); /* signal */
        ptr = next_field(ptr, len); /* blocked */
        ptr = next_field(ptr, len); /* sigignore */
        ptr = next_field(ptr, len); /* sigcatch */
        ptr = next_field(ptr, len); /* wchan */
        ptr = next_field(ptr, len); /* nswap */
        ptr = next_field(ptr, len); /* cnswap */
        ptr = next_field(ptr, len); /* exit_signal */

        /* finally - get the processor */
        stats->processor = strtol(ptr, NULL, 10);

        /* that's all we care about from this data - ignore the rest */

        /* now create the status filename for this proc */
        memset(data, 0, sizeof(data));
        numchars = pmix_snprintf(data, sizeof(data), "/proc/%d/status", pid);
        if (numchars >= sizeof(data)) {
            return PMIX_ERROR;
        }

        if (NULL == (fp = fopen(data, "r"))) {
            /* ignore this */
            return PMIX_SUCCESS;
        }

        /* parse it according to proc(3) */
        while (NULL != (dptr = local_getline(fp))) {
            if (NULL == (value = local_stripper(dptr))) {
                /* cannot process */
                continue;
            }
            /* look for VmPeak */
            if (0 == strncmp(dptr, "VmPeak", strlen("VmPeak"))) {
                stats->peak_vsize = convert_value(value);
            } else if (0 == strncmp(dptr, "VmSize", strlen("VmSize"))) {
                stats->vsize = convert_value(value);
            } else if (0 == strncmp(dptr, "VmRSS", strlen("VmRSS"))) {
                stats->rss = convert_value(value);
            }
        }
        fclose(fp);

        /* now create the smaps filename for this proc */
        memset(data, 0, sizeof(data));
        numchars = pmix_snprintf(data, sizeof(data), "/proc/%d/smaps", pid);
        if (numchars >= sizeof(data)) {
            return PMIX_ERROR;
        }

        if (NULL == (fp = fopen(data, "r"))) {
            /* ignore this */
            return PMIX_SUCCESS;
        }

        /* parse it to find lines that start with "Pss" */
        while (NULL != (dptr = local_getline(fp))) {
            if (NULL == (value = local_stripper(dptr))) {
                /* cannot process */
                continue;
            }
            /* look for Pss */
            if (0 == strncmp(dptr, "Pss", strlen("Pss"))) {
                stats->pss += convert_value(value);
            }
        }
        fclose(fp);
    }

    if (NULL != nstats) {
        nstats->node = strdup(pmix_globals.hostname);

        /* get the loadavg data */
        if (0 > (fd = open("/proc/loadavg", O_RDONLY))) {
            /* not an error if we don't find this one as it
             * isn't critical
             */
            goto diskstats;
        }

        /* absorb all of the file's contents in one gulp - we'll process
         * it once it is in memory for speed
         */
        memset(data, 0, sizeof(data));
        len = read(fd, data, sizeof(data) - 1);
        close(fd);
        if (len < 0) {
            goto diskstats;
        }

        /* remove newline at end */
        data[len] = '\0';

        /* we only care about the first three numbers */
        nstats->la = strtof(data, &ptr);
        nstats->la5 = strtof(ptr, &eptr);
        nstats->la15 = strtof(eptr, NULL);

        /* see if we can open the meminfo file */
        if (NULL == (fp = fopen("/proc/meminfo", "r"))) {
            /* ignore this */
            goto diskstats;
        }

        /* read the file one line at a time */
        while (NULL != (dptr = local_getline(fp))) {
            if (NULL == (value = local_stripper(dptr))) {
                /* cannot process */
                continue;
            }
            if (0 == strcmp(dptr, "MemTotal")) {
                nstats->total_mem = convert_value(value);
            } else if (0 == strcmp(dptr, "MemFree")) {
                nstats->free_mem = convert_value(value);
            } else if (0 == strcmp(dptr, "Buffers")) {
                nstats->buffers = convert_value(value);
            } else if (0 == strcmp(dptr, "Cached")) {
                nstats->cached = convert_value(value);
            } else if (0 == strcmp(dptr, "SwapCached")) {
                nstats->swap_cached = convert_value(value);
            } else if (0 == strcmp(dptr, "SwapTotal")) {
                nstats->swap_total = convert_value(value);
            } else if (0 == strcmp(dptr, "SwapFree")) {
                nstats->swap_free = convert_value(value);
            } else if (0 == strcmp(dptr, "Mapped")) {
                nstats->mapped = convert_value(value);
            }
        }
        fclose(fp);

    diskstats:
        /* look for the diskstats file */
        if (NULL == (fp = fopen("/proc/diskstats", "r"))) {
            /* not an error if we don't find this one as it
             * isn't critical
             */
            goto netstats;
        }
        PMIX_CONSTRUCT(&cache, pmix_list_t);
        /* read the file one line at a time */
        while (NULL != (dptr = local_getline(fp))) {
            /* look for the local disks */
            if (NULL == strstr(dptr, "sd")) {
                continue;
            }
            /* parse to extract the fields */
            fields = NULL;
            local_getfields(dptr, &fields);
            if (NULL == fields) {
                continue;
            }
            if (14 < PMIx_Argv_count(fields)) {
                PMIx_Argv_free(fields);
                continue;
            }
            /* pack the ones of interest into the struct */
            ds = PMIX_NEW(dstats_t);
            ds->dstat.disk = strdup(fields[2]);
            ds->dstat.num_reads_completed = strtoul(fields[3], NULL, 10);
            ds->dstat.num_reads_merged = strtoul(fields[4], NULL, 10);
            ds->dstat.num_sectors_read = strtoul(fields[5], NULL, 10);
            ds->dstat.milliseconds_reading = strtoul(fields[6], NULL, 10);
            ds->dstat.num_writes_completed = strtoul(fields[7], NULL, 10);
            ds->dstat.num_writes_merged = strtoul(fields[8], NULL, 10);
            ds->dstat.num_sectors_written = strtoul(fields[9], NULL, 10);
            ds->dstat.milliseconds_writing = strtoul(fields[10], NULL, 10);
            ds->dstat.num_ios_in_progress = strtoul(fields[11], NULL, 10);
            ds->dstat.milliseconds_io = strtoul(fields[12], NULL, 10);
            ds->dstat.weighted_milliseconds_io = strtoul(fields[13], NULL, 10);
            pmix_list_append(&cache, &ds->super);
            PMIx_Argv_free(fields);
        }
        fclose(fp);
        if (0 < (len = pmix_list_get_size(&cache))) {
            PMIX_DISK_STATS_CREATE(nstats->diskstats, len);
            i = 0;
            PMIX_LIST_FOREACH (ds, &cache, dstats_t) {
                memcpy(&nstats->diskstats[i], &ds->dstat, sizeof(pmix_disk_stats_t));
                ++i;
            }
        }
        PMIX_LIST_DESTRUCT(&cache);

    netstats:
        /* look for the netstats file */
        if (NULL == (fp = fopen("/proc/net/dev", "r"))) {
            /* not an error if we don't find this one as it
             * isn't critical
             */
            goto complete;
        }
        /* skip the first two lines as they are headers */
        local_getline(fp);
        local_getline(fp);
        /* read the file one line at a time */
        PMIX_CONSTRUCT(&cache, pmix_list_t);
        while (NULL != (dptr = local_getline(fp))) {
            /* the interface is at the start of the line */
            if (NULL == (ptr = strchr(dptr, ':'))) {
                continue;
            }
            *ptr = '\0';
            ptr++;
            /* parse to extract the fields */
            fields = NULL;
            local_getfields(ptr, &fields);
            if (NULL == fields) {
                continue;
            }
            /* pack the ones of interest into the struct */
            ns = PMIX_NEW(ndstats_t);
            ns->nstat.net_interface = strdup(dptr);
            ns->nstat.num_bytes_recvd = strtoul(fields[0], NULL, 10);
            ns->nstat.num_packets_recvd = strtoul(fields[1], NULL, 10);
            ns->nstat.num_recv_errs = strtoul(fields[2], NULL, 10);
            ns->nstat.num_bytes_sent = strtoul(fields[8], NULL, 10);
            ns->nstat.num_packets_sent = strtoul(fields[9], NULL, 10);
            ns->nstat.num_send_errs = strtoul(fields[10], NULL, 10);
            pmix_list_append(&cache, &ns->super);
            PMIx_Argv_free(fields);
        }
        fclose(fp);
        if (0 < (len = pmix_list_get_size(&cache))) {
            PMIX_NET_STATS_CREATE(nstats->netstats, len);
            i = 0;
            PMIX_LIST_FOREACH (ns, &cache, ndstats_t) {
                memcpy(&nstats->netstats[i], &ns->nstat, sizeof(pmix_net_stats_t));
                ++i;
            }
        }
        PMIX_LIST_DESTRUCT(&cache);
    }

complete:
    return PMIX_SUCCESS;
}

static char *local_getline(FILE *fp)
{
    char *ret, *ptr;

    ret = fgets(input, PMIX_STAT_MAX_LENGTH, fp);
    if (NULL != ret) {
        input[strlen(input) - 1] = '\0'; /* remove newline */
        /* strip leading white space */
        ptr = input;
        while (!isalnum(*ptr)) {
            ptr++;
        }
        return ptr;
    }

    return NULL;
}

static char *local_stripper(char *data)
{
    char *ptr, *end, *enddata;
    int len = strlen(data);

    /* find the colon */
    if (NULL == (end = strchr(data, ':'))) {
        return NULL;
    }
    ptr = end;
    --end;
    /* working backwards, look for first non-whitespace */
    while (end != data && !isalnum(*end)) {
        --end;
    }
    ++end;
    *end = '\0';
    /* now look for value */
    ptr++;
    enddata = &(data[len - 1]);
    while (ptr != enddata && !isalnum(*ptr)) {
        ++ptr;
    }
    return ptr;
}

static void local_getfields(char *dptr, char ***fields)
{
    char *ptr, *end;

    /* set default */
    *fields = NULL;

    /* find the beginning */
    ptr = dptr;
    while ('\0' != *ptr && !isalnum(*ptr)) {
        ptr++;
    }
    if ('\0' == *ptr) {
        return;
    }

    /* working from this point, find the end of each
     * alphanumeric field and store it on the stack.
     * Then shift across the white space to the start
     * of the next one
     */
    end = ptr; /* ptr points to an alnum */
    end++;     /* look at next character */
    while ('\0' != *end) {
        /* find the end of this alpha string */
        while ('\0' != *end && isalnum(*end)) {
            end++;
        }
        /* terminate it */
        *end = '\0';
        /* store it on the stack */
        PMIx_Argv_append_nosize(fields, ptr);
        /* step across any white space */
        end++;
        while ('\0' != *end && !isalnum(*end)) {
            end++;
        }
        if ('\0' == *end) {
            ptr = NULL;
            break;
        }
        ptr = end;
        end++;
    }
    if (NULL != ptr) {
        /* have a hanging field */
        PMIx_Argv_append_nosize(fields, ptr);
    }
}

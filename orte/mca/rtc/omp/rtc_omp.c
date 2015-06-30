/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved
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

#include "opal/mca/hwloc/hwloc.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"

#include "orte/util/show_help.h"
#include "orte/util/error_strings.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps_types.h"

#include "orte/mca/rtc/base/base.h"
#include "rtc_omp.h"

static int init(void);
static void finalize(void);
static void set(orte_job_t *jdata,
                orte_proc_t *proc,
                char ***environ_copy,
                int write_fd);

orte_rtc_base_module_t orte_rtc_omp_module = {
    init,
    finalize,
    NULL,
    set,
    NULL
};

static int init(void)
{
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    return;
}

static void set(orte_job_t *jobdat,
                orte_proc_t *child,
                char ***environ_copy,
                int write_fd)
{
    char *param;
    char *cpu_bitmap;
    char **ranges, *ptr, *tmp, **results;
    int i, n, start, end;
    
    opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                        "%s omp:set envars on child %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == child) ? "NULL" : ORTE_NAME_PRINT(&child->name));

    if (NULL == jobdat || NULL == child) {
        /* nothing for us to do */
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s omp:set jobdat %s child %s - nothing to do",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == jobdat) ? "NULL" : ORTE_JOBID_PRINT(jobdat->jobid),
                            (NULL == child) ? "NULL" : ORTE_NAME_PRINT(&child->name));
        return;
    }

    /* See if we are bound */
    cpu_bitmap = NULL;
    if (!orte_get_attribute(&child->attributes, ORTE_PROC_CPU_BITMAP, (void**)&cpu_bitmap, OPAL_STRING) ||
        NULL == cpu_bitmap || 0 == strlen(cpu_bitmap)) {
        /* we are not bound, so indicate that by setting OMP_PROC_BIND = false */
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s omp:set not bound - set OMP_PROC_BIND=FALSE",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        opal_setenv("OMP_PROC_BIND", "FALSE", true, environ_copy);
    } else {
        /* we are bound to something, so indicate that by setting OMP_PROC_BIND = true */
        opal_output_verbose(2, orte_rtc_base_framework.framework_output,
                            "%s omp:set not bound - set OMP_PROC_BIND=SPREAD",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        opal_setenv("OMP_PROC_BIND", "SPREAD", true, environ_copy);
        /* compose OMP_PLACES to indicate where we are bound - sadly, the OMP folks
         * use a different syntax than HWLOC, an so we can't just provide the bitmap
         * string. So we will traverse the bitmap and convert as required */
        ranges = opal_argv_split(cpu_bitmap, ',');
        results = NULL;
        for (i=0; NULL != ranges[i]; i++) {
            if (NULL == (ptr = strchr(ranges[i], '-'))) {
                asprintf(&tmp, "{%s}", ranges[i]);
                opal_argv_append_nosize(&results, tmp);
                free(tmp);
            } else {
                *ptr = '\0';
                ++ptr;
                start = strtol(ranges[i], NULL, 10);
                end = strtol(ptr, NULL, 10);
                for (n=start; n <= end; n++) {
                    asprintf(&tmp, "{%d}", n);
                    opal_argv_append_nosize(&results, tmp);
                    free(tmp);
                }
            }
        }
        opal_argv_free(ranges);
        param = opal_argv_join(results, ',');
        opal_argv_free(results);
        opal_setenv("OMP_PLACES", param, true, environ_copy);
        free(param);
    }
    if (NULL != cpu_bitmap) {
        free(cpu_bitmap);
    }
}

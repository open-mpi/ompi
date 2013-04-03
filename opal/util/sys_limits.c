/*
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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * This file is only here because some platforms have a broken strncpy
 * (e.g., Itanium with RedHat Advanced Server glibc).
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include "opal/constants.h"
#include "opal/runtime/opal_params.h"

#include "opal/util/sys_limits.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

/*
 * Create and initialize storage for the system limits
 */
OPAL_DECLSPEC opal_sys_limits_t opal_sys_limits = {
    /* initialized = */     false,
    /* num_files   = */     -1,
    /* num_procs   = */     -1,
    /* file_size   = */      0
};

static rlim_t opal_setlimit(int resource, char *value)
{
    struct rlimit rlim, rlim_set;
    rlim_t maxlim;

    rlim.rlim_cur = 0;

    if (0 == strcmp(value, "max")) {
            maxlim = -1;
    } else if (0 == strncmp(value, "unlimited", strlen(value))) {
            maxlim = RLIM_INFINITY;
    } else {
        maxlim = strtol(value, NULL, 10);
    }

    if (0 <= getrlimit (RLIMIT_NOFILE, &rlim)) {
        if (maxlim < 0) {
            rlim_set.rlim_cur = rlim.rlim_cur;
            rlim_set.rlim_max = rlim.rlim_max;
        } else {
            rlim_set.rlim_cur = maxlim;
            rlim_set.rlim_max = maxlim;
        }
        if (0 <= setrlimit (RLIMIT_NOFILE, &rlim_set)) {
            rlim.rlim_cur = rlim.rlim_cur;
        }
    }
    return rlim.rlim_cur;
}

int opal_util_init_sys_limits(void)
{
    char **lims, **lim, *setlim;
    int i;

    /* if limits were not given, then nothing to do */
    if (NULL == opal_set_max_sys_limits) {
        return OPAL_SUCCESS;
    }

    /* parse the requested limits to set */
    lims = opal_argv_split(opal_set_max_sys_limits, ',');

    /* each limit is expressed as a "param:value" pair */
    for (i=0; NULL != lims[i]; i++) {
        lim = opal_argv_split(lims[i], ':');
        if (1 == opal_argv_count(lim)) {
            setlim = "max";
        } else {
            setlim = lim[1];
        }

        /* for historical reasons, a value of "1" means
         * that we set the limits on #files, #children,
         * and max file size
         */
        if (0 == strcmp(lim[0], "1")) {
            opal_sys_limits.num_files = opal_setlimit(RLIMIT_NOFILE, "max");
#if HAVE_DECL_RLIMIT_NPROC
            opal_sys_limits.num_procs = opal_setlimit(RLIMIT_NPROC, "max");
#endif
            opal_sys_limits.file_size = opal_setlimit(RLIMIT_FSIZE, "max");
            break;
        } else if (0 == strcmp(lim[0], "0")) {
            /* user didn't want anything set */
            goto cleanup;
        }
 
        /* process them separately */
        if (0 == strcmp(lim[0], "core")) {
            opal_setlimit(RLIMIT_CORE, setlim);
        } else if (0 == strcmp(lim[0], "filesize")) {
            opal_setlimit(RLIMIT_FSIZE, setlim);
        } else if (0 == strcmp(lim[0], "maxmem")) {
            opal_setlimit(RLIMIT_AS, setlim);
        } else if (0 == strcmp(lim[0], "openfiles")) {
            opal_setlimit(RLIMIT_NOFILE, setlim);
        } else if (0 == strcmp(lim[0], "stacksize")) {
            opal_setlimit(RLIMIT_STACK, setlim);
#if HAVE_DECL_RLIMIT_NPROC
        } else if (0 == strcmp(lim[0], "maxchildren")) {
            opal_setlimit(RLIMIT_NPROC, "max");
#endif
        } else {
            opal_output(0, "Unrecognized setlimit option: %s - ignored", lim[0]);
        }
    }

 cleanup:
    if (NULL != lim) {
        opal_argv_free(lim);
    }
    if (NULL != lims) {
        opal_argv_free(lims);
    }

    /* indicate we initialized the limits structure */
    opal_sys_limits.initialized = true;

    return OPAL_SUCCESS;
}

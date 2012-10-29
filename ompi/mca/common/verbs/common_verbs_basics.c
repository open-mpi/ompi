/*
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* This is crummy, but <infiniband/driver.h> doesn't work on all
   platforms with all compilers.  Specifically, trying to include it
   on RHEL4U3 with the PGI 32 bit compiler will cause problems because
   certain 64 bit types are not defined.  Per advice from Roland D.,
   just include the one prototype that we need in this case
   (ibv_get_sysfs_path()). */
#include <infiniband/verbs.h>
#ifdef HAVE_INFINIBAND_DRIVER_H
#include <infiniband/driver.h>
#else
const char *ibv_get_sysfs_path(void);
#endif

#include "ompi/constants.h"

#include "common_verbs.h"

/***********************************************************************/

bool ompi_common_verbs_check_basics(void)
{
#if defined(__linux__)
    int rc;
    char *file;
    struct stat s;

    /* Check to see if $sysfsdir/class/infiniband/ exists */
    asprintf(&file, "%s/class/infiniband", ibv_get_sysfs_path());
    if (NULL == file) {
        return false;
    }
    rc = stat(file, &s);
    free(file);
    if (0 != rc || !S_ISDIR(s.st_mode)) {
        return false;
    }
#endif

    /* It exists and is a directory -- good enough */
    return true;
}


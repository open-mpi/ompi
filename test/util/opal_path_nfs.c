/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2010      Oak Ridge National Laboratory.
 *                         All rights reserved.
 * Copyright (c) 2010-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/param.h>
#ifdef HAVE_SYS_MOUNT_H
#    include <sys/mount.h>
#endif
#ifdef HAVE_SYS_STATFS_H
#    include <sys/statfs.h>
#endif
#ifdef HAVE_SYS_VFS_H
#    include <sys/vfs.h>
#endif

#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/printf.h"
#include "support.h"

#define DEBUG

/*
 * On Linux, if this test is run with no command line params, it will
 * run "mount" and analyze the output.  It will slurp up all mount
 * points and figure out if they are network mounts or not.  Then it
 * will send each of the mount points through opal_path_nfs() and
 * compare the answer it gets to the answer it slurped from the output
 * of "mount".
 *
 * On all platforms, if you provide one or more command line options,
 * each command line option is given to opal_path_nfs() and the result
 * is sent to stdout.
 */

#if !defined(__linux__)
int main(int argc, char *argv[])
{
    if (argc > 1) {
        int i;

        printf("Interactive opal_path_nfs() test:\n");
        for (i = 1; i < argc; i++) {
            printf("Is dir[%d]:%s one of the detected network file systems? %s\n", i, argv[i],
                   opal_path_nfs(argv[i], NULL) ? "Yes" : "No");
        }

        return 0;
    }

    printf("No filename was given; nothing to do\n");
    return 77;
}

#else /* __linux__ */

static void test(char *file, bool expect);
static void get_mounts(int *num_dirs, char **dirs[], bool **nfs);

int main(int argc, char *argv[])
{
    int num_dirs;
    char **dirs;
    bool *nfs;

    test_init("opal_path_nfs()");
#    ifdef DEBUG
    printf("Test usage: ./opal_path_nfs [DIR]\n");
    printf(
        "On Linux interprets output from mount(8) to check for nfs and verify opal_path_nfs()\n");
    printf("Additionally, you may specify multiple DIR on the cmd-line, of which you the output\n");
#    endif

    if (1 < argc) {
        int i;
        for (i = 1; i < argc; i++)
            printf("Is dir[%d]:%s one of the detected network file systems? %s\n", i, argv[i],
                   opal_path_nfs(argv[i], NULL) ? "Yes" : "No");
    }

    get_mounts(&num_dirs, &dirs, &nfs);
    while (num_dirs--) {
        test(dirs[num_dirs], nfs[num_dirs]);
    }

    /* All done */
    return test_finalize();
}

void test(char *file, bool expect)
{
#    ifdef DEBUG
    printf("test(): file:%s bool:%d\n", file, expect);
#    endif
    if (expect == opal_path_nfs(file, NULL)) {
        test_success();
    } else {
        char *msg;
        opal_asprintf(&msg, "Mismatch: input \"%s\", expected:%d got:%d\n", file, expect, !expect);
        test_failure(msg);
        free(msg);
    }
}

void get_mounts(int *num_dirs, char **dirs[], bool *nfs[])
{
#    define SIZE 1024
    char *cmd = "mount | cut -f3,5 -d' ' > opal_path_nfs.out";
    int rc;
    int i;
    FILE *file;
    char **dirs_tmp;
    bool *nfs_tmp;
    char buffer[SIZE];
    struct statfs mystatfs;

    rc = system(cmd);

    if (-1 == rc) {
        *num_dirs = 0;
        **dirs = NULL;
        *nfs = NULL;
    }

    /* First, count how many mount points there are.  Previous
       versions of this test tried to have a (large) constant-sized
       array for the mount points, but periodically it would break
       because we would run this test on a system with a larger number
       of mount points than the array.  So just count and make sure to
       have an array large enough. */
    file = fopen("opal_path_nfs.out", "r");
    int count = 0;
    while (NULL != fgets(buffer, SIZE, file)) {
        ++count;
    }
    printf("Found %d mounts\n", count);

    // Add one more so we can have a NULL entry at the end
    ++count;

    dirs_tmp = (char **) calloc(count, sizeof(char *));
    nfs_tmp = (bool *) calloc(count, sizeof(bool));

    i = 0;
    rc = 4711;
    rewind(file);
    // i should never be more than count, but be safe anyway.
    while (i < count && NULL != fgets(buffer, SIZE, file)) {
        int mount_known;
        char fs[MAXNAMLEN];

        if (!dirs_tmp[i]) {
            dirs_tmp[i] = malloc(MAXNAMLEN);
        }

        if (2 != (rc = sscanf(buffer, "%s %s\n", dirs_tmp[i], fs))) {
            goto out;
        }

        /*
         * rpc_pipefs is a FS mounted on /var/lib/nfs/rpc_pipefs for NFS4
         * Cannot distinguish it from NFS in opal_path_nfs, therefore just
         * disregard it, as it is NOT an parallel filesystem...
         */
        if (0 == strcasecmp(fs, "rpc_pipefs")) {
            continue;
        }

        /* Per https://svn.open-mpi.org/trac/ompi/ticket/4767, Linux
           lies about fuse filesystems.  Skip them. */
        if (0 == strncasecmp(fs, "fuse.", 5)) {
            continue;
        }

        /* If we get an fs type of "none", skip it (e.g.,
           https://www.open-mpi.org/community/lists/devel/2012/09/11493.php) */
        if (0 == strcasecmp(fs, "none")) {
            continue;
        }

        /* If we can not stat the fs, skip it */
        if (statfs(dirs_tmp[i], &mystatfs)) {
            continue;
        }

        /*
         * Later mounts override earlier mounts
         * (e.g. nfs-mount of /, vs. initial rootfs-mount of /).
         */
        for (mount_known = 0; mount_known < i; mount_known++) {
            /* If we know this mount-point, then exit early */
            if (0 == strcasecmp(dirs_tmp[mount_known], dirs_tmp[i])) {
#    ifdef DEBUG
                printf("get_mounts: already know dir[%d]:%s\n", mount_known, dirs_tmp[mount_known]);
#    endif
                break;
            }
        }

        nfs_tmp[mount_known] = false;
        if (0 == strcasecmp(fs, "nfs") || 0 == strcasecmp(fs, "nfs4")
            || 0 == strcasecmp(fs, "lustre") || 0 == strcasecmp(fs, "panfs")
            || 0 == strcasecmp(fs, "gpfs"))
            nfs_tmp[mount_known] = true;
#    ifdef DEBUG
        printf("get_mounts: dirs[%d]:%s fs:%s nfs:%s\n", mount_known, dirs_tmp[mount_known], fs,
               nfs_tmp[mount_known] ? "Yes" : "No");
#    endif

        if (mount_known >= i)
            i++;
    }
out:
    *num_dirs = i;
    *dirs = dirs_tmp;
    *nfs = nfs_tmp;
}

#endif /* __linux__ */

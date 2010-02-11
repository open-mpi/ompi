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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>

#include "support.h"
#include "opal/util/path.h"
#include "opal/util/output.h"

/*
#define DEBUG
*/

static void test(char* file, bool expect);
static void get_mounts (int * num_dirs, char ** dirs[], bool ** nfs);


int main(int argc, char* argv[])
{
    int num_dirs;
    char ** dirs;
    bool * nfs;

    test_init("opal_path_nfs()");
#ifdef DEBUG
    printf ("Test usage: ./opal_path_nfs [DIR]\n");
    printf ("On Linux interprets output from mount(8) to check for nfs and verify opal_path_nfs()\n");
    printf ("Additionally, you may specify multiple DIR on the cmd-line, of which you the output\n");
#endif

    if (1 < argc) {
        int i;
        for (i = 1; i < argc; i++)
            printf ("Is dir[%d]:%s one of the detected network file systems? %s\n",
                    i, argv[i], opal_path_nfs (argv[i]) ? "Yes": "No");
    }

#ifdef __linux__
    get_mounts (&num_dirs, &dirs, &nfs);
    while (num_dirs--) {
        test (dirs[num_dirs], nfs[num_dirs]);
    }
#endif

#ifdef __WINDOWS__
#endif

    /* All done */
    return test_finalize();
}


void test(char* file, bool expect)
{
#ifdef DEBUG
    printf ("test(): file:%s bool:%d\n",
            file, expect);
#endif 
    if (expect == opal_path_nfs (file)) {
        test_success();
    } else {
        char * msg;
        asprintf(&msg, "Mismatch: input \"%s\", expected:%d got:%d\n",
                 file, expect, !expect);
        test_failure(msg);
        free(msg);
    }
}

void get_mounts (int * num_dirs, char ** dirs[], bool * nfs[])
{
#define MAX_DIR 256
#define SIZE 1024
    char * cmd = "mount | cut -f3,5 -d' ' > opal_path_nfs.out";
    int rc;
    int i;
    FILE * file;
    char ** dirs_tmp;
    bool * nfs_tmp;
    char buffer[SIZE];

    rc = system (cmd);

    if (-1 == rc) {
        *num_dirs = 0;
        **dirs = NULL;
        *nfs = NULL;
    }
    dirs_tmp = (char**) malloc (MAX_DIR * sizeof(char**));
    nfs_tmp = (bool*) malloc (MAX_DIR * sizeof(bool));

    file = fopen("opal_path_nfs.out", "r");
    i = 0;
    rc = 4711;
    while (NULL != fgets (buffer, SIZE, file)) {
        char fs[MAXNAMLEN];

        dirs_tmp[i] = malloc (MAXNAMLEN);
        if (2 != (rc = sscanf (buffer, "%s %s\n", dirs_tmp[i], fs))) {
            goto out;
        }

        /*
         * rpc_pipefs is a FS mounted on /var/lib/nfs/rpc_pipefs for NFS4
         * Cannot distinguish it from NFS in opal_path_nfs, therefore just
         * disregard it, as it is NOT an parallel filesystem...
         */
        if (0 == strcasecmp (fs, "rpc_pipefs"))
            continue;

        nfs_tmp[i] = false;
        if (0 == strcasecmp (fs, "nfs") ||
            0 == strcasecmp (fs, "lustre") ||
            0 == strcasecmp (fs, "panfs"))
            nfs_tmp[i] = true;
#ifdef DEBUG
        printf ("get_mounts: dirs[%d]:%s fs:%s nfs:%s\n",
                i, dirs_tmp[i], fs, nfs_tmp[i] ? "Yes" : "No");
#endif
        i++;
    }
out:
    *num_dirs = i;
    *dirs = dirs_tmp;
    *nfs = nfs_tmp;
}


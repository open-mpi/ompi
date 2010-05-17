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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <sys/stat.h>

#include "support.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"
#include "opal/util/os_path.h"
#include "opal/util/os_dirpath.h"

#ifdef __WINDOWS__
#define PATH_SEP "\\"
#else
#define PATH_SEP "/"
#endif

static const char *path_sep = PATH_SEP;

static bool test1(void);   /* trivial test */
static bool test2(void);   /* test existing path, both with and without correct mode */
static bool test3(void);   /* test making a directory tree */


int main(int argc, char* argv[])
{
    opal_init(&argc, &argv);

    test_init("opal_os_create_dirpath_t");

    /* All done */

    if (test1()) {
        test_success();
    }
    else {
      test_failure("opal_os_create_dirpath test1 failed");
    }

    if (test2()) {
        test_success();
    }
    else {
      test_failure("opal_os_create_dirpath test2 failed");
    }

    if (test3()) {
        test_success();
    }
    else {
      test_failure("opal_os_create_dirpath test3 failed");
    }

    test_finalize();
    opal_finalize();

    return 0;
}


static bool test1(void)
{

    /* Test trivial functionality. Program should return OPAL_ERROR when called with NULL path. */

    if (OPAL_SUCCESS == opal_os_dirpath_create(NULL, S_IRWXU))
            return(false);

    return true;
}


static bool test2(void)
{
    char *tmp;
    struct stat buf;
 
    if (NULL == path_sep) {
        printf("test2 cannot be run\n");
        return(false);
    }
    tmp = opal_os_path(true, "tmp", NULL);
    if (0 != mkdir(tmp, S_IRWXU)) {
        printf("test2 could not be run - directory could not be made\n");
        return(false);
    }

    if (OPAL_SUCCESS != opal_os_dirpath_create(tmp, S_IRWXU)) {
        rmdir(tmp);
        return(false);
    }

    chmod(tmp, S_IRUSR);

    if (OPAL_SUCCESS != opal_os_dirpath_create(tmp, S_IRWXU)) {
        rmdir(tmp);
        return(false);
    }

    stat(tmp, &buf);
    if (S_IRWXU != (S_IRWXU & buf.st_mode)) {
        rmdir(tmp);
        return(false);
    }

    rmdir(tmp);

    free(tmp);
    return true;
}


static bool test3(void)
{
    char *out;
    struct stat buf;
    char *a[] = { "aaa", "bbb", "ccc", NULL };

    if (NULL == path_sep) {
        printf("test3 cannot be run\n");
        return(false);
    }

    out = opal_os_path(true, a[0], a[1], a[2], NULL);
    if (OPAL_SUCCESS != opal_os_dirpath_create(out, S_IRWXU)) {
        out = opal_os_path(true, a[0], a[1], a[2], NULL);
        if (0 == stat(out, &buf))
            rmdir(out);
        out = opal_os_path(true, a[0], a[1], NULL);
        if (0 == stat(out, &buf))
            rmdir(out);
        out = opal_os_path(true, a[0], NULL);
        if (0 == stat(out, &buf))
            rmdir(out);
        return(false);
    }

    free(out);

    return(true);
}

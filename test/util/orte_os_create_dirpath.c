/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/constants.h"
#include "util/sys_info.h"
#include "opal/util/os_path.h"
#include "opal/util/os_create_dirpath.h"
#include "support.h"

static bool test1(void);   /* trivial test */
static bool test2(void);   /* test existing path, both with and without correct mode */
static bool test3(void);   /* test making a directory tree */


int main(int argc, char* argv[])
{
    test_init("opal_os_create_dirpath_t");

    orte_sys_info(); /* initialize system info */

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
    return 0;

}


static bool test1(void)
{

    /* Test trivial functionality. Program should return OMPI_ERROR when called with NULL path. */

    if (OMPI_ERROR != opal_os_create_dirpath(NULL, S_IRWXU))
            return(false);

    return true;
}


static bool test2(void)
{
    char *tmp;
    struct stat buf;
 
    if (NULL == orte_system_info.path_sep) {
        printf("test2 cannot be run\n");
        return(false);
    }
    tmp = opal_os_path(true, "tmp", NULL);
    if (0 != mkdir(tmp, S_IRWXU)) {
        printf("test2 could not be run - directory could not be made\n");
        return(false);
    }

    if (OMPI_ERROR == opal_os_create_dirpath(tmp, S_IRWXU)) {
        rmdir(tmp);
        return(false);
    }

    chmod(tmp, S_IRUSR);

    if (OMPI_ERROR == opal_os_create_dirpath(tmp, S_IRWXU)) {
        rmdir(tmp);
        return(false);
    }

    stat(tmp, &buf);
    if (S_IRWXU != (S_IRWXU & buf.st_mode)) {
        rmdir(tmp);
        return(false);
    }

    rmdir(tmp);
    return true;
}


static bool test3(void)
{
    char *out;
    struct stat buf;
    char *a[] = { "aaa", "bbb", "ccc", NULL };

    if (NULL == orte_system_info.path_sep) {
        printf("test3 cannot be run\n");
        return(false);
    }

    out = opal_os_path(true, a[0], a[1], a[2], NULL);
    if (OMPI_ERROR == opal_os_create_dirpath(out, S_IRWXU)) {
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
    return(true);
}

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
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "opal/runtime/opal.h"
#include "opal/util/os_path.h"
#include "support.h"

#ifdef __WINDOWS__
#define PATH_SEP "\\"
#else
#define PATH_SEP "/"
#endif

static const char *path_sep = PATH_SEP;

static bool test1(void);   /* trivial answer test */
static bool test2(void);   /* relative path test */
static bool test3(void);   /* absolute path test */
static bool test4(void);   /* missing path separator test */


int main(int argc, char* argv[])
{
    opal_init(&argc, &argv);

    test_init("opal_os_path_t");
    
    if (test1()) {
        test_success();
    }
    else {
      test_failure("opal_os_path_t test1 failed");
    }

    if (test2()) {
        test_success();
    }
    else {
      test_failure("opal_os_path_t test2 failed");
    }

    if (test3()) {
        test_success();
    }
    else {
      test_failure("opal_os_path_t test3 failed");
    }

    if (test4()) {
        test_success();
    }
    else {
      test_failure("opal_os_path_t test4 failed");
    }

    opal_finalize();

    test_finalize();
    return 0;
}


static bool test1(void)
{
    char *out, answer[100];

    /* Test trivial functionality. Program should return ".[separator]" when called in relative
     * mode, and the separator character when called in absolute mode. */
    if (NULL != (out = opal_os_path(true,NULL))) {
        answer[0] = '\0';
        strcat(answer, ".");
        strcat(answer, path_sep);
        if (0 != strcmp(answer, out))
            return(false);
        free(out);
    }
    if (NULL != (out = opal_os_path(false,NULL))) {
        if (0 != strcmp(path_sep, out))
            return(false);
        free(out);
    }

    return true;
}


static bool test2(void)
{
    char out[1024];
    char *tmp;
    char *a[] = { "aaa", "bbb", "ccc", NULL };
 
    if (NULL == path_sep) {
        printf("test2 cannot be run\n");
        return(false);
    }

    /* Construct a relative path name and see if it comes back correctly. Check multiple depths. */
    out[0] = '\0';
    strcat(out, ".");
    strcat(out, path_sep);
    strcat(out, a[0]);

    tmp = opal_os_path(true, a[0], NULL);
    if (0 != strcmp(out, tmp))
        return(false);
    free(tmp);

    strcat(out, path_sep);
    strcat(out, a[1]);
    tmp = opal_os_path(true, a[0], a[1], NULL);
    if (0 != strcmp(out, tmp))
        return(false);
    free(tmp);

    strcat(out, path_sep);
    strcat(out, a[2]);
    tmp = opal_os_path(true, a[0], a[1], a[2], NULL);
    if (0 != strcmp(out, tmp))
        return(false);
    free(tmp);

    return true;
}


static bool test3(void)
{
    char out[1024];
    char *tmp;
    char *a[] = { "aaa", "bbb", "ccc", NULL };

    if (NULL == path_sep) {
        printf("test3 cannot be run\n");
        return(false);
    }

    /* Same as prior test, only with absolute path name */
    out[0] = '\0';
    strcat(out, path_sep);
    strcat(out, a[0]);
    tmp = opal_os_path(false, a[0], NULL);
    if (0 != strcmp(out, tmp))
        return(false);
    free(tmp);

    strcat(out, path_sep);
    strcat(out, a[1]);
    tmp = opal_os_path(false, a[0], a[1], NULL);
    if (0 != strcmp(out, tmp))
        return(false);
    free(tmp);

    strcat(out, path_sep);
    strcat(out, a[2]);
    tmp = opal_os_path(false, a[0], a[1], a[2], NULL);
    if (0 != strcmp(out, tmp))
        return(false);
    free(tmp);

    return true;
}

static bool test4(void)
{
    char a[MAXPATHLEN + 10];
    int i;

    if (NULL == path_sep) {
        printf("test4 cannot be run\n");
        return(false);
    }

    for (i=0; i< MAXPATHLEN+5; i++) {
        a[i] = 'a';
    }
    a[i] = '\0';
    if (NULL != opal_os_path(false, a, NULL)) {
        return(false);
    }
    return (true);
}
